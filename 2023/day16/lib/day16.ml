open Base
open Batteries

type coord =
  { x : int
  ; y : int
  }

module CoordMap = Map.Make (struct
    type t = coord

    let compare lhs rhs =
      match Int.compare lhs.y rhs.y with
      | 0 -> Int.compare lhs.x rhs.x
      | n -> n
    ;;
  end)

type tile =
  | Empty of bool
  | MirrorFW of bool
  | MirrorBW of bool
  | SplitterHor of bool * bool
  | SplitterVer of bool * bool

type tilemap = tile CoordMap.t

type direction =
  | East
  | North
  | West
  | South

let step (pos : coord) (map : tilemap) (dir : direction) (tile : tile)
  : tilemap * direction list
  =
  match tile with
  | Empty _ -> CoordMap.add pos (Empty true) map, [ dir ]
  | MirrorFW _ ->
    let newmap = CoordMap.add pos (MirrorFW true) map in
    (match dir with
     | East -> newmap, [ North ]
     | North -> newmap, [ East ]
     | West -> newmap, [ South ]
     | South -> newmap, [ West ])
  | MirrorBW _ ->
    let newmap = CoordMap.add pos (MirrorBW true) map in
    (match dir with
     | East -> newmap, [ South ]
     | North -> newmap, [ West ]
     | West -> newmap, [ North ]
     | South -> newmap, [ East ])
  | SplitterHor (used, _) ->
    (match dir with
     | (East | West) as dir ->
       CoordMap.add pos (SplitterHor (used, true)) map, [ dir ]
     | North | South ->
       if not used
       then CoordMap.add pos (SplitterHor (true, true)) map, [ East; West ]
       else map, [])
  | SplitterVer (used, _) ->
    (match dir with
     | (North | South) as dir ->
       CoordMap.add pos (SplitterVer (used, true)) map, [ dir ]
     | East | West ->
       if not used
       then CoordMap.add pos (SplitterVer (true, true)) map, [ North; South ]
       else map, [])
;;

let merge_tilemaps_helper _ (opt1 : tile option) (opt2 : tile option)
  : tile option
  =
  match opt1, opt2 with
  | Some v1, Some v2 ->
    (match v2 with
     | Empty true
     | MirrorFW true
     | MirrorBW true
     | SplitterHor (_, true)
     | SplitterVer (_, true) -> Some v2
     | _ -> Some v1)
  | (_ as v1_opt), _ -> v1_opt
;;

let find_start_point (tiles : tilemap) : coord =
  let y = CoordMap.fold (fun c _ acc -> Int.max c.y acc) tiles 0 in
  { x = -1; y }
;;

(* NOTE: Part 2 needs memoization *)
let traverse (tiles : tilemap) =
  let rec aux (acc : tilemap) (cur : coord) (dir : direction) =
    let next_coord =
      match dir with
      | East -> { cur with x = cur.x + 1 }
      | North -> { cur with y = cur.y + 1 }
      | West -> { cur with x = cur.x - 1 }
      | South -> { cur with y = cur.y - 1 }
    in
    match CoordMap.find_opt next_coord acc with
    | Some next_tile ->
      let acc, next_dirs = step next_coord acc dir next_tile in
      List.fold_left
        (fun (acc : tilemap) (cur_dir : direction) ->
          CoordMap.merge merge_tilemaps_helper acc (aux acc next_coord cur_dir))
        acc
        next_dirs
    | None -> acc
  in
  let final_tilemap = aux tiles (find_start_point tiles) East in
  CoordMap.fold
    (fun _ (tile : tile) acc ->
      acc
      +
      match tile with
      | Empty true
      | MirrorFW true
      | MirrorBW true
      | SplitterHor (_, true)
      | SplitterVer (_, true) -> 1
      | _ -> 0)
    final_tilemap
    0
;;

(* NOTE: (0, 0) is LOWER, LEFT corner *)
let (parse : string list -> tilemap) =
  List.rev
  %> List.fold_lefti
       (fun (map : tilemap) (y : int) (line : string) ->
         String.explode line
         |> List.fold_lefti
              (fun (acc : tilemap) (x : int) (ch : char) ->
                CoordMap.add
                  { x; y }
                  (match ch with
                   | '.' -> Empty false
                   | '/' -> MirrorFW false
                   | '\\' -> MirrorBW false
                   | '-' -> SplitterHor (false, false)
                   | '|' -> SplitterVer (false, false)
                   | _ -> failwith "unreachable")
                  acc)
              map)
       CoordMap.empty
;;

let%test_unit "test input 1a" =
  let input =
    [ {|.|...\....|}
    ; {||.-.\.....|}
    ; {|.....|-...|}
    ; {|........|.|}
    ; {|..........|}
    ; {|.........\|}
    ; {|..../.\\..|}
    ; {|.-.-/..|..|}
    ; {|.|....-|.\|}
    ; {|..//.|....|}
    ]
  in
  let tiles = parse input in
  [%test_eq: int] (traverse tiles) 46
;;
