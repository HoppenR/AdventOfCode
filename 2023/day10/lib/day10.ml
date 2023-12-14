open Base
open Batteries

type coord =
  { x : int
  ; y : int
  }

module CoordMap = Map.Make (struct
    type t = coord

    let compare range1 range2 =
      Int.compare range1.x range2.x
      |> function
      | 0 -> Int.compare range1.y range2.y
      | n -> n
    ;;
  end)

type pipe =
  | Horizontal
  | Ground
  | NE
  | NW
  | SE
  | SW
  | Start
  | Vertical

let (pipe_from_char : char -> pipe) = function
  | '-' -> Horizontal
  | '.' -> Ground
  | '7' -> NW
  | 'F' -> NE
  | 'J' -> SW
  | 'L' -> SE
  | 'S' -> Start
  | '|' -> Vertical
  | _ -> failwith "unreachable"
;;

let (find_start_pos : pipe CoordMap.t -> coord) =
  CoordMap.to_seq
  %> Seq.find (function
    | _, Start -> true
    | _ -> false)
  %> function
  | Some (c, _) -> c
  | None -> failwith "unreachable"
;;

let step_one (prev : coord) (cur : coord) : pipe -> coord = function
  | Horizontal ->
    if prev.x < cur.x
    then { cur with x = cur.x + 1 }
    else { cur with x = cur.x - 1 }
  | Ground -> failwith "unreachable"
  | NE ->
    if Int.equal prev.x cur.x
    then { cur with x = cur.x + 1 }
    else { cur with y = cur.y + 1 }
  | NW ->
    if Int.equal prev.x cur.x
    then { cur with x = cur.x - 1 }
    else { cur with y = cur.y + 1 }
  | SE ->
    if Int.equal prev.x cur.x
    then { cur with x = cur.x + 1 }
    else { cur with y = cur.y - 1 }
  | SW ->
    if Int.equal prev.x cur.x
    then { cur with x = cur.x - 1 }
    else { cur with y = cur.y - 1 }
  | Start -> { cur with y = cur.y + 1 }
  | Vertical ->
    if prev.y < cur.y
    then { cur with y = cur.y + 1 }
    else { cur with y = cur.y - 1 }
;;

let max_distance_from_start (pipe_map : pipe CoordMap.t) =
  let rec aux steps prevpos curpos =
    match CoordMap.find curpos pipe_map with
    | Start -> steps / 2
    | p -> aux (steps + 1) curpos (step_one prevpos curpos p)
  in
  let start = find_start_pos pipe_map in
  aux 1 start (step_one start start Start)
;;

let (parse : string list -> pipe CoordMap.t) =
  let aux pipe_map y line =
    String.to_seq line
    |> Seq.fold_lefti
         (fun acc x ch -> CoordMap.add { x; y } (pipe_from_char ch) acc)
         pipe_map
  in
  List.fold_lefti aux CoordMap.empty
;;

let%test_unit "test input 1a" =
  let input = [ "-L|F7"; "7S-7|"; "L|7||"; "-L-J|"; "L|-JF" ] in
  let pipe_map = parse input in
  [%test_eq: int] (max_distance_from_start pipe_map) 4
;;

let%test_unit "test input 1b" =
  let input = [ "7-F7-"; ".FJ|7"; "SJLL7"; "|F--J"; "LJ.LJ" ] in
  let pipe_map = parse input in
  [%test_eq: int] (max_distance_from_start pipe_map) 8
;;
