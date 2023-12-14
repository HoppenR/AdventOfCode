open Base
open Batteries

type galaxy =
  { x : int
  ; y : int
  }

let galaxies_by_col (col : int) : galaxy list -> galaxy list =
  List.filter (fun g -> Int.equal g.x col)
;;

let galaxies_by_row (row : int) : galaxy list -> galaxy list =
  List.filter (fun g -> Int.equal g.y row)
;;

let (max_coords : galaxy list -> int * int) =
  List.fold_left
    (fun (accx, accy) galaxy -> Int.max accx galaxy.x, Int.max accy galaxy.y)
    (0, 0)
;;

let move_galaxies (xoff : int) (yoff : int) : galaxy list -> galaxy list =
  List.map (fun g -> { x = g.x + xoff; y = g.y + yoff })
;;

let manhattan_distance (g1 : galaxy) (g2 : galaxy) =
  Int.abs (g2.y - g1.y) + Int.abs (g2.x - g1.x)
;;

let sum_shortest_paths (galaxies : galaxy list) : int =
  let rec aux acc = function
    | i, _ when Int.equal i (List.length galaxies) -> acc
    | i, j when Int.equal j (List.length galaxies) -> aux acc (i + 1, i + 2)
    | i, j ->
      let gi = List.nth galaxies i in
      let gj = List.nth galaxies j in
      aux (acc + manhattan_distance gi gj) (i, j + 1)
  in
  aux 0 (0, 1)
;;

let expand_universe (offset : int) (orig_galaxies : galaxy list) : galaxy list =
  let max_coords_x, max_coords_y = max_coords orig_galaxies in
  let rec aux_horizontal xoff i galaxies : galaxy list =
    if Int.equal i (max_coords_x + 1)
    then []
    else (
      match galaxies_by_col i galaxies with
      | [] -> aux_horizontal (xoff + offset - 1) (i + 1) galaxies
      | g -> move_galaxies xoff 0 g @ aux_horizontal xoff (i + 1) galaxies)
  in
  let rec aux_vertical yoff i galaxies : galaxy list =
    if Int.equal i (max_coords_y + 1)
    then []
    else (
      match galaxies_by_row i galaxies with
      | [] -> aux_vertical (yoff + offset - 1) (i + 1) galaxies
      | g -> move_galaxies 0 yoff g @ aux_vertical yoff (i + 1) galaxies)
  in
  orig_galaxies |> aux_horizontal 0 0 |> aux_vertical 0 0
;;

let parse (lines : string list) : galaxy list =
  List.fold_lefti
    (fun acc y ->
      String.to_seqi
      %> Seq.filter_map (fun (x, c) ->
        match c with
        | '#' -> Some { x; y }
        | '.' -> None
        | _ -> failwith "unreachable")
      %> List.of_seq
      %> ( @ ) acc)
    []
    lines
;;

let%test_unit "test input" =
  let input =
    [ "...#......"
    ; ".......#.."
    ; "#........."
    ; ".........."
    ; "......#..."
    ; ".#........"
    ; ".........#"
    ; ".........."
    ; ".......#.."
    ; "#...#....."
    ]
  in
  let parsed_lines = parse input in
  [%test_eq: int] (sum_shortest_paths (expand_universe 2 parsed_lines)) 374;
  [%test_eq: int] (sum_shortest_paths (expand_universe 10 parsed_lines)) 1030;
  [%test_eq: int] (sum_shortest_paths (expand_universe 100 parsed_lines)) 8410
;;
