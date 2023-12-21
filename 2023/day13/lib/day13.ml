open Base
open Batteries

type pattern =
  | Horizontal of string list
  | Vertical of string list

let differing_chars (str1 : string) (str2 : string) : int =
  let rec aux diffs = function
    | [], [] -> diffs
    | hd1 :: tl1, hd2 :: tl2 ->
      if Char.equal hd1 hd2
      then aux diffs (tl1, tl2)
      else aux (diffs + 1) (tl1, tl2)
    | _ -> failwith "unreachable"
  in
  aux 0 (String.explode str1, String.explode str2)
;;

let folds_equal ~(smudges : int) : string list * string list -> bool =
  let rec aux (diffs : int) (found_one_or_more : bool) = function
    | hd1 :: tl1, hd2 :: tl2 ->
      (match diffs, differing_chars hd1 hd2 with
       | n, 0 when n <= smudges -> aux diffs true (tl1, tl2)
       | 0, 1 -> aux 1 true (tl1, tl2)
       | _ -> false)
    | _ -> found_one_or_more && Int.equal diffs smudges
  in
  aux 0 false
;;

let reflected_lines_count
  (comp_fun : string list * string list -> bool)
  (lst : string list)
  : int
  =
  let make_reflections i =
    let a, b = List.split_at i lst in
    List.rev a, b
  in
  let rec aux i =
    if Int.equal i (List.length lst)
    then 0
    else if comp_fun (make_reflections i)
    then i
    else aux (i + 1)
  in
  aux 0
;;

let summarize_patterns (comp_fun : string list * string list -> bool)
  : pattern list -> int
  =
  List.map (function
    | Vertical lst -> 1 * reflected_lines_count comp_fun lst
    | Horizontal lst -> 100 * reflected_lines_count comp_fun lst)
  %> List.sum
;;

let transpose_lines (lst : string list) : string list =
  let make_vert_line i = List.map (fun s -> s.[i - 1]) %> String.of_list in
  let rec aux acc = function
    | 0 -> acc
    | i -> aux (make_vert_line i lst :: acc) (i - 1)
  in
  aux [] (String.length (List.nth lst 0))
;;

let (parse : string list -> pattern list) =
  String.concat "\n"
  %> String.split_on_string ~by:"\n\n"
  %> List.fold_left
       (fun (acc : pattern list) segment ->
         let lines = String.split_on_char '\n' segment in
         Vertical (transpose_lines lines) :: Horizontal lines :: acc)
       []
;;

let%test_unit "test input" =
  let input =
    [ "#.##..##."
    ; "..#.##.#."
    ; "##......#"
    ; "##......#"
    ; "..#.##.#."
    ; "..##..##."
    ; "#.#.##.#."
    ; ""
    ; "#...##..#"
    ; "#....#..#"
    ; "..##..###"
    ; "#####.##."
    ; "#####.##."
    ; "..##..###"
    ; "#....#..#"
    ]
  in
  let patterns = parse input in
  [%test_eq: int] (summarize_patterns (folds_equal ~smudges:0) patterns) 405;
  [%test_eq: int] (summarize_patterns (folds_equal ~smudges:1) patterns) 400
;;
