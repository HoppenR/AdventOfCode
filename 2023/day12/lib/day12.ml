open Base
open Batteries

type spring =
  | Unknown
  | Operational
  | Damaged

let rec (generate_permutations : spring list -> spring list list) = function
  | [] -> [ [] ]
  | hd :: tl ->
    (match hd with
     | Operational | Damaged ->
       List.map (fun perm -> hd :: perm) (generate_permutations tl)
     | Unknown ->
       let opt_operational =
         List.map (fun perm -> Operational :: perm) (generate_permutations tl)
       in
       let opt_damaged =
         List.map (fun perm -> Damaged :: perm) (generate_permutations tl)
       in
       opt_operational @ opt_damaged)
;;

let valid_permutations ((springs, sizes) : spring list * int list) : int =
  let rec count_group_sizes (count : int) = function
    | [] when count > 0 -> [ count ]
    | [] -> []
    | Operational :: tl when count > 0 -> count :: count_group_sizes 0 tl
    | Operational :: tl -> count_group_sizes 0 tl
    | Damaged :: tl -> count_group_sizes (count + 1) tl
    | Unknown :: _ -> failwith "unreachable"
  in
  List.fold_left
    (fun acc perm ->
      acc
      + if List.equal Int.equal (count_group_sizes 0 perm) sizes then 1 else 0)
    0
    (generate_permutations springs)
;;

let (sum_valid_permutations : (spring list * int list) list -> int) =
  List.fold_left (fun acc -> valid_permutations %> ( + ) acc) 0
;;

let parse (lines : string list) : (spring list * int list) list =
  let parse_springs =
    String.explode
    %> List.map (function
      | '?' -> Unknown
      | '.' -> Operational
      | '#' -> Damaged
      | _ -> failwith "unreachable")
  in
  List.map
    (fun line ->
      let groups, sizes = String.split ~by:" " line in
      ( parse_springs groups
      , String.split_on_char ',' sizes |> List.map Int.of_string ))
    lines
;;

let%test_unit "test input" =
  let input =
    [ "???.### 1,1,3"
    ; ".??..??...?##. 1,1,3"
    ; "?#?#?#?#?#?#?#? 1,3,1,6"
    ; "????.#...#... 4,1,1"
    ; "????.######..#####. 1,6,5"
    ; "?###???????? 3,2,1"
    ]
  in
  let spring_rows = parse input in
  [%test_eq: int] (sum_valid_permutations spring_rows) 21
;;
