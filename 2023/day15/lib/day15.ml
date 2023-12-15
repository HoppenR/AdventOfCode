open Base
open Batteries

let (hash : string -> int) =
  String.to_seq
  %> Seq.fold_left
       (fun acc ch ->
         acc |> ( + ) (Char.code ch) |> ( * ) 17 |> flip Int.rem 256)
       0
;;

let (sum_hashes : string list -> int) = List.map hash %> List.sum

let (parse : string list -> string list) =
  List.concat_map (String.split_on_char ',')
;;

let%test_unit "test input 1a" =
  let input = [ "HASH" ] in
  let init_sequence = parse input in
  [%test_eq: int] (sum_hashes init_sequence) 52
;;

let%test_unit "test input 1b" =
  let input = [ "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" ] in
  let init_sequence = parse input in
  [%test_eq: int] (sum_hashes init_sequence) 1320
;;
