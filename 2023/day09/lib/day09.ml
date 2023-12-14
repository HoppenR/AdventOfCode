open Base
open Batteries

let rec (pair_differences : int list -> int list) = function
  | [] | [ _ ] -> []
  | [ hd1; hd2 ] -> [ hd2 - hd1 ]
  | hd1 :: (hd2 :: _ as tl) -> (hd2 - hd1) :: pair_differences tl
;;

let rec (sequences : int list -> int list list) = function
  | lst when List.for_all (Int.equal 0) lst -> []
  | lst -> lst :: sequences (pair_differences lst)
;;

let (extrapolate_prev : int list list -> int) =
  List.fold_left (fun acc sequence -> List.first sequence - acc) 0
;;

let (extrapolate_next : int list list -> int) =
  List.fold_left (fun acc sequence -> List.last sequence + acc) 0
;;

let (sum_prev_extrapolations : int list list -> int) =
  List.fold_left
    (fun acc readings ->
      acc + extrapolate_prev (sequences readings |> List.rev))
    0
;;

let (sum_next_extrapolations : int list list -> int) =
  List.fold_left
    (fun acc readings -> acc + extrapolate_next (sequences readings))
    0
;;

let (parse : string -> int list) =
  String.split_on_char ' ' %> List.map Int.of_string
;;

let%test_unit "test input" =
  let input = [ "0 3 6 9 12 15"; "1 3 6 10 15 21"; "10 13 16 21 30 45" ] in
  let start_sequences = List.map parse input in
  [%test_eq: int] (sum_next_extrapolations start_sequences) 114;
  [%test_eq: int] (sum_prev_extrapolations start_sequences) 2
;;
