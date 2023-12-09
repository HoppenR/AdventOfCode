open Base
open Batteries

let rec (pair_differences : int list -> int list) =
    function
    | [] | [_] -> []
    | [hd1; hd2] -> [hd2 - hd1]
    | hd1 :: (hd2 :: _ as tl) -> hd2 - hd1 :: pair_differences tl
;;

let rec (sequences : int list -> int list list) =
    function
    | lst when List.for_all (Int.equal 0) lst -> []
    | lst -> lst :: sequences (pair_differences lst)
;;

let (extrapolate : int list list -> int) =
    List.fold_left (fun acc -> List.last %> (+) acc) 0
;;

let (extrapolate_and_sum : int list list -> int) =
    List.fold_left (fun acc -> sequences %> List.rev %> extrapolate %> (+) acc) 0
;;

let (parse : string -> int list) =
    String.split_on_char ' ' %> List.map Int.of_string
;;

let%test_unit "test input" =
    let input = [
        "0 3 6 9 12 15";
        "1 3 6 10 15 21";
        "10 13 16 21 30 45";
    ]
    in
    let parsed_lines = List.map parse input in
    [%test_eq: int] (extrapolate_and_sum parsed_lines) 114;
;;
