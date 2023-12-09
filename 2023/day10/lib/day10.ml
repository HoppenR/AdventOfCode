open Base
open Batteries

let (parse : string -> int list) =
    String.split_on_char ' ' %> List.map Int.of_string
;;

let%test_unit "test input" =
    (*
    let input = [
        "0 3 6 9 12 15";
        "1 3 6 10 15 21";
        "10 13 16 21 30 45";
    ]
    in
    let parsed_lines = List.map parse input in
    [%test_eq: int] (sum_next_extrapolations parsed_lines) 114;
    [%test_eq: int] (sum_prev_extrapolations parsed_lines) 2;
    *)
    [%test_eq: bool] true false;

;;
