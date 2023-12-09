open Base
open Batteries

type cdigit =
    | Literal of int
    | Spelled of int

let written_nums: (string * int) array = [|
    ("one", 1);
    ("two", 2);
    ("three", 3);
    ("four", 4);
    ("five", 5);
    ("six", 6);
    ("seven", 7);
    ("eight", 8);
    ("nine", 9);
    |]

let filter_num (digit : cdigit) (extnum : bool) : int option =
    match (digit, extnum) with
    | (Literal v, _) -> Some v
    | (Spelled v, true) -> Some v
    | _ -> None
;;

let sum_digit_line (lines : cdigit list list) (extnum : bool) : int =
    List.fold_left (fun acc digits ->
        let concat_hd_tl lst = (List.first lst) * 10 + (List.last lst) in
        let remaining = List.filter_map (fun d -> filter_num d extnum) digits in
        acc + concat_hd_tl remaining
    ) 0 lines
;;

let parse (input : string) : cdigit list =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    List.concat_map (fun line ->
        List.filter_map (fun (offset, cur) ->
            let find_pfx (substr, _) =
                try String.starts_with substr (String.sub line offset (String.length substr))
                with Invalid_argument _ -> false
            in
            if is_digit cur then
                Some (Literal (String.make 1 cur |> Int.of_string))
            else
                match Array.find_opt find_pfx written_nums with
                | Some (_, v) -> Some (Spelled v)
                | None -> None
        ) (List.mapi (fun i c -> (i, c)) (String.to_seq line |> List.of_seq))
    ) (String.split_on_char '\n' input)
;;

let%test_unit "test input 1" =
    let input = [
        "1abc2";
        "pqr3stu8vwx";
        "a1b2c3d4e5f";
        "treb7uchet";
    ]
    in
    let calibration_digits = input |> List.map parse in
    [%test_eq: int] (sum_digit_line calibration_digits false) 142;
;;

let%test_unit "test input 2" =
    let input = [
        "two1nine";
        "eightwothree";
        "abcone2threexyz";
        "xtwone3four";
        "4nineeightseven2";
        "zoneight234";
        "7pqrstsixteen";
    ]
    in
    let calibration_digits = input |> List.map parse in
    [%test_eq: int] (sum_digit_line calibration_digits true) 281;
;;
