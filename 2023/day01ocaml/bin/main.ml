open Printf
open String

type cal_dig =
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

let filter_num (digit : cal_dig) (extnum : bool) : int option =
    match (digit, extnum) with
    | (Literal v), _ -> Some v
    | (Spelled v), true -> Some v
    | _ -> None
;;

let sum_digit_line (lines : cal_dig list list) (extnum : bool) : int =
    List.fold_left (fun acc digits ->
        let fmt_hd_tl l = sprintf "%d%d" (List.hd l) (List.rev l |> List.hd) in
        let remaining = List.filter_map (fun d -> filter_num d extnum) digits in
        acc + int_of_string (fmt_hd_tl remaining)
    ) 0 lines
;;

let parse (input : string) : cal_dig list =
    let is_digit = function '0' .. '9' -> true | _ -> false in
    List.concat_map (fun line ->
        List.filter_map (fun (offset, cur) ->
            let find_pfx (substr, _) =
                try starts_with substr ~prefix:(sub line offset (length substr))
                with Invalid_argument _ -> false
            in
            if is_digit cur then
                Some (Literal (make 1 cur |> int_of_string))
            else
                match Array.find_opt find_pfx written_nums with
                | Some (_, v) -> Some (Spelled v)
                | None -> None
        ) (List.mapi (fun i c -> (i, c)) (to_seq line |> List.of_seq))
    ) (split_on_char '\n' input)
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.map parse in
    printf "p1: %d\n" @@ sum_digit_line parsed_lines false;
    printf "p2: %d\n" @@ sum_digit_line parsed_lines true;
    0
;;

let () =
    exit (main ())
