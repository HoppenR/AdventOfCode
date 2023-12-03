open String
open Printf

type point = {x : int; y : int}

type _number = {n : int; p : point; len : int }
type _symbol = {c : char; p : point }

type token =
    | Symbol of _symbol
    | Number of _number

let rec is_adjacent_symbol (num : _number) (tokens : token list) : bool =
    match tokens with
    | [] -> false
    | Symbol sym :: rest ->
        if let rec check_distance i =
            if i = num.len then
                false
            else if abs (sym.p.x - num.p.x - i) <= 1 && abs (sym.p.y - num.p.y) <= 1 then
                true
            else
                check_distance (i + 1)
            in
            check_distance 0
        then true
        else is_adjacent_symbol num rest
    | Number _ :: rest -> is_adjacent_symbol num rest
;;

let sum_partials (tokens : token list) : int =
    List.fold_left (fun acc ->
        function
        | Number num when is_adjacent_symbol num tokens -> acc + num.n
        | _ -> acc
    ) 0 tokens
;;

let parse (y : int) (line : string) : token list =
    let delimiter_pattern = Str.regexp "[^0-9]" in
    let rec extract (acc, x) =
        let p = { x; y } in
        function
        | [] -> List.rev acc
        | Str.Delim "." :: rest ->
            extract (acc, x + 1) rest
        | Str.Delim pre :: rest ->
            extract (Symbol {c = (get pre 0); p} :: acc, x + 1) rest
        | Str.Text n :: rest ->
            let len = length n in
            extract (Number {n = (int_of_string n);  p; len} :: acc, x + len) rest
    in
    extract ([], 0) (Str.full_split delimiter_pattern line)
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.mapi parse |> List.flatten in
    printf "p1: %d\n" @@ sum_partials parsed_lines;
    0
;;

let () =
    exit (main ())
