open Base
open Batteries

type _point =
  { x : int
  ; y : int
  }

type _number =
  { n : int
  ; p : _point
  ; len : int
  }

type _symbol =
  { c : char
  ; p : _point
  }

type token =
  | Symbol of _symbol
  | Number of _number

let distance_one_off (num : _number) (sym : _symbol) : bool =
  let rec loop i =
    if i = num.len
    then false
    else if abs (sym.p.x - num.p.x - i) <= 1 && abs (sym.p.y - num.p.y) <= 1
    then true
    else loop (i + 1)
  in
  loop 0
;;

let has_adjacent_symbol (num : _number) (tokens : token list) : bool =
  List.exists
    (function
      | Symbol sym -> distance_one_off num sym
      | _ -> false)
    tokens
;;

let sum_partials (tokens : token list) : int =
  List.fold_left
    (fun acc -> function
      | Number num when has_adjacent_symbol num tokens -> acc + num.n
      | _ -> acc)
    0
    tokens
;;

let get_adj_nums (symbol : _symbol) (tokens : token list) : _number list =
  List.filter_map
    (function
      | Number num when distance_one_off num symbol -> Some num
      | _ -> None)
    tokens
;;

let sum_gear_ratios (tokens : token list) : int =
  List.fold_left
    (fun acc -> function
      | Symbol sym when Char.equal sym.c '*' ->
        (match get_adj_nums sym tokens with
         | [ n1; n2 ] -> acc + (n1.n * n2.n)
         | _ -> acc)
      | _ -> acc)
    0
    tokens
;;

let parse (y : int) (line : string) : token list =
  let rec loop acc x =
    let p = { x; y } in
    function
    | [] -> List.rev acc
    | Str.Delim "." :: rest -> loop acc (x + 1) rest
    | Str.Delim sym :: rest ->
      let c = String.get sym 0 in
      loop (Symbol { c; p } :: acc) (x + 1) rest
    | Str.Text num :: rest ->
      let n = Int.of_string num in
      let len = String.length num in
      loop (Number { n; p; len } :: acc) (x + len) rest
  in
  loop [] 0 (Str.full_split (Str.regexp "[^0-9]") line)
;;

let%test_unit "test input" =
  let input =
    [ "467..114.."
    ; "...*......"
    ; "..35..633."
    ; "......#..."
    ; "617*......"
    ; ".....+.58."
    ; "..592....."
    ; "......755."
    ; "...$.*...."
    ; ".664.598.."
    ]
  in
  let tokens = input |> List.mapi parse |> List.flatten in
  [%test_eq: int] (sum_partials tokens) 4361;
  [%test_eq: int] (sum_gear_ratios tokens) 467835
;;
