open Base
open Batteries

module IntSet = Set.Int

let sum_winning_tickets (ticket_wins : int list) : int =
    List.fold_left (fun acc wins ->
        if wins > 0 then acc + (1 lsl (wins - 1))
        else acc
    ) 0 ticket_wins
;;

let num_recurse_tickets (ticket_wins : int list) : int =
    let rec get_extras (amt : int) =
        function
        | [], _ -> []
        | rest, 0 -> rest
        | hd :: tl, n -> (hd + amt) :: get_extras amt (tl, n - 1)
    in
    let extras = List.make (List.length ticket_wins) 1 in
    let rec loop acc i =
        function
        | [] -> acc
        | hd :: tl ->
            loop (acc + hd) (i + 1) (get_extras hd (tl, List.nth ticket_wins i))
    in
    loop 0 0 extras
;;

let parse (line : string) : int =
    let parse_ticket_side str =
        Str.full_split (Str.regexp " +") str
        |> List.fold_left (fun ticket_numbers ->
            function
            | Str.Text num -> IntSet.add (Int.of_string num) ticket_numbers
            | _ -> ticket_numbers
        ) IntSet.empty
    in
    let strip_label ticket =
        ticket |> String.split_on_char ':' |> List.rev |> List.hd
    in
    strip_label line
    |> String.split_on_char '|'
    |> List.map parse_ticket_side
    |> function
    | [wins; scratches] -> IntSet.cardinal (IntSet.inter wins scratches)
    | _ -> failwith "unreachable"
;;

let%test_unit "test input" =
    let input = [
        "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53";
        "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19";
        "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1";
        "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83";
        "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36";
        "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";
    ]
    in
    let parsed_lines = input |> List.map parse in
    [%test_eq: int] (sum_winning_tickets parsed_lines) 13;
    [%test_eq: int] (num_recurse_tickets parsed_lines) 30;
;;
