open Batteries
open Printf

module Set = Stdlib.Set.Make(struct type t = int let compare = compare end)

type ticket = {
    wins : Set.t;
    scratches : Set.t;
}

let wins_per_ticket (tickets : ticket list) : int list =
    let int_of_bool b = if b then 1 else 0 in
    List.map (fun ticket ->
        Set.fold (fun scratch acc ->
            acc + int_of_bool (Set.mem scratch ticket.wins)
        ) ticket.scratches 0
    ) tickets
;;

let sum_winning_tickets (ticket_wins : int list) : int =
    List.fold_left (fun acc wins ->
        if wins > 0 then acc + 1 lsl (wins - 1)
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

let parse (line : string) : ticket =
    let parse_ticket_side str =
        Str.full_split (Str.regexp " +") str
        |> List.fold_left (fun ticket_numbers ->
            function
            | Str.Text num -> Set.add (int_of_string num) ticket_numbers
            | _ -> ticket_numbers
        ) Set.empty
    in
    let strip_label ticket =
        ticket |> String.split_on_char ':' |> List.rev |> List.hd
    in
    strip_label line |> String.split_on_char '|' |> List.map parse_ticket_side
    |> function
    | [wins; scratches] -> {wins; scratches}
    | _ -> failwith "unreachable"
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.map parse |> wins_per_ticket in
    printf "p1: %d\n" @@ sum_winning_tickets parsed_lines;
    printf "p2: %d\n" @@ num_recurse_tickets parsed_lines;
    0
;;

let () =
    exit (main ())
