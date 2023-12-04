open Batteries
open Printf

module Set = Stdlib.Set.Make(struct type t = int let compare = compare end)

type ticket = {
    wins : Set.t;
    scratches : Set.t;
}

let sum_winning_tickets (tickets : ticket list) : int =
    List.fold_left (fun acc ticket ->
        acc + Set.fold (fun scratch acc ->
            if Set.mem scratch ticket.wins then
                acc * 2
            else
                acc
        ) ticket.scratches 1
        / 2
    ) 0 tickets
;;

let parse (line : string) : ticket =
    let rec loop acc =
        function
        | [] -> acc
        | Str.Text num :: rest -> loop (Set.add (int_of_string num) acc) rest
        | _ :: rest -> loop acc rest
    in
    let regexp = (Str.regexp "\\ +") in
    let strip_label game = String.split_on_char ':' game |> List.rev |> List.hd in
    let parts = strip_label line |> String.split_on_char '|' in
    let win_part, scratch_part = (List.nth parts 0, List.nth parts 1) in
    {
        wins = loop Set.empty (Str.full_split regexp win_part);
        scratches = loop Set.empty ( Str.full_split regexp scratch_part);
    }
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.map parse in
    printf "p1: %d\n" @@ sum_winning_tickets parsed_lines;
    (*
    printf "p2: %d\n" @@ sum_gear_ratios parsed_lines;
    *)
    0
;;

let () =
    exit (main ())
