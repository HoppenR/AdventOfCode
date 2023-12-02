open Printf
open String

type draw =
    | Red of int
    | Green of int
    | Blue of int

let count_set_colors (set : draw list) : int * int * int =
    List.fold_left (fun (num_r, num_g, num_b) color ->
        match color with
        | Red amt -> (num_r + amt, num_g, num_b)
        | Green amt -> (num_r, num_g + amt, num_b)
        | Blue amt -> (num_r, num_g, num_b + amt)
    ) (0, 0, 0) set
;;

let is_valid_set (set : draw list) : bool =
    let num_r, num_g, num_b = count_set_colors set in
    num_r <= 12 && num_g <= 13 && num_b <= 14
;;

let sum_valid_ids (games : draw list list list) : int =
    let games_with_ids = List.mapi (fun i game -> (i + 1, game)) games in
    List.fold_left (fun acc (gameid, game) ->
        if List.for_all is_valid_set game then acc + gameid else acc
    ) 0 games_with_ids
;;

let minimum_cubes_needed (game : draw list list) : int * int * int  =
    List.fold_left (fun (min_r, min_g, min_b) set ->
        let min_r', min_g', min_b' = count_set_colors set in
        (max min_r min_r', max min_g min_g', max min_b min_b')
    ) (0, 0, 0) game
;;

let sum_power_of_minimum_games (games : draw list list list) : int =
    List.fold_left (fun acc game ->
        let min_r, min_g, min_b = minimum_cubes_needed game in
        acc + min_r * min_g * min_b
    ) 0 games
;;

let parse (line : string) : draw list list =
    let strip_label game = split_on_char ':' game |> List.rev |> List.hd in
    List.map (fun set ->
        List.map (fun reveal ->
            Scanf.sscanf reveal " %d %s" (fun number color ->
                match (number, color) with
                | (amt, "red") -> Red(amt)
                | (amt, "green") -> Green(amt)
                | (amt, "blue") -> Blue(amt)
                | _ -> failwith "unreachable"
            )
        ) (split_on_char ',' set)
    ) (line |> strip_label |> split_on_char ';')
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.map parse in
    printf "p1: %d\n" @@ sum_valid_ids parsed_lines;
    printf "p2: %d\n" @@ sum_power_of_minimum_games parsed_lines;
    0
;;

let () =
    exit (main ())
