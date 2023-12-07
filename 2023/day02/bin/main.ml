open Day02
open Printf

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

exit (main ())
