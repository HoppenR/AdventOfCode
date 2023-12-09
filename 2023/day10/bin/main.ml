open Day10
open Printf

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.map parse in
    (*
    printf "p1: %d\n" @@ sum_next_extrapolations parsed_lines;
    printf "p2: %d\n" @@ sum_prev_extrapolations parsed_lines;
    *)
    0
;;

exit (main ())
