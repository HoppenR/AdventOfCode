open Day01
open Printf

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

exit (main ())
