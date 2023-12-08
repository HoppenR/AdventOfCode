open Day08
open Printf

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let directions, network = read_lines [] |> parse in
    printf "p1: %d\n" @@ walk_AAA_ZZZ directions network;
    printf "p2: %d\n" @@ walk_A_Z_in_parts directions network;
    0
;;

exit (main ())
