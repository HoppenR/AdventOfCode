open Day05
open Printf

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let seeds, product_maps = read_lines [] |> parse in
    printf "p1: %d\n" @@ lowest_seed_number product_maps seeds;
    printf "p2: %d\n" @@ find_lowest_start (find_output_mappings product_maps seeds);
    0
;;

exit (main ())
