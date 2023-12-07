open Day06
open Printf

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let limits, records = read_lines [] |> parse in
    printf "p1: %.0f\n" @@ intersect_diffs_product limits records;
    printf "p2: %.0f\n" @@ intersect_diff (concat_f limits) (concat_f records);
    0
;;

exit (main ())
