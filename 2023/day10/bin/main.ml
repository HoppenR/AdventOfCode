open Day10
open Printf

let main () : int =
    let parsed_lines = In_channel.input_lines stdin |> List.map parse in
    (*
    printf "p1: %d\n" @@ sum_next_extrapolations parsed_lines;
    printf "p2: %d\n" @@ sum_prev_extrapolations parsed_lines;
    *)
    0
;;

exit (main ())
