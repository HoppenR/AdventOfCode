open Day03
open Printf

let main () : int =
    let tokens = In_channel.input_lines stdin |> List.mapi parse |> List.flatten in
    printf "p1: %d\n" @@ sum_partials tokens;
    printf "p2: %d\n" @@ sum_gear_ratios tokens;
    0
;;

exit (main ())
