open Day08
open Printf

let main () : int =
    let directions, network = In_channel.input_lines stdin |> parse in
    printf "p1: %d\n" @@ walk_AAA_ZZZ directions network;
    printf "p2: %d\n" @@ walk_A_Z_in_parts directions network;
    0
;;

exit (main ())
