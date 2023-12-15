open Day15
open Printf

let main () : int =
  let init_sequence = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ hash_helper init_sequence;
  0
;;

exit (main ())
