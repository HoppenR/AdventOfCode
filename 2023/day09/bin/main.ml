open Day09
open Printf

let main () : int =
  let start_sequences = In_channel.input_lines stdin |> List.map parse in
  printf "p1: %d\n" @@ sum_next_extrapolations start_sequences;
  printf "p2: %d\n" @@ sum_prev_extrapolations start_sequences;
  0
;;

exit (main ())
