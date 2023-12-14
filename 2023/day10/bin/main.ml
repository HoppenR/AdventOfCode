open Day10
open Printf

let main () : int =
  let pipe_map = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ max_distance_from_start pipe_map;
  0
;;

exit (main ())
