open Day16
open Printf

let main () : int =
  let tiles = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ traverse tiles;
  0
;;

exit (main ())
