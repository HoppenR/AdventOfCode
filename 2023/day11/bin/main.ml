open Day11
open Printf

let main () : int =
  let galaxies = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ sum_shortest_paths (expand_universe 2 galaxies);
  printf "p2: %d\n" @@ sum_shortest_paths (expand_universe 1000000 galaxies);
  0
;;

exit (main ())
