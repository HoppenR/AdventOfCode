open Day05
open Printf

let main () : int =
  let seeds, maps = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ lowest_seed_number maps seeds;
  printf "p2: %d\n" @@ find_lowest_start (find_output_mappings maps seeds);
  0
;;

exit (main ())
