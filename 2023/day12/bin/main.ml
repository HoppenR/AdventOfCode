open Day12
open Printf

let main () : int =
  let spring_rows = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ sum_valid_permutations spring_rows;
  0
;;

exit (main ())
