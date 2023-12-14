open Day02
open Printf

let main () : int =
  let games = In_channel.input_lines stdin |> List.map parse in
  printf "p1: %d\n" @@ sum_valid_ids games;
  printf "p2: %d\n" @@ sum_power_of_minimum_games games;
  0
;;

exit (main ())
