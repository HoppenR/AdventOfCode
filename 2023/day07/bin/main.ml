open Day07
open Printf

let main () : int =
  let cards_and_bids = In_channel.input_lines stdin |> List.map parse in
  printf "p1: %d\n" @@ sum_winnings false cards_and_bids;
  printf "p2: %d\n" @@ sum_winnings true cards_and_bids;
  0
;;

exit (main ())
