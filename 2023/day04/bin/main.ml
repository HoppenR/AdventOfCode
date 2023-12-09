open Day04
open Printf

let main () : int =
    let ticket_wins = In_channel.input_lines stdin |> List.map parse in
    printf "p1: %d\n" @@ sum_winning_tickets ticket_wins;
    printf "p2: %d\n" @@ num_recurse_tickets ticket_wins;
    0
;;

exit (main ())
