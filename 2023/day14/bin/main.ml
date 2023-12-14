open Day14
open Printf

let main () : int =
  let lines = In_channel.input_lines stdin in
  let rocks = lines |> parse in
  printf "p1: %d\n" @@ rock_score (List.length lines) rocks;
  0
;;

exit (main ())
