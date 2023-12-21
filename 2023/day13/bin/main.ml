open Day13
open Printf

let main () : int =
  let patterns = In_channel.input_lines stdin |> parse in
  printf "p1: %d\n" @@ summarize_patterns (folds_equal ~smudges:0) patterns;
  printf "p2: %d\n" @@ summarize_patterns (folds_equal ~smudges:1) patterns;
  0
;;

exit (main ())
