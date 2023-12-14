open Day06
open Printf

let main () : int =
  let limits, records = In_channel.input_lines stdin |> parse in
  printf "p1: %.0f\n" @@ intersect_diffs_product limits records;
  printf "p2: %.0f\n" @@ intersect_diff (concat_f limits) (concat_f records);
  0
;;

exit (main ())
