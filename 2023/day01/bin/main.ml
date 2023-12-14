open Day01
open Printf

let main () : int =
  let calibration_digits = In_channel.input_lines stdin |> List.map parse in
  printf "p1: %d\n" @@ sum_digit_line calibration_digits false;
  printf "p2: %d\n" @@ sum_digit_line calibration_digits true;
  0
;;

exit (main ())
