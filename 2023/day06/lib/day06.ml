open Base
open Batteries

let intersect_diff (timelimit : float) (record : float) : float =
    let discriminant = Float.pow timelimit 2.0 -. 4.0 *. record in
    let root_low = (timelimit -. Float.sqrt discriminant) /. 2.0 in
    let root_high = (timelimit +. Float.sqrt discriminant) /. 2.0 in
    Float.ceil root_high -. Float.floor root_low -. 1.
;;

let (intersect_diffs_product : float list -> float list -> float) =
    List.fold_left2 (fun acc timelimit record ->
        acc *. intersect_diff timelimit record
    ) 1.0
;;

let (parse : string list -> float list * float list) =
    let rec loop acc =
        function
        | [] -> List.rev acc
        | Str.Text sym :: rest -> loop (Float.of_string sym :: acc) rest
        | Str.Delim _ :: rest -> loop acc rest
    in
    List.map (
        String.split_on_char ':'
        %> List.rev
        %> List.hd
        %> Str.full_split (Str.regexp "[^0-9]") %> loop []
    ) %> function
    | [limits; records] -> (limits, records)
    | _ -> failwith "unreachable"
;;

let (concat_f : float list -> float) =
    List.map (Printf.sprintf "%.0f") %> String.concat "" %> Float.of_string
;;

let%test_unit "test input" =
    let input = [
        "Time:      7  15   30";
        "Distance:  9  40  200";
    ]
    in
    let limits, records = parse input in
    [%test_eq: float] (intersect_diffs_product limits records) 288.0;
    [%test_eq: float] (intersect_diff (concat_f limits) (concat_f records)) 71503.0;
;;
