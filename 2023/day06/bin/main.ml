open Printf
open Batteries

let intersect_diff (timelimit : float) (record : float) : float =
    let discriminant = Float.pow timelimit 2.0 -. 4.0 *. record in
    let root_low = (timelimit -. sqrt discriminant) /. 2.0 in
    let root_high = (timelimit +. sqrt discriminant) /. 2.0 in
    Float.floor root_high -. Float.ceil root_low +. 1.
;;

let (intersect_diffs_product : float list -> float list -> float) =
    List.fold_left2 (fun acc timelimit record ->
        acc *. intersect_diff timelimit record
    ) 1.0
;;

let (parse : string -> float list) =
    let strip_label = String.split_on_char ':' %> List.rev %> List.hd in
    let rec loop acc =
        function
        | [] -> List.rev acc
        | Str.Text sym :: rest -> loop (float_of_string sym :: acc) rest
        | Str.Delim _ :: rest -> loop acc rest
    in
    strip_label %> Str.full_split (Str.regexp "[^0-9]") %> loop []
;;

let (concat_f : float list -> float) =
    List.map (sprintf "%.0f") %> String.concat "" %> float_of_string
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let limits, records =
        match read_lines [] |> List.map parse with
        | [limits; records] -> (limits, records)
        | _ -> failwith "unreachable"
    in
    printf "p1: %.0f\n" @@ intersect_diffs_product limits records;
    printf "p2: %.0f\n" @@ intersect_diff (concat_f limits) (concat_f records);
    0
;;

let () =
    exit (main ())
