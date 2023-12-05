open Printf

type range = {
    start : int;
    stop : int;
}

module RangeMap = Map.Make(struct
    type t = range
    let compare = compare
end)

let (get_seed_number : int -> int RangeMap.t list -> int) =
    List.fold_left (fun seed product_map ->
        RangeMap.fold (fun (r : range) (target : int) acc ->
            if (r.start <= seed) && (seed <= r.stop) then (seed + target)
            else acc
        ) product_map seed
    )

let lowest_seed_number (maps : int RangeMap.t list) (seeds : int list) : int =
    List.fold_left (fun min seed ->
        Int.min min (get_seed_number seed maps)
    ) Int.max_int seeds
;;

let (parse : string list -> int list * int RangeMap.t list) =
    let extract_header_seeds input =
        match String.split_on_char ':' input with
        | [_; last] -> Str.split (Str.regexp_string " ") last |> List.map int_of_string
        | _ -> failwith "unreachable"
    in
    let build_map = List.map (fun (product_map : string) ->
        List.fold_left (fun (acc : int RangeMap.t) (line : string) ->
            match String.split_on_char ' ' line |> List.map int_of_string with
            | [dest_start; source_start; span] ->
                let start = source_start in
                let stop = source_start + span - 1 in
                RangeMap.add {start; stop} (dest_start - source_start) acc
            | _ -> failwith "unreachable"
        ) RangeMap.empty (String.split_on_char '\n' product_map |> List.tl)
    )
    in
    function
    | header :: rest -> (extract_header_seeds header , build_map rest)
    | _ -> failwith "unreachable"
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let seeds, product_maps =
        read_lines []
        |> String.concat "\n"
        |> Str.split (Str.regexp_string "\n\n")
        |> parse
    in
    printf "p1: %d\n" @@ lowest_seed_number product_maps seeds;
    (*
    printf "p2: %d\n" @@ lowest_seed_number parsed_lines seeds
    *)
    0
;;

let () =
    exit (main ())
