open Base
open Batteries

type range = {
    start : int;
    stop : int;
}

module RangeMap = Map.Make(struct
    type t = range
    let compare range1 range2 =
        Int.compare range1.start range2.start
        |> function
        | 0 -> Int.compare range1.stop range2.stop
        | n -> n
end)

let (get_seed_number : int -> int RangeMap.t list -> int) =
    List.fold_left (fun seed product_map ->
        RangeMap.fold (fun (r : range) (target : int) (acc : int) ->
            if (r.start <= seed) && (seed <= r.stop) then (seed + target)
            else acc
        ) product_map seed
    )
;;

let lowest_seed_number (maps : int RangeMap.t list) (seeds : int list) : int =
    List.fold_left (fun min seed ->
        Int.min min (get_seed_number seed maps)
    ) Int.max_num seeds
;;

let (find_lowest_start : range list -> int) =
    List.fold_left (fun (acc : int) (r : range) ->
        min acc r.start
    ) Int.max_num
;;

let get_new_ranges (r : range) (in_range : range) (change: int) : (range list) =
    let overlap_start = max r.start in_range.start in
    let overlap_stop = min r.stop in_range.stop in
    if overlap_start <= overlap_stop then
        let range1 = { start = r.start; stop = overlap_start - 1} in
        let range2 = { start = overlap_start + change; stop = overlap_stop + change} in
        let range3 = { start = overlap_stop + 1; stop = r.stop} in
        if r.start < overlap_start && r.stop > overlap_stop then
            [ range1; range2; range3; ]
        else if r.start < overlap_start then
            [ range1; range2; ]
        else if r.stop > overlap_stop then
            [ range2; range3; ]
        else
            [ range2; ]
    else
        [r]

let find_output_mappings (maps : int RangeMap.t list) (seeds : int list) =
    let rec seed_ranges =
        function
        | [] -> []
        | start :: span :: tl -> {start; stop = start + span - 1} :: seed_ranges tl
        | _ -> failwith "unreachable"
    in
    List.map (fun (seed_range : range) ->
        List.fold_left (fun acc product_map ->
            RangeMap.fold (fun (in_range : range) (change : int) (acc : range list) ->
                List.concat_map (fun (r : range) ->
                    get_new_ranges r in_range change
                ) acc
            ) product_map acc
        ) [seed_range] maps
    ) (seed_ranges seeds)
    |> List.flatten
;;

let (parse : string list -> int list * int RangeMap.t list) =
    let extract_header_seeds input =
        match String.split_on_char ':' input with
        | [_; last] -> Str.split (Str.regexp_string " ") last |> List.map Int.of_string
        | _ -> failwith "unreachable"
    in
    let build_map = List.map (fun (product_map : string) ->
        List.fold_left (fun (acc : int RangeMap.t) (line : string) ->
            match String.split_on_char ' ' line |> List.map Int.of_string with
            | [dest_start; source_start; span] ->
                let start = source_start in
                let stop = source_start + span - 1 in
                RangeMap.add {start; stop} (dest_start - source_start) acc
            | _ -> failwith "unreachable"
        ) RangeMap.empty (String.split_on_char '\n' product_map |> List.tl)
    )
    in
    String.concat "\n"
    %> Str.split (Str.regexp_string "\n\n")
    %> function
    | header :: rest -> (extract_header_seeds header , build_map rest)
    | _ -> failwith "unreachable"
;;

let%test_unit "test input" =
    let input = [
        "seeds: 79 14 55 13";
        "";
        "seed-to-soil map:";
        "50 98 2";
        "52 50 48";
        "";
        "soil-to-fertilizer map:";
        "0 15 37";
        "37 52 2";
        "39 0 15";
        "";
        "fertilizer-to-water map:";
        "49 53 8";
        "0 11 42";
        "42 0 7";
        "57 7 4";
        "";
        "water-to-light map:";
        "88 18 7";
        "18 25 70";
        "";
        "light-to-temperature map:";
        "45 77 23";
        "81 45 19";
        "68 64 13";
        "";
        "temperature-to-humidity map:";
        "0 69 1";
        "1 0 69";
        "";
        "humidity-to-location map:";
        "60 56 37";
        "56 93 4";
    ]
    in
    let seeds, maps = parse input in
    [%test_eq: int] (lowest_seed_number maps seeds) 35;
    [%test_eq: int] (find_lowest_start (find_output_mappings maps seeds)) 46;
;;
