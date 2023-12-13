open Base
open Batteries

type pattern =
    | Horizontal of string list
    | Vertical of string list

let (list_part_equal : string list * string list -> bool) =
    let rec aux found_one_or_more =
        function
        | ([], _) | (_, []) -> found_one_or_more
        | (hd1 :: tl1, hd2 :: tl2) ->
            if String.equal hd1 hd2 then aux true (tl1, tl2)
            else false
    in
    aux false
;;

let nlines_before_reflection (lst : string list) : int =
    let make_reflections i =
        let a, b = List.split_at i lst in
        (List.rev a, b)
    in
    let rec aux i =
        if Int.equal i (List.length lst) then 0
        else if list_part_equal (make_reflections i) then i
        else aux (i + 1)
    in
    aux 0
;;

let (summarize_patterns : pattern list -> int) =
    List.map (
        function
        | Vertical lst -> nlines_before_reflection lst
        | Horizontal lst -> 100 * nlines_before_reflection lst
    )
    %> List.sum
;;

let transpose_lines (lst : string list) : string list =
    let make_vert_line i = List.map (fun s -> s.[i-1]) %> String.of_list in
    let rec aux acc =
        function
        | 0 -> acc
        | i -> aux (make_vert_line i lst :: acc) (i - 1)
    in
    aux [] (String.length (List.nth lst 0))
;;

let (parse : string list -> pattern list) =
    String.concat "\n"
    %> (String.split_on_string ~by:"\n\n")
    %> List.fold_left (fun (acc : pattern list) segment ->
        let lines = String.split_on_char '\n' segment in
        Vertical (transpose_lines lines)
        :: Horizontal lines
        :: acc
    ) []
;;

let%test_unit "test input" =
    let input = [
        "#.##..##.";
        "..#.##.#.";
        "##......#";
        "##......#";
        "..#.##.#.";
        "..##..##.";
        "#.#.##.#.";
        "";
        "#...##..#";
        "#....#..#";
        "..##..###";
        "#####.##.";
        "#####.##.";
        "..##..###";
        "#....#..#";
    ]
    in
    let patterns = parse input in
    [%test_eq: int] (summarize_patterns patterns) 405;
;;
