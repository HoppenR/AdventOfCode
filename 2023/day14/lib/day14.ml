open Base
open Batteries

type coord = {
    x : int;
    y : int;
}

type rock =
    | Cube of coord
    | Round of coord

let is_rock_on_coord (coord : coord) : (rock -> bool) =
    function
    | Cube cb -> Int.equal coord.x cb.x && Int.equal coord.y cb.y
    | Round rd -> Int.equal coord.x rd.x && Int.equal coord.y rd.y
;;

let rock_iteration (rocks : rock list) : (rock list) * bool =
    let has_changed = ref false in
    let rocks' = List.map (
        function
        | Cube coord -> Cube coord
        | Round coord ->
            let next_coord = { coord with y = coord.y - 1 } in
            match List.find_opt (is_rock_on_coord next_coord) rocks with
            | None when next_coord.y >= 0 ->
                has_changed := true;
                Round next_coord
            | Some _ | None -> Round coord
    ) rocks
    in
    (rocks', !has_changed)
;;

let (iterate_rocks_until_stop : rock list -> rock list) =
    let rec aux lst =
        match rock_iteration lst with
        | (lst', true) -> aux lst'
        | (_, false) -> lst
    in
    aux
;;

let rock_score (max_score : int) : (rock list -> int) =
    iterate_rocks_until_stop
    %> List.map (
        function
        | Round c -> max_score - c.y
        | _ -> 0
    )
    %> List.sum
;;

let (parse : string list -> rock list) =
    List.mapi (fun y ->
        String.explode
        %> List.filteri_map (fun x ->
            function
            | '#' -> Some (Cube { x; y })
            | 'O' -> Some (Round { x; y })
            | '.' -> None
            | _ -> failwith "unreachable"
        )
    )
    %> List.concat
;;

let%test_unit "test input" =
    let input = [
        "O....#....";
        "O.OO#....#";
        ".....##...";
        "OO.#O....O";
        ".O.....O#.";
        "O.#..O.#.#";
        "..O..#O..O";
        ".......O..";
        "#....###..";
        "#OO..#....";
    ]
    in
    let rocks = parse input in
    [%test_eq: int] (rock_score (List.length input) rocks) 136;
;;
