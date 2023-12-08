open Base
open Batteries

type dir =
    | Left
    | Right

module Network = Map.String

type network = (string * string) Network.t

let rec gcd (a : int) (b : int) =
    if b = 0 then a
    else gcd b (Int.rem a b)
;;

let lcm (a : int) (b : int) =
    abs (a * b) / gcd a b
;;

let walk (dirs : dir list) (net : network) (pred : string -> bool) (start : string) : int =
    let step (location, steps) =
        let location' = Network.find location net in
        function
        | Left -> (Tuple2.first location', steps + 1)
        | Right -> (Tuple2.second location', steps + 1)
    in
    let rec aux state =
        match List.fold_left step state dirs with
        | (position, steps) when pred position -> steps
        | state' -> aux state'
    in
    aux (start, 0)
;;

let walk_AAA_ZZZ (dirs : dir list) (net : network) : int =
    walk dirs net (String.equal "ZZZ") "AAA"
;;

let walk_A_Z_in_parts (dirs : dir list) (net : network) : int =
    let third_char_equal ch str = Char.equal str.[2] ch in
    Network.keys net
    |> Enum.filter(third_char_equal 'A')
    |> Enum.map (walk dirs net (third_char_equal 'Z'))
    |> Enum.fold lcm 1
;;

let (parse : string list -> dir list * network) =
    let rec (char_to_dirs : char list -> dir list) =
        function
        | [] -> []
        | 'L' :: rest -> Left :: char_to_dirs rest
        | 'R' :: rest -> Right :: char_to_dirs rest
        | _ -> failwith "unreachable"
    in
    let (build_net : string list -> (string * string) Network.t) =
        List.fold_left (fun net line ->
            Scanf.sscanf line "%3s = (%3s, %3s)" (fun from left right ->
                Network.add from (left, right) net
            )
        ) Network.empty
    in
    function
    | hd :: _ :: tl -> (char_to_dirs (String.explode hd), build_net tl)
    | _ -> failwith "unreachable"
;;

let%test_unit "test input 1a" =
    let input = [
        "RL";
        "";
        "AAA = (BBB, CCC)";
        "BBB = (DDD, EEE)";
        "CCC = (ZZZ, GGG)";
        "DDD = (DDD, DDD)";
        "EEE = (EEE, EEE)";
        "GGG = (GGG, GGG)";
        "ZZZ = (ZZZ, ZZZ)";
    ]
    in
    let directions, network = parse input in
    [%test_eq: int] (walk_AAA_ZZZ directions network) 2;
;;

let%test_unit "test input 1b" =
    let input = [
        "LLR";
        "";
        "AAA = (BBB, BBB)";
        "BBB = (AAA, ZZZ)";
        "ZZZ = (ZZZ, ZZZ)";
    ]
    in
    let directions, network = parse input in
    [%test_eq: int] (walk_AAA_ZZZ directions network) 6;
;;

let%test_unit "test input 2" =
    let input = [
        "LR";
        "";
        "11A = (11B, XXX)";
        "11B = (XXX, 11Z)";
        "11Z = (11B, XXX)";
        "22A = (22B, XXX)";
        "22B = (22C, 22C)";
        "22C = (22Z, 22Z)";
        "22Z = (22B, 22B)";
        "XXX = (XXX, XXX)";
    ]
    in
    let directions, network = parse input in
    [%test_eq: int] (walk_A_Z_in_parts directions network) 6;
;;
