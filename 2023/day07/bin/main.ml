open Printf
open Batteries

type hand = {
    cards: char list;
    bid: int;
    rank: int;
}

let int_of_val (part2 : bool) : (char -> int) =
    function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' when not part2 -> 11
    | 'T' -> 10
    | 'J' when part2 -> 1
    | n -> int_of_char n - int_of_char '0'
;;

let compare_hands (part2 : bool) (hand1 : hand) (hand2 : hand) : int =
    let comp_val c1 c2 = compare (int_of_val part2 c1) (int_of_val part2 c2) in
    match compare hand1.rank hand2.rank with
    | 0 -> List.compare comp_val hand1.cards hand2.cards
    | n -> n
;;

let card_counts (chars : char list) : (char * int) list =
    List.filter_map(fun card ->
        let cnt = List.count_matching (Char.equal card) chars in
        if cnt > 0 then Some (card, cnt)
        else None
    ) [ '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'T'; 'J'; 'Q'; 'K'; 'A' ]
;;

let (apply_jokers : (char * int) list -> (char * int) list) =
    function
    | ('J', n1) :: (c1, n2) :: tl -> (c1, n1 + n2) :: tl
    | (ch, n1) :: ('J', n2) :: tl -> (ch, n1 + n2) :: tl
    | (ch, n1) :: e2 :: ('J', n2) :: tl -> (ch, n1 + n2) :: e2 :: tl
    | (ch, n1) :: e2 :: e3 :: ('J', n2) :: tl -> (ch, n1 + n2) :: e2 :: e3 :: tl
    | (ch, n1) :: e2 :: e3 :: e4 :: ('J', n2) :: tl -> (ch, n1 + n2) :: e2 :: e3 :: e4 :: tl
    | _ as rest -> rest
;;

let hand_of_card_count (cards : char list) (bid : int) : (char * int) list -> hand =
    function
    | (_, 5) :: _ -> {cards; bid; rank = 7}
    | (_, 4) :: _ -> {cards; bid; rank = 6}
    | (_, 3) :: (_, 2) :: _ -> {cards; bid; rank = 5}
    | (_, 3) :: _ -> {cards; bid; rank = 4}
    | (_, 2) :: (_, 2) :: _ -> {cards; bid; rank = 3}
    | (_, 2) :: _ -> {cards; bid; rank = 2}
    | (_, 1) :: _ -> {cards; bid; rank = 1}
    | _ -> failwith "unreachable"
;;

let make_hand (part2 : bool) ((cards, bid) : char list * int) : hand =
    card_counts cards
    |> List.sort (fun (_, count1) (_, count2) -> Int.compare count2 count1)
    |> (if part2 then apply_jokers else identity)
    |> hand_of_card_count cards bid
;;

let sum_winnings (part2 : bool) : (char list * int) list -> int =
    List.map (make_hand part2)
    %> List.sort (compare_hands part2)
    %> List.fold_lefti (fun acc i hand -> acc + (i + 1) * hand.bid) 0
;;

let parse (line : string) : char list * int =
    let hand_str, bid = String.split ~by:" " line in
    (String.explode hand_str, (int_of_string bid))
;;

let main () : int =
    let rec read_lines acc =
        try read_lines (read_line () :: acc)
        with End_of_file -> List.rev acc
    in
    let parsed_lines = read_lines [] |> List.map parse in
    printf "p1: %d\n" @@ sum_winnings false parsed_lines;
    printf "p2: %d\n" @@ sum_winnings true parsed_lines;
    0
;;

let () =
    exit (main ())
