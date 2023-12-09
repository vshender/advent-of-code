(** Day 7: Camel Cards *)

open List

(** [group lst] groups consecutive duplicate elements in [lst]. *)
let group lst =
  let rec group_list_iter acc cur = function
    | []      -> filter_map Fun.id [cur] @ acc
    | y :: tl ->
      match cur with
      | None            -> group_list_iter acc (Some (1, y)) tl
      | Some (count, x) ->
        if x = y then
          group_list_iter acc (Some (count + 1, y)) tl
        else
          group_list_iter ((count, x) :: acc) (Some (1, y)) tl
  in group_list_iter [] None lst

(** [card_rank joker card] gets the rank of [card]. *)
let card_rank joker = function
  | '2'..'9' as c -> c
  | 'T'           -> 'A'
  | 'J'           -> if joker then '1' else 'B'
  | 'Q'           -> 'C'
  | 'K'           -> 'D'
  | 'A'           -> 'E'
  | _             -> failwith "card_rank"

(** [hand_type_rank joker hand] calculates the rank of the hand type of
    [hand]. *)
let hand_type_rank joker hand =
  (* Convert to list. *)
  let hand' = hand |> String.to_seq |> of_seq in
  (* Filter out jokers. *)
  let joker_count, hand' = if joker then
      filter ((=) 'J') hand' |> length, filter ((<>) 'J') hand'
    else
      0, hand'
  in
  (* Group the cards. *)
  let hand' = hand' |> sort Stdlib.compare |> group |> sort Stdlib.compare |> rev in
  (* Apply jokers. *)
  let hand' = match hand' with
    | (n, c) :: tl -> (n + joker_count, c) :: tl
    | []           -> [(5, 'J')]
  in
  match hand' with
  | (5, _) :: _           -> 6  (* five of a kind *)
  | (4, _) :: _           -> 5  (* four of a kind *)
  | (3, _) :: (2, _) :: _ -> 4  (* full house *)
  | (3, _) :: _           -> 3  (* three of a kind *)
  | (2, _) :: (2, _) :: _ -> 2  (* two pair *)
  | (2, _) :: _           -> 1  (* one pair *)
  | (1, _) :: _           -> 0  (* high card *)
  | _                     -> failwith "hand_type_rank"

(** [compare joker hand1 hand2] compares [hand1] and [hand2]. *)
let compare joker hand1 hand2 =
  Stdlib.compare
    (hand_type_rank joker hand1, String.map (card_rank joker) hand1)
    (hand_type_rank joker hand2, String.map (card_rank joker) hand2)

(** [parse_hand_bid line] parses [line] and gets the hand and its corresponding
    bid. *)
let parse_hand_bid line =
  match String.split_on_char ' ' line with
  | [hand; bid] -> hand, int_of_string bid
  | _           -> failwith "parse_hand_bid"

(** [calc joker hand_bids] calculates the total winning. *)
let calc joker hand_bids =
  hand_bids
  |> sort (fun (hand1, _) (hand2, _) -> compare joker hand1 hand2)
  |> mapi (fun i (_, bid) -> (i + 1) * bid)
  |> fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let hand_bids = List.map parse_hand_bid lines in
  Printf.printf "Part One: %d\n" (calc false hand_bids);
  Printf.printf "Part Two: %d\n" (calc true hand_bids)
