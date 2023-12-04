(** Day 4: Scratchcards *)

(** [parse_scratchcard card] parses a string representation of a
    scratchcard. *)
let parse_scratchcard card =
  let open Str in
  match split (regexp ": *") card with
  | [_; numbers] ->
    (match split (regexp "| *") numbers with
     | [winning_numbers; your_numbers] ->
       (winning_numbers |> split (regexp " +") |> List.map int_of_string,
        your_numbers |> split (regexp " +") |> List.map int_of_string)
     | _ -> failwith "parse_scratchcard")
  | _ -> failwith "parse_scratchcard"

(** [inter a b] gets the intersection of the lists [a] and [b]. *)
let inter a b =
  List.filter (fun x -> List.mem x a) b

(** [scratchcard_value card] calculates the value of [card]. *)
let scratchcard_value (winning_numbers, your_numbers) =
  match List.length (inter winning_numbers your_numbers) with
  | 0 -> 0
  | n -> 1 lsl (n - 1)

(** [part1 scratchcards] calculates the total value of [scratchcards]. *)
let part1 scratchcards =
  scratchcards |> List.map scratchcard_value |> List.fold_left (+) 0

(** [scratchcard_matching_numbers card] gets the number of matching numbers of
    [card]. *)
let scratchcard_matching_numbers (winning_numbers, your_numbers) =
  List.length (inter winning_numbers your_numbers)

(** [add_to_first_items x n lst] adds the given integer [x] to the first [n]
    elements of [lst]. *)
let rec add_to_first_items x n lst =
  match n, lst with
  | 0, _      -> lst
  | _, []     -> x :: add_to_first_items x (n - 1) []
  | _, h :: t -> x + h :: add_to_first_items x (n - 1) t

(** [part2 scratchcards] processes [scratchcards] and calculates how many total
    scratchcards you end up with. *)
let part2 scratchcards =
  scratchcards
  |> List.fold_left
    (fun (acc, copies) scratchcard ->
       let n = scratchcard_matching_numbers scratchcard in
       let card_copies, rest_copies = match copies with
         | []        -> 1, []
         | m :: rest -> (m + 1), rest
       in card_copies :: acc, add_to_first_items card_copies n rest_copies)
    ([], [])
  |> fst
  |> List.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let scratchcards = lines |> List.map parse_scratchcard in
  Printf.printf "Part One: %d\n" (part1 scratchcards);
  Printf.printf "Part Two: %d\n" (part2 scratchcards)
