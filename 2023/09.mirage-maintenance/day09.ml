(** Day 9: Mirage Maintenance *)

open List

(** [parse_sequence seq] parses a string representation of an integer
    sequence. *)
let parse_sequence seq = seq |> String.split_on_char ' ' |> map int_of_string

(** [last lst] is the last element of a non-empty list. *)
let rec last = function
  | []        -> failwith "last"
  | [x]       -> x
  | _ :: rest -> last rest

(** [remove_last lst] removes the last element from a non-empty list. *)
let rec remove_last = function
  | []             -> failwith "remove_last"
  | [x]            -> []
  | x :: y :: rest -> x :: remove_last (y :: rest)

(** [predict_next seq] predicts the next value of the given sequence. *)
let rec predict_next seq =
  let diffs = map2 (-) (tl seq) (remove_last seq) in
  if for_all ((=) 0) diffs then
    last seq
  else
    last seq + predict_next diffs

(** [predict_prev seq] predicts the previous value of the given sequence. *)
let rec predict_prev seq =
  let diffs = map2 (-) (tl seq) (remove_last seq) in
  if for_all ((=) 0) diffs then
    hd seq
  else
    hd seq - predict_prev diffs

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let seqs = map parse_sequence lines in
  Printf.printf "Part One: %d\n" (seqs |> map predict_next |> fold_left (+) 0);
  Printf.printf "Part Two: %d\n" (seqs |> map predict_prev |> fold_left (+) 0)
