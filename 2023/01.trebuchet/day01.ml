(** Day 1: Trebuchet?! *)

(** [int_of_digit c] converts the character [c] representing a digit into its
    corresponding integer value. *)
let int_of_digit c = Char.code c - Char.code '0'

(** The type of a calibration value parsing state. *)
type state =
  | AwaitingFirstDigit
  | AwaitingLastDigit

(** [recover1 str] recovers the calibration value from [str] as described in
    part 1. *)
let recover1 str =
  let _, first, last =
    String.fold_left
      (fun (state, first, last) c ->
         match c, state with
         | '0'..'9', AwaitingFirstDigit -> (AwaitingLastDigit, c, c)
         | '0'..'9', AwaitingLastDigit  -> (AwaitingLastDigit, first, c)
         | _, _                         -> (state, first, last))
      (AwaitingFirstDigit, ' ', ' ')
      str
  in int_of_digit first * 10 + int_of_digit last

(** Digits spelled out with letters. *)
let digits = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

(** [get_digit str i] is the digit from [str] at the position [i]. *)
let get_digit str i =
  let len = String.length str in
  match str.[i] with
  | '0' .. '9' as c -> Some c
  | _               ->
    let suffix = String.sub str i (len - i) in
    digits
    |> List.find_index (fun digit -> String.starts_with ~prefix:digit suffix)
    |> Option.map (fun i -> Char.chr (i + Char.code '1'))

(** [recover2 str] recovers the calibration value from [str] as decribed in
    part 2. *)
let recover2 str =
  let len = String.length str in
  let rec recover_iter state first last i =
    if i < len then
      match get_digit str i, state with
      | Some c, AwaitingFirstDigit -> recover_iter AwaitingLastDigit c c (i + 1)
      | Some c, AwaitingLastDigit  -> recover_iter AwaitingLastDigit first c (i + 1)
      | None, _                    -> recover_iter state first last (i + 1)
    else
      int_of_digit first * 10 + int_of_digit last
  in recover_iter AwaitingFirstDigit ' ' ' ' 0

(** [calibration_values_sum recover lines] calculates the sum of all of the
    calibration values from [lines]. *)
let calibration_values_sum recover lines =
  lines |> List.map recover |> List.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  Printf.printf "Part One: %d\n" (calibration_values_sum recover1 lines);
  Printf.printf "Part Two: %d\n" (calibration_values_sum recover2 lines)
