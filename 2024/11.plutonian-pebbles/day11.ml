(** Day 11: Plutonian Pebbles *)

(** [parse_pebble_numbers line] parses a space-separated string of pebble
    numbers into a list of integers. *)
let parse_pebble_numbers line =
  String.split_on_char ' ' line |> List.map int_of_string

(** [number_length n] computes the number of digits in the integer [n]. *)
let number_length n =
  int_of_float (log10 (float n)) + 1

(** [repeat f x n] applies the function [f] to [x], [n] times. *)
let rec repeat f x = function
  | 0 -> x
  | n -> f (repeat f x (n - 1))

(** [exp10 n] computes 10 raised to the power of [n]. *)
let exp10 = repeat (( * ) 10) 1

(** [blink pebble_numbers] processes a list of pebble numbers once. *)
let rec blink = function
  | []      -> []
  | n :: ns ->
    if n = 0 then
      1 :: blink ns
    else
      let len = number_length n in
      if len mod 2 = 0 then
        let e = exp10 (len / 2) in
        n / e :: n mod e :: blink ns
      else
        n * 2024 :: blink ns

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let pebble_numbers = In_channel.input_line ic |> Option.get |> parse_pebble_numbers in
  Printf.printf "Part One: %d\n" @@ List.length @@ repeat blink pebble_numbers 25
