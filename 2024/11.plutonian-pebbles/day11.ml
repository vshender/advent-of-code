(** Day 11: Plutonian Pebbles *)

(** [parse_pebble_numbers line] parses a space-separated string of pebble
    numbers into a list of integers. *)
let parse_pebble_numbers line =
  String.split_on_char ' ' line |> List.map int_of_string

(** [number_length n] returns the number of digits in the integer [n]. *)
let number_length n =
  int_of_float (log10 (float n)) + 1

(** [repeat f n x] applies the function [f] to [x], [n] times. *)
let rec repeat f n x =
  match n with
  | 0 -> x
  | n -> f (repeat f (n - 1) x)

(** [exp10 n] computes 10 raised to the power of [n]. *)
let exp10 n = repeat (( * ) 10) n 1

(** Module for counting occurrences of values. *)
module Counter = struct
  type 'a t = ('a, int) Hashtbl.t

  let create () = Hashtbl.create 128

  let incr cnt key inc =
    let n = match Hashtbl.find_opt cnt key with
      | Some n -> n
      | None   -> 0
    in
    Hashtbl.replace cnt key (n + inc)

  let of_list seq =
    let cnt = create () in
    List.iter (fun key -> incr cnt key 1) seq;
    cnt

  let iter f cnt = Hashtbl.iter f cnt

  let fold f accu cnt =
    Hashtbl.fold (fun v n accu -> f accu v n) cnt accu
end

(** [blink cnt] processes pebble numbers once. *)
let blink cnt =
  let cnt' = Counter.create () in
  Counter.iter
    (fun n c ->
       if n = 0 then
         Counter.incr cnt' 1 c
       else
         let len = number_length n in
         if len mod 2 = 0 then
           let e = exp10 (len / 2) in
           Counter.incr cnt' (n / e) c;
           Counter.incr cnt' (n mod e) c
         else
           Counter.incr cnt' (n * 2024) c)
    cnt;
  cnt'

(** [solve pebble_numbers n] computes the total count of all pebbles after
    applying [blink] [n] times. *)
let solve pebble_numbers n =
  pebble_numbers
  |> Counter.of_list
  |> repeat blink n
  |> Counter.fold (fun n _ i -> n + i) 0

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let pebble_numbers = In_channel.input_line ic |> Option.get |> parse_pebble_numbers in
  Printf.printf "Part One: %d\n" @@ solve pebble_numbers 25;
  Printf.printf "Part Two: %d\n" @@ solve pebble_numbers 75
