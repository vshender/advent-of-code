(** Day 15: Lens Libary *)

(** [(f %> g) x] is [g (f x)]. *)
let (%>) f g x = g (f x)

(** [parse_init_seq init_seq] parses a string representation of an
    initialization sequence. *)
let parse_init_seq = String.(trim %> split_on_char ',')

(** [hash str] computes the hash value of [str]. *)
let hash str =
  let h = ref 0 in
  for i = 0 to String.length str - 1 do
    h := (!h + Char.code str.[i]) * 17 mod 256
  done;
  !h

(** [part1 init_seq] runs the HASH algorithm on each step in the initialization
    sequqnce and calculates the sum of the results. *)
let part1 = List.map hash %> List.fold_left (+) 0

(** The type of commands. *)
type command = Remove | Add of int

(** [parse_init_step init_step] parses a string representation of an
    initialization sequence step. *)
let parse_init_step init_step =
  let open Str in
  match split (regexp "[-=]") init_step with
  | [label]               -> (label, Remove)
  | [label; focal_length] -> (label, Add (int_of_string focal_length))
  | _                     -> failwith "parse_init_seq"

(** [add_assoc (key, value) alist] adds or updates a [(key, value)] pair in
    [alist]. *)
let rec add_assoc (key, value) = function
  | []                   -> [(key, value)]
  | (key', value') :: tl ->
    if key = key' then
      (key, value) :: tl
    else
      (key', value') :: add_assoc (key, value) tl

(** [process_init_step boxes init_step] process the given initialization
    sequence step. *)
let process_init_step boxes (label, command) =
  let i = hash label in
  match command with
  | Remove           -> boxes.(i) <- List.remove_assoc label boxes.(i)
  | Add focal_length -> boxes.(i) <- add_assoc (label, focal_length) boxes.(i)

(** [box_focusing_power box_number box] calculates the total focusing power of
    the lenses in [box]. *)
let box_focusing_power box_number box =
  box
  |> List.mapi (fun slot_number (_, focal_length) ->
      (box_number + 1) * (slot_number + 1) * focal_length)
  |> List.fold_left (+) 0

(** [focusing_power boxes] calculates the total focusing power of the lenses in
    [boxes]. *)
let focusing_power =
  Array.to_seq %> Seq.mapi box_focusing_power %> Seq.fold_left (+) 0

(** [part2 init_seq] follows the initialization sequence and then calculates
    the focusing power of the resulting lens configuration. *)
let part2 init_seq =
  let boxes = Array.make 256 [] in
  init_seq |> List.map parse_init_step |> List.iter (process_init_step boxes);
  focusing_power boxes

let () =
  let init_seq = open_in "input" |> In_channel.input_all |> parse_init_seq in
  Printf.printf "Part One: %d\n" (part1 init_seq);
  Printf.printf "Part Two: %d\n" (part2 init_seq)
