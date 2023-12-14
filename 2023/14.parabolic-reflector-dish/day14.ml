(** Day 14: Parabolic Reflector Dish *)

(** [(f %> g) x] is [g (f x)]. *)
let ( %> ) f g x = g (f x)

(** [apply_repeatedly f n x] applies a function [f] to the initial value [x],
    [n] times. *)
let rec apply_repeatedly f n x =
  match n with
  | 0 -> x
  | n -> apply_repeatedly f (n - 1) (f x)

(** [transpose lsts] transposes the given matrix (list of lists). *)
let rec transpose = function
  | [] | [] :: _ -> []
  | lsts         -> List.(map hd lsts :: transpose (map tl lsts))

(** [rotate lsts] rotates the given matrix (list of lists) 90 degrees
    clockwise. *)
let rotate = List.(map rev) %> transpose

(** [repeat x n] creates a list containing the element [x] repeated [n]
    times. *)
let rec repeat x = function
  | 0 -> []
  | n -> x :: repeat x (n - 1)

(** [parse_map lines] parses [lines] and gets the rocks arrangement. *)
let parse_map = List.map (String.to_seq %> List.of_seq) %> transpose

(** [tilt_north map] returns the rocks arrangement after tilting the platform
    to the north. *)
let tilt_north map =
  let rec roll rc ec = function
    | 'O' :: tl -> roll (rc + 1) ec tl
    | '.' :: tl -> roll rc (ec + 1) tl
    | '#' :: tl -> repeat 'O' rc @ repeat '.' ec @ ['#'] @ roll 0 0 tl
    | []        -> repeat 'O' rc @ repeat '.' ec
    | _         -> failwith "tilt_north"
  in List.map (roll 0 0) map

(** [spin_cycle map] returns the rocks arrangement after a single spin
    cycle. *)
let spin_cycle = apply_repeatedly (tilt_north %> rotate) 4

(** [detect_loop map] detects a loop in the stones arrangement during running
    spin cycles .*)
let detect_loop map =
  let h = Hashtbl.create 1024 in
  let rec detect_loop n map' =
    match Hashtbl.find_opt h map', n with
    | _, 0 | None, _ ->
      Hashtbl.add h map' n;
      detect_loop (n + 1) (spin_cycle map')
    | Some start, _ ->
      start, n - start
  in detect_loop 0 map

(** [total_load map] calculates the total load caused by the rocks. *)
let total_load map =
  let n = List.length map in
  map
  |> List.map
    (List.mapi (fun i c -> if c = 'O' then n - i else 0) %> List.fold_left (+) 0)
  |> List.fold_left (+) 0

(** [part1 map] calculates the total load on the north support beams after
    tilting the platform to the north. *)
let part1 = tilt_north %> total_load

(** [part2 map] calculates the total load on the north support beams after
    running the spin cycle for [1_000_000_000] cycles. *)
let part2 map =
  let from, len = detect_loop map in
  let n = from + (1_000_000_000 - from) mod len in
  map |> apply_repeatedly spin_cycle n |> total_load

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let map = parse_map lines in
  Printf.printf "Part One: %d\n" (part1 map);
  Printf.printf "Part Two: %d\n" (part2 map)
