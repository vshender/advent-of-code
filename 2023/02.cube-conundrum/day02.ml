(** Day 2: Cube Conundrum *)

(** [parse_game game] parses a string representation of a game. *)
let parse_game game =
  let open Str in
  match split (regexp ": *") game with
  | [game; subsets] ->
    let game_id = Scanf.sscanf game "Game %d" Fun.id
    and subsets =
      subsets
      |> split (regexp "; *")
      |> List.map (fun subset ->
          subset
          |> split (regexp ", *")
          |> List.map (fun cubes ->
              Scanf.sscanf cubes "%d %s" (fun num color -> (color, num))))
    in (game_id, subsets)
  | _ -> failwith "parse_game"

(** Number of cubes of different colors in the bag. *)
let bag = [
  ("red", 12);
  ("green", 13);
  ("blue", 14);
]

(** [check_cubes_subset subset] checks whether the given cubes subset is
    possible. *)
let check_cubes_subset =
  List.for_all (fun (color, num) -> num <= List.assoc color bag)

(** [check_game subsets] checks whether the given game is possible. *)
let check_game = List.for_all check_cubes_subset

(** [part1 lines] calculates the sum of possible game IDs from [lines]. *)
let part1 lines =
  lines
  |> List.map parse_game
  |> List.filter (fun (_, subsets) -> check_game subsets)
  |> List.map fst
  |> List.fold_left (+) 0

(** [calc_min_set_for_cubes_subset r g b subset] calculates the minimum set of
    cubes for the given cubes subset. *)
let rec calc_min_set_for_cubes_subset r g b = function
  | []                   -> (r, g, b)
  | ("red", n)   :: rest -> calc_min_set_for_cubes_subset (max r n) g b rest
  | ("green", n) :: rest -> calc_min_set_for_cubes_subset r (max g n) b rest
  | ("blue", n)  :: rest -> calc_min_set_for_cubes_subset r g (max b n) rest
  | _                    -> failwith "calc_min_set_for_cubset_subset"

(** [calc_min_set_for_game subsets] calculates the minimum set of cubes for the
    given game. *)
let calc_for_game =
  List.fold_left
    (fun (r, g, b) -> calc_min_set_for_cubes_subset r g b)
    (0, 0, 0)

(** [part2 lines] finds the minimum set of cubes for each game and then
    calculates the sum of the power of these sets. *)
let part2 lines =
  lines
  |> List.map (fun line ->
      let _, subsets = parse_game line in
      let r, g, b = calc_for_game subsets in
      r * g * b)
  |> List.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  Printf.printf "Part One: %d\n" (part1 lines);
  Printf.printf "Part Two: %d\n" (part2 lines)
