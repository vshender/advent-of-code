(** Day 21: Step Counter *)

(** [parse_map lines] parses [lines] and gets the map. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> line.[i]))
  |> Array.of_list

(** [starting_position map] returns the staring position on [map]. *)
let starting_position map =
  let exception Found of int * int in
  try
    for x = 0 to Array.length map - 1 do
      for y = 0 to Array.length map.(0) - 1 do
        if map.(x).(y) = 'S' then
          raise (Found (x, y))
      done
    done;
    failwith "starting_position"
  with Found (x, y) -> (x, y)

(** Positions set. *)
module PosSet = Set.Make (struct type t = int * int let compare = compare end)

(** [step (x, y)] returns the positions that can be reached in one step from
    [(x, y)]. *)
let step (x, y) =
  [(1, 0); (0, 1); (-1, 0); (0, -1)]
  |> List.map (fun (dx, dy) -> (x + dx, y + dy))

(** [walk map (sx, sy) n] returns the positions that can be reached in [n]
    steps from [(sx, sy)]. *)
let walk map (sx, sy) steps =
  let n = Array.length map and m = Array.length map.(0) in
  let rec walk_iter positions = function
    | 0     -> positions
    | steps ->
      walk_iter
        (PosSet.fold
           (fun (x, y) positions' ->
              let new_positions =
                (x, y)
                |> step
                |> List.filter (fun (x, y) -> 0 <= x && x < n && 0 <= y && y < m && map.(x).(y) <> '#')
              in
              PosSet.add_seq (List.to_seq new_positions) positions')
           positions PosSet.empty)
        (steps - 1)
  in walk_iter (PosSet.singleton (sx, sy)) steps

(** [part1 map] finds the number of positions the Elf can reach in exactly 64
    steps. *)
let part1 map =
  let spos = starting_position map in
  walk map spos 64 |> PosSet.cardinal

let () =
  let map = open_in "input" |> In_channel.input_lines |> parse_map in
  Printf.printf "Part One: %d\n" (part1 map)
