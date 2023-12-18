(** Day 16: The Floor Will Be Lava *)

(** The type of grid cells. *)
type cell =
  | Empty
  | RightTiltedMirror
  | LeftTiltedMirror
  | VerticalSplitter
  | HorizontalSplitter

(** [parse_cell cell] parses a character representation of a grid cell. *)
let parse_cell = function
  | '.'  -> Empty
  | '/'  -> RightTiltedMirror
  | '\\' -> LeftTiltedMirror
  | '|'  -> VerticalSplitter
  | '-'  -> HorizontalSplitter
  | _    -> failwith "parse_cell"

(** [parse_grid lines] parses [lines] and gets the grid. *)
let parse_grid lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> parse_cell line.[i]))
  |> Array.of_list

(** [next_directions cell (dx, dy)] returns the next directions from [cell] to
    which we came in direction [(dx, dy)]. *)
let next_directions cell (dx, dy) =
  match cell, (dx, dy) with
  | Empty,              _       -> [(dx, dy)]
  | RightTiltedMirror,  (1, 0)  -> [(0, -1)]
  | RightTiltedMirror,  (0, 1)  -> [(-1, 0)]
  | RightTiltedMirror,  (-1, 0) -> [(0, 1)]
  | RightTiltedMirror,  (0, -1) -> [(1, 0)]
  | LeftTiltedMirror,   (1, 0)  -> [(0, 1)]
  | LeftTiltedMirror,   (0, 1)  -> [(1, 0)]
  | LeftTiltedMirror,   (-1, 0) -> [(0, -1)]
  | LeftTiltedMirror,   (0, -1) -> [(-1, 0)]
  | VerticalSplitter,   (dx, 0) -> [(dx, 0)]
  | VerticalSplitter,   (0, dy) -> [(1, 0); (-1, 0)]
  | HorizontalSplitter, (dx, 0) -> [(0, 1); (0, -1)]
  | HorizontalSplitter, (0, dy) -> [(0, dy)]
  | _                           -> failwith "next_directions"

(** [walk grid (x, y) (dx, dy)] traverses [grid] from the given position and in
    the given direction and returns the number of cells visited. *)
let walk grid (x, y) (dx, dy) =
  let module SeenSet = Set.Make
      (struct type t = (int * int) * (int * int) let compare = compare end) in
  let module PosSet = Set.Make
      (struct type t = int * int let compare = compare end) in
  let n = Array.length grid and m = Array.length grid.(0) in

  let rec walk_aux seen (x, y) (dx, dy) =
    if SeenSet.mem ((x, y), (dx, dy)) seen then
      seen
    else
      next_directions grid.(x).(y) (dx, dy)
      |> List.fold_left
        (fun seen (dx, dy) ->
           let x = x + dx and y = y + dy in
           if 0 <= x && x < n && 0 <= y && y < m then
             walk_aux seen (x, y) (dx, dy)
           else
             seen)
        (SeenSet.add ((x, y), (dx, dy)) seen)
  in
  SeenSet.fold
    (fun ((x, y), _) -> PosSet.add (x, y))
    (walk_aux SeenSet.empty (x, y) (dx, dy))
    PosSet.empty
  |> PosSet.cardinal

(** [part1 grid] calculates how many tiles end up being energized if the beam
    starting in the top-left heading right. *)
let part1 grid =
  walk grid (0, 0) (0, 1)

(** [range a b] generates a list of integers from [a] to [b - 1]. *)
let rec range a b =
  if a < b then a :: range (a + 1) b else []

(** [part2 grid] calculates the maximum number of energized tiles. *)
let part2 grid =
  let n = Array.length grid and m = Array.length grid.(0) in
  (range 0 n |> List.map (fun x -> (x, 0), (0, 1))) @
  (range 0 n |> List.map (fun x -> (x, m - 1), (0, -1))) @
  (range 0 m |> List.map (fun y -> (0, y), (1, 0))) @
  (range 0 m |> List.map (fun y -> (n - 1, y), (-1, 0)))
  |> List.fold_left
    (fun m ((x, y), (dx, dy)) -> max m (walk grid (x, y) (dx, dy)))
    0

let () =
  let grid = open_in "input" |> In_channel.input_lines |> parse_grid in
  Printf.printf "Part One: %d\n" (part1 grid);
  Printf.printf "Part Two: %d\n" (part2 grid)
