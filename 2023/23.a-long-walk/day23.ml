(** Day 23: A Long Walk *)

(** The type of directions. *)
type direction = Up | Right | Down | Left

(** The type of cells. *)
type cell = Path | Forest | Slope of direction

(** [parse_direction dir] parses a character representation of a direction. *)
let parse_direction = function
  | '^' -> Up
  | '>' -> Right
  | 'v' -> Down
  | '<' -> Left
  | _   -> failwith "parse_direction"

(** [parse_cell cell] parses a character representation of a cell. *)
let parse_cell = function
  | '.'                        -> Path
  | '#'                        -> Forest
  | '^' | '>' | 'v' | '<' as d -> Slope (parse_direction d)
  | _                          -> failwith "parse_direction"

(** [parse_map lines] parses [lines] and gets the map. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> parse_cell line.[i]))
  |> Array.of_list

(** [find_path_in_row map x] finds a path cell in the row [x] of [map]. *)
let find_path_in_row map x =
  match Array.find_index (fun c -> c = Path) map.(x) with
  | Some y -> (x, y)
  | None   -> failwith "find_path_in_row"

(** [possible_directions cell] returns possible directions from the given
    cell. *)
let possible_directions = function
  | Path      -> [Up; Right; Down; Left]
  | Forest    -> []
  | Slope dir -> [dir]

(** [direction dir] converts the given direction into a corresponding
    coordinates offset. *)
let direction = function
  | Up    -> (-1, 0)
  | Right -> (0, 1)
  | Down  -> (1, 0)
  | Left  -> (0, -1)

(** A set of positions. *)
module PosSet = Set.Make (struct type t = int * int let compare = compare end)

(** [find_longest_path map] computes the length of the longest path between
    starting and ending positions on [map]. *)
let find_longest_path map =
  let n = Array.length map and m = Array.length map.(0) in
  let sx, sy = find_path_in_row map 0
  and ex, ey = find_path_in_row map (n - 1) in
  let rec find_iter seen (x, y) =
    if x = ex && y = ey then
      PosSet.cardinal seen
    else
      possible_directions map.(x).(y)
      |> List.map direction
      |> List.fold_left
        (fun max_len (dx, dy) ->
           let x' = x + dx and y' = y + dy in
           if 0 <= x' && x' < n && 0 <= y' && y' < m && not (PosSet.mem (x', y') seen) then
             max max_len (find_iter (PosSet.add (x, y) seen) (x', y'))
           else
             max_len)
        0
  in find_iter PosSet.empty (sx, sy)

let () =
  let map = open_in "input" |> In_channel.input_lines |> parse_map in
  Printf.printf "Part One: %d\n" (find_longest_path map)
