(** Day 10: Pipe Maze *)

(** The type of directions. *)
type direction = North | South | East | West

(** The type of pipes. *)
type pipe =
  | NorthSouth  (* | *)
  | EastWest    (* - *)
  | NorthEast   (* L *)
  | NorthWest   (* J *)
  | SouthEast   (* F *)
  | SouthWest   (* 7 *)
  | Ground      (* . *)
  | Start       (* S *)

(** [parse_pipe c] parses a character representation of a pipe. *)
let parse_pipe = function
  | '|' -> NorthSouth
  | '-' -> EastWest
  | 'L' -> NorthEast
  | 'J' -> NorthWest
  | 'F' -> SouthEast
  | '7' -> SouthWest
  | '.' -> Ground
  | 'S' -> Start
  | _   -> failwith "parse_pipe"

(** [parse_maze lines] parses [lines] and gets the maze. *)
let parse_maze lines =
  lines
  |> List.map (fun s -> Array.init (String.length s) (fun i -> parse_pipe s.[i]))
  |> Array.of_list

(** [find_starting_position maze] finds the starting position in [maze]. *)
let find_starting_position maze =
  let exception Found of int * int in
  try
    for i = 0 to Array.length maze - 1 do
      for j = 0 to Array.length maze.(0) - 1 do
        if maze.(i).(j) = Start then
          raise (Found (i, j))
      done
    done;
    failwith "find_starting_position"
  with
  | Found (i, j) -> (i, j)

(** [move pos direction] computes a new position by moving one step from [pos]
    in the specified [direction]. *)
let move (x, y) = function
  | North -> (x - 1, y)
  | South -> (x + 1, y)
  | East  -> (x, y + 1)
  | West  -> (x, y - 1)

(** [is_valid_move dir dst_pipe] checks if moving in the specified direction
    [dir] is valid for the given destination pipe [dst_pipe]. *)
let is_valid_move dir dst_pipe =
  let valid_moves = [
    North, [NorthSouth; SouthEast; SouthWest];
    South, [NorthSouth; NorthEast; NorthWest];
    East,  [EastWest;   NorthWest; SouthWest];
    West,  [EastWest;   NorthEast; SouthEast];
  ] in
  valid_moves |> List.assoc dir |> List.mem dst_pipe

(** [find_first_move maze pos] finds a valid move from the given starting
    position. *)
let find_first_move maze (i, j) =
  let n = Array.length maze and m = Array.length maze.(0) in
  List.find
    (fun dir ->
       let i, j = move (i, j) dir in
       0 <= i && i < n && 0 <= j && j < m && is_valid_move dir maze.(i).(j))
    [North; South; East; West]

(** [next_direction pipe prev_dir] determines the next direction to move in,
    based on the current pipe and the previous movement direction. *)
let next_direction pipe prev_dir =
  match pipe, prev_dir with
  | NorthSouth, North | NorthEast, West  | NorthWest, East  -> North
  | NorthSouth, South | SouthEast, West  | SouthWest, East  -> South
  | EastWest,   East  | NorthEast, South | SouthEast, North -> East
  | EastWest,   West  | NorthWest, South | SouthWest, North -> West
  | _                                                       -> failwith "next_direction"

(** [find_loop maze] finds a loop path in [maze] starting from a designated
    starting position. *)
let find_loop maze =
  let rec find_loop_aux acc (i, j) dir =
    match maze.(i).(j) with
    | Start -> List.rev acc
    | pipe  ->
      let dir = next_direction pipe dir in
      let i, j = move (i, j) dir in
      find_loop_aux ((i, j) :: acc) (i, j) dir
  in
  let (i, j) = find_starting_position maze in
  let dir = find_first_move maze (i, j) in
  find_loop_aux [(i, j)] (move (i, j) dir) dir

(** [find_area_inside_loop maze] calculates the area inside a loop in
    [maze]. *)
let find_area_inside_loop maze =
  let module Positions = Set.Make (struct type t = int * int let compare = compare end) in
  let loop = Positions.(List.fold_right add (find_loop maze) empty) in
  let m = Array.length maze.(0) in
  let rec count_inside_row count is_inside i j =
    if j = m then
      count
    else
    if Positions.mem (i, j) loop then
      count_inside_row
        count
        (is_inside <> List.mem maze.(i).(j) [NorthSouth; NorthEast; NorthWest])
        i (j + 1)
    else
      count_inside_row
        (count + if is_inside then 1 else 0)
        is_inside
        i (j + 1)
  in
  maze
  |> Array.mapi (fun i row -> count_inside_row 0 false i 0)
  |> Array.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let maze = parse_maze lines in
  Printf.printf "Part One: %d\n" (List.length (find_loop maze) / 2);
  Printf.printf "Part Two: %d\n" (find_area_inside_loop maze)
