(** Day 10: Pipe Maze *)

module Direction = struct
  (** The type of directions. *)
  type t = North | South | East | West

  (** [opposite dir] is the direction opposite to [dir]. *)
  let opposite = function
    | North -> South
    | South -> North
    | East  -> West
    | West  -> East

  (** [move pos direction] computes a new position by moving one step from
      [pos] in the specified [direction]. *)
  let move (x, y) = function
    | North -> (x - 1, y)
    | South -> (x + 1, y)
    | East  -> (x, y + 1)
    | West  -> (x, y - 1)
end

module Pipe = struct
  include Set.Make (struct type t = Direction.t let compare = compare end)

  (** [is_valid_move dir dst_pipe] checks if moving in the specified direction
      [dir] is valid for the given destination pipe [dst_pipe]. *)
  let is_valid_move dir dst_pipe = mem (Direction.opposite dir) dst_pipe

  (** [next_direction pipe prev_dir] determines the next direction to move in,
      based on the current pipe and the previous movement direction. *)
  let next_direction pipe prev_dir =
    remove (Direction.opposite prev_dir) pipe |> choose

  (** [parse c] parses a character representation of a pipe. *)
  let parse = function
    | '|' -> of_list [North; South]
    | '-' -> of_list [East; West]
    | 'L' -> of_list [North; East]
    | 'J' -> of_list [North; West]
    | 'F' -> of_list [South; East]
    | '7' -> of_list [South; West]
    | '.' -> empty
    | 'S' -> of_list [North; South; East; West]
    | _   -> failwith "parse_pipe"
end

(** [find_starting_position lines] parses [lines] and finds the starting
    position. *)
let find_starting_position lines =
  lines
  |> List.mapi (fun i line -> String.index_opt line 'S' |> Option.map (fun j -> i, j))
  |> List.filter_map Fun.id
  |> List.hd

(** [parse_maze lines starting_position] parses [lines] and gets the maze. *)
let parse_maze lines =
  let (si, sj) = find_starting_position lines
  and maze =
    lines
    |> List.map (fun s -> Array.init (String.length s) (fun i -> Pipe.parse s.[i]))
    |> Array.of_list
  in
  (* Leave only valid directions at the starting position. *)
  maze.(si).(sj) <- Pipe.filter
      (fun dir ->
         let (i, j) = Direction.move (si, sj) dir in
         Pipe.is_valid_move dir maze.(i).(j))
      maze.(si).(sj);
  (si, sj), maze

(** [find_loop maze] finds a loop path in [maze] starting from the given
    starting position. *)
let find_loop maze (si, sj) =
  let rec find_loop_aux acc (i, j) dir =
    if i = si && j = sj then
      List.rev acc
    else
      let dir = Pipe.next_direction maze.(i).(j) dir in
      let i, j = Direction.move (i, j) dir in
      find_loop_aux ((i, j) :: acc) (i, j) dir
  in
  let dir = Pipe.choose maze.(si).(sj) in
  let i, j = Direction.move (si, sj) dir in
  find_loop_aux [(i, j)] (i, j) dir

(** [find_area_inside_loop maze] calculates the area inside a loop in
    [maze]. *)
let find_area_inside_loop maze (si, sj) =
  let module Positions = Set.Make (struct type t = int * int let compare = compare end) in
  let loop = Positions.(List.fold_right add (find_loop maze (si, sj)) empty) in
  let m = Array.length maze.(0) in
  let rec count_in_row count inside i j =
    if j = m then
      count
    else
    if Positions.mem (i, j) loop then
      count_in_row count (inside <> Pipe.mem North maze.(i).(j)) i (j + 1)
    else
      count_in_row (count + if inside then 1 else 0) inside i (j + 1)
  in
  maze
  |> Array.mapi (fun i row -> count_in_row 0 false i 0)
  |> Array.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let start, maze = parse_maze lines in
  Printf.printf "Part One: %d\n" (List.length (find_loop maze start) / 2);
  Printf.printf "Part Two: %d\n" (find_area_inside_loop maze start)
