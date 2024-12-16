(** Day 15: Warehouse Woes *)

(** [parse_map lines] parses the list of strings [lines] into a 2D array of
    characters representing a warehouse map. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (String.get line))
  |> Array.of_list

(** [parse_move move] converts a movement character [move] into a direction
    vector. *)
let parse_move = function
  | '<' -> ( 0, -1)
  | '^' -> (-1,  0)
  | '>' -> ( 0,  1)
  | 'v' -> ( 1,  0)
  | _   -> assert false

(** [parse_moves lines] converts a list of strings representing moves into a
    sequence of direction vectors. *)
let parse_moves lines =
  lines
  |> List.to_seq
  |> Seq.map String.to_seq
  |> Seq.concat
  |> Seq.map parse_move

(** [find_robot map] finds the position of the robot in [map]. *)
let find_robot map =
  let exception Found of int * int in
  try
    for i = 0 to Array.length map - 1 do
      for j = 0 to Array.length map.(0) - 1 do
        if map.(i).(j) = '@' then
          raise @@ Found (i, j)
      done
    done;
    assert false
  with Found (i, j) ->
    (i, j)

(** [parse_input lines] parses the entire input and returns the map, the robot
    position and the robot moves. *)
let parse_input lines =
  let rec collect_map accu = function
    | "" :: tl ->
      let map, moves = parse_map (List.rev accu), parse_moves tl in
      let i, j = find_robot map in
      map.(i).(j) <- '.';
      map, (i, j), moves
    | h  :: tl -> collect_map (h :: accu) tl
    | []       -> assert false
  in collect_map [] lines

(** [move_robot map pos dir] attempts to move the robot from the position [pos]
    in the direction [dir]. *)
let move_robot map (i, j) (di, dj) =
  let rec push_boxes (i, j) =
    if map.(i).(j) = 'O' then
      let i' = i + di and j' = j + dj in
      push_boxes (i', j');
      if map.(i').(j') = '.' then begin
        map.(i').(j') <- 'O';
        map.(i).(j) <- '.'
      end
  in
  let i' = i + di and j' = j + dj in
  push_boxes (i', j');
  if map.(i').(j') = '.' then (i', j') else (i, j)

(** [box_gps_coordinate pos] calculates the GPS coordinate for a box located at
    [pos]. *)
let box_gps_coordinate (i, j) = 100 * i + j

(** [sum_of_box_gps_coordinates map] is the sum of the GPS coordinates of all
    boxes in [map]. *)
let sum_of_box_gps_coordinates map =
  let sum = ref 0 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if map.(i).(j) = 'O' then
        sum := !sum + box_gps_coordinate (i, j)
    done
  done;
  !sum

(** [solve map pos moves] simulates the movement of the robot in the warehouse
    and calculates the total sum of GPS coordinates for all boxes after all
    movements. *)
let solve map pos moves =
  ignore @@ Seq.fold_left (fun pos move -> move_robot map pos move) pos moves;
  sum_of_box_gps_coordinates map

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let map, pos, moves = In_channel.input_lines ic |> parse_input in
  Printf.printf "Part One: %d\n" @@ solve map pos moves
