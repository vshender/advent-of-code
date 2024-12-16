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
  (* [push_boxes pos] attempts to push boxes from the position [pos] in the
     direction [dir]. *)
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
      if map.(i).(j) = 'O' || map.(i).(j) = '[' then
        sum := !sum + box_gps_coordinate (i, j)
    done
  done;
  !sum

(** [resize_map map] resizes the warehouse map to be twice as wide while
    converting elements accordingly. *)
let resize_map map =
  let n = Array.length map and m = Array.length map.(0) in
  Array.init
    n
    (fun i ->
       let row = Array.make (m * 2) ' ' in
       for j = 0 to m - 1 do
         match map.(i).(j) with
         | '#' -> row.(j * 2) <- '#'; row.(j * 2 + 1) <- '#'
         | 'O' -> row.(j * 2) <- '['; row.(j * 2 + 1) <- ']'
         | '.' -> row.(j * 2) <- '.'; row.(j * 2 + 1) <- '.'
         | _   -> assert false
       done;
       row)

(** A set of 2D positions. *)
module PosSet = Set.Make (struct type t = (int * int) let compare = compare end)

(** [move_robot_2 map pos move] attempts to move the robot from the position
    [pos] in the direction [dir] in the resized warehouse. *)
let move_robot_2 map (i, j) (di, dj) =
  (* [push_boxes_horizontally pos] attempts to push boxes horizontally from the
     position [pos] in the direction [(0, dj)]. *)
  let rec push_boxes_horizontally (i, j) =
    if map.(i).(j) = '[' || map.(i).(j) = ']' then
      let j' = j + dj in
      push_boxes_horizontally (i, j');
      if map.(i).(j') = '.' then begin
        map.(i).(j') <- map.(i).(j);
        map.(i).(j) <- '.'
      end
  in

  (** [hit_the_wall_vertically positions] is [true] if it's not possible to
      push boxes vertically from the positions [positions] because of wall. *)
  let hit_the_wall_vertically =
    PosSet.exists (fun (i, j) -> map.(i + di).(j) = '#')
  in

  (* [collect_affected_vertically positions] computes the set of positions of
     the boxes on the next row that will be affected if the boxes at the given
     positions [positions] are moved vertically in the direction [(di, 0)]. *)
  let collect_affected_vertically positions =
    PosSet.fold
      (fun (i, j) aff_poss ->
         let i' = i + di in
         match map.(i').(j) with
         | '[' -> aff_poss |> PosSet.add (i', j) |> PosSet.add (i', j + 1)
         | ']' -> aff_poss |> PosSet.add (i', j - 1) |> PosSet.add (i', j)
         | _   -> aff_poss)
      positions
      PosSet.empty
  in

  (* [push_boxes_vertically positions] attempts to push boxes vertically from
     the positions [positions] in the direction [(di, 0)]. *)
  let rec push_boxes_vertically positions =
    if not (PosSet.is_empty positions) &&
       not (hit_the_wall_vertically positions) then begin
      push_boxes_vertically (collect_affected_vertically positions);
      if PosSet.for_all (fun (i, j) -> map.(i + di).(j) = '.') positions then
        PosSet.iter
          (fun (i, j) ->
             map.(i + di).(j) <- map.(i).(j);
             map.(i).(j) <- '.')
          positions
    end
  in

  let i' = i + di and j' = j + dj in
  if di = 0 then
    push_boxes_horizontally (i, j')
  else
    push_boxes_vertically (collect_affected_vertically (PosSet.singleton (i, j)));

  if map.(i').(j') = '.' then (i', j') else (i, j)

(** [solve move_robot map pos moves] simulates the movement of the robot in the
    warehouse using the [move_robot] function and calculates the total sum of
    GPS coordinates for all boxes after all movements. *)
let solve move_robot map pos moves =
  ignore @@ Seq.fold_left (fun pos move -> move_robot map pos move) pos moves;
  sum_of_box_gps_coordinates map

(** [copy_map map] is a copy of [map]. *)
let copy_map map =
  Array.init
    (Array.length map)
    (fun i -> Array.copy map.(i))

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let map, (i, j), moves = In_channel.input_lines ic |> parse_input in
  Printf.printf "Part One: %d\n" @@ solve move_robot (copy_map map) (i, j) moves;
  let map = resize_map map in
  Printf.printf "Part Two: %d\n" @@ solve move_robot_2 map (i, j * 2) moves
