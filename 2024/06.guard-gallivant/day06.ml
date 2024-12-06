(** Day 6: Guard Gallivant *)

(** [parse_map lines] parses the problem input and returns the map and the
    guard position. *)
let parse_map lines =
  let exception Found of int * int in

  let map =
    lines
    |> List.map (fun line -> Array.init (String.length line) (String.get line))
    |> Array.of_list
  in

  try
    for i = 0 to Array.length map - 1 do
      for j = 0 to Array.length map.(0) - 1 do
        if map.(i).(j) = '^' then begin
          map.(i).(j) <- '.';
          raise @@ Found (i, j)
        end
      done
    done;
    assert false
  with Found (i, j) ->
    map, (i, j)

(** [turn_right dir] returns the direction to the right of [dir]. *)
let turn_right = function
  | (-1,  0) -> ( 0,  1)
  | ( 0,  1) -> ( 1,  0)
  | ( 1,  0) -> ( 0, -1)
  | ( 0, -1) -> (-1,  0)
  | _        -> assert false

(** Positions set module. *)
module PosSet = Set.Make (struct type t = int * int let compare = compare end)

(** [guard_path_positions map seen start dir] returns all distinct positions the
    guard will visit before leaving the mapped area. *)
let rec guard_path_positions map seen (i, j) (di, dj) =
  let seen = PosSet.add (i, j) seen in
  let i' = i + di and j' = j + dj in
  if i' < 0 || i' >= Array.length map ||
     j' < 0 || j' >= Array.length map.(0) then
    seen
  else if map.(i').(j') = '#' then
    guard_path_positions map seen (i, j) (turn_right (di, dj))
  else
    guard_path_positions map seen (i', j') (di, dj)

(** [solve1 map guard] solves the part one of the problem. *)
let solve1 map guard =
  guard_path_positions map PosSet.empty guard (-1, 0) |> PosSet.cardinal

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let map, guard = In_channel.input_lines ic |> parse_map in
  Printf.printf "Part One: %d\n" @@ solve1 map guard
