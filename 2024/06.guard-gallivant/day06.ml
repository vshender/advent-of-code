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

(** [turn_right dir] is the direction to the right of [dir]. *)
let turn_right = function
  | (-1,  0) -> ( 0,  1)
  | ( 0,  1) -> ( 1,  0)
  | ( 1,  0) -> ( 0, -1)
  | ( 0, -1) -> (-1,  0)
  | _        -> assert false

(** Positions set module. *)
module PosSet = Set.Make (struct type t = int * int let compare = compare end)

(** Module for sets of positions with directions. *)
module PosDirSet = Set.Make (struct type t = int * int * int * int let compare = compare end)

(** [inside_map map (i, j)] checks if the position [(i, j)] is within the
    bounds of [map]. *)
let inside_map map (i, j) =
  0 <= i && i < Array.length map && 0 <= j && j < Array.length map.(0)

(** [walk map seen f accu (i, j) (di, dj)] performs a walk on [map] starting
    from [(i, j)] in the direction [(di, dj)].

    - [seen] is a set of already visited positions with directions.
    - [f] is a function that updates the accumulator during the walk.
    - [accu] is the initial value for the accumulator.

    Returns a tuple [(loop_detected, accu)], where [loop_detected] is [true] if
    a loop was detected, and [accu] contains the final result. *)
let walk map seen f accu (i, j) (di, dj) =
  let rec walk_aux seen accu (i, j) (di, dj) =
    let seen = PosDirSet.add (i, j, di, dj) seen in
    let accu = f seen accu (i, j) (di, dj) in
    let i' = i + di and j' = j + dj in
    if not @@ inside_map map (i', j') then
      false, accu
    else if PosDirSet.mem (i', j', di, dj) seen then
      true, accu
    else if map.(i').(j') = '#' then
      walk_aux seen accu (i, j) (turn_right (di, dj))
    else
      walk_aux seen accu (i', j') (di, dj)
  in walk_aux seen accu (i, j) (di, dj)

(** [guard_path_positions map seen start dir] returns all distinct positions
    the guard will visit before leaving the mapped area. *)
let guard_path_positions map (i, j) =
  walk
    map PosDirSet.empty
    (fun _ positions (i, j) _ -> PosSet.add (i, j) positions) PosSet.empty
    (i, j) (-1, 0)
  |> snd

(** [solve1 map guard] solves the part one of the problem. *)
let solve1 map guard =
  guard_path_positions map guard |> PosSet.cardinal

(** [find_loop map seen (i, j) (di, dj)] detects if a loop occurs while walking
    from the position [(i, j)] in the direction [(di, dj)]. *)
let rec find_loop map seen (i, j) (di, dj) =
  walk
    map seen
    (fun _ _ _ _ -> ()) ()
    (i, j) (di, dj)
  |> fst

(** [find_obstructions map (i, j) (di, dj)] finds obstruction positions that
    can cause loops in [map] starting from the position [(i, j)] in the
    direction [(di, dj)]. *)
let find_obstructions map (i, j) (di, dj) =
  walk
    map PosDirSet.empty
    (fun seen obstructions (i, j) (di, dj) ->
       let i' = i + di and j' = j + dj in
       let is_loop =
         if inside_map map (i', j') && map.(i').(j') = '.' &&
            not (List.exists
                 (fun (di, dj) -> PosDirSet.mem (i', j', di, dj) seen)
                 [(-1, 0); (0, 1); (1, 0); (0, -1)])
         then begin
           map.(i').(j') <- '#';
           let is_loop = find_loop map seen (i, j) (turn_right (di, dj)) in
           map.(i').(j') <- '.';
           is_loop
         end else
           false
       in
       if is_loop then
         PosSet.add (i', j') obstructions
       else
         obstructions)
    PosSet.empty
    (i, j) (di, dj)
  |> snd

(** [solve2 map guard] solves the part two of the problem. *)
let solve2 map guard =
  find_obstructions map guard (-1, 0) |> PosSet.cardinal

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let map, guard = In_channel.input_lines ic |> parse_map in
  Printf.printf "Part One: %d\n" @@ solve1 map guard;
  Printf.printf "Part Two: %d\n" @@ solve2 map guard
