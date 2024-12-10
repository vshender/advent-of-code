(** Day 10: Hoof It *)

(** [digit c] converts a character [c] (representing a digit) into its
    corresponding integer value. *)
let digit c = Char.code c - Char.code '0'

(** [parse_map lines] parses the list of strings [lines] representing a
    topographic map into a 2D array of integers. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> line.[i] |> digit))
  |> Array.of_list

(** [inside_map n m (i, j)] checks whether the position [(i, j)] is within the
    bounds of the map. *)
let inside_map n m (i, j) =
  0 <= i && i < n && 0 <= j && j < m

(** [dirs] defines the four possible movement directions. *)
let dirs = [(0, 1); (1, 0); (0, -1); (-1, 0)]

(** [hashtbl_inc h key] increments the value associated with [key] in the hash
    table [h]. *)
let hashtbl_inc h key =
  let v = match Hashtbl.find_opt h key with
    | Some v -> v
    | None   -> 0
  in
  Hashtbl.replace h key (v + 1)

(** [compute_trailends map trailhead] computes all possible trail ends
    that can be reached from the given [trailhead] following the rules
    of the hiking trail.

    Returns a hash table where the keys are the positions of reachable
    trail ends, and the values are the number of distinct paths that
    reached each position. *)
let compute_trailends map trailhead =
  let n = Array.length map and m = Array.length map.(0) in
  let queue = Queue.create () and trailends = Hashtbl.create 128 in

  Queue.add trailhead queue;
  let rec loop () =
    match Queue.take_opt queue with
    | None        -> trailends
    | Some (i, j) ->
      if map.(i).(j) = 9 then
        hashtbl_inc trailends (i, j)
      else
        List.iter
          (fun (di, dj) ->
             let i' = i + di and j' = j + dj in
             if inside_map n m (i', j') && map.(i).(j) + 1 = map.(i').(j') then
               Queue.add (i', j') queue)
          dirs;
      loop ()
  in loop ()

(** [trailhead_score trailends] computes the score of a trailhead based on the
    trail ends it can reach. *)
let trailhead_score trailends =
  Hashtbl.fold (fun _ _ cnt -> cnt + 1) trailends 0

(** [trailhead_score trailends] computes the rating of a trailhead based on the
    trail ends it can reach. *)
let trailhead_rating trailends =
  Hashtbl.fold (fun _ npaths cnt -> cnt + npaths) trailends 0

(** [solve map f] computes the total sum of a property [f] of all trailheads
    on the topographic map. *)
let solve map f =
  let sum = ref 0 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if map.(i).(j) = 0 then
        sum := !sum + f (compute_trailends map (i, j))
    done
  done;
  !sum

let _ =
  In_channel.with_open_text "input" @@ fun ic ->
  let map = In_channel.input_lines ic |> parse_map in
  Printf.printf "Part One: %d\n" @@ solve map trailhead_score;
  Printf.printf "Part One: %d\n" @@ solve map trailhead_rating
