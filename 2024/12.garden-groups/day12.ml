(** Day 12: Garden Groups *)

(** [parse_map lines] parses the list of strings [lines] representing a garden
    map into a 2D array of characters. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (String.get line))
  |> Array.of_list

(** The four possible movement directions. *)
let dirs = [(-1, 0); (0, 1); (1, 0); (0, -1)]

(** The four directions to identify adjacent corners of a plot. *)
let corners = [(-1, -1); (-1, 1); (1, 1); (1, -1)]

(** [adjacent_to_corner corner] computes adjacent positions for a given
    corner. *)
let adjacent_to_corner (i, j) = [(0, j); (i, 0)]

(** A set of plots. *)
module PlotSet = Set.Make (struct type t = (int * int) let compare = compare end)

(** [inside_map n m (i, j)] checks whether the position [(i, j)] is within the
    bounds of a map of size [n] x [m]. *)
let inside_map n m (i, j) =
  0 <= i && i < n && 0 <= j && j < m

(** [area map region] is the area of [region] as the number of plots it
    contains. *)
let area _ = PlotSet.cardinal

(** [perimeter map region] computes the perimeter of [region]. *)
let perimeter map region =
  let n = Array.length map and m = Array.length map.(0) in
  PlotSet.fold
    (fun (i, j) p ->
       p +
       List.fold_left
         (fun p (di, dj) ->
            let i' = i + di and j' = j + dj in
            p + if not (inside_map n m (i', j')) || map.(i').(j') <> map.(i).(j) then 1 else 0)
         0 dirs)
    region 0

(** [is_external_corner map (i, j) (di, dj)] checks if the corner [(i, j)] in
    direction [(di, dj)] is an external corner of a region. *)
let is_external_corner map (i, j) (di, dj) =
  let n = Array.length map and m = Array.length map.(0) in
  List.for_all
    (fun (di, dj) ->
       not (inside_map n m (i + di, j + dj)) || map.(i + di).(j + dj) <> map.(i).(j))
    (adjacent_to_corner (di, dj))

(** [is_internal_corner map (i, j) (di, dj)] checks if the corner [(i, j)] in
    direction [(di, dj)] is an internal corner of a region. *)
let is_internal_corner map (i, j) (di, dj) =
  let n = Array.length map and m = Array.length map.(0) in
  (inside_map n m (i + di, j + dj) && map.(i + di).(j + dj) <> map.(i).(j)) &&
  List.for_all
    (fun (di, dj) ->
       inside_map n m (i + di, j + dj) && map.(i + di).(j + dj) = map.(i).(j))
    (adjacent_to_corner (di, dj))

(** [sides_number map region] computes the total number of sides of [region].
    The function actually calculates the number of corners of [region], which
    is equal to the number of sides. *)
let sides_number map region =
  PlotSet.fold
    (fun (i, j) n ->
       n +
       List.fold_left
         (fun n (di, dj) ->
            n
            + (if is_external_corner map (i, j) (di, dj) then 1 else 0)
            + (if is_internal_corner map (i, j) (di, dj) then 1 else 0))
         0 corners)
    region 0

(** [find_region map (i, j)] identifies and returns the connected region
    starting from the position [(i, j)]. *)
let find_region map (i, j) =
  let n = Array.length map and m = Array.length map.(0) in
  let q = Queue.create () in
  let rec loop region =
    match Queue.take_opt q with
    | None        -> region
    | Some (i, j) ->
      loop
        (List.fold_left
           (fun region (di, dj) ->
              let i' = i + di and j' = j + dj in
              if not (PlotSet.mem (i', j') region) &&
                 inside_map n m (i', j') && map.(i).(j) = map.(i').(j') then begin
                Queue.add (i', j') q;
                PlotSet.add (i', j') region
              end else
                region)
           region
           dirs)
  in
  Queue.add (i, j) q;
  loop (PlotSet.singleton (i, j))

(** [solve perim map] calculates the total price of fencing all regions on the
    garden map. *)
let solve perim map =
  let seen = ref PlotSet.empty and total_price = ref 0 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if not (PlotSet.mem (i, j) !seen) then
        let region = find_region map (i, j) in
        seen := PlotSet.union !seen region;
        total_price := !total_price + (perim map region * area map region)
    done
  done;
  !total_price

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let map = In_channel.input_lines ic |> parse_map in
  Printf.printf "Part One: %d\n" @@ solve perimeter map;
  Printf.printf "Part Two: %d\n" @@ solve sides_number map
