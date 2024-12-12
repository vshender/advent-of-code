(** Day 12: Garden Groups *)

(** [parse_map lines] parses the list of strings [lines] representing a garden
    map into a 2D array of characters. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (String.get line))
  |> Array.of_list

(** [dirs] defines the four possible movement directions. *)
let dirs = [(-1, 0); (0, 1); (1, 0); (0, -1)]

(** A set of int pairs. *)
module IntPairSet = Set.Make (struct type t = (int * int) let compare = compare end)

(** [inside_map n m i j] checks whether the position [(i, j)] is within the
    bounds of the map. *)
let inside_map n m i j =
  0 <= i && i < n && 0 <= j && j < m

(** [region_perimeter_and_area map i j] computes the region, perimeter, and
    area for a connected group of identical plots starting from the position
    [(i, j)]. *)
let region_perimeter_and_area map i j =
  let n = Array.length map and m = Array.length map.(0) in
  let q = Queue.create () in
  let rec loop region perimeter area =
    match Queue.take_opt q with
    | None        -> (region, perimeter, area)
    | Some (i, j) ->
      let region, plot_perimeter =
        List.fold_left
          (fun (region, p) (di, dj) ->
             let i' = i + di and j' = j + dj in
             let region =
               if not (IntPairSet.mem (i', j') region) &&
                  inside_map n m i' j' && map.(i).(j) = map.(i').(j') then begin
                 Queue.add (i', j') q;
                 IntPairSet.add (i', j') region
               end else
                 region
             in
             (region,
              p + if inside_map n m i' j' && map.(i).(j) = map.(i').(j') then 0 else 1))
          (region, 0)
          dirs
      in
      loop region (perimeter + plot_perimeter) (area + 1)
  in
  Queue.add (i, j) q;
  loop (IntPairSet.singleton (i, j)) 0 0

(** [solve map] computes the total price of all regions in the garden map. *)
let solve map =
  let seen = ref IntPairSet.empty and total_price = ref 0 in
  for i = 0 to Array.length map - 1 do
    for j = 0 to Array.length map.(0) - 1 do
      if not (IntPairSet.mem (i, j) !seen) then
        let region, perimeter, area = region_perimeter_and_area map i j in
        seen := IntPairSet.union !seen region;
        total_price := !total_price + (perimeter * area)
    done
  done;
  !total_price

let _ =
  In_channel.with_open_text "input" @@ fun ic ->
  let map = In_channel.input_lines ic |> parse_map in
  Printf.printf "Part One: %d\n" @@ solve map
