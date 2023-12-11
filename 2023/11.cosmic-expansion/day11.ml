(** Day 11: Cosmic Expansion *)

module IntSet = Set.Make(Int)

(** [parse_image lines] parses [lines] and gets the image of universe. *)
let parse_image lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> line.[i]))
  |> Array.of_list

(** [range a b] generates a list of integers from [a] to [b - 1]. *)
let rec range a b =
  if a < b then a :: range (a + 1) b else []

(** [rows_to_expand image] finds all the rows of [image] to expand. *)
let rows_to_expand image =
  let check_row i = Array.for_all ((=) '.') image.(i) in
  range 0 (Array.length image) |> List.filter check_row |> IntSet.of_list

(** [columns_to_expand image] finds all the columns of [image] to expand. *)
let columns_to_expand image =
  let n = Array.length image and m = Array.length image.(0) in
  let check_column j = range 0 n |> List.for_all (fun i -> image.(i).(j) = '.') in
  range 0 m |> List.filter check_column |> IntSet.of_list

(** [get_galaxies image] returns a list of galaxies from [image]. *)
let get_galaxies image =
  let galaxies = ref [] in
  for i = 0 to Array.length image - 1 do
    for j = 0 to Array.length image.(0) - 1 do
      if image.(i).(j) = '#' then
        galaxies := (i, j) :: !galaxies
    done
  done;
  !galaxies

(** [distance (rows_to_expand, columns_to_expand) expansion_factor (i, j) (i', j')]
    calculates the distance between two adjacent positions [(i, j)] and
    [(i', j')]. *)
let distance (rows_to_expand, columns_to_expand) expansion_factor (i, j) (i', j') =
  if i <> i' then
    if IntSet.mem i rows_to_expand then expansion_factor else 1
  else
    if IntSet.mem j columns_to_expand then expansion_factor else 1

(** [find_shortest_paths (n, m) (rows_to_expand, columns_to_expand) expansion_factor (i, j) galaxies]
    finds the shortest paths from the point [(i, j)] to all the [galaxies]. *)
let find_shortest_paths
    (n, m) (rows_to_expand, columns_to_expand) expansion_factor (i, j) galaxies =
  let dist = distance (rows_to_expand, columns_to_expand) expansion_factor in
  let dists = Array.make_matrix n m (-1) in
  let q = Queue.create () in
  dists.(i).(j) <- 0;
  Queue.push (i, j) q;
  while not (Queue.is_empty q) do
    let i, j = Queue.pop q in
    [(0, 1); (0, -1); (1, 0); (-1, 0)] |> List.iter
      (fun (di, dj) ->
         let i' = i + di and j' = j + dj in
         if 0 <= i' && i' < n && 0 <= j' && j' < m && dists.(i').(j') = -1 then begin
           dists.(i').(j') <- dists.(i).(j) + dist (i, j) (i', j');
           Queue.push (i', j') q
         end)
  done;
  galaxies |> List.map (fun (i, j) -> dists.(i).(j))

(** [solve image expansion_factor] finds the sum of the lengths of the shortest
    paths between all pairs of galaxies in [image]. *)
let solve image expansion_factor =
  let n = Array.length image and m = Array.length image.(0) in
  let re = rows_to_expand image and ce = columns_to_expand image in
  let galaxies = get_galaxies image in
  let sum =
    galaxies
    |> List.map (fun (i, j) ->
        find_shortest_paths (n, m) (re, ce) expansion_factor (i, j) galaxies)
    |> List.flatten
    |> List.fold_left (+) 0
  in sum / 2

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let image = parse_image lines in
  Printf.printf "Part One: %d\n" (solve image 2);
  Printf.printf "Part Two: %d\n" (solve image 1_000_000)
