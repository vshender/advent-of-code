(** Day 11: Cosmic Expansion *)

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
  range 0 (Array.length image) |> List.filter check_row

(** [columns_to_expand image] finds all the columns of [image] to expand. *)
let columns_to_expand image =
  let n = Array.length image and m = Array.length image.(0) in
  let check_column j = range 0 n |> List.for_all (fun i -> image.(i).(j) = '.') in
  range 0 m |> List.filter check_column

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
    calculates the shortest distance between two positions [(i, j)] and
    [(i', j')]. *)
let distance (rows_to_expand, columns_to_expand) expansion_factor (i, j) (i', j') =
  let i, i' = min i i', max i i' and j, j' = min j j', max j j' in
  let nr = rows_to_expand |> List.filter (fun x -> i <= x && x <= i') |> List.length
  and nc = columns_to_expand |> List.filter (fun y -> j <= y && y <= j') |> List.length
  in (i' - i + nr * (expansion_factor - 1)) + (j' - j + nc * (expansion_factor - 1))

(** [solve image expansion_factor] finds the sum of the lengths of the shortest
    paths between all pairs of galaxies in [image]. *)
let solve image expansion_factor =
  let re = rows_to_expand image and ce = columns_to_expand image in
  let dist = distance (re, ce) expansion_factor in
  let galaxies = get_galaxies image in
  let sum =
    galaxies
    |> List.map (fun from -> List.map (dist from) galaxies)
    |> List.flatten
    |> List.fold_left (+) 0
  in sum / 2

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let image = parse_image lines in
  Printf.printf "Part One: %d\n" (solve image 2);
  Printf.printf "Part Two: %d\n" (solve image 1_000_000)
