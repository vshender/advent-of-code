(** Day 11: Cosmic Expansion *)

(** [parse_image lines] parses [lines] and gets the image of universe. *)
let parse_image lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> line.[i]))
  |> Array.of_list

(** [range a b] generates a list of integers from [a] to [b - 1]. *)
let rec range a b =
  if a < b then a :: range (a + 1) b else []

(** [count p lst] counts the number of elements in [lst] that satisfy the
    predicate [p]. *)
let count p =
  let rec count_iter count = function
    | []     -> count
    | h :: t -> count_iter (count + if p h then 1 else 0) t
  in count_iter 0

(** [expand_image image] expands the given image of universe. *)
let expand_image image =
  let n = Array.length image and m = Array.length image.(0) in

  let check_row i = Array.for_all ((=) '.') image.(i)
  and check_column j = range 0 n |> List.for_all (fun i -> image.(i).(j) = '.') in
  let rows_to_expand = range 0 n |> List.filter check_row
  and columns_to_expand = range 0 m |> List.filter check_column in

  let image' = Array.make_matrix
      (n + List.length rows_to_expand) (m + List.length columns_to_expand) '.'
  in
  for i = 0 to n - 1 do
    let i' = i + count (fun row -> row < i) rows_to_expand in
    for j = 0 to m - 1 do
      let j' = j + count (fun column -> column < j) columns_to_expand in
      image'.(i').(j') <- image.(i).(j)
    done
  done;
  image'

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

(** [find_shortest_paths (n, m) (i, j) galaxies] finds the shortest paths from
    the point [(i, j)] to all the [galaxies]. *)
let find_shortest_paths (n, m) (i, j) galaxies =
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
           dists.(i').(j') <- dists.(i).(j) + 1;
           Queue.push (i', j') q
         end)
  done;
  galaxies |> List.map (fun (i, j) -> dists.(i).(j))

(** [part1 image] finds the sum of the lengths of the shortest paths between
    all pairs of galaxies in [image]. *)
let part1 image =
  let n = Array.length image and m = Array.length image.(0) in
  let galaxies = get_galaxies image in
  let sum = galaxies
            |> List.map (fun (i, j) -> find_shortest_paths (n, m) (i, j) galaxies)
            |> List.flatten
            |> List.fold_left (+) 0
  in sum / 2

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let image = parse_image lines in
  let image = expand_image image in
  Printf.printf "Part One: %d\n" (part1 image)
