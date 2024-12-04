(** Day 4: Ceres Search *)

(** [count_xmas_occurrences grid n m i j] computes the number of valid
    occurrences of the word "XMAS" starting at position [(i, j)] in [grid].
    It checks all 8 possible directions (horizontal, vertical, and
    diagonal). *)
let count_xmas_occurrences grid n m i j =
  let xmas = "XMAS" in
  List.fold_left
    (fun cnt (di, dj) ->
       try
         for k = 0 to String.length xmas - 1 do
           let i' = i + di * k and j' = j + dj * k in
           if i' < 0 || i' >= n || j' < 0 || j' >= m || grid.(i').(j') <> xmas.[k] then
             raise Not_found
         done;
         cnt + 1
       with Not_found ->
         cnt)
    0
    [(1, 0); (-1, 0); (0, 1); (0, -1); (1, 1); (-1, -1); (1, -1); (-1, 1)]

(** [count_x_mas_shape_occurrence grid n m i j] checks for the specific "X-MAS"
    shape in [grid] at position [(i, j)].  Returns [1] if the shape is found,
    and [0] otherwise. *)
let count_x_mas_shape_occurrence grid n m i j =
  if i < n - 2 && j < m - 2 &&
     grid.(i + 1).(j + 1) = 'A' &&
     ((grid.(i).(j) = 'M' && grid.(i + 2).(j + 2) = 'S') ||
      (grid.(i).(j) = 'S' && grid.(i + 2).(j + 2) = 'M')) &&
     ((grid.(i).(j + 2) = 'M' && grid.(i + 2).(j) = 'S') ||
      (grid.(i).(j + 2) = 'S' && grid.(i + 2).(j) = 'M')) then
    1
  else
    0

(** [count_words grid count_func] computes the total count of occurrences of
    the pattern in [grid] using the given [count_func]. *)
let count_words grid count_func =
  let n = Array.length grid and m = Array.length grid.(0) in
  let cnt = ref 0 in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      cnt := !cnt + count_func grid n m i j
    done
  done;
  !cnt

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let grid =
    In_channel.input_lines ic
    |> List.map (fun line -> Array.init (String.length line) (String.get line))
    |> Array.of_list
  in
  Printf.printf "Part One: %d\n" @@ count_words grid count_xmas_occurrences;
  Printf.printf "Part Two: %d\n" @@ count_words grid count_x_mas_shape_occurrence
