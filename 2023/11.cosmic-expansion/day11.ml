(** Day 11: Cosmic Expansion *)

open List

(** [parse_image lines] parses [lines] and returns the galaxies. *)
let parse_image lines =
  lines
  |> mapi (fun i line ->
      line
      |> String.to_seqi
      |> Seq.filter (fun (_, c) -> c = '#')
      |> Seq.map (fun (j, _) -> (i, j))
      |> of_seq)
  |> flatten

(** [get_size galaxies] returns the size of the universe. *)
let get_size galaxies =
  let (n, m) =
    fold_left (fun (mi, mj) (i, j) -> (max mi i, max mj j)) (0, 0) galaxies
  in (n + 1, m + 1)

(** [range a b] generates a list of integers from [a] to [b - 1]. *)
let rec range a b =
  if a < b then a :: range (a + 1) b else []

(** [rows_to_expand galaxies n] finds all the rows of the universe to
    expand. *)
let rows_to_expand galaxies n =
  range 0 n |> filter (fun x -> galaxies |> for_all (fun (i, _) -> x <> i))

(** [columns_to_expand galaxies m] finds all the columns of the universe to
    expand. *)
let columns_to_expand galaxies m =
  range 0 m |> filter (fun y -> galaxies |> for_all (fun (_, j) -> y <> j))

(** [expand galaxies expansion_factor] expands the universe using
    [expansion_factor]. *)
let expand galaxies expansion_factor =
  let n, m = get_size galaxies in
  let re = rows_to_expand galaxies n and ce = columns_to_expand galaxies m in
  galaxies
  |> map (fun (i, j) ->
      (i + (re |> filter (fun r -> r < i) |> length) * (expansion_factor - 1), j))
  |> map (fun (i, j) ->
      (i, j + (ce |> filter (fun c -> c < j) |> length) * (expansion_factor - 1)))

(** [distance a b] calculates Manhattan distance between [a] and [b]. *)
let distance (i, j) (i', j') = abs (i - i') + abs (j - j')

(** [solve galaxies expansion_factor] finds the sum of the lengths of the
    shortest paths between all pairs of [galaxies]. *)
let solve galaxies expansion_factor =
  let galaxies = expand galaxies expansion_factor in
  let sum =
    galaxies
    |> map (fun from -> map (distance from) galaxies)
    |> flatten
    |> fold_left (+) 0
  in sum / 2

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let galaxies = parse_image lines in
  Printf.printf "Part One: %d\n" (solve galaxies 2);
  Printf.printf "Part Two: %d\n" (solve galaxies 1_000_000)
