(** Day 13: Point of Incidence *)

open List

(** [parse_patterns data] parses a string representation of patterns. *)
let parse_patterns data =
  let open Str in
  data |> split (regexp "\n\n") |> map (
    fun pattern -> pattern
                   |> split (regexp "\n")
                   |> map (fun line -> line |> String.to_seq |> of_seq))

(** [partition_at n lst] splits [lst] into two parts at the position [n]. *)
let rec partition_at n lst =
  match n, lst with
  | 0, _ | _, [] -> [], lst
  | n, h :: t    -> let a, b = partition_at (n - 1) t in h :: a, b

(** [transpose lsts] transposes the given list of lists. *)
let rec transpose = function
  | [] | [] :: _ -> []
  | lsts         -> map hd lsts :: transpose (map tl lsts)

(** [count_mismatches a b] counts the total number of mismatches between
    corresponding elements in two lists of lists [a] and [b]. *)
let rec count_mismatches a b =
  match a, b with
  | [], _ | _, []      -> 0
  | ha :: ta, hb :: tb ->
    (combine ha hb |> filter (fun (x, y) -> x <> y) |> length) +
    count_mismatches ta tb

(** [find_reflection smudges pattern] finds the horizontal line of reflection
    in [pattern].  [smudges] is a number of smudges to fix. *)
let find_reflection smudges pattern =
  let rec find_mirror_iter = function
    | 0 -> 0
    | i ->
      let a, b = partition_at i pattern in
      if count_mismatches (rev a) b = smudges then
        i
      else
        find_mirror_iter (i - 1)
  in find_mirror_iter (length pattern - 1)

(** [pattern_value smudges pattern] summarizes the given pattern based on found
    reflections. *)
let summarize smudges pattern =
  find_reflection smudges pattern * 100 +
  find_reflection smudges (transpose pattern)

(** [solve smudges patterns] solves the problem by summarizing all the given
    patterns. *)
let solve smudges patterns =
  patterns |> map (summarize smudges) |> fold_left (+) 0

let () =
  let input = open_in "input" |> In_channel.input_all in
  let patterns = parse_patterns input in
  Printf.printf "Part One: %d\n" (solve 0 patterns);
  Printf.printf "Part Two: %d\n" (solve 1 patterns)
