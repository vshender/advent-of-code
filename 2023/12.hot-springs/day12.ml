(** Day 12: Hot Springs *)

open Seq

(** The type of spring conditions. *)
type spring = Operational | Damaged | Unknown

(** [parse_spring c] parses a character [c] representing a spring condition. *)
let parse_spring = function
  | '.' -> Operational
  | '#' -> Damaged
  | '?' -> Unknown
  | _   -> failwith "parse_spring"

(** [parse_condition_record_1 record] parses a string representation of a
    conditional record as described in part 1. *)
let parse_condition_record_1 record =
  let open Str in
  Scanf.sscanf record "%s %s"
    (fun springs damaged_groups ->
       String.to_seq springs |> map parse_spring |> Array.of_seq,
       split (regexp ", *") damaged_groups |> List.map int_of_string |> Array.of_list)

(** [parse_condition_record_2 record] parses a string representation of a
    conditional record as described in part 2. *)
let parse_condition_record_2 record =
  let open Str in
  Scanf.sscanf record "%s %s"
    (fun springs damaged_groups ->
       let springs = String.concat "?" (repeat springs |> take 5 |> List.of_seq)
       and damaged_groups = String.concat "," (repeat damaged_groups |> take 5 |> List.of_seq) in
       String.to_seq springs |> map parse_spring |> Array.of_seq,
       split (regexp ", *") damaged_groups |> List.map int_of_string |> Array.of_list)

(** [range a b] generates a sequence of integers from [a] to [b - 1]. *)
let range a b = ints a |> take (b - a)

(** [fits springs damaged_group j] checks if the given group of damaged springs
    fits at position [j] in [springs]. *)
let fits springs damaged_group j =
  let from = j - damaged_group + 1 in
  from >= 0 &&
  (from = 0 || springs.(from - 1) <> Damaged) &&
  (range from (j + 1) |> for_all (fun i -> springs.(i) <> Operational))

(** [count cond_record] calculates the number of different arrangements of
    operational and broken springs that match the given conditional record. *)
let count (springs, damaged_groups) =
  let n = Array.length damaged_groups and m = Array.length springs in
  let dp = Array.make_matrix (n + 1) (m + 1) 0 in

  dp.(0).(0) <- 1;
  range 1 m
  |> take_while (fun i -> springs.(i - 1) <> Damaged)
  |> iter (fun i -> dp.(0).(i) <- 1);

  for i = 1 to n do
    let damaged_group = damaged_groups.(i - 1) in
    for j = 1 to m do
      if springs.(j - 1) <> Damaged then
        dp.(i).(j) <- dp.(i).(j - 1);
      if fits springs damaged_group (j - 1) then
        let j' = j - damaged_group - 1 in
        dp.(i).(j) <- dp.(i).(j) + dp.(i - 1).(if j' >= 0 then j' else 0)
    done
  done;

  dp.(n).(m)

(** [solve records] solves the problem for a list of condition records, finding
    the total number of possible spring arrangements. *)
let solve records =
  records |> List.map count |> List.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  Printf.printf "Part One: %d\n" (lines |> List.map parse_condition_record_1 |> solve);
  Printf.printf "Part Two: %d\n" (lines |> List.map parse_condition_record_2 |> solve)
