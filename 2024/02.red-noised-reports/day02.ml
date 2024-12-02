(** Day 2: Red-Nosed Reports *)

(** [parse_report line] parses the given report string. *)
let parse_report line =
  line |> String.split_on_char ' ' |> List.map int_of_string

(** [is_safe dampener report] is [true] if [report] is safe, and [false]
    otherwise.  If [dampener] is [true], then [report] is still considered safe
    if it can be made safe by removing one level. *)
let rec is_safe dampener report =
  let are_adjacent_levels_safe inc l1 l2 =
    let diff = abs (l1 - l2) in
    ((inc && l1 < l2) || (not inc && l1 > l2)) &&
    1 <= diff && diff <= 3
  in

  let rec is_safe_aux inc dampened = function
    | []             -> assert false
    | [_]            -> true
    | l1 :: l2 :: tl ->
      are_adjacent_levels_safe inc l1 l2 && is_safe_aux inc dampened (l2 :: tl) ||
      (not dampened && is_safe_aux inc true (l1 :: tl))
  in

  match report with
  | [] | [_] -> true
  | _ :: tl  ->
    is_safe_aux true (not dampener) report ||
    is_safe_aux false (not dampener) report ||
    (dampener && is_safe false tl)

(** [count_safe_reports is_safe reports] is the number of safe reports among
    [reports]. *)
let count_safe_reports is_safe reports =
  reports |> List.filter is_safe |> List.length

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let reports = In_channel.input_lines ic |> List.map parse_report in
  Printf.printf "Part One: %d\n" @@ count_safe_reports (is_safe false) reports;
  Printf.printf "Part Two: %d\n" @@ count_safe_reports (is_safe true) reports
