(** Day 2: Red-Nosed Reports *)

(** [parse_report line] parses the given report string. *)
let parse_report line =
  line |> String.split_on_char ' ' |> List.map int_of_string

(** [is_safe report] is [true] if [report] is safe, and [false] otherwise. *)
let is_safe report =
  let rec is_safe_aux inc = function
    | []             -> assert false
    | [_]            -> true
    | l1 :: l2 :: tl ->
      let diff = abs (l1 - l2) in
      ((inc && (l1 < l2)) || (not inc && (l1 > l2))) &&
      1 <= diff && diff <= 3 &&
      is_safe_aux inc (l2 :: tl)
  in
  match report with
  | [] | [_]       -> true
  | l1 :: l2 :: tl -> is_safe_aux (l1 < l2) report

(** [count_safe_reports reports] is the number of safe reports among [reports]. *)
let count_safe_reports reports =
  reports |> List.filter is_safe |> List.length

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let reports = In_channel.input_lines ic |> List.map parse_report in
  Printf.printf "Part One: %d\n" @@ count_safe_reports reports
