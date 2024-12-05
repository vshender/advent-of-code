(** Day 05: Print Queue *)

module IntSet = Set.Make (Int)

(** [parse_ordering_rules lines] parses page ordering rules. *)
let parse_ordering_rules lines =
  let rules = Hashtbl.create 128 in
  List.iter
    (fun line ->
       match String.split_on_char '|' line |> List.map int_of_string with
       | [a; b] ->
         Hashtbl.find_opt rules a
         |> Option.value ~default:IntSet.empty
         |> fun s -> Hashtbl.replace rules a (IntSet.add b s)
       | _ -> assert false)
    lines;
  rules

(** [parse_updates lines] parses updates. *)
let parse_updates =
  List.map
    (fun line ->
       String.split_on_char ',' line
       |> List.map int_of_string
       |> Array.of_list)

(** [parse_input lines] parses the problem input and returns a list of page
    ordering rules and a list of updates. *)
let parse_input =
  let rec loop ordering_rules = function
    | "" :: lines   -> parse_ordering_rules ordering_rules, parse_updates lines
    | line :: lines -> loop (line :: ordering_rules) lines
    | _             -> assert false
  in loop []

(** [pages_ordered ordering_rules p1 p2] is [true] if [p1] and [p2] are ordered
    according to [ordering_rules]. *)
let pages_ordered ordering_rules p1 p2 =
  try
    Hashtbl.find ordering_rules p1 |> IntSet.mem p2
  with
    Not_found -> false

(** [check_update ordering_rules update] is [true] if [update] is already in
    the right order according to [ordering_rules]. *)
let check_update ordering_rules update =
  let exception Unordered in
  try
    for i = 0 to Array.length update - 2 do
      for j = i + 1 to Array.length update - 1 do
        if not (pages_ordered ordering_rules update.(i) update.(j)) then
          raise Unordered
      done
    done;
    true
  with Unordered ->
    false

(** [fix_update ordering_rules update] fixes [update] ordering according to
    [ordering_rules]. *)
let fix_update ordering_rules update =
  for i = 0 to Array.length update - 2 do
    for j = i + 1 to Array.length update - 1 do
      if not (pages_ordered ordering_rules update.(i) update.(j)) then begin
        let t = update.(i) in
        update.(i) <- update.(j);
        update.(j) <- t
      end
    done
  done;
  update

(** [middle_page_number update] is the middle page number of [update]. *)
let middle_page_number update =
  update.(Array.length update / 2)

(** [solve1 ordering_rules update] solves the part one of the problem. *)
let solve1 ordering_rules updates =
  updates
  |> List.filter (check_update ordering_rules)
  |> List.map middle_page_number
  |> List.fold_left (+) 0

(** [solve2 ordering_rules update] solves the part two of the problem. *)
let solve2 ordering_rules updates =
  updates
  |> List.filter (fun update -> not (check_update ordering_rules update))
  |> List.map (fix_update ordering_rules)
  |> List.map middle_page_number
  |> List.fold_left (+) 0

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let ordering_rules, updates = In_channel.input_lines ic |> parse_input in
  Printf.printf "Day One: %d\n" @@ solve1 ordering_rules updates;
  Printf.printf "Day Two: %d\n" @@ solve2 ordering_rules updates
