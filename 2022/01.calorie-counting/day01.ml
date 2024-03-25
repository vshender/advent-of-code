(** Day 1: Calorie Counting *)

(** [parse_calories lines] parses [lines] and returns the list of items'
    calories grouped by elf. *)
let parse_calories =
  let rec parse_iter elves cur_elf = function
    | []           -> cur_elf :: elves
    | "" :: rest   -> parse_iter (cur_elf :: elves) [] rest
    | line :: rest -> parse_iter elves (int_of_string line :: cur_elf) rest
  in parse_iter [] []

(** [part1 calories] finds the elf carrying the most calories and returns the
    number of calories that elf carrying. *)
let part1 calories =
  calories
  |> List.map (List.fold_left (+) 0)
  |> List.sort (fun a b -> b - a)
  |> List.hd

(** [take lst] is the list of the first [n] elements of [lst]. *)
let rec take n lst =
  match n, lst with
  | 0, _      -> []
  | n, []     -> []
  | n, h :: t -> h :: take (n - 1) t

(** [part2 calories] finds the top three elves carrying the most calories and
    returns the number of calories those elves carrying in total. *)
let part2 calories =
  calories
  |> List.map (List.fold_left (+) 0)
  |> List.sort (fun a b -> b - a)
  |> take 3
  |> List.fold_left (+) 0

let () =
  let calories = open_in "input" |> In_channel.input_lines |> parse_calories in
  Printf.printf "Part One: %d\n" (part1 calories);
  Printf.printf "Part Two: %d\n" (part2 calories)
