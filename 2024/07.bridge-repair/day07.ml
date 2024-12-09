(** Day 7: Bridge Repair *)

open List

(** [parse_equation line] parses an equation. *)
let parse_equation line =
  let test_value, numbers = Scanf.sscanf line "%d: %[0-9 ]" (fun tv ns -> (tv, ns)) in
  test_value, numbers |> String.split_on_char ' ' |> map int_of_string

(** [solve_equation ops (test_value, numbers)] checks whether the equation can be
    made true by inserting operators from [ops] between the numbers. *)
let solve_equation ops (test_value, numbers)  =
  let rec solve_equation_aux value = function
    | []      -> value = test_value
    | n :: ns ->
      List.exists
        (fun op -> try solve_equation_aux (op value n) ns with Invalid_argument _ -> false)
        ops
  in solve_equation_aux (hd numbers) (tl numbers)

(** [concat_nums a b] combines the digits of two numbers [a] and [b] into a
    single number. *)
let concat_nums a b =
  Printf.sprintf "%d%d" a b |> int_of_string

(** [solve f equations] calculates the sum of all test values from [equations]
    that satisfy the predicate [f]. *)
let solve f equations =
  equations
  |> List.filter f
  |> List.map fst
  |> List.fold_left (+) 0

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let equations = In_channel.input_lines ic |> List.map parse_equation in
  Printf.printf "Part One: %d\n" @@ solve (solve_equation [( + ); ( * )]) equations;
  Printf.printf "Part Two: %d\n" @@ solve (solve_equation [( + ); ( * ); concat_nums]) equations
