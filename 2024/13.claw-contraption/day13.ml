(** Day 13: Claw Contraption *)

(** The type representing a claw machine's configuration (buttons behavior and
    a prize location). *)
type claw_machine = { a : int * int; b : int * int; prize : int * int }

(** [parse_machines lines] parses the list of strings [lines] into a list of
    [claw_machine] records. *)
let parse_machines =
  let rec loop machines = function
    | [] -> List.rev machines
    | "" :: tl -> loop machines tl
    | a :: b :: p :: tl ->
      let a = Scanf.sscanf a "Button A: X+%d, Y+%d" (fun x y -> x, y)
      and b = Scanf.sscanf b "Button B: X+%d, Y+%d" (fun x y -> x, y)
      and prize = Scanf.sscanf p "Prize: X=%d, Y=%d" (fun x y -> x, y)
      in loop ({ a; b; prize } :: machines) tl
    | _ -> assert false
  in loop []

(** [solve_eq is_valid machine] finds the number of times buttons A and B need
    to be pressed to position the claw over the prize for the given
    [claw_machine].  [is_valid] is a function that checks if a number of button
    presses is valid.

    The function computes result by solving the following system of equations:

    - xa * a + xb * b = xp
    - ya * a + yb * b = yp
*)
let solve_machine is_valid { a = xa, ya; b = xb, yb; prize = xp, yp } =
  let ka = xa * yb - ya * xb and kp = xp * yb - yp * xb in
  if ka = 0 then
    if kp = 0 && xp mod xb = 0 then Some (0, xp / xb) else None
  else
    let a = kp / ka and ra = kp mod ka in
    if ra <> 0 || not (is_valid a) then
      None
    else
      let b = (xp - xa * a) / xb and rb = (xp - xa * a) mod xb in
      if rb <> 0 || not (is_valid b) then
        None
      else
        Some (a, b)

(** [tokens_number (a, b)] calculates the number of tokens required to press
    button A [a] times and button B [b] times. *)
let tokens_number (a, b) = a * 3 + b

(** [solve machines] computes the minimum number of tokens required to
    win prizes from as many claw machines as possible. *)
let solve is_valid machines =
  machines
  |> List.map (solve_machine is_valid)
  |> List.filter_map Fun.id
  |> List.map tokens_number
  |> List.fold_left (+) 0

let fix_position { a; b; prize = xp, yp } =
  { a; b; prize = xp + 10_000_000_000_000, yp + 10_000_000_000_000 }

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let machines = In_channel.input_lines ic |> parse_machines in
  Printf.printf "Part One: %d\n" @@ solve (fun n -> 0 <= n && n <= 100) machines;
  let machines' = List.map fix_position machines in
  Printf.printf "Part Two: %d\n" @@ solve (fun n -> 0 <= n) machines'
