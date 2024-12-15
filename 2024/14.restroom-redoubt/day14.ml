(** Day 14: Restroom Redoubt *)

(** The size of the space. *)
let n = 101 and m = 103

(** The type of robots. *)
type robot = { px : int; py : int; vx : int; vy : int }

(** [parse_robot line] parses [line] into a [robot] record. *)
let parse_robot line =
  Scanf.sscanf line "p=%d,%d v=%d,%d" (fun px py vx vy -> { px; py; vx; vy })

(** [step robot] performs a single step of [robot]. *)
let step { px; py; vx; vy } =
  { px = (px + vx + n) mod n; py = (py + vy + m) mod m; vx; vy }

(** [repeat f n x] applies the function [f] to [x], [n] times. *)
let rec repeat f n x =
  if n = 0 then x else f (repeat f (n - 1) x)

(** [safety_factor robots] calculates the safety factor for [robots]. *)
let safety_factor robots =
  let q1, q2, q3, q4 =
    List.fold_left
      (fun (q1, q2, q3, q4) { px; py; _ } ->
         if px < n / 2 && py < m / 2 then
           (q1 + 1, q2, q3, q4)
         else if px > n / 2 && py < m / 2 then
           (q1, q2 + 1, q3, q4)
         else if px > n / 2 && py > m / 2 then
           (q1, q2, q3 + 1, q4)
         else if px < n / 2 && py > m / 2 then
           (q1, q2, q3, q4 + 1)
         else
           (q1, q2, q3, q4))
      (0, 0, 0, 0)
      robots
  in q1 * q2 * q3 * q4

(** [print_robots robots] prints [robots] to the screen. *)
let print_robots robots =
  let lines = Array.init m (fun _ -> Bytes.make n ' ') in
  List.iter (fun { px; py; _ } -> Bytes.set lines.(py) px '*') robots;
  Array.iter (fun line -> Printf.printf "%s\n" @@ Bytes.unsafe_to_string line) lines

(** The set of points. *)
module PointSet = Set.Make (struct type t = (int * int) let compare = compare end)

(** [all_positions_unique robots] is [true] if all the robots occupy different
    positions. *)
let all_positions_unique robots =
  List.length robots = PointSet.cardinal
    (List.fold_left
       (fun rset { px; py; _ } -> PointSet.add (px, py) rset)
       PointSet.empty robots)

(** [part1 robots] solves the part one of the problem. *)
let part1 robots =
  robots |> List.map (repeat step 100) |> safety_factor

(** [part2 robots] solves the part two of the problem. *)
let part2 robots =
  let rec loop robots n =
    let robots = List.map step robots in
    if all_positions_unique robots then begin
      print_robots robots;
      n
    end else
      loop robots (n + 1)
  in loop robots 1

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let robots = In_channel.input_lines ic |> List.map parse_robot in
  Printf.printf "Part One: %d\n" @@ part1 robots;
  Printf.printf "Part Two: %d\n" @@ part2 robots
