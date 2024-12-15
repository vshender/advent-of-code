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

(** [part1 robots] solves the part one of the problem. *)
let part1 robots =
  robots |> List.map (repeat step 100) |> safety_factor

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let robots = In_channel.input_lines ic |> List.map parse_robot in
  Printf.printf "Part One: %d\n" @@ part1 robots
