(** Day 24: Never Tell Me The Odds *)

(** The type of hailstones. *)
type hailstone =
  {x : float; y : float; z : float; vx : float; vy : float; vz : float}

(** The type of lines. *)
type line = {a : float; b : float; c : float}

(** [parse_hailstone hs] parses a string representation of a hailstone. *)
let parse_hailstone hs =
  Scanf.sscanf hs "%f, %f, %f @ %f, %f, %f"
    (fun x y z vx vy vz -> {x; y; z; vx; vy; vz})

(** [line_of_hailstone hs] returns the line corresponding to the trajectory of
    [hs]. *)
let line_of_hailstone {x; y; vx; vy; _} =
  {a = vy; b = -.vx; c = -.x *. vy +. y *. vx}

(** [intersection_point l1 l2] computes the intersection point of [l1] and
    [l2]. *)
let intersection_point {a = a1; b = b1; c = c1} {a = a2; b = b2; c = c2} =
  if a1 *. b2 <> a2 *. b1 then
    Some ((b1 *. c2 -. c1 *. b2) /. (a1 *. b2 -. b1 *. a2),
          (a1 *. c2 -. c1 *. a2) /. (b1 *. a2 -. a1 *. b2))
  else
    None

(** [point_inside_test_area p] checks whether [p] is inside the test area. *)
let point_inside_test_area (x, y) =
  200000000000000. <= x && x <= 400000000000000. &&
  200000000000000. <= y && y <= 400000000000000.

(** [point_on_path p hs] checks whether [p] is on the path of [hs]. *)
let point_on_path (x', y') {x; y; vx; vy; _} =
  (x' -. x) *. vx >= 0. && (y' -. y) *. vy >= 0.

(** [part1 hss] computes how many intersections of [hss] occur within the test
    area. *)
let rec part1 = function
  | []         -> 0
  | hs1 :: hss ->
    part1 hss +
    (hss
     |> List.map
       (fun hs2 ->
          match intersection_point (line_of_hailstone hs1) (line_of_hailstone hs2) with
          | None   -> 0
          | Some p ->
            if point_inside_test_area p && List.for_all (point_on_path p) [hs1; hs2] then 1 else 0)
     |> List.fold_left (+) 0)

let () =
  let hailstones = open_in "input" |> In_channel.input_lines |> List.map parse_hailstone in
  Printf.printf "Part One: %d\n" (part1 hailstones)
