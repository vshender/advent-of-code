(** Day 22: Sand Slabs *)

module Brick = struct
  (** The type of cubes. *)
  type cube = {x : int; y : int; z : int}

  (** The type of bricks. *)
  type t = {s : cube; e : cube}

  (** [parse brick] parses a string representation of a brick. *)
  let parse brick =
    Scanf.sscanf brick "%d,%d,%d~%d,%d,%d"
      (fun sx sy sz ex ey ez ->
         {s = {x = sx; y = sy; z = sz}; e = {x = ex; y = ey; z = ez}})

  (** [overlap a b] checks whether the projections of [a] and [b] onto the
      XY-plane overlap. *)
  let overlap a b =
    max a.s.x b.s.x <= min a.e.x b.e.x && max a.s.y b.s.y <= min a.e.y b.e.y

  (** [compare a b] compares [a] and [b]. *)
  let compare = Stdlib.compare
end

module BrickSet = Set.Make (Brick)

(** [compare_z a b] compares the z-coordinates of the top ends of the bricks
    [a] and [b]. *)
let compare_z a b = Brick.(Stdlib.compare a.e.z b.e.z)

(** [drop_down_brick brick bricks] drops [brick] down until it is settled on
    [bricks] that are located below it. *)
let drop_down_brick brick bricks =
  let open Brick in
  let brick_height = brick.e.z - brick.s.z + 1 in
  match List.(bricks |> filter (overlap brick) |> sort compare_z |> rev) with
  | first_supporting_brick :: _ ->
    let z = first_supporting_brick.e.z + 1 in
    {s = {brick.s with z}; e = {brick.e with z = z + brick_height - 1}}
  | [] ->
    {s = {brick.s with z = 1}; e = {brick.e with z = brick_height}}

(** [drop_down_bricks bricks] drops [bricks] down until they are settled on the
    bricks below them. *)
let drop_down_bricks bricks =
  let rec drop_down_iter settled_bricks = function
    | []               -> settled_bricks
    | brick :: bricks' ->
      drop_down_iter (drop_down_brick brick settled_bricks :: settled_bricks) bricks'
  in bricks |> List.sort compare_z |> drop_down_iter []

(** [supporting_bricks brick bricks] returns the set of bricks from [bricks]
    that support [brick]. *)
let supporting_bricks brick bricks =
  bricks
  |> List.filter (fun brick' ->
      Brick.overlap brick brick' && brick.s.z = brick'.e.z + 1)
  |> BrickSet.of_list

(** [find_supporting_bricks bricks] finds the supporting bricks for [bricks]. *)
let find_supporting_bricks bricks =
  let rec find_iter acc = function
    | []              -> acc
    | brick :: bricks ->
      find_iter ((brick, acc |> List.map fst |> supporting_bricks brick) :: acc) bricks
  in bricks |> List.sort compare_z |> find_iter []

(** [part1 bricks] determines how many bricks could be safely chosen as the one
    to get disintegrated. *)
let part1 bricks =
  let cannot_be_desintegrated =
    bricks
    |> drop_down_bricks
    |> find_supporting_bricks
    |> List.fold_left
      (fun acc (brick, supporting_bricks) ->
         if BrickSet.cardinal supporting_bricks = 1 then
           BrickSet.add (BrickSet.choose supporting_bricks) acc
         else
           acc)
      BrickSet.empty
  in
  List.length bricks - BrickSet.cardinal cannot_be_desintegrated

let () =
  let bricks = open_in "input"
               |> In_channel.input_lines
               |> List.map Brick.parse
  in
  Printf.printf "Part One: %d\n" (part1 bricks)
