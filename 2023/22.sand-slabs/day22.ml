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

module Graph (V : Set.OrderedType) = struct
  (** Vertices set. *)
  module VSet = Set.Make (V)

  (** The type of graphs. *)
  type t = {
    outgoing : (V.t, VSet.t) Hashtbl.t;
    incoming : (V.t, VSet.t) Hashtbl.t;
  }

  (** [create ()] creates an empty graph. *)
  let create () = {
    outgoing = Hashtbl.create 64;
    incoming = Hashtbl.create 64;
  }

  (** [add_edge graph a b] adds the edge [a]->[b] to [graph]. *)
  let add_edge { outgoing; incoming } a b =
    Hashtbl.replace outgoing a
      (Hashtbl.find_opt outgoing a |> Option.value ~default:VSet.empty |> VSet.add b);
    Hashtbl.replace incoming b
      (Hashtbl.find_opt incoming b |> Option.value ~default:VSet.empty |> VSet.add a)

  (** [remove_source_and_dependents graph start] performs an iterative removal
      of source vertices (vertices with no incoming edges) from [graph],
      starting with the [start] vertex, and subsequently removes any vertices
      that become sources as a result.  The starting vertex may have incoming
      edges. *)
  let remove_source_and_dependents { outgoing; incoming } src =
    let q = Queue.create () and removed = ref VSet.empty in
    Queue.add src q;
    while not (Queue.is_empty q) do
      let v = Queue.pop q in
      removed := VSet.add v !removed;
      match Hashtbl.find_opt outgoing v with
      | None       -> ()
      | Some v_out ->
        VSet.iter
          (fun u ->
             if VSet.diff (Hashtbl.find incoming u) !removed |> VSet.cardinal = 0 then
               Queue.push u q)
          v_out
    done;
    !removed
end

module BrickGraph = Graph (Brick)

(** [build_support_graph bricks] builds a graph in which an edge [a]->[b]
    exists if the brick [a] supports the brick [b]. *)
let build_support_graph bricks =
  let g = BrickGraph.create () in
  let rec build_iter seen = function
    | []              -> ()
    | brick :: bricks ->
      supporting_bricks brick seen
      |> BrickSet.iter (fun sbrick -> BrickGraph.add_edge g sbrick brick);
      build_iter (brick :: seen) bricks
  in bricks |> List.sort compare_z |> build_iter [];
  g

(** [part2 bricks] for each brick determines how many other bricks would fall
    if that brick were disintegrated.  Returns the sum of the numbers of other
    bricks that would fall. *)
let part2 bricks =
  let bricks = drop_down_bricks bricks in
  let graph = build_support_graph bricks in
  bricks
  |> List.map (fun brick ->
      BrickSet.cardinal (BrickGraph.remove_source_and_dependents graph brick) - 1)
  |> List.fold_left (+) 0

let () =
  let bricks = open_in "input"
               |> In_channel.input_lines
               |> List.map Brick.parse
  in
  Printf.printf "Part One: %d\n" (part1 bricks);
  Printf.printf "Part Two: %d\n" (part2 bricks)
