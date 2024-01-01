(** Day 23: A Long Walk *)

(** The type of directions. *)
type direction = Up | Right | Down | Left

(** The type of cells. *)
type cell = Path | Forest | Slope of direction

(** [parse_direction dir] parses a character representation of a direction. *)
let parse_direction = function
  | '^' -> Up
  | '>' -> Right
  | 'v' -> Down
  | '<' -> Left
  | _   -> failwith "parse_direction"

(** [parse_cell cell] parses a character representation of a cell. *)
let parse_cell = function
  | '.'                        -> Path
  | '#'                        -> Forest
  | '^' | '>' | 'v' | '<' as d -> Slope (parse_direction d)
  | _                          -> failwith "parse_direction"

(** [parse_map lines] parses [lines] and gets the map. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> parse_cell line.[i]))
  |> Array.of_list

(** [find_path_in_row map x] finds a path cell in the row [x] of [map]. *)
let find_path_in_row map x =
  match Array.find_index (fun c -> c = Path) map.(x) with
  | Some y -> (x, y)
  | None   -> failwith "find_path_in_row"

(** [direction dir] converts the given direction into a corresponding
    coordinates offset. *)
let direction = function
  | Up    -> (-1, 0)
  | Right -> (0, 1)
  | Down  -> (1, 0)
  | Left  -> (0, -1)

module Graph (V : Set.OrderedType) = struct
  (** The type of outgoing edges. *)
  type 'a edges = (V.t, 'a) Hashtbl.t

  (** The type of graphs. *)
  type 'a t = (V.t, 'a edges) Hashtbl.t

  (** [create ()] creates an empty graph. *)
  let create () = Hashtbl.create 64

  (** [add_edge graph v u l] adds the edge [v]->[u] with the label [l] to
      [graph]. *)
  let add_edge graph v u l =
    if not (Hashtbl.mem graph v) then
      Hashtbl.add graph v (Hashtbl.create 64);
    Hashtbl.add (Hashtbl.find graph v) u l

  (** [outgoing graph v] retrieves the outgoing edges of [v]. *)
  let outgoing graph v =
    match Hashtbl.find_opt graph v with
    | None       -> []
    | Some edges -> Hashtbl.fold (fun u l acc -> (u, l) :: acc) edges []
end

module Pos = struct type t = int * int let compare = compare end
module PosSet = Set.Make (Pos)
module PosGraph = Graph (Pos)

(** [build_graph map possible_directions] builds an undirected graph out of all
    crossings (i.e., positions with more than two neighbours), as well as the
    starting and ending position. *)
let build_graph map possible_directions =
  let n = Array.length map and m = Array.length map.(0) in
  let s = find_path_in_row map 0 and e = find_path_in_row map (n - 1) in

  let graph = PosGraph.create () in

  let vertices =
    let vertices = ref (PosSet.of_list [s; e]) in
    for x = 0 to n - 1 do
      for y = 0 to m - 1 do
        if map.(x).(y) != Forest then
          let neighbors =
            [Up; Right; Down; Left]
            |> List.map direction
            |> List.filter
              (fun (dx, dy) ->
                 let x' = x + dx and y' = y + dy in
                 0 <= x' && x' < n && 0 <= y' && y' < m && map.(x').(y') <> Forest)
            |> List.length
          in
          if neighbors > 2 then
            vertices := PosSet.add (x, y) !vertices
      done
    done;
    !vertices
  in

  let add_edges_from v =
    let rec dfs seen l (x, y) =
      if PosSet.mem (x, y) vertices && l > 0 then
        PosGraph.add_edge graph v (x, y) l
      else
        map.(x).(y)
        |> possible_directions
        |> List.map direction
        |> List.iter (fun (dx, dy) ->
            let x' = x + dx and y' = y + dy in
            if 0 <= x' && x' < n && 0 <= y' && y' < m &&
               map.(x').(y') <> Forest && not (PosSet.mem (x', y') seen) then
              dfs (PosSet.add (x', y') seen) (l + 1) (x', y'))
    in dfs (PosSet.singleton v) 0 v
  in
  PosSet.iter add_edges_from vertices;

  graph, s, e

(** [find_longest_path graph s e] computes the length of the longest path
    between the positions [s] and [e] on [map]. *)
let find_longest_path graph s e =
  let rec dfs seen v =
    let seen = PosSet.add v seen in
    if v = e then
      0
    else
      PosGraph.outgoing graph v
      |> List.sort compare
      |> List.fold_left
        (fun max_len (u, l) ->
           max max_len (if not (PosSet.mem u seen) then (l + dfs seen u) else Int.min_int))
        Int.min_int
  in dfs PosSet.empty s

(** [part1 map] computes how many steps long is the longest hike on [map]
    according to part one. *)
let part1 map =
  let graph, s, e = build_graph map
      (function
        | Path      -> [Up; Right; Down; Left]
        | Forest    -> []
        | Slope dir -> [dir])
  in find_longest_path graph s e

(** [part2 map] computes how many steps long is the longest hike on [map]
    according to part two. *)
let part2 map =
  let graph, s, e = build_graph map
      (function
        | Path | Slope _ -> [Up; Right; Down; Left]
        | Forest         -> [])
  in find_longest_path graph s e

let () =
  let map = open_in "input" |> In_channel.input_lines |> parse_map in
  Printf.printf "Part One: %d\n" (part1 map);
  Printf.printf "Part Two: %d\n" (part2 map)
