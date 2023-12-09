(** Day 8: Haunted Wasteland *)

(** The type of node directions. *)
type node_directions = { left : string; right : string }

(** The nodes hash table. *)
module Nodes = Hashtbl.Make(String)

(** [parse_node node] parses a string representation of a node. *)
let parse_node node =
  Scanf.sscanf node "%3s = (%3s, %3s)" (fun node left right -> (node, { left; right }))

(** [parse_map lines] parses [lines] and gets the map. *)
let parse_map lines =
  let instructions = lines |> List.hd |> String.to_seq |> List.of_seq in
  let node_lines = List.tl (List.tl lines) in
  let nodes = Nodes.create (List.length node_lines) in
  node_lines
  |> List.map parse_node
  |> List.iter (fun (node, dirs) -> Nodes.add nodes node dirs);
  instructions, nodes

(** [walk nodes instructions start] starts at the node [start] and follows
    [instructions] until it reaches a node whose name ends with ['Z']. *)
let walk nodes instructions start =
  let rec walk_iter count node instrs =
    if String.ends_with ~suffix:"Z" node then
      count
    else
      match instrs with
      | []            -> walk_iter count node instructions
      | 'L' :: instrs -> walk_iter (count + 1) (Nodes.find nodes node).left instrs
      | 'R' :: instrs -> walk_iter (count + 1) (Nodes.find nodes node).right instrs
      | _             -> failwith "walk"
  in walk_iter 0 start instructions

(** [part1 instructions nodes] calculates how many steps it takes to reach a
    node whose name ends with ['Z'] if you start at the node ["AAA"]. *)
let part1 instructions nodes =
  walk nodes instructions "AAA"

(** [gcd a b] is the greatest common divisor of [a] and [b]. *)
let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

(** [lcm a b] is the least common multiple of [a] and [b]. *)
let lcm a b = a * b / gcd a b

(** [part2 instructions nodes] calculates how many steps it takes to reach
    nodes whose names end with ['Z'] if you simultaneously start at every node
    whose name ends with ['A']. *)
let part2 instructions nodes =
  Nodes.fold
    (fun node _ acc ->
       if String.ends_with ~suffix:"A" node then
         lcm acc (walk nodes instructions node)
       else
         acc)
    nodes
    1

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let instructions, nodes = parse_map lines in
  Printf.printf "Part One: %d\n" (part1 instructions nodes);
  Printf.printf "Part Two: %d\n" (part2 instructions nodes)
