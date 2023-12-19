(** Day 19: Aplenty *)

module Part = struct
  (** The type of parts. *)
  type t = (char * int) list

  (** [parse_part_rating rating] parses a string representation of a single
      rating of a part. *)
  let parse_part_rating rating =
    let category = rating.[0]
    and rating = int_of_string (String.sub rating 2 (String.length rating - 2))
    in category, rating

  (** [parse part] parses a string representation of a part. *)
  let parse part =
    String.sub part 1 (String.length part - 2)
    |> String.split_on_char ','
    |> List.map parse_part_rating

  (** [value part] calculates the value of [part]. *)
  let value part = part |> List.map snd |> List.fold_left (+) 0
end

module Workflow = struct
  (** The type of rule targets. *)
  type target =
    | GoTo of string
    | Accept
    | Reject

  (** The type of workflow rules. *)
  type rule = {
    pred : Part.t -> bool;
    target : target;
  }

  (** [parse_pred pred] parses a string representation of a workflow rule's
      predicate. *)
  let parse_pred pred =
    let category = pred.[0] and comparison = pred.[1]
    and value = int_of_string (String.sub pred 2 (String.length pred - 2)) in
    match comparison with
    | '<' -> fun part -> List.assoc category part < value
    | '>' -> fun part -> List.assoc category part > value
    | _   -> failwith "Workflow.parse_pred"

  (** [parse_target target] parses a string representation of a workflow rule's
      target. *)
  let parse_target = function
    | "A"    -> Accept
    | "R"    -> Reject
    | target -> GoTo target

  (** [parse_rule rule] parses a string representation of a workflow rule. *)
  let parse_rule rule =
    match String.split_on_char ':' rule with
    | [pred; target] -> { pred = parse_pred pred; target = parse_target target }
    | [target]       -> { pred = (fun _ -> true); target = parse_target target }
    | _              -> failwith "Workflow.parse_rule"

  (** [parse workflow] parses a string representation of a workflow. *)
  let parse workflow =
    let open Str in
    if string_match (regexp "\\([^{]+\\){\\([^}]+\\)}") workflow 0 then
      let name = matched_group 1 workflow and rules = matched_group 2 workflow in
      name, rules |> String.split_on_char ',' |> List.map parse_rule
    else
      failwith "Workflow.parse"

  (** [parse_workflows lines] parses a list of string representations of
      workflows. *)
  let parse_workflows lines =
    let workflows = Hashtbl.create 1024 in
    lines |> List.iter (fun line ->
        let name, workflow = parse line in
        Hashtbl.add workflows name workflow);
    workflows

  (** [execute workflow part] executes [workflow] for [part] and returns the
      matching target. *)
  let rec execute workflow part =
    match workflow with
    | { pred; target } :: rules' ->
      if pred part then target else execute rules' part
    | [] ->
      assert false

  (** [execute_workflows workflows part] execute [workflows] for [part]
      starting with the workflow named ["in"] until the part is accepted or
      rejected. *)
  let execute_workflows workflows part =
    let rec execute_iter workflow =
      match execute workflow part with
      | Accept      -> true
      | Reject      -> false
      | GoTo target -> execute_iter (Hashtbl.find workflows target)
    in execute_iter (Hashtbl.find workflows "in")
end

(** [partition_at n lst] splits [lst] into two parts at the position [n]. *)
let rec partition_at n lst =
  match n, lst with
  | 0, _ | _, [] -> [], lst
  | n, h :: t    -> let a, b = partition_at (n - 1) t in h :: a, b

(** [parse_input lines] parses the input lines and returns the workflows and
    the ratings of the parts. *)
let parse_input lines =
  match List.find_index (fun line -> line = "") lines with
  | Some n ->
    let workflows, parts = partition_at n lines in
    Workflow.parse_workflows workflows, parts |> List.tl |> List.map Part.parse
  | None -> failwith "parse_input"

(** [part1 workflows parts] finds the sum of the rating numbers of all the
    parts accepted by [workflows]. *)
let part1 workflows parts =
  parts
  |> List.filter (Workflow.execute_workflows workflows)
  |> List.map Part.value
  |> List.fold_left (+) 0

let () =
  let workflows, parts = open_in "input" |> In_channel.input_lines |> parse_input in
  Printf.printf "Part One: %d\n" (part1 workflows parts)
