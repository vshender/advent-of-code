(** Day 19: Aplenty *)

module Cond = struct
  (** The type of conditions. *)
  type t =
    | Lt of int
    | Gt of int

  (** [check cond x] checks if [x] satisfies [cond]. *)
  let check cond x = match cond with
    | Lt y -> x < y
    | Gt y -> x > y

  (** [parse cond] parses a string representation of a condition. *)
  let parse cond =
    let n = int_of_string (String.sub cond 1 (String.length cond - 1)) in
    match cond.[0] with
    | '<' -> Lt n
    | '>' -> Gt n
    | _   -> failwith "Cond.parse"
end

module Range = struct
  (** The type of ranges. *)
  type t = int * int

  (** [length rng] is the length of [rng]. *)
  let length (a, b) = max (b - a + 1) 0

  (** [apply_cond cond rng] applies [cond] to [rng]. *)
  let apply_cond cond (a, b) = match cond with
    | Cond.Lt x -> (a, min b (x - 1))
    | Cond.Gt x -> (max a (x + 1), b)

  (** [apply_neg_cond cond rng] applies the negation of [cond] to [rng]. *)
  let apply_neg_cond cond (a, b) = match cond with
    | Cond.Lt x -> (max a x, b)
    | Cond.Gt x -> (a, min b x)
end

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

module PartsRange = struct
  (** The type of parts ranges. *)
  type t = (char * Range.t) list

  (** [update pr category rng] updates the parts range's [category] with
      [rng]. *)
  let update pr category rng =
    let rec update_iter = function
      | (category', rng') as cr :: rest ->
        if category = category' then (category, rng) :: rest else cr :: update_iter rest
      | [] -> failwith "PartsRange.update"
    in update_iter pr

  (** [parts_number pr] calculates the number of possible parts for [pr]. *)
  let parts_number pr =
    pr |> List.map (fun (_, rng) -> Range.length rng) |> List.fold_left ( * ) 1
end

module Workflow = struct
  (** The type of rule targets. *)
  type target =
    | GoTo of string
    | Accept
    | Reject

  (** The type of workflow rules. *)
  type rule = {
    pred : (char * Cond.t) option;
    target : target;
  }

  (** [parse_pred pred] parses a string representation of a workflow rule's
      predicate. *)
  let parse_pred pred =
    let category = pred.[0]
    and cond = Cond.parse (String.sub pred 1 (String.length pred - 1))
    in category, cond

  (** [parse_target target] parses a string representation of a workflow rule's
      target. *)
  let parse_target = function
    | "A"    -> Accept
    | "R"    -> Reject
    | target -> GoTo target

  (** [parse_rule rule] parses a string representation of a workflow rule. *)
  let parse_rule rule =
    match String.split_on_char ':' rule with
    | [pred; target] -> { pred = Some (parse_pred pred); target = parse_target target }
    | [target]       -> { pred = None;                   target = parse_target target }
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

  (** [check_predicate pred part] check [pred] for [part]. *)
  let check_predicate pred part =
    match pred with
    | Some (category, cond) -> Cond.check cond (List.assoc category part)
    | None                  -> true

  (** [apply_predicate pred pr] applies [pred] to the parts range [pr].
      Returns the subrange of [pr] for which the predicate is true, as well as
      the subrange of [pr] for which the predicate is false. *)
  let apply_predicate pred pr =
    match pred with
    | Some (category, cond) ->
      let rng = List.assoc category pr in
      rng |> Range.apply_cond cond |> PartsRange.update pr category,
      rng |> Range.apply_neg_cond cond |> PartsRange.update pr category
    | None -> pr, [('x', (1, 0))]

  (** [execute workflow part] executes [workflow] for [part] and returns the
      matching target. *)
  let rec execute workflow part =
    match workflow with
    | { pred; target } :: rules' ->
      if check_predicate pred part then target else execute rules' part
    | [] ->
      assert false

  (** [execute_for_range workflow pr] executes [workflow] for the parts range
      [pr].  Returns the subranges of [pr] with the corresponding targets. *)
  let execute_for_range workflow pr =
    let rec execute_iter pr = function
      | { pred; target } :: rules' ->
        let pr1, pr2 = apply_predicate pred pr in
        (pr1, target) :: execute_iter pr2 rules'
      | _ -> []
    in
    execute_iter pr workflow
    |> List.filter (fun (pr, target) -> PartsRange.parts_number pr > 0)

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

  (** [execute_workflows_for_range workflows pr] executes [forkflows] for the
      parts range [pr].  Returns the subranges of [pr] that are accepted by
      [workflows]. *)
  let execute_workflows_for_range workflows pr =
    let rec execute_iter workflow pr =
      execute_for_range workflow pr
      |> List.map (fun (pr, target) ->
          match target with
          | Accept      -> [pr]
          | Reject      -> []
          | GoTo target -> execute_iter (Hashtbl.find workflows target) pr)
      |> List.flatten
    in execute_iter (Hashtbl.find workflows "in") pr
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

(** [part2 workflows] calculates how many distinct combinations of part ratings
    are accepted by [workflows]. *)
let part2 workflows =
  ['x'; 'm'; 'a'; 's']
  |> List.map (fun category -> category, (1, 4000))
  |> Workflow.execute_workflows_for_range workflows
  |> List.map PartsRange.parts_number
  |> List.fold_left (+) 0

let () =
  let workflows, parts = open_in "input" |> In_channel.input_lines |> parse_input in
  Printf.printf "Part One: %d\n" (part1 workflows parts);
  Printf.printf "Part Two: %d\n" (part2 workflows)
