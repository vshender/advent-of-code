(** Day 5: If You Give A Seed A Fertilizer *)

(** [parse_seeds seeds] parses a string representation of seeds. *)
let parse_seeds seeds =
  let open Str in
  match split (regexp ": *") seeds with
  | [_; seeds] -> split (regexp " +") seeds |> List.map int_of_string
  | _          -> failwith "parse_seeds"

(** The type of ranges. *)
type range = { start : int; len : int }

(** The type of map ranges. *)
type map_range = { rng : range; dst : int }

(** [parse_map_range line] parses a string representation of a map range. *)
let parse_map_range line =
  match Str.(split (regexp " +") line) |> List.map int_of_string with
  | [dst; src; len] -> { rng = { start = src; len }; dst }
  | _               -> failwith "parse_map_range"

(** [parse_maps lines] parses [lines] and gets the maps. *)
let parse_maps =
  let rec parse_maps_iter maps curmap = function
    | []            -> List.rev (curmap :: maps)
    | line :: lines ->
      if String.ends_with ~suffix:"map:" line then
        parse_maps_iter maps [] lines
      else if line = "" then
        parse_maps_iter (curmap :: maps) [] lines
      else
        parse_maps_iter maps (parse_map_range line :: curmap) lines
  in
  parse_maps_iter [] []

(** [in_range rng v] is [true] if [v] belongs to [rng]. *)
let in_range { start; len } v = start <= v && v <= start + len

(** [convert_value map v] converts [v] using [map]. *)
let convert_value map v =
  match List.find_opt (fun { rng; _ } -> in_range rng v) map with
  | Some { rng = { start = src; _ }; dst } -> v - src + dst
  | None                                   -> v

(** [find_location maps seed] finds the location for [seed] by applying [maps]
    to it. *)
let find_location maps seed =
  List.fold_left (fun v map -> convert_value map v) seed maps

(** [part1 maps seeds] finds the lowest location number for [seeds]. *)
let part1 maps seeds =
  seeds |> List.map (find_location maps) |> List.fold_left min Int.max_int

(** [seed_ranges seeds] returns seed ranges by [seeds]. *)
let rec seed_ranges = function
  | []             -> []
  | [x]            -> failwith "seed_ranges"
  | a :: b :: rest -> { start = a; len = b } :: seed_ranges rest

(** [intersect a b] is the intersection of two ranges [a] and [b]. *)
let intersect a b =
  let start = max a.start b.start
  and end_ = min (a.start + a.len) (b.start + b.len) in
  if start < end_ then Some { start; len = end_ - start } else None

(** [diff a b] is the difference between two ranges [a] and [b], returning the
    parts of [a] that do not overlap with [b].  The returning list can contain
    zero, one, or two ranges. *)
let diff a b =
  match intersect a b with
  | Some i ->
    let a = if a.start < i.start then
        Some { start = a.start; len = i.start - a.start }
      else
        None
    and b = if i.start + i.len < a.start + a.len then
        Some { start = i.start + i.len; len = a.start + a.len - i.start - i.len }
      else
        None
    in List.filter_map Fun.id [a; b]
  | None -> [a]

(** [map_range mrng rng] maps the given range using the map range [mrng].  The
    function returns a pair of the mapped range and the ranges that could not
    be mapped using [mrng]. *)
let map_range mrng rng =
  match intersect mrng.rng rng with
  | Some i -> Some { i with start = i.start - mrng.rng.start + mrng.dst }, diff rng i
  | None   -> None, [rng]

(** [map_ranges mrng rngs] maps the given ranges using the map range [mrng].
    The function returns a pair of the mapped ranges and the ranges that could
    not be mapped using [mrng]. *)
let map_ranges mrng rngs =
  let rec convert_ranges_iter converted unconverted = function
    | []          -> converted, List.flatten unconverted
    | rng :: rngs ->
      match map_range mrng rng with
      | Some c, u -> convert_ranges_iter (c :: converted) (u :: unconverted) rngs
      | None, u   -> convert_ranges_iter converted (u :: unconverted) rngs
  in convert_ranges_iter [] [] rngs

(** [convert_ranges map rngs] converts [rngs] using [map]. *)
let rec convert_ranges map rngs =
  match map with
  | []           -> rngs
  | mrng :: map' ->
    let converted, unconverted = map_ranges mrng rngs in
    converted @ convert_ranges map' unconverted

(** [find_locations maps rngs] finds the locations for the given seed ranges by
    applying [maps] to them. *)
let rec find_locations maps rngs =
  List.fold_left (fun rngs map -> convert_ranges map rngs) rngs maps

(** [part2 maps rngs] finds the lowest location number for [rngs]. *)
let part2 maps rngs =
  find_locations maps rngs
  |> List.map (fun { start; _ } -> start)
  |> List.fold_left min Int.max_int

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let seeds = parse_seeds (List.hd lines)
  and maps = parse_maps (List.tl (List.tl lines)) in
  Printf.printf "Part One: %d\n" (part1 maps seeds);
  Printf.printf "Part Two: %d\n" (part2 maps (seed_ranges seeds))
