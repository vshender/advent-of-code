(** Day 18. Lavaduct Lagoon *)

(** [parse_dir dir] parses a string representation of a direction. *)
let parse_dir = function
  | 'U' -> (-1, 0)
  | 'D' -> (1, 0)
  | 'L' -> (0, -1)
  | 'R' -> (0, 1)
  | _   -> failwith "parse_dir"

(** [dir_of_hex h] gets the direction by the given hexadecimal
    representation. *)
let dir_of_hex = function
  | '0' -> 'R'
  | '1' -> 'D'
  | '2' -> 'L'
  | '3' -> 'U'
  | _   -> failwith "pasrse_hex_dir"

(** [parse_dig_instruction dig] parses a string representation a dig
    instruction. *)
let parse_dig_instruction dig =
  Scanf.sscanf dig "%c %d (#%6s)" (fun dir len _ -> parse_dir dir, len)

(** [extract_dig_instruction_from_hex] extracts the dig instruction from the
    given hexadecimal code. *)
let extract_dig_instruction_from_hex dig =
  Scanf.sscanf dig "%c %d (#%6s)"
    (fun _ _ hex ->
       hex.[5] |> dir_of_hex |> parse_dir,
       String.sub hex 0 5 |> Printf.sprintf "0x%s" |> int_of_string)

(** [build_polygon digs] builds a pologon by the given trenches. *)
let build_polygon =
  let rec build_polygon_iter poly = function
    | [] ->
      List.rev poly
    | ((dx, dy), n) :: digs ->
      let x, y = List.hd poly in
      build_polygon_iter ((x + dx * n, y + dy * n) :: poly) digs
  in build_polygon_iter [(0, 0)]

(** [remove_last lst] removes the last element from a non-empty list. *)
let rec remove_last = function
  | []             -> failwith "remove_last"
  | [x]            -> []
  | x :: y :: rest -> x :: remove_last (y :: rest)

(** [polygon_area poly] calculates the area of the given polygon. *)
let polygon_area poly =
  List.map2 (fun x y -> (x, y)) (remove_last poly) (List.tl poly)
  |> List.fold_left
    (fun area ((x1, y1), (x2, y2)) ->
       area +
       if x1 = x2 && y2 > y1 then
         y2 - y1
       else if y1 = y2 && x1 < x2 then
         (y1 + 1) * (x2 - x1)
       else if y1 = y2 && x1 > x2 then
         y1 * (x2 - x1)
       else
         0)
    1

let () =
  let lines = open_in "input" |> In_channel.input_lines in

  let polygon = lines |> List.map parse_dig_instruction |> build_polygon in
  Printf.printf "Part One: %d\n" (polygon_area polygon);

  let polygon = lines |> List.map extract_dig_instruction_from_hex |> build_polygon in
  Printf.printf "Part Two: %d\n" (polygon_area polygon)
