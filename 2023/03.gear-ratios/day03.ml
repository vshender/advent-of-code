(** Day 3: Gear Ratios *)

(** [int_of_digit c] converts a character representing a digit into its
    corresponding integer value. *)
let int_of_digit c = Char.code c - Char.code '0'

(** [range i j] returns a list containing an ascending range of integers from
    [i] to [j - 1]. *)
let rec range i j =
  if i < j then i :: range (i + 1) j else []

(** The type of an engine schematic line parsing state. *)
type state =
  | AwaitingNumber
  | ParsingNumber of {
      num    : int;  (* current number value *)
      st_idx : int;  (* starting index of the number in the string *)
    }

(** [update_number c i st] updates the number in the parsing state [st] given
    the next digit [c] found at the position [i]. *)
let update_number c i = function
  | AwaitingNumber ->
    ParsingNumber { num = int_of_digit c; st_idx = i}
  | ParsingNumber { num; st_idx } ->
    ParsingNumber { num = num * 10 + int_of_digit c; st_idx }

(** [number_of_state line_idx end_idx st] returns the number and its positions
    stored in the parsing state [st]. *)
let number_of_state line_idx end_idx = function
  | ParsingNumber { num; st_idx } ->
    num, List.map (fun j -> (line_idx, j)) (range st_idx end_idx)
  | AwaitingNumber ->
    failwith "number_of_state"

(** [parse_engine_schematics lines] parses the given engine schematics and
    returns all its numbers and symbols. *)
let parse_engine_schematics lines =
  let process_line line line_idx numbers symbols =
    let len = String.length line in
    let rec process_char i state numbers symbols =
      if i = len then
        match state with
        | AwaitingNumber -> numbers, symbols
        | _              -> number_of_state line_idx i state :: numbers, symbols
      else
      if '0' <= line.[i] && line.[i] <= '9' then
        process_char
          (i + 1)
          (update_number line.[i] i state)
          numbers
          symbols
      else
        process_char
          (i + 1)
          AwaitingNumber
          (match state with
           | AwaitingNumber -> numbers
           | _              -> number_of_state line_idx i state :: numbers)
          (match line.[i] with
           | '.' -> symbols
           | c   -> (c, (line_idx, i)) :: symbols)
    in process_char 0 AwaitingNumber numbers symbols
  in

  lines
  |> List.mapi (fun i line -> (line, i))
  |> List.fold_left
    (fun (numbers, symbols) (line, line_index) ->
       process_line line line_index numbers symbols)
    ([], [])

(** [adjacent_positions pos] returns all the positions adjacent to [pos]. *)
let adjacent_positions (i, j) =
  [(i - 1, j - 1); (i - 1, j); (i - 1, j + 1);
   (i, j - 1); (i, j + 1);
   (i + 1, j - 1); (i + 1, j); (i + 1, j + 1)]

(** [adjacent_numbers numbers positions] returns the numbers adjacent to the
    given [positions]. *)
let adjacent_numbers numbers positions =
  let adj_positions = positions |> List.map adjacent_positions |> List.flatten in
  numbers
  |> List.filter
    (fun (_, num_positions) ->
       num_positions |> List.exists (fun pos -> List.mem pos adj_positions))
  |> List.map fst

(** [find_part_numbers numbers symbols] filters [numbers] leaving only part
    numbers. *)
let find_part_numbers numbers symbols =
  symbols |> List.map snd |> adjacent_numbers numbers

(** [part1 numbers symbols] calculates the sum of all the part numbers. *)
let part1 numbers symbols =
  find_part_numbers numbers symbols |> List.fold_left (+) 0

(** [find_gears numbers symbols ] gets all the gears and their gear ratios. *)
let find_gears numbers symbols =
  let gear_ratio = List.fold_left ( * ) 1 in
  symbols
  |> List.filter (fun (c, _) -> c = '*')
  |> List.map (fun (_, pos) -> (pos, adjacent_numbers numbers [pos]))
  |> List.filter (fun (_, adj_numbers) -> List.length adj_numbers = 2)
  |> List.map (fun (pos, adj_numbers) -> (pos, gear_ratio adj_numbers))

(** [part2 numbers symbols] calculcates the sum of all the gear ratios. *)
let part2 numbers symbols =
  find_gears numbers symbols |> List.map snd |> List.fold_left (+) 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let numbers, symbols = parse_engine_schematics lines in
  Printf.printf "Part One: %d\n" (part1 numbers symbols);
  Printf.printf "Part Two: %d\n" (part2 numbers symbols)
