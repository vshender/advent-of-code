(** Day 6: Wait For It *)

open List

(** [parse_races lines] parses [lines] and gets the races according to part
    one. *)
let parse_races lines =
  let data = lines
             |> map Str.(split (regexp " +"))
             |> map tl
             |> map (map int_of_string)
  in match data with
  | [times; distances] -> combine times distances
  | _                  -> failwith "parse_races"

(** [parse_race lines] parses [lines] and gets the race according to part
    two. *)
let parse_race lines =
  let open Str in
  let data = lines
             |> map (fun line -> bounded_split (regexp " +") line 2)
             |> map tl
             |> flatten
             |> map (global_replace (regexp " +") "")
             |> map int_of_string
  in match data with
  | [time; distance] -> (time, distance)
  | _                -> failwith "parse_race"

(** [calc race] calculates the number of ways you could beat the record in
    [race]. *)
let calc (time, distance) =
  let rec calc_iter acc hold =
    if hold = time then
      acc
    else
      calc_iter (acc + if hold * (time - hold) > distance then 1 else 0) (hold + 1)
  in calc_iter 0 0

let () =
  let lines = open_in "input" |> In_channel.input_lines in
  let races = parse_races lines in
  Printf.printf "Part One: %d\n" (races |> map calc |> fold_left ( * ) 1);
  let race = parse_race lines in
  Printf.printf "Part Two: %d\n" (calc race)
