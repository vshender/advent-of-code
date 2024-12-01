(** Day 1: Historian Hysteria *)

(** [parse_location_ids lines] parses [lines] and gets two lists of location
    IDs. *)
let parse_location_ids lines =
  lines
  |> List.map (fun line -> Scanf.sscanf line "%d %d" (fun a b -> a, b))
  |> List.split

(** [total_distance left right] is the total distance between [left] and
    [right]. *)
let total_distance left right =
  List.map2
    (fun l r -> abs (l - r))
    (List.sort Int.compare left)
    (List.sort Int.compare right)
  |> List.fold_left (+) 0

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let left, right = In_channel.input_lines ic |> parse_location_ids in
  Printf.printf "Part One: %d\n" @@ total_distance left right
