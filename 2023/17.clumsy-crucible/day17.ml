(** Day 17: Clumsy Crucible *)

(** Min-heap data structure. *)
module Heap = struct
  (** The type of heaps. *)
  type 'a t = {
    mutable data : 'a array;
    mutable size : int;
    dummy : 'a;
  }

  (** [Empty] is raised when attempting to remove an element from an empty
      heap. *)
  exception Empty

  (** [create n dummy] initializes a new heap with the capacity [n] and the
      [dummy] element to initialize the underlying array. *)
  let create n dummy = {
    data = Array.make n dummy;
    size = 0;
    dummy = dummy;
  }

  (** [enlarge h] doubles the size of the heap's array when the heap is
      full. *)
  let enlarge h =
    let data = Array.make (Array.length h.data lsl 1) h.dummy in
    Array.blit h.data 0 data 0 h.size;
    h.data <- data

  (** [heapify h i] maintains the min-heap property starting from index [i] in
      the heap [h]. *)
  let rec heapify h i =
    let l = i lsl 1 + 1 and r = i lsl 1 + 2 in
    let smallest = if l < h.size && h.data.(l) < h.data.(i) then l else i in
    let smallest = if r < h.size && h.data.(r) < h.data.(smallest) then r else smallest in
    if smallest <> i then begin
      let t = h.data.(i) in
      h.data.(i) <- h.data.(smallest);
      h.data.(smallest) <- t;
      heapify h smallest
    end

  (** [add h x] adds [x] to [h]. *)
  let add h x =
    if h.size = Array.length h.data then
      enlarge h;
    let rec moveup i =
      let p = (i - 1) lsr 1 in
      if i > 0 && h.data.(p) > x then begin
        h.data.(i) <- h.data.(p);
        moveup p
      end else
        h.data.(i) <- x
    in moveup h.size;
    h.size <- h.size + 1

  (** [remove h] removes and returns the minimum element from [h].
      Raises [Empty] if the heap is empty. *)
  let remove h =
    if h.size = 0 then raise Empty;
    let min = h.data.(0) in
    h.data.(0) <- h.data.(h.size - 1);
    h.size <- h.size - 1;
    heapify h 0;
    min
end

(** [int_of_digit c] converts the character [c] representing a digit into its
    corresponding integer value. *)
let int_of_digit c = Char.code c - Char.code '0'

(** [parse_map lines] parses [lines] and gets the map. *)
let parse_map lines =
  lines
  |> List.map (fun line -> Array.init (String.length line) (fun i -> int_of_digit line.[i]))
  |> Array.of_list

(** [shortest_path map is_dst can_move_forward can_turn] finds the shortest
    path from the upper left corner to the lower right corner of [map] taking
    into account the restrictions imposed by [is_dst], [can_move_forward], and
    [can_turn]. *)
let shortest_path map is_dst can_move_forward can_turn =
  let mx = Array.length map and my = Array.length map.(0) in
  let heap = Heap.create (mx * my) (0, 0, 0, 0, 0, 0) in
  let seen = Hashtbl.create (mx * my) in

  let rec walk () =
    match Heap.remove heap with
    | exception Heap.Empty ->
      -1
    | (d, x, y, dx, dy, n) ->
      if is_dst x y n then
        d
      else begin
        if not (Hashtbl.mem seen (x, y, dx, dy, n)) then begin
          Hashtbl.add seen (x, y, dx, dy, n) true;
          [(1, 0); (0, 1); (-1, 0); (0, -1)] |> List.iter
            (fun (dx', dy') ->
               let x' = x + dx' and y' = y + dy' in
               if 0 <= x' && x' < mx && 0 <= y' && y' < my then begin
                 if (dx', dy') = (dx, dy) && can_move_forward n then
                   Heap.add heap (d + map.(x').(y'), x', y', dx', dy', n + 1);
                 if (dx', dy') <> (dx, dy) && (dx', dy') <> (-dx, -dy) && can_turn n then
                   Heap.add heap (d + map.(x').(y'), x', y', dx', dy', 1)
               end)
        end;
        walk ()
      end
  in
  Heap.add heap (0, 0, 0, 1, 0, 0);
  Heap.add heap (0, 0, 0, 0, 1, 0);
  walk ()

(** [part1 map] finds the least heat loss when using an ordinal crucible. *)
let part1 map =
  let mx = Array.length map and my = Array.length map.(0) in
  shortest_path map
    (fun x y _ -> x = mx - 1 && y = my - 1)
    (fun n -> n < 3)
    (fun _ -> true)

(** [part2 map] finds the least heat loss when using an ultra crucible. *)
let part2 map =
  let mx = Array.length map and my = Array.length map.(0) in
  shortest_path map
    (fun x y n -> x = mx - 1 && y = my - 1 && n >= 4)
    (fun n -> n < 10)
    (fun n -> n >= 4)

let () =
  let map = open_in "input" |> In_channel.input_lines |> parse_map in
  Printf.printf "Part One: %d\n" (part1 map);
  Printf.printf "Part Two: %d\n" (part2 map)
