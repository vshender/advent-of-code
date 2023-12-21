(** Day 20: Pulse Propagation *)

(** The type of pulses. *)
type pulse = Low | High

(** Base class for modules. *)
class virtual module_ (targets : string list) = object
  val targets = targets
  method targets = targets
  method set_sources (sources : string list) = ()
  method virtual send_pulse : string -> pulse -> (string * pulse) list
end

(** Flip flop modules. *)
class flip_flop targets = object
  inherit module_ targets
  val mutable is_on = false
  method send_pulse _ = function
    | High -> []
    | Low  ->
      is_on <- not is_on;
      let pulse = if is_on then High else Low in
      targets |> List.map (fun target -> target, pulse)
end

(** Conjunction modules. *)
class conjunction targets = object
  inherit module_ targets
  val memory = Hashtbl.create 16
  method set_sources = List.iter (fun src -> Hashtbl.add memory src Low)
  method send_pulse source pulse =
    Hashtbl.replace memory source pulse;
    let vals = memory |> Hashtbl.to_seq_values in
    let pulse = if vals |> Seq.for_all ((=) High) then Low else High in
    targets |> List.map (fun target -> target, pulse)
end

(** Broadcase modules. *)
class broadcast targets = object
  inherit module_ targets
  method send_pulse _ pulse = targets |> List.map (fun target -> target, pulse)
end

(** [parse_module mc] parses a string representation of a module. *)
let parse_module mc : string * module_ =
  let open Str in
  match split (regexp " s*-> *") mc with
  | [ name; targets ] ->
    let targets = split (regexp ", *") targets in
    (match name.[0] with
     | '%' -> String.sub name 1 (String.length name - 1), new flip_flop targets
     | '&' -> String.sub name 1 (String.length name - 1), new conjunction targets
     | 'b' -> "broadcaster", new broadcast targets
     | _   -> failwith "parse_module")
  | _ -> failwith "parse_module"

(** [sources modules name] returns the sources for the module with the name
    [name]. *)
let sources modules name =
  Hashtbl.fold
    (fun src md srcs -> if List.mem name md#targets then src :: srcs else srcs)
    modules
    []

(** [parse_module_configuration lines] parses [lines] and get the module
    configuration. *)
let parse_module_configuration lines =
  let modules = Hashtbl.create 64 in

  lines
  |> List.map parse_module
  |> List.iter (fun (name, md) -> Hashtbl.add modules name md);

  modules
  |> Hashtbl.iter (fun name md -> sources modules name |> md#set_sources);

  modules

(** [push_button modules wait_for_high] pushes the button, counts the number of
    low pulses and high pulses sent, and also returns those modules from the
    list [wait_for_high] that sent a high pulse. *)
let push_button modules wait_for_high =
  let low_cnt = ref 0 and high_cnt = ref 0
  and q = Queue.create ()
  and sent_high = ref [] in
  Queue.push ("button", "broadcaster", Low) q;
  while Queue.length q > 0 do
    let source, target, pulse = Queue.pop q in
    incr (if pulse = Low then low_cnt else high_cnt);
    if List.mem source wait_for_high && pulse = High then
      sent_high := source :: !sent_high;
    match Hashtbl.find_opt modules target with
    | Some md ->
      md#send_pulse source pulse
      |> List.iter (fun (target', pulse') -> Queue.push (target, target', pulse') q)
    | None -> ()
  done;
  !low_cnt, !high_cnt, !sent_high

(** [part1 modules] pushes the button 1000 times, calculates the number of low
    pulses and high pulses sent, and returns the product of these numbers. *)
let part1 modules =
  let low_cnt = ref 0 and high_cnt = ref 0 in
  for i = 1 to 1000 do
    let low_cnt', high_cnt', _ = push_button modules [] in
    low_cnt := !low_cnt + low_cnt'; high_cnt := !high_cnt + high_cnt'
  done;
  !low_cnt * !high_cnt

(** [gcd a b] is the greatest common divisor of [a] and [b]. *)
let rec gcd a b =
  if b = 0 then
    a
  else
    gcd b (a mod b)

(** [lcm a b] is the least common multiple of [a] and [b]. *)
let lcm a b = a / gcd a b * b

(** [part2 modules] finds the fewest number of button presses required to
    deliver a single low pulse to the module named "rx". *)
let part2 modules =
  (* The conjunction module that is source for "rx". *)
  let rx_src =
    Hashtbl.fold
      (fun name md rx_srcs ->
        if List.mem "rx" md#targets then name :: rx_srcs else rx_srcs)
      modules []
    |> List.hd in

  (* The conjunction modules that are sources for the source for "rx". *)
  let rx_src_srcs = Hashtbl.fold
      (fun name md srcs ->
         if List.mem rx_src md#targets then name :: srcs else srcs)
      modules [] in

  (* The steps at which modules from [rx_src_srcs] first send a high pulse. *)
  let first_high = ref [] in

  (* The lengths of loops in which modules from [rx_src_srcs] send a hight
     pulse. *)
  let loop_lengths = ref [] in

  let rec iter n =
    let _, _, high = push_button modules rx_src_srcs in

    high |> List.iter (fun name ->
      if not (List.mem_assoc name !first_high) then
        first_high := (name, n) :: !first_high
      else if not (List.mem_assoc name !loop_lengths) then
        loop_lengths := (name, n - List.assoc name !first_high) :: !loop_lengths);

    if List.length !loop_lengths < List.length rx_src_srcs then
      iter (n + 1)
  in iter 1;

  !loop_lengths |> List.map snd |> List.fold_left lcm 1

let () =
  let modules = open_in "input"
                |> In_channel.input_lines
                |> parse_module_configuration in
  Printf.printf "Part One: %d\n" (part1 modules);
  Printf.printf "Part Two: %d\n" (part2 modules)
