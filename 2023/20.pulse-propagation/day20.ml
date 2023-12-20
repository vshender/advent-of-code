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
let parse_module mc =
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

(** [push_button modules] pushes the button and counts the number of low
    pulses and high pulses sent. *)
let push_button modules =
  let low_cnt = ref 0 and high_cnt = ref 0
  and q = Queue.create () in
  Queue.push ("button", "broadcaster", Low) q;
  while Queue.length q > 0 do
    let source, target, pulse = Queue.pop q in
    incr (if pulse = Low then low_cnt else high_cnt);
    match Hashtbl.find_opt modules target with
    | Some md ->
      md#send_pulse source pulse
      |> List.iter (fun (target', pulse') -> Queue.push (target, target', pulse') q)
    | None -> ()
  done;
  !low_cnt, !high_cnt

(** [part1 modules] pushes the button 1000 times, calculates the number of low
    pulses and high pulses sent, and returns the product of these numbers. *)
let part1 modules =
  let low_cnt = ref 0 and high_cnt = ref 0 in
  for i = 1 to 1000 do
    let low_cnt', high_cnt' = push_button modules in
    low_cnt := !low_cnt + low_cnt'; high_cnt := !high_cnt + high_cnt'
  done;
  !low_cnt * !high_cnt

let () =
  let modules = open_in "input"
                |> In_channel.input_lines
                |> parse_module_configuration in
  Printf.printf "Part One: %d\n" (part1 modules)
