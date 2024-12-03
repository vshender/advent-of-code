(** Day 3: Mull It Over *)

(** The type of instructions. *)
type instr =
  | Mul of int * int
  | Do
  | Dont

(** The instruction regular expression. *)
let instr_re = Str.regexp {|mul(\([0-9]+,[0-9]+\))\|\(do()\)\|don't()|}

(** [next_instr program start] searches [program] for the next instruction.
    The search starts at position [start] and proceeds towards the end of
    [program].  Returns the instruction and its position or [None] if no
    instruction found. *)
let next_instr program start =
  try
    let pos = Str.search_forward instr_re program start in
    try
      Str.matched_group 1 program
      |> String.split_on_char ','
      |> List.map int_of_string
      |> function
      | [x; y] -> Some (Mul (x, y), pos)
      | _      -> assert false
    with Not_found ->
    try
      ignore @@ Str.matched_group 2 program;
      Some (Do, pos)
    with Not_found ->
      Some (Dont, pos)
  with Not_found ->
    None

(** [get_instructions program] returns instructions of [program]. *)
let get_instructions program =
  let rec loop accu pos =
    match next_instr program pos with
    | Some (instr, pos) -> loop (instr :: accu) (pos + 1)
    | None              -> List.rev accu
  in loop [] 0

(** The type of program execution states. *)
type execution_state =
  | Execute
  | Skip

(** [execute instrs] executes [instrs] and returns the sum of all
    multiplications. *)
let execute instrs =
  instrs
  |> List.fold_left
    (fun (sum, state) instr ->
       match instr, state with
       | Mul (x, y), Execute -> (sum + x * y, Execute)
       | Mul _     , Skip    -> (sum, Skip)
       | Do        , _       -> (sum, Execute)
       | Dont      , _       -> (sum, Skip))
    (0, Execute)
  |> fst

(** [filer_out_conditionals instrs] filters out all conditional
    instructions. *)
let filter_out_conditionals = List.filter (function Mul _ -> true | _ -> false)

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let instrs = In_channel.input_all ic |> get_instructions in
  Printf.printf "Part One: %d\n" @@ execute @@ filter_out_conditionals instrs;
  Printf.printf "Part Two: %d\n" @@ execute instrs
