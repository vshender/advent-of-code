(** Day 09: Disk Fragmenter *)

(** [digit c] converts a character [c] (representing a digit) into its
    corresponding integer value. *)
let digit c = Char.code c - Char.code '0'

(** The value used to represent free space in the disk layout. *)
let free_space = -1

(** [unpack_disk_map dm] unpacks a disk map from a string [dm] into an array
    representing the disk layout. *)
let unpack_disk_map dm =
  let sum = ref 0 in
  for i = 0 to String.length dm - 1 do
    sum := !sum + digit dm.[i]
  done;

  let disk_layout = Array.make !sum (-1) in
  let block_pos = ref 0 and file_id = ref 0 in
  for i = 0 to String.length dm - 1 do
    let file_id =
      if i mod 2 = 0 then begin
        incr file_id;
        !file_id - 1
      end else
        free_space
    in
    for _ = 0 to digit dm.[i] - 1 do
      disk_layout.(!block_pos) <- file_id;
      incr block_pos
    done
  done;
  disk_layout

(** [compact disk_layout] rearranges the blocks in the [disk_layout] array
    so that all blocks are moved to the front, and gaps are shifted to the
    back. *)
let compact disk_layout =
  let rec loop i j =
    if i >= j then
      disk_layout
    else if disk_layout.(i) <> free_space then
      loop (i + 1) j
    else if disk_layout.(j) = free_space then
      loop i (j - 1)
    else begin
      disk_layout.(i) <- disk_layout.(j);
      disk_layout.(j) <- free_space;
      loop (i + 1) (j - 1)
    end
  in loop 0 (Array.length disk_layout - 1)

(** [checksum disk_layout] computes the checksum for the given
    [disk_layout]. *)
let checksum disk_layout =
  let n = Array.length disk_layout in
  let rec loop sum i =
    if i >= n || disk_layout.(i) = free_space then
      sum
    else
      loop (sum + i * disk_layout.(i)) (i + 1)
  in loop 0 0

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let disk_layout = In_channel.input_line ic |> Option.get |> unpack_disk_map in
  Printf.printf "Part One: %d\n" @@ checksum @@ compact disk_layout
