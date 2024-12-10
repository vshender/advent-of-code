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

(** [get_files_and_free_spaces disk_layout] calculates information about files
    and free spaces present in the [disk_layout]. *)
let get_files_and_free_spaces disk_layout =
  let n = Array.length disk_layout in
  let files = ref [] and free_spaces = ref [] in

  let register_file file_id file_pos file_len =
    if file_len > 0 then
      if file_id = free_space then
        free_spaces := (file_pos, file_len) :: !free_spaces
      else
        files := (file_id, file_pos, file_len) :: !files
  in

  let rec loop file_id file_pos file_len i =
    if i = n then
      register_file file_id file_pos file_len
    else if disk_layout.(i) = file_id then
      loop file_id file_pos (file_len + 1) (i + 1)
    else begin
      register_file file_id file_pos file_len;
      loop disk_layout.(i) i 1 (i + 1)
    end
  in
  loop disk_layout.(0) 0 1 1;

  !files |> List.rev |> Array.of_list, !free_spaces |> List.rev |> Array.of_list

(** [defragment disk_layout] attempts to move files in [disk_layout] to earlier
    positions by filling available free space.  It moves each file into the
    first free space slot that is large enough to fit the file. *)
let defragment disk_layout =
  let files, free_spaces = get_files_and_free_spaces disk_layout in

  (* Defragment. *)
  let exception Break in
  for fi = Array.length files - 1 downto 0 do
    let (f_id, f_pos, f_len) = files.(fi) in
    try
      for fsi = 0 to Array.length free_spaces - 1 do
        let (fs_pos, fs_len) = free_spaces.(fsi) in
        if fs_pos < f_pos && fs_len >= f_len then begin
          files.(fi) <- (f_id, fs_pos, f_len);
          free_spaces.(fsi) <- (fs_pos + f_len, fs_len - f_len);
          raise Break
        end
      done
    with Break -> ()
  done;

  (* Build defragmented disk layout. *)
  Array.fill disk_layout 0 (Array.length disk_layout) (-1);
  for fi = 0 to Array.length files - 1 do
    let (f_id, f_pos, f_len) = files.(fi) in
    for j = 0 to f_len - 1 do
      disk_layout.(f_pos + j) <- f_id
    done
  done;

  disk_layout

(** [checksum disk_layout] computes the checksum for the given
    [disk_layout]. *)
let checksum disk_layout =
  let sum = ref 0 in
  for i = 0 to Array.length disk_layout - 1 do
    if disk_layout.(i) <> free_space then
      sum := !sum + i * disk_layout.(i)
  done;
  !sum

let () =
  In_channel.with_open_text "input" @@ fun ic ->
  let disk_layout = In_channel.input_line ic |> Option.get |> unpack_disk_map in
  Printf.printf "Part One: %d\n" @@ checksum (compact (Array.copy disk_layout));
  Printf.printf "Part Two: %d\n" @@ checksum (defragment (Array.copy disk_layout))
