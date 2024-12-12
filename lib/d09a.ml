open Printf

let rec checksum_file id position length =
  if length <> 0 then (id * position) + checksum_file id (position + 1) (length - 1) else 0

let checksum (s : string) =
  printf "input: %s\n" s ;
  let slen = String.length s in
  let rec checksum l_index l_position ?l_remaining_space r_index r_blocks_consumed =
    if l_index >= slen then (
      printf "terminating - reached the end\n" ;
      0 )
    else
      let length = int_of_string @@ String.sub s l_index 1 in
      printf "index %d at position %d -> " l_index l_position ;
      if l_index mod 2 = 0 then (
        if
          (* It's a file, consume entirely *)
          l_index >= r_index
        then (
          let r_id = r_index / 2 in
          let r_length = int_of_string @@ String.sub s r_index 1 in
          let r_blocks_remaining = r_length - r_blocks_consumed in
          printf "terminating - all files to the right have been moved\n" ;
          printf "Note: using up the %d remaining blocks of file id %d with index %d\n" r_blocks_remaining r_id r_index ;
          checksum_file r_id l_position r_blocks_remaining )
        else
          let l_id = l_index / 2 in
          printf "file with id %d of length %d\n" l_id length ;
          checksum_file l_id l_position length + checksum (l_index + 1) (l_position + length) r_index r_blocks_consumed
        )
      else (
        printf "free space of length %d that " length ;
        if l_index >= r_index then (
          printf "does not need filling\n" ;
          0 )
        else
          match l_remaining_space with
          | Some 0 ->
              (* It used to be free space, but we filled it completely *)
              printf "we just filled completely\n" ;
              checksum (l_index + 1) l_position r_index r_blocks_consumed
          | None ->
              (* It's free space and we haven't started filling it *)
              printf "we haven't touched\n  retry: " ;
              checksum l_index l_position ~l_remaining_space:length r_index r_blocks_consumed
          | Some l_remaining_space ->
              (* It's free space, fill it with blocks from the right *)
              printf "has %d blocks left to fill\n" l_remaining_space ;
              let r_id = r_index / 2 in
              let r_length = int_of_string @@ String.sub s r_index 1 in
              let r_blocks_remaining = r_length - r_blocks_consumed in
              if r_blocks_remaining > 0 then (
                let blocks_consuming = min l_remaining_space r_blocks_remaining in
                printf
                  "    which will be filled with %d blocks from file id %d with index %d that had %d blocks remaining\n"
                  blocks_consuming r_id r_index r_blocks_remaining ;
                let l_remaining_space = l_remaining_space - blocks_consuming in
                let r_blocks_consumed = r_blocks_consumed + blocks_consuming in
                checksum_file r_id l_position blocks_consuming
                + checksum l_index (l_position + blocks_consuming) ~l_remaining_space r_index r_blocks_consumed )
              else (
                printf
                  "    but the file id %d with index %d has been used up completely, so we will start consuming \
                   another file\n\
                  \  retry: "
                  r_id r_index ;
                (* We used up the file from the right, but there is space remaining, so consume more *)
                checksum l_index l_position ~l_remaining_space (r_index - 2) 0 ) )
  in
  let r_index = slen - 2 + (slen mod 2) in
  checksum 0 0 r_index 0

let solve (input : string array) : string =
  let s = Array.get input 0 in
  Int.to_string @@ checksum s
