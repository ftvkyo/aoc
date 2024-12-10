open Printf

type file_id = int
type length = int
type fragment = File of file_id * length | Free of length

let fragment_of (i : int) (c : char) =
  let length = int_of_string @@ Char.escaped c in
  if i mod 2 = 0 then File (i / 2, length) else Free length

let rec checksum_of ~at (id : file_id) (length : length) =
  if length <> 0 then (id * at) + checksum_of ~at:(at + 1) id (length - 1)
  else 0

class disk (d : string) =
  object (self)
    val data = ref @@ List.of_seq @@ Seq.mapi fragment_of @@ String.to_seq d

    method checksum () : int =
      let f (acc_pos, acc_sum) (frag : fragment) =
        match frag with
        | Free free -> (acc_pos + free, acc_sum)
        | File (id, length) ->
            (acc_pos + length, acc_sum + checksum_of id length ~at:acc_pos)
      in
      let _, sum = List.fold_left f (0, 0) !data in
      sum

    method str () : string =
      let f acc (frag : fragment) =
        match frag with
        | Free free -> acc ^ String.init free (fun _ -> '.')
        | File (id, length) ->
            acc ^ String.concat ""
            @@ List.init length (fun _ -> sprintf "[%d]" id)
      in
      List.fold_left f "" !data

    method try_reorder (frag : fragment) : unit =
      match frag with
      | File (file_id, file_length) ->
          let rec update ?(moved = false) into =
            match into with
            | [] -> []
            | (File (head_file_id, head_file_length) as head) :: tail ->
                if file_id = head_file_id then
                  (* Met the file we are trying to move.
                     Stop searching for free space. *)
                  if moved then
                    (* Remove the file from this position as it was moved. *)
                    Free head_file_length :: tail
                  else
                    (* Keep the file here as we failed to move it. *)
                    head :: tail
                else
                  (* It's just a file, keep intact. *)
                  head :: update ~moved tail
            | (Free free_space as head) :: tail ->
                if (not moved) && free_space >= file_length then
                  (* Insert the file here and reduce remaining free space.
                     Assuming 0-block-long files don't exist. *)
                  frag
                  :: (update ~moved:true
                     @@ (Free (free_space - file_length) :: tail))
                else (* Can't insert the file here. *)
                  head :: update ~moved tail
          in
          (* Note: free space never needs to be merged togeter as if the file was removed from between
             two free spaces, no other file could enter that area, as files only move left. *)
          data := update !data
      | _ -> () (* Don't try to move free space *)

    method defrag () : unit =
      let r = List.rev !data in
      List.iter self#try_reorder r
  end

let solve (input : string array) : string =
  let d = new disk @@ Array.get input 0 in
  printf "BEFORE: %s\n" @@ d#str ();
  d#defrag ();
  printf "AFTER:  %s\n" @@ d#str ();
  Int.to_string @@ d#checksum ()
