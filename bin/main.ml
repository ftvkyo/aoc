let extract_ids line : int * int =
  let delim = Str.regexp {|[ \t]+|} in
  let string_ids = Str.split delim line in
  let int_ids = List.map int_of_string string_ids in
  (List.hd int_ids, List.hd (List.tl int_ids))

let transpose id_pairs : int array * int array = Array.split id_pairs

let distance pair : int =
  let a, b = pair in
  abs (a - b)

let distances ids_a ids_b = Seq.map distance (Seq.zip ids_a ids_b)

let filename =
  try Sys.argv.(1)
  with e ->
    print_endline "Expected filename";
    raise e

let ids_a, ids_b =
  let lines = Arg.read_arg filename in
  transpose (Array.map extract_ids lines)
;;

Array.sort compare ids_a;;
Array.sort compare ids_b

let ds = distances (Array.to_seq ids_a) (Array.to_seq ids_b)
let answer = Seq.fold_left ( + ) 0 ds
let () = print_endline (Int.to_string answer)
