let extract_ids line : int * int =
  let delim = Str.regexp {|[ \t]+|} in
  let string_ids = Str.split delim line in
  let int_ids = List.map int_of_string string_ids in
  (List.hd int_ids, List.hd @@ List.tl int_ids)

let distance (a, b) : int = abs @@ (a - b)
let distances ids_a ids_b = Seq.map distance @@ Seq.zip ids_a ids_b

let solve (input : string array) : string =
  let ids_a, ids_b = Array.split @@ Array.map extract_ids input in
  Array.sort compare ids_a;
  Array.sort compare ids_b;
  let ds = distances (Array.to_seq ids_a) (Array.to_seq ids_b) in
  Int.to_string @@ Seq.fold_left ( + ) 0 ds
