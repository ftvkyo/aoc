let extract_ids line : int * int =
  let ints = Common.ints line in
  List.(hd ints, hd @@ tl ints)

let distance (a, b) : int = abs @@ (a - b)
let distances ids_a ids_b = Seq.map distance @@ Seq.zip ids_a ids_b

let solve (input : string array) : string =
  let ids_a, ids_b = Array.split @@ Array.map extract_ids input in
  Array.sort compare ids_a;
  Array.sort compare ids_b;
  let ds = distances (Array.to_seq ids_a) (Array.to_seq ids_b) in
  Int.to_string @@ Seq.fold_left ( + ) 0 ds
