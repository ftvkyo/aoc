let rec process_mul re str =
  try
    let _ = Str.search_forward re str 0 in
    let p_end = Str.match_end () in
    let m1 = int_of_string @@ Str.matched_group 1 str in
    let m2 = int_of_string @@ Str.matched_group 2 str in
    let tail_length = String.length str - p_end in
    let tail = String.sub str p_end tail_length in
    (m1 * m2) + process_mul re tail
  with Not_found -> 0

let solve (input : string array) : string =
  let text = String.concat "\n" @@ Array.to_list input in
  let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  Int.to_string @@ process_mul re text
