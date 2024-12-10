let inf = Int.max_int

(**
  Perform a forward search of `re` in `str`.
  Return the match position and the tail of the string.
*)
let match_something re str : (int * string) option =
  try
    let match_beginning = Str.search_forward re str 0 in
    let match_end = Str.match_end () in
    let tail_length = String.length str - match_end in
    let tail = String.sub str match_end tail_length in
    Some (match_beginning, tail)
  with Not_found -> None

(**
  Evaluate the next `mul(...)`.
  Return the match position, the tail of the string and the result of evaluation.
*)
let match_mul str : int * string * int =
  let re = Str.regexp {|mul(\([0-9]+\),\([0-9]+\))|} in
  let evaluate (r1, r2) =
    let m1 = int_of_string @@ Str.matched_group 1 str in
    let m2 = int_of_string @@ Str.matched_group 2 str in
    r1, r2, m1 * m2
  in
  Option.value ~default:(inf, "", 0) @@ Option.map evaluate @@ match_something re str

(**
  Find the next `do()`.
  Return the match position and the tail of the string.
*)
let match_do str : int * string =
  let re = Str.regexp @@ Str.quote "do()" in
  Option.value ~default:(inf, "") @@ match_something re str

(**
  Find the next `don't()`.
  Return the match position and the tail of the string.
*)
let match_dont str : int * string =
  let re = Str.regexp @@ Str.quote "don't()" in
  Option.value ~default:(inf, "") @@ match_something re str

let rec process_string ~(enabled : bool) str : int =
  if str = "" then 0
  else if enabled then
    let mul_pos, mul_tail, mul_val = match_mul str in
    let dont_pos, dont_tail = match_dont str in
    if dont_pos < mul_pos then process_string ~enabled:false dont_tail
    else mul_val + process_string ~enabled:true mul_tail
  else
    let _, do_tail = match_do str in
    process_string ~enabled:true do_tail

let solve (input : string array) : string =
  let text = String.concat "\n" @@ Array.to_list input in
  Int.to_string @@ process_string ~enabled:true text
