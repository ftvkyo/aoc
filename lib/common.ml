let rec gcd (n : int) (m : int) = if m = 0 then n else gcd m (n mod m)

let ints (s : string) : int list =
  let delim = Str.regexp {|[ \t]+|} in
  let strings = Str.split delim s in
  List.map int_of_string strings

let rec count (re : Str.regexp) (s : string) : int =
  try
    let _ = Str.search_forward re s 0 in
    let match_end = Str.match_end () in
    let tail = Str.string_after s match_end in
    1 + count re tail
  with _ -> 0
