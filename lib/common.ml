let ints (s : string) : int list =
  let delim = Str.regexp {|[ \t]+|} in
  let strings = Str.split delim s in
  List.map int_of_string strings
