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

class matrix (data : char array array) =
  object (_self)
    val data = data
    method get x y = data.(x).(y)
    method set x y v = Array.set data.(x) y v

    method to_string =
      let f acc row = acc ^ "\n" ^ String.of_seq @@ Array.to_seq row in
      Array.fold_left f "" data

    method findi (f : char -> bool) : (int * int) option =
      let row =
        let f row = Array.exists f row in
        Array.find_index f data
      in
      match row with
      | Some row -> (
          let col = Array.find_index f data.(row) in
          match col with Some col -> Some (row, col) | None -> None)
      | None -> None
  end

let matrix_of (default : char) (data : string array) =
  let x = Array.length data in
  let y = Array.fold_left max 0 @@ Array.map (fun s -> String.length s) data in
  let get x y = try String.get data.(x) y with Not_found -> default in
  new matrix @@ Array.init_matrix x y get
