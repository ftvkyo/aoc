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

class ['a] matrix (d : 'a array array) =
  object (self)
    val data = d

    method get_data = data

    method x = Array.length d

    method y = try Array.length @@ d.(0) with _ -> 0

    method get x y = data.(x).(y)

    method set x y v = Array.set data.(x) y v

    method try_get x y = try Option.some @@ self#get x y with _ -> None

    method try_set x y v = try Option.some @@ self#set x y v with _ -> None

    method iteri (f : int -> int -> 'a -> unit) : unit =
      let f x row = Array.iteri (f x) row in
      Array.iteri f data

    method mapi (f : int -> int -> 'a -> 'b) : 'b matrix =
      let f x row = Array.mapi (f x) row in
      new matrix @@ Array.mapi f data

    method findi (f : 'a -> bool) : (int * int) option =
      let row =
        let f row = Array.exists f row in
        Array.find_index f data
      in
      match row with
      | Some row -> (
          let col = Array.find_index f data.(row) in
          match col with
          | Some col -> Some (row, col)
          | None -> None )
      | None -> None
  end

let matrix_of (default : char) (data : string array) =
  let x = Array.length data in
  let y = Array.fold_left max 0 @@ Array.map (fun s -> String.length s) data in
  let get x y = try String.get data.(x) y with Not_found -> default in
  new matrix @@ Array.init_matrix x y get

let matrix_init (default : 'a) x y = new matrix @@ Array.init_matrix x y (fun _ _ -> default)

let string_of_matrix (m : char matrix) =
  let f acc row = acc ^ "\n" ^ String.of_seq @@ Array.to_seq row in
  Array.fold_left f "" m#get_data
