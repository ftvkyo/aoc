open Common

let dir_of c =
  match c with
  | '^' -> Some (-1, 0)
  | '>' -> Some (0, 1)
  | 'v' -> Some (1, 0)
  | '<' -> Some (0, -1)
  | _ -> None

let turn (dx, dy) = (dy, -dx)

let walk (m : matrix) =
  let start_x, start_y =
    Option.get @@ m#findi (fun c -> Option.is_some @@ dir_of c)
  in
  let start_dir = Option.get @@ dir_of @@ m#get start_x start_y in
  m#set start_x start_y '.';
  let rec step cur dir =
    let cx, cy = cur in
    m#set cx cy 'X';
    let dx, dy = dir in
    try
      match m#get (cx + dx) (cy + dy) with
      | '#' -> step cur @@ turn dir
      | _ -> step (cx + dx, cy + dy) dir
    with _ -> ()
  in
  step (start_x, start_y) start_dir

let solve (input : string array) : string =
  let m = matrix_of '.' input in
  walk m;
  let s = m#to_string () in
  print_endline s;
  let re = Str.regexp @@ Str.quote "X" in
  Int.to_string @@ count re s
