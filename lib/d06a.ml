open Common

let dir_of c =
  match c with
  | '^' -> Some (-1, 0)
  | '>' -> Some (0, 1)
  | 'v' -> Some (1, 0)
  | '<' -> Some (0, -1)
  | _ -> None

let turn (dx, dy) = dy, -dx

let walk (m : char Mat.t) =
  let start_x, start_y, _ = Option.get @@ Mat.findi (fun _ _ c -> Option.is_some @@ dir_of c) m in
  let start_dir = Option.get @@ dir_of @@ Mat.get m start_x start_y in
  Mat.set m start_x start_y '.' ;
  let rec step cur dir =
    let cx, cy = cur in
    Mat.set m cx cy 'X' ;
    let dx, dy = dir in
    try
      match Mat.get m (cx + dx) (cy + dy) with
      | '#' -> step cur @@ turn dir
      | _ -> step (cx + dx, cy + dy) dir
    with _ -> ()
  in
  step (start_x, start_y) start_dir

let solve (input : string array) : string =
  let m = Mat.mat_of input in
  walk m ;
  let s = Mat.to_string m in
  print_endline s ;
  let re = Str.regexp @@ Str.quote "X" in
  Int.to_string @@ count re s
