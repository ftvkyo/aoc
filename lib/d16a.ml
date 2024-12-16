open Printf

let turn (x, y) = -y, x

let solve input =
  let m = Mat.mat_of input in
  let costs = Mat.make (Mat.x m) (Mat.y m) Int.max_int in
  let rec visit x y (dx, dy) cost =
    let known_cost = Mat.get costs x y in
    if cost < known_cost then begin
      Mat.set costs x y cost ;
      match Mat.get m x y with
      | '.' | 'E' | 'S' ->
          let dx1, dy1 = turn (dx, dy) in
          let dx3, dy3 = turn @@ turn (dx1, dy1) in
          visit (x + dx) (y + dy) (dx, dy) (cost + 1) ;
          visit (x + dx1) (y + dy1) (dx1, dy1) (cost + 1001) ;
          visit (x + dx3) (y + dy3) (dx3, dy3) (cost + 1001)
      | '#' -> ()
      | _ as c -> failwith @@ sprintf "Found '%c'?" c
    end
  in
  let sx, sy, _ = Option.get @@ Mat.findi (fun _ _ c -> c = 'S') m in
  visit sx sy (0, 1) 0 ;
  let ex, ey, _ = Option.get @@ Mat.findi (fun _ _ c -> c = 'E') m in
  Int.to_string @@ Mat.get costs ex ey
