type position = Pos of int * int

type velocity = Vel of int * int

let robot_parse line =
  let open Str in
  let re_robot = regexp {|p=\([-0-9]+\),\([-0-9]+\) v=\([-0-9]+\),\([-0-9]+\)|} in
  let mg i = int_of_string @@ matched_group i line in
  let _ = string_match re_robot line 0 in
  let px, py = mg 1, mg 2 in
  let vx, vy = mg 3, mg 4 in
  Pos (px, py), Vel (vx, vy)

let robot_move grid robot seconds =
  let gx, gy = grid in
  let Pos (px, py), Vel (vx, vy) = robot in
  let npx = (px + (vx * seconds)) mod gx in
  let npy = (py + (vy * seconds)) mod gy in
  let npx = if npx < 0 then gx + npx else npx in
  let npy = if npy < 0 then gy + npy else npy in
  Pos (npx, npy), Vel (vx, vy)

let solve (gx, gy) (input : string array) : string =
  let seconds = 100 in
  let gmx, gmy = gx / 2, gy / 2 in
  let quadrant_of x y =
    if x < gmx && y < gmy then 1
    else if x < gmx && y > gmy then 2
    else if x > gmx && y < gmy then 3
    else if x > gmx && y > gmy then 4
    else 0
  in
  let counters = Array.make 5 0 in
  let iter line =
    if line <> "" then
      let robot = robot_parse line in
      let Pos (px, py), _ = robot_move (gx, gy) robot seconds in
      let q = quadrant_of px py in
      Array.set counters q @@ (1 + Array.get counters q)
  in
  Array.iter iter input ;
  Int.to_string @@ Array.(get counters 1 * get counters 2 * get counters 3 * get counters 4)
