open Printf
open Common

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

let robot_move grid seconds robot =
  let gx, gy = grid in
  let Pos (px, py), Vel (vx, vy) = robot in
  let npx = (px + (vx * seconds)) mod gx in
  let npy = (py + (vy * seconds)) mod gy in
  let npx = if npx < 0 then gx + npx else npx in
  let npy = if npy < 0 then gy + npy else npy in
  Pos (npx, npy), Vel (vx, vy)

let solve (gx, gy) (input : string array) : string =
  let input = Array.of_seq @@ Seq.filter (fun l -> l <> "") @@ Array.to_seq input in
  let rex = Str.regexp @@ Str.quote "XXXXX" in
  let robot_move = robot_move (gx, gy) in
  let seconds_max = 10000 in
  let robots = Array.map robot_parse input in
  let display (robots : (position * velocity) array) seconds =
    let m = Mat.make gx gy ' ' in
    Array.iter (fun (Pos (x, y), _) -> Mat.set m x y 'X') robots ;
    let ms = Mat.string_of_mat m in
    if count rex ms > 0 then begin
      printf "\n\n%s\n" @@ String.make gy '=' ;
      printf "\n\nAfter %d seconds:\n\n" seconds ;
      printf "%s\n" ms
    end
  in
  let rec move_and_display seconds =
    let robot_move = robot_move seconds in
    let robots = Array.map robot_move robots in
    display robots seconds ;
    if seconds < seconds_max then move_and_display @@ (seconds + 1)
  in
  move_and_display 0 ;
  "Done"
