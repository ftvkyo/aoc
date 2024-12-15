open Printf

let dirs = [ '^', (-1, 0); '>', (0, 1); 'v', (1, 0); '<', (0, -1) ]

let dir_of c = List.assoc c dirs

let walk map moves =
  let rec find_spot (at_x, at_y) (dir_x, dir_y) =
    match Mat.get map at_x at_y with
    | '#' -> None
    | '.' -> Some (at_x, at_y)
    | 'O' -> find_spot (at_x + dir_x, at_y + dir_y) (dir_x, dir_y)
    | _ as c -> failwith @@ sprintf "Found '%c' when looking for a free spot?" c
  in
  let step (robot_x, robot_y) (move_x, move_y) =
    let next_x, next_y = robot_x + move_x, robot_y + move_y in
    match find_spot (next_x, next_y) (move_x, move_y) with
    | None -> robot_x, robot_y
    | Some (spot_x, spot_y) ->
        Mat.set map spot_x spot_y @@ Mat.get map next_x next_y ;
        Mat.set map next_x next_y '@' ;
        Mat.set map robot_x robot_y '.' ;
        next_x, next_y
  in
  let robot_x, robot_y, _ = Option.get @@ Mat.findi (fun _ _ c -> c = '@') map in
  let _ = Seq.fold_left step (robot_x, robot_y) moves in
  ()

let solve (input : string array) : string =
  let input = Array.to_seq input in
  let map = Mat.mat_of @@ Array.of_seq @@ Seq.take_while (( <> ) "") input in
  let moves = Seq.map dir_of @@ Seq.flat_map String.to_seq @@ Seq.drop (1 + Mat.x map) input in
  walk map moves ;
  print_endline @@ Mat.string_of_mat map ;
  let count acc x y c = acc + if c = 'O' then (x * 100) + y else 0 in
  Int.to_string @@ Mat.fold_lefti count 0 map
