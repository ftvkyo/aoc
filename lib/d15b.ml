open Printf

let widen line =
  let wideners = [ '#', "##"; 'O', "[]"; '.', ".."; '@', "@." ] in
  let widen c = List.assoc c wideners in
  String.concat "" @@ List.of_seq @@ Seq.map widen @@ String.to_seq line

let check_of get move =
  let fail c = failwith @@ sprintf "Found '%c' when checking for free spots?" c in
  let rec check_leftright dy (at_x, at_y) =
    match get at_x (at_y + dy) with
    | '#' -> false
    | '.' -> true
    | '[' | ']' -> check_leftright dy (at_x, at_y + dy)
    | _ as c -> fail c
  in
  let rec check_updown dx (at_x, at_y) =
    match get (at_x + dx) at_y with
    | '#' -> false
    | '.' -> true
    | '[' -> check_updown dx (at_x + dx, at_y) && check_updown dx (at_x + dx, at_y + 1)
    | ']' -> check_updown dx (at_x + dx, at_y - 1) && check_updown dx (at_x + dx, at_y)
    | _ as c -> fail c
  in
  let checks = [ '^', check_updown (-1); '>', check_leftright 1; 'v', check_updown 1; '<', check_leftright (-1) ] in
  List.assoc move checks

let update_of get set move =
  let fail c = failwith @@ sprintf "Found '%c' when moving into free spots?" c in
  let rec move_leftright dy (at_x, at_y) c_from =
    let ny = at_y + dy in
    let c_to = get at_x ny in
    match c_to with
    | '.' -> set at_x ny c_from
    | '[' | ']' ->
        set at_x ny c_from ;
        move_leftright dy (at_x, ny) c_to
    | _ -> fail c_to
  in
  let rec move_updown dx (at_x, at_y) c_from =
    let nx = at_x + dx in
    let c_to = get nx at_y in
    match c_to with
    | '.' -> set nx at_y c_from
    | '[' ->
        set nx at_y c_from ;
        move_updown dx (nx, at_y) '[' ;
        set nx (at_y + 1) '.' ;
        move_updown dx (nx, at_y + 1) ']'
    | ']' ->
        set nx (at_y - 1) '.' ;
        move_updown dx (nx, at_y - 1) '[' ;
        set nx at_y c_from ;
        move_updown dx (nx, at_y) ']'
    | _ -> fail c_to
  in
  let moves = [ '^', move_updown (-1); '>', move_leftright 1; 'v', move_updown 1; '<', move_leftright (-1) ] in
  List.assoc move moves

let walk map moves =
  let step move =
    let robot_x, robot_y, _ = Option.get @@ Mat.findi (fun _ _ c -> c = '@') map in
    let check = check_of (Mat.get map) move in
    if check (robot_x, robot_y) then (
      let update = update_of (Mat.get map) (Mat.set map) move in
      Mat.set map robot_x robot_y '.' ;
      update (robot_x, robot_y) '@' )
  in
  Seq.iter step moves

let solve (input : string array) : string =
  let input = Array.to_seq input in
  let map = Mat.mat_of @@ Array.of_seq @@ Seq.map widen @@ Seq.take_while (( <> ) "") input in
  let moves = Seq.flat_map String.to_seq @@ Seq.drop (1 + Mat.x map) input in
  walk map moves ;
  print_endline @@ Mat.string_of_mat map ;
  let count acc x y c = acc + if c = '[' then (x * 100) + y else 0 in
  Int.to_string @@ Mat.fold_lefti count 0 map
