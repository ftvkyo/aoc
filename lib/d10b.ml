open Common

let heights = List.init 10 (fun x -> char_of_int @@ (int_of_char '0' + x))

let last_height = List.hd heights

let heights = List.rev heights

let adjacent_for x y = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]

let solve (input : string array) : string =
  let map = matrix_of '.' input in
  let trails = matrix_init [] map#x map#y in
  let build_trail height_from height_to =
    let probe at_x at_y at_height =
      if height_from = at_height then
        let at_trails = trails#get at_x at_y in
        let try_update (adj_x, adj_y) =
          match map#try_get adj_x adj_y with
          | Some adj_height ->
              if adj_height = height_to then trails#set adj_x adj_y @@ at_trails @ trails#get adj_x adj_y
          | _ -> ()
        in
        List.iter try_update @@ adjacent_for at_x at_y
    in
    map#iteri probe
  in
  let rec build_trails_for heights =
    match heights with
    | height_from :: height_to :: tail ->
        build_trail height_from height_to ;
        build_trails_for (height_to :: tail)
    | _ -> ()
  in
  let add_start_points_at height =
    let probe at_x at_y at_height = if at_height = height then trails#set at_x at_y [ at_x, at_y ] in
    map#iteri probe
  in
  add_start_points_at @@ List.hd heights ;
  build_trails_for heights ;
  let total_score = ref 0 in
  let count_trails to_height at_x at_y accessible_from =
    if map#get at_x at_y = to_height then total_score := !total_score + List.length accessible_from
  in
  trails#iteri (count_trails last_height) ;
  Int.to_string !total_score
