type state = int * int * char

let dirs = [ '^', '>'; '>', 'v'; 'v', '<'; '<', '^' ]

let is_dir c = Option.is_some @@ List.assoc_opt c dirs

let next x y d =
  let dx, dy = Option.get @@ D06a.dir_of d in
  x + dx, y + dy

let turn d = List.assoc d dirs

let walk (m : char Mat.t) =
  let rec step ?blocked (visited : state list) x y d =
    if List.exists (( = ) (x, y, d)) visited then (
      print_endline @@ String.concat "\n" @@ List.map (fun (x, y, d) -> Printf.sprintf "%c %d,%d" d x y) visited ;
      print_endline @@ Printf.sprintf "blocked: %d,%d\n" (fst @@ Option.get blocked) (snd @@ Option.get blocked) ;
      [ Option.get blocked ] )
    else
      let visited = (x, y, d) :: visited in
      let nx, ny = next x y d in
      try
        let obstacle = '#' = Mat.get m nx ny || Some (nx, ny) = blocked in
        let used_path = List.exists (fun (x, y, _d) -> x = nx && y = ny) visited in
        if obstacle then step ?blocked visited x y @@ turn d
        else if None = blocked && not used_path then
          let ans_blocked = step ~blocked:(nx, ny) [] x y @@ turn d in
          let ans_unblocked = step ?blocked visited nx ny d in
          ans_blocked @ ans_unblocked
        else step ?blocked visited nx ny d
      with _ -> []
  in
  let x, y, _ = Option.get @@ Mat.findi (fun _ _ c -> is_dir c) m in
  let blocks = step [] x y @@ Mat.get m x y in
  let blocks = List.sort_uniq compare blocks in
  let blocks = List.filter (( <> ) (x, y)) blocks in
  let blocks = List.length blocks in
  print_endline @@ Printf.sprintf "blocks: %d" blocks ;
  blocks

let solve (input : string array) : string =
  let m = Mat.mat_of input in
  Int.to_string @@ walk m
