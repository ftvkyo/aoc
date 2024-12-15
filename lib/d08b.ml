open Common

let empty = '.'

let node = '#'

let find_antennas (map : char Mat.t) =
  let antennas : (char, int * int) Hashtbl.t = Hashtbl.create @@ (Mat.x map * Mat.y map) in
  let add_antenna x y a = if a <> empty then Hashtbl.add antennas a (x, y) in
  Mat.iteri add_antenna map ;
  antennas

let find_antinodes (map_nodes : char Mat.t) antennas ac (ax, ay) =
  let siblings = Hashtbl.find_all antennas ac in
  let rec add_nodes (x, y) (dx, dy) =
    if Option.is_some @@ Mat.set_opt map_nodes x y node then add_nodes (x + dx, y + dy) (dx, dy)
  in
  let add_nodes (x, y) =
    if (x, y) <> (ax, ay) then (
      let dx, dy = x - ax, y - ay in
      let gcd = gcd dx dy in
      let dx, dy = dx / gcd, dy / gcd in
      add_nodes (x, y) (dx, dy) ;
      add_nodes (x, y) (-dx, -dy) )
  in
  List.iter add_nodes siblings

let solve (input : string array) : string =
  let city_map = Mat.mat_of input in
  let antennas = find_antennas city_map in
  let node_map = Mat.make (Mat.x city_map) (Mat.y city_map) empty in
  Hashtbl.iter (find_antinodes node_map antennas) antennas ;
  print_endline @@ Mat.string_of_mat city_map ;
  print_endline @@ Mat.string_of_mat node_map ;
  let re_node = Str.regexp @@ Str.quote @@ Char.escaped node in
  Int.to_string @@ count re_node @@ Mat.string_of_mat node_map
