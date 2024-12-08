open Common

let empty = '.'
let node = '#'

let find_antennas (map : matrix) =
  let antennas : (char, int * int) Hashtbl.t =
    Hashtbl.create @@ (map#x * map#y)
  in
  let add_antenna x y a = if a <> empty then Hashtbl.add antennas a (x, y) in
  map#iteri add_antenna;
  antennas

let find_antinodes (map_nodes : matrix) antennas ac (ax, ay) =
  let siblings = Hashtbl.find_all antennas ac in
  let add_nodes (x, y) =
    if (x, y) <> (ax, ay) then
      let dx, dy = (x - ax, y - ay) in
      let _ = map_nodes#try_set (x + dx) (y + dy) node in
      let _ = map_nodes#try_set (ax - dx) (ay - dy) node in
      ()
  in
  List.iter add_nodes siblings

let solve (input : string array) : string =
  let city_map = matrix_of empty input in
  let antennas = find_antennas city_map in
  let node_map = matrix_init empty city_map#x city_map#y in
  Hashtbl.iter (find_antinodes node_map antennas) antennas;
  print_endline @@ city_map#to_string;
  print_endline @@ node_map#to_string;
  let re_node = Str.regexp @@ Str.quote @@ Char.escaped node in
  Int.to_string @@ count re_node node_map#to_string