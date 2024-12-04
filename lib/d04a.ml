type rectangle = char array array

let to_rect lines = Array.map (fun row -> Array.of_seq @@ String.to_seq row) lines

let from_rect rect = String.concat "\n" @@ Array.to_list @@ Array.map (fun row -> String.of_seq @@ Array.to_seq row) rect

let transpose (rect : rectangle) =
  let rect_x = Array.length rect in
  let rect_y = Array.length @@ Array.get rect 0 in
  let get x y = try rect.(y).(x) with _ -> '.' in
  Array.init_matrix rect_y rect_x get

let rotate_180 (rect : rectangle) =
  let rect_x = Array.length rect in
  let rect_y = Array.length @@ Array.get rect 0 in
  let get x y = try rect.(rect_x - 1 - x).(rect_y - 1 - y) with _ -> '.' in
  Array.init_matrix rect_x rect_y get

let rotate_45_cwise (rect : rectangle) =
  let rect_x = Array.length rect in
  let rect_y = Array.length @@ Array.get rect 0 in
  let new_x = rect_y + rect_x - 1 in
  let get x y = try rect.(x - y).(y) with _ -> '.' in
  Array.init_matrix new_x rect_y get

let rotate_45_ccwise (rect : rectangle) =
  let rect_x = Array.length rect in
  let rect_y = Array.length @@ Array.get rect 0 in
  let new_x = rect_y + rect_x - 1 in
  let get x y = try rect.(x + y - (rect_y - 1)).(y) with _ -> '.' in
  Array.init_matrix new_x rect_y get

let transforms : (rectangle -> rectangle) list = [
  Fun.id;
  transpose;
  rotate_45_cwise;
  rotate_45_ccwise;
  rotate_180;
  Fun.compose transpose rotate_180;
  Fun.compose rotate_45_cwise rotate_180;
  Fun.compose rotate_45_ccwise rotate_180;
]

let solve (input : string array) : string =
  let rect = to_rect input in
  let xmas = Str.regexp @@ Str.quote "XMAS" in
  let apply acc transform =
    let s = from_rect @@ transform rect in
    acc + Common.count xmas s
  in
  Int.to_string @@ List.fold_left apply 0 transforms
