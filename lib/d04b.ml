open Fun

type rectangle = char array array
type pattern = (char * (int * int)) list

let to_rect lines =
  Array.map (fun row -> Array.of_seq @@ String.to_seq row) lines

(* The pattern is symmetric so we don't need transpositions *)

let patterns : pattern list =
  let pattern_base =
    [
      ('A', (0, 0));
      ('M', (-1, -1));
      ('M', (-1, 1));
      ('S', (1, -1));
      ('S', (1, 1));
    ]
  in
  let rotate_90 (c, (x, y)) = (c, (y, -x)) in
  let transforms =
    [
      id;
      rotate_90;
      compose rotate_90 rotate_90;
      compose rotate_90 @@ compose rotate_90 rotate_90;
    ]
  in
  let apply tf = List.map tf pattern_base in
  List.map apply transforms

let check_pattern get (pat : pattern) =
  let check_character (c, (x, y)) = c = get x y in
  List.for_all check_character pat

let solve (input : string array) : string =
  let rect = to_rect input in
  let get at_x at_y dx dy = try rect.(at_x + dx).(at_y + dy) with _ -> '.' in
  let count_at x acc y _e =
    acc + if List.exists (check_pattern @@ get x y) patterns then 1 else 0
  in
  let count_at_x acc x row =
    acc + (Seq.fold_lefti (count_at x) 0 @@ Array.to_seq row)
  in
  Int.to_string @@ Seq.fold_lefti count_at_x 0 @@ Array.to_seq rect
