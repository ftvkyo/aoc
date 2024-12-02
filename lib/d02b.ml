type report = int list
type 'a trend = 'a -> 'a -> bool

let is_diff_safe h1 h2 =
  let diff = abs (h1 - h2) in
  1 <= diff && diff <= 3

let rec is_safe (dampened : bool) (trend : int trend) (re : report) : bool =
  match re with
  | [] -> true
  | _ :: [] -> true
  | h1 :: h2 :: t ->
      if is_diff_safe h1 h2 && trend h1 h2 then is_safe dampened trend (h2 :: t)
      else (not dampened) && is_safe true trend (h1 :: t)

let solve (input : string array) : string =
  let fold acc line =
    let test line =
      let ints = Common.ints line in
      is_safe false ( < ) ints || is_safe false ( > ) ints
      || (is_safe true ( < ) @@ List.tl ints)
      || (is_safe true ( > ) @@ List.tl ints)
    in
    acc + if test line then 1 else 0
  in
  Int.to_string @@ Array.fold_left fold 0 input
