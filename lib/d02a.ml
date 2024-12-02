type report = int list
type 'a trend = ('a -> 'a -> bool) option

let rec is_safe (tr : int trend) (re : report) : bool =
  match re with
  | [] -> true
  | _ :: [] -> true
  | h1 :: h2 :: t ->
      let diff = abs @@ (h1 - h2) in
      let diff_safe = 1 <= diff && diff <= 3 in
      let tr = Option.value tr ~default:(if h1 < h2 then ( < ) else ( > )) in
      let tr_safe = tr h1 h2 in
      diff_safe && tr_safe && is_safe (Some tr) (h2 :: t)

let solve (input : string array) : string =
  let fold acc line = acc + if is_safe None @@ Common.ints line then 1 else 0 in
  Int.to_string @@ Array.fold_left fold 0 input
