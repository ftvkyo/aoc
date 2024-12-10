type report = int list

type 'a trend = 'a -> 'a -> bool

let rec check_safe (dampened : bool) (trend : int trend) (re : report) : bool =
  match re with
  | [] -> true
  | _ :: [] -> true
  | h1 :: h2 :: t ->
      if trend h1 h2 then check_safe dampened trend (h2 :: t) else (not dampened) && check_safe true trend (h1 :: t)

let is_safe (re : report) : bool =
  let diff_safe a b = abs (a - b) <= 3 in
  let trend op a b = op a b && diff_safe a b in
  (*
    One of the functions in the `<` & `>` pair is guaranteed to exit after the first trend check.
    This also returns early if a safe representation is found.
    So in the worst case, this will run through the entire list at most twice.
  *)
  check_safe false (trend ( > )) re
  || check_safe false (trend ( < )) re
  || (check_safe true (trend ( > )) @@ List.tl re)
  || (check_safe true (trend ( < )) @@ List.tl re)

let solve (input : string array) : string =
  let fold acc line = acc + if is_safe @@ Common.ints line then 1 else 0 in
  Int.to_string @@ Array.fold_left fold 0 input
