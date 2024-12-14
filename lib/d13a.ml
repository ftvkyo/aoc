let re_button = Str.regexp {|Button [AB]: X\+\([0-9]+\), Y\+\([0-9]+\)|}

let re_prize = Str.regexp {|Prize: X=\([0-9]+\), Y=\([0-9]+\)|}

let parse_button line =
  let _ = Str.string_match re_button line 0 in
  let x = int_of_string @@ Str.matched_group 1 line in
  let y = int_of_string @@ Str.matched_group 2 line in
  x, y

let parse_prize line =
  let _ = Str.string_match re_prize line 0 in
  let x = int_of_string @@ Str.matched_group 1 line in
  let y = int_of_string @@ Str.matched_group 2 line in
  x, y

let rec cost a b ?(af = 0) ?(bf = 0) prize =
  (* Climb b first because we are trying to minimise the cost *)
  if af <= 100 then
    if bf <= 100 then
      let ax, ay = a in
      let bx, by = b in
      let px, py = prize in
      if (ax * af) + (bx * bf) = px && (ay * af) + (by * bf) = py then (af * 3) + bf
      else cost a b ~af ~bf:(bf + 1) prize
    else cost a b ~af:(af + 1) prize
  else 0

let solve (input : string array) : string =
  let fold_input acc line =
    let total, state = acc in
    match state with
    | [ a; b; prize ] -> total + cost a b prize, []
    | [ a; b ] -> total, [ a; b; parse_prize line ]
    | a :: [] -> total, [ a; parse_button line ]
    | _ -> total, [ parse_button line ]
  in
  let total, _ = Array.fold_left fold_input (0, []) input in
  Int.to_string total
