open Printf

let parse_button = D13a.parse_button

let parse_prize ~add_ten_trillion line =
  let px, py = D13a.parse_prize line in
  if add_ten_trillion then px + 10_000_000_000_000, py + 10_000_000_000_000 else px, py

let mul (x, y) n = n * x, n * y

let add_cwise (ax, ay) (bx, by) = ax + bx, ay + by

let sub_cwise (ax, ay) (bx, by) = ax - bx, ay - by

let div_cwise (ax, ay) (bx, by) = ax / bx, ay / by

let div_cwise_min (ax, ay) (bx, by) = min (ax / bx) (ay / by)

let div_cwise_max (ax, ay) (bx, by) = max (ax / bx) (ay / by)

let mod_cwise (ax, ay) (bx, by) = ax mod bx, ay mod by

let mod_cwise_max (ax, ay) (bx, by) = max (ax mod bx) (ay mod by)

let fmt_xy (x, y) = sprintf "(x=%d y=%d)" x y

let cost a b p =
  printf "\nA %s, B %s, P %s\n" (fmt_xy a) (fmt_xy b) (fmt_xy p) ;
  (* Find how many B pushes we need to revert to let A pushes take the whole remaining distance *)
  let rec try_with ?(a_factor_acc = 0) prize_left =
    printf " -> with A factor accumulator %d and prize location %s\n" a_factor_acc (fmt_xy prize_left) ;
    let b_factor = div_cwise_min prize_left b in
    let b_rem = sub_cwise prize_left @@ mul b b_factor in
    printf " ->  -> B factor %d, remainder for A is %s\n" b_factor (fmt_xy b_rem) ;
    (* Find how many A pushes can fit in the remaining distance *)
    let a_factor = div_cwise_min b_rem a in
    let a_rem = sub_cwise b_rem @@ mul a a_factor in
    printf " ->  -> A factor %d, remainder to be fixed %s\n" a_factor (fmt_xy a_rem) ;
    if a_rem = (0, 0) then Some (a_factor_acc + a_factor, b_factor)
    else if b_factor <= 0 then None
    else begin
      let new_a_factor = max 1 @@ div_cwise_max a_rem a in
      let new_a_rem = sub_cwise prize_left @@ mul a new_a_factor in
      try_with ~a_factor_acc:(a_factor_acc + new_a_factor) new_a_rem
    end
  in
  match try_with p with
  | Some (a_factor, b_factor) -> begin
    printf "Found!\n" ;
    (a_factor * 3) + b_factor
  end
  | None -> begin
    printf "Impossible!\n" ;
    0
  end

let solve ?(add_ten_trillion = false) (input : string array) : string =
  let fold_input acc line =
    if line = "" then acc
    else
      let total, state = acc in
      match state with
      | [ a; b ] -> total + (cost a b @@ parse_prize ~add_ten_trillion line), []
      | a :: [] -> total, [ a; parse_button line ]
      | _ -> total, [ parse_button line ]
  in
  let total, _ = Array.fold_left fold_input (0, []) input in
  Int.to_string total
