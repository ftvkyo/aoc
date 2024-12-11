open Common

let has_even_num_of_digits rock =
  let digits = String.length @@ Int.to_string rock in
  digits mod 2 = 0

let split rock =
  let rock = Int.to_string rock in
  let digits = String.length rock in
  let rock_a, rock_b = String.sub rock 0 (digits / 2), String.sub rock (digits / 2) (digits / 2) in
  int_of_string rock_a, int_of_string rock_b

let rec blink rocks =
  match rocks with
  | [] -> []
  | rock :: tail ->
      if rock = 0 then 1 :: blink tail
      else if rock |> has_even_num_of_digits then
        let rock_a, rock_b = split rock in
        rock_a :: rock_b :: blink tail
      else (rock * 2024) :: blink tail

let rec blink25 ?(count = 0) rocks = if count >= 25 then rocks else blink25 ~count:(count + 1) @@ blink rocks

let solve (input : string array) : string =
  let rocks = input.(0) |> ints in
  let rocks25 = blink25 rocks in
  Int.to_string @@ List.length rocks25
