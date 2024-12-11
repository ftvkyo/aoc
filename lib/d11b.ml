open Common

type rock = Rock of int

type times = Times of int

let ( ~- ) t =
  let (Times t) = t in
  if t > 0 then Times (t - 1) else failwith "Tried to decrement `times` past zero"

let blink_once ~at =
  let open String in
  let (Rock r) = at in
  if r = 0 then [ Rock 1 ]
  else
    let r_str = Int.to_string r in
    let r_digits = length r_str in
    if r_digits mod 2 = 0 then
      let split_l = int_of_string @@ sub r_str 0 (r_digits / 2) in
      let split_r = int_of_string @@ sub r_str (r_digits / 2) (r_digits / 2) in
      [ Rock split_l; Rock split_r ]
    else [ Rock (r * 2024) ]

let blink ntimes rocks =
  let open Hashtbl in
  (* Memoize what is produced after blinking once at a rock *)
  let blink_once_mem = create 100_000 in
  let blink_once ~at =
    match find_opt blink_once_mem at with
    | Some rs -> rs
    | None ->
        let rs = blink_once ~at in
        add blink_once_mem at rs ;
        rs
  in
  (* Memoize how many rocks are produced after blinking many times at a rock *)
  let blink_many_mem = create 100_000 in
  let rec blink_many blinks ~at =
    if Times 0 = blinks then 1
    else
      match find_opt blink_many_mem (blinks, at) with
      | Some count -> count
      | None ->
          let post_rocks = blink_once ~at in
          let blink_fold blinks acc r = acc + blink_many blinks ~at:r in
          let count = List.fold_left (blink_fold ~-blinks) 0 post_rocks in
          add blink_many_mem (blinks, at) count ;
          count
  in
  let blink_many_fold acc r = acc + blink_many (Times ntimes) ~at:(Rock r) in
  List.fold_left blink_many_fold 0 rocks

let solve (input : string array) : string =
  let rocks = input.(0) |> ints in
  Int.to_string @@ blink 75 rocks
