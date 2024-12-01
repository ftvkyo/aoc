let count el arr = List.length @@ List.filter (( = ) el) arr

let solve (input : string array) : string =
  let ids_left, ids_right = Array.split @@ Array.map D01a.extract_ids input in
  let calc id_left = id_left * (count id_left @@ Array.to_list ids_right) in
  let calcs = Array.map calc ids_left in
  Int.to_string @@ Array.fold_left ( + ) 0 calcs
