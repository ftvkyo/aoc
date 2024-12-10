let approach_sort rules update =
  let rec approach_sort update (rule_l, rule_r) =
    match update with
    | [] -> []
    | entry :: tail ->
        if entry = rule_r then
          let new_tail = List.filter (( <> ) rule_l) tail in
          if List.(length tail <> length new_tail) then
            (* The tail contains rule_l, move it forward *)
            rule_l :: rule_r :: new_tail
          else entry :: tail
        else entry :: approach_sort tail (rule_l, rule_r)
  in
  List.fold_left approach_sort update rules

let rec calc ?(corrected = false) rules update =
  let sorted = approach_sort rules update in
  let changed = not @@ List.equal ( = ) update sorted in
  if changed then calc ~corrected:true rules sorted
  else if corrected then
    let len = List.length sorted in
    let middle = len / 2 in
    int_of_string @@ List.nth sorted middle
  else 0

let solve (input : string array) : string =
  let re_rule = Str.regexp @@ Str.quote "|" in
  let is_rule line = try 0 <= Str.search_forward re_rule line 0 with _ -> false in
  let rules = ref [] in
  let re_update = Str.regexp @@ Str.quote "," in
  let updates = ref [] in
  let ingest line =
    if is_rule line then
      match Str.split re_rule line with
      | [ p1; p2 ] -> rules := (p1, p2) :: !rules
      | _ -> failwith "Expected 2 rules?"
    else
      let update = Str.split re_update line in
      if not @@ List.is_empty update then updates := update :: !updates
  in
  Array.iter ingest input ;
  let fold acc update = acc + calc !rules update in
  Int.to_string @@ List.fold_left fold 0 !updates
