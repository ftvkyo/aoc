let check rules update =
  let rec ban update rule_l =
    match update with
    | [] -> true
    | u_entry :: u_tail -> u_entry <> rule_l && ban u_tail rule_l
  in
  let rec check update (rule_l, rule_r) =
    match update with
    | [] -> true
    | u_entry :: u_tail -> if u_entry = rule_r then ban u_tail rule_l else check u_tail (rule_l, rule_r)
  in
  if List.for_all (check update) rules then
    let len = List.length update in
    let middle = len / 2 in
    int_of_string @@ List.nth update middle
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
  let check = check !rules in
  let fold acc update = acc + check update in
  Int.to_string @@ List.fold_left fold 0 !updates
