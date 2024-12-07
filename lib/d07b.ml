let ops =
  [
    ( + );
    ( * );
    (fun a b -> int_of_string @@ Int.to_string a ^ Int.to_string b);
  ]

let guess_ops answer args =
  let rec guess_ops acc args =
    if acc > answer then
      (* Stop the recursion early if we went past the answer *)
      false
    else
      match args with
      | arg :: tail ->
          let check op =
            let acc = op acc arg in
            guess_ops acc tail
          in
          List.exists check ops
      | _ -> acc = answer
  in
  guess_ops (List.hd args) (List.tl args)

let check line =
  let re_colon = Str.regexp @@ Str.quote ":" in
  match Str.split re_colon line with
  | [ answer; args ] ->
      let answer = int_of_string answer in
      let args = Common.ints args in
      if guess_ops answer args then answer else 0
  | _ -> failwith "No colon in line?"

let solve (input : string array) : string =
  let fold acc line = acc + check line in
  Int.to_string @@ Array.fold_left fold 0 input
