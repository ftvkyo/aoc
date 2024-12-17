open Printf

let shl = Int.shift_left

let shr = Int.shift_right_logical

let print_constraints constrs =
  let print (cbit, cval) = printf "[%d] = %d, " cbit cval in
  List.iter print constrs ;
  printf "\n"

let constrain o =
  let constraints =
    let id v = v in
    let inv v = 1 - v in
    let c0 _ = 0 in
    let c1 _ = 1 in
    [ (5, id), (4, inv), (3, inv)
    ; (4, id), (3, inv), (2, c0)
    ; (3, id), (2, c0), (1, c0)
    ; (2, c0), (1, c1), (0, c1)
    ; (9, inv), (8, inv), (7, inv)
    ; (8, inv), (7, inv), (6, id)
    ; (7, inv), (6, id), (5, inv)
    ; (6, inv), (5, id), (4, id)
    ]
  in
  let bit0 = o mod 2 in
  let bit1 = shr o 1 mod 2 in
  let bit2 = shr o 2 mod 2 in
  let for_option opt =
    let (a, af), (b, bf), (c, cf) = opt in
    [ a, af bit2; b, bf bit1; c, cf bit0 ]
  in
  List.map for_option constraints

let constraints_shift n constraints = List.map (fun (cbit, cval) -> cbit + n, cval) constraints

let undo o_seq =
  let rec undo ?(depth = 0) o_seq a constraints =
    let prefix = String.make (depth * 2) ' ' in
    match o_seq with
    | o :: o_tail ->
        printf "\n" ;
        printf "%sCurrent constraints:\n%s" prefix prefix ;
        print_constraints constraints ;
        printf "%sA=%d, O=%d\n" prefix a o ;
        let a = shl a 3 + o in
        let constraints = constraints_shift 3 constraints in
        printf "%s -> new A=%d\n" prefix a ;
        let bit n = shr a n mod 2 in
        let try_with new_constraints =
          let check (bit_num, bit_val) = bit bit_num = bit_val in
          let constraints = List.sort_uniq compare (constraints @ new_constraints) in
          if List.for_all check constraints then undo ~depth:(depth + 1) o_tail a constraints else None
        in
        let possible_new_constraints = constrain o in
        let fold_min acc el =
          match acc, el with
          | Some acc, Some el -> Some (min acc el)
          | Some acc, None -> Some acc
          | None, Some el -> Some el
          | _ -> None
        in
        let results = List.map try_with possible_new_constraints in
        let result_min = List.fold_left fold_min None results in
        result_min
    | [] ->
        (* Reconstructed all elements of the sequence successfully *)
        Some a
  in
  undo o_seq 0 []

let solve input =
  let match_program line =
    let re = Str.regexp {|Program: \([0-9,]+\)|} in
    let _ = Str.string_match re line 0 in
    Str.matched_group 1 line
  in
  (* Note: this is only meant to work with my personal input *)
  let program = match_program @@ Array.get input 4 in
  let program = List.rev @@ List.of_seq @@ Seq.filter (( <> ) ',') @@ String.to_seq program in
  let program = List.map (fun c -> Char.code c - Char.code '0') program in
  let a = Option.get @@ undo program in
  Int.to_string a
