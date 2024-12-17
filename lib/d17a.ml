open Printf

type register = A | B | C

type operand_combo = Literal of int | Register of register

type instruction =
  | Adv of operand_combo
  | Bxl of int
  | Bst of operand_combo
  | Jnz of int
  | Bxc
  | Out of operand_combo
  | Bdv of operand_combo
  | Cdv of operand_combo

type cpu_state = { ip: int; ra: int; rb: int; rc: int; mem: instruction array; out: char list }

let parse_operand_literal (operand : char) : int =
  if '0' <= operand && operand <= '7' then Char.code operand - Char.code '0'
  else failwith @@ sprintf "Unknown literal operand '%c'" operand

let parse_operand_combo (operand : char) : operand_combo =
  if '0' <= operand && operand <= '3' then Literal (Char.code operand - Char.code '0')
  else
    match operand with
    | '4' -> Register A
    | '5' -> Register B
    | '6' -> Register C
    | _ -> failwith @@ sprintf "Unknown combo operand '%c'" operand

let parse_instruction (opcode : char) (operand : char) : instruction =
  match opcode with
  | '0' -> Adv (parse_operand_combo operand)
  | '1' -> Bxl (parse_operand_literal operand)
  | '2' -> Bst (parse_operand_combo operand)
  | '3' -> Jnz (parse_operand_literal operand)
  | '4' -> Bxc
  | '5' -> Out (parse_operand_combo operand)
  | '6' -> Bdv (parse_operand_combo operand)
  | '7' -> Cdv (parse_operand_combo operand)
  | _ -> failwith @@ sprintf "Unknown opcode '%c'" opcode

let parse_program (program : string) : instruction array =
  let program = List.of_seq @@ Seq.filter (( <> ) ',') @@ String.to_seq program in
  let rec parse_program program =
    match program with
    | opcode :: operand :: tail -> parse_instruction opcode operand :: parse_program tail
    | _ :: [] -> failwith "Program length % 2 != 0"
    | _ -> []
  in
  Array.of_list @@ parse_program program

let exec st =
  let get_combo op =
    match op with
    | Literal n -> n
    | Register r -> (
      match r with
      | A -> st.ra
      | B -> st.rb
      | C -> st.rc )
  in
  let nst = { st with ip= st.ip + 1 } in
  let exec_adv op =
    let n = st.ra in
    let m = Int.shift_left 1 @@ get_combo op in
    { nst with ra= n / m }
  in
  let exec_bxl op = { nst with rb= Int.logxor st.rb op } in
  let exec_bst op = { nst with rb= get_combo op mod 8 } in
  let exec_jnz op =
    if st.ra = 0 then nst
    else if op mod 2 = 0 then { st with ip= op / 2 }
    else failwith @@ sprintf "Tried to jump to a non-even address: %d" op
  in
  let exec_bxc () = { nst with rb= Int.logxor st.rb st.rc } in
  let exec_out op =
    let i = get_combo op mod 8 in
    let c = Char.chr @@ (Char.code '0' + i) in
    { nst with out= c :: st.out }
  in
  let exec_bdv op =
    let n = st.ra in
    let m = Int.shift_left 1 @@ get_combo op in
    { nst with rb= n / m }
  in
  let exec_cdv op =
    let n = st.ra in
    let m = Int.shift_left 1 @@ get_combo op in
    { nst with rc= n / m }
  in
  function
  | Adv op -> exec_adv op
  | Bxl op -> exec_bxl op
  | Bst op -> exec_bst op
  | Jnz op -> exec_jnz op
  | Bxc -> exec_bxc ()
  | Out op -> exec_out op
  | Bdv op -> exec_bdv op
  | Cdv op -> exec_cdv op

let rec run (st : cpu_state) =
  let open Array in
  let mem_sz = length st.mem in
  if st.ip < mem_sz then
    let instr = get st.mem st.ip in
    run @@ exec st instr
  else st (* Halt *)

let solve input =
  let match_reg line =
    let re = Str.regexp {|Register [ABC]: \([0-9]+\)|} in
    let _ = Str.string_match re line 0 in
    int_of_string @@ Str.matched_group 1 line
  in
  let match_program line =
    let re = Str.regexp {|Program: \([0-9,]+\)|} in
    let _ = Str.string_match re line 0 in
    Str.matched_group 1 line
  in
  let st =
    run
      { ip= 0
      ; ra= 0 |> Array.get input |> match_reg
      ; rb= 1 |> Array.get input |> match_reg
      ; rc= 2 |> Array.get input |> match_reg
      ; mem= 4 |> Array.get input |> match_program |> parse_program
      ; out= []
      }
  in
  let output =
    Seq.repeat ',' |> Seq.take @@ (List.length st.out - 1) |> Seq.interleave @@ List.to_seq @@ List.rev st.out
  in
  String.of_seq output
