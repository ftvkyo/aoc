open Alcotest

let test_problem_io problem filename expected () =
  let produced = Aoc.solve ~problem @@ Arg.read_arg filename in
  check string "same string" produced expected

let test_problem problem expected =
  let id_re = Str.regexp {|^\(d[0-9]+\)[ab]$|} in
  if Str.string_match id_re problem 0 then
    let id = Str.matched_group 1 problem in
    let filename = "../data/" ^ id ^ ".txt" in
    (problem, `Quick, test_problem_io problem filename expected)
  else raise @@ Invalid_argument ("could not parse id " ^ problem)

let suite = [ test_problem "d01a" "11"; test_problem "d01b" "31" ]
let () = Alcotest.run "AoC examples" [ ("Example", suite) ]
