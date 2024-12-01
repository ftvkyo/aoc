open Alcotest

let test_problem problem filename expected () =
  let got = Aoc.solve ~problem @@ Arg.read_arg filename in
  check string "same string" got expected

let suite =
  [ ("d01a", `Quick, test_problem "d01a" "../data/d01.txt" "11") ]

let () = Alcotest.run "AoC examples" [ ("Example", suite) ]
