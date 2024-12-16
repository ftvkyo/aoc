open Alcotest

let expect_io expected ~problem ~input () =
  let produced = Aoc.solve ~problem @@ Arg.read_arg input in
  check string "same string" expected produced

let expect expected ~problem ~input =
  let name = Printf.sprintf "%s (with %s.txt)" problem input in
  let input = Printf.sprintf "../data/%s.txt" input in
  name, `Quick, expect_io expected ~problem ~input

let suite =
  [ expect ~problem:"d01a" ~input:"d01" "11"
  ; expect ~problem:"d01b" ~input:"d01" "31"
  ; expect ~problem:"d02a" ~input:"d02" "2"
  ; expect ~problem:"d02b" ~input:"d02" "4"
  ; expect ~problem:"d03a" ~input:"d03a" "161"
  ; expect ~problem:"d03b" ~input:"d03b" "48"
  ; expect ~problem:"d04a" ~input:"d04" "18"
  ; expect ~problem:"d04b" ~input:"d04" "9"
  ; expect ~problem:"d05a" ~input:"d05" "143"
  ; expect ~problem:"d05b" ~input:"d05" "123"
  ; expect ~problem:"d06a" ~input:"d06" "41"
  ; expect ~problem:"d06b" ~input:"d06" "6"
  ; expect ~problem:"d07a" ~input:"d07" "3749"
  ; expect ~problem:"d07b" ~input:"d07" "11387"
  ; expect ~problem:"d08a" ~input:"d08" "14"
  ; expect ~problem:"d08b" ~input:"d08" "34"
  ; expect ~problem:"d09a" ~input:"d09" "1928"
  ; expect ~problem:"d09b" ~input:"d09" "2858"
  ; expect ~problem:"d10a" ~input:"d10" "36"
  ; expect ~problem:"d10b" ~input:"d10" "81"
  ; expect ~problem:"d11a" ~input:"d11" "55312"
  ; expect ~problem:"d12a" ~input:"d12" "1930"
  ; expect ~problem:"d12b" ~input:"d12" "1206"
  ; expect ~problem:"d12b" ~input:"d12-extra" "368"
  ; expect ~problem:"d13a" ~input:"d13" "480"
  ; expect ~problem:"d13b-test" ~input:"d13" "480"
  ; expect ~problem:"d14a" ~input:"d14" "12"
  ; expect ~problem:"d15a" ~input:"d15-small" "2028"
  ; expect ~problem:"d15a" ~input:"d15-large" "10092"
  ; expect ~problem:"d15b" ~input:"d15-large" "9021"
  ; expect ~problem:"d16a" ~input:"d16-small" "7036"
  ; expect ~problem:"d16a" ~input:"d16-large" "11048"
  ]

let () = Alcotest.run "AoC examples" [ "Example", suite ]
