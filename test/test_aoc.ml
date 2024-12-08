open Alcotest

let expect_io expected ~problem ~input () =
  let produced = Aoc.solve ~problem @@ Arg.read_arg input in
  check string "same string" expected produced

let expect expected ~problem ~input =
  let input = Printf.sprintf "../data/%s.txt" input in
  (problem, `Quick, expect_io expected ~problem ~input)

let suite =
  [
    expect ~problem:"d01a" ~input:"d01" "11";
    expect ~problem:"d01b" ~input:"d01" "31";
    expect ~problem:"d02a" ~input:"d02" "2";
    expect ~problem:"d02b" ~input:"d02" "4";
    expect ~problem:"d03a" ~input:"d03a" "161";
    expect ~problem:"d03b" ~input:"d03b" "48";
    expect ~problem:"d04a" ~input:"d04" "18";
    expect ~problem:"d04b" ~input:"d04" "9";
    expect ~problem:"d05a" ~input:"d05" "143";
    expect ~problem:"d05b" ~input:"d05" "123";
    expect ~problem:"d06a" ~input:"d06" "41";
    expect ~problem:"d06b" ~input:"d06" "6";
    expect ~problem:"d07a" ~input:"d07" "3749";
    expect ~problem:"d07b" ~input:"d07" "11387";
    expect ~problem:"d08a" ~input:"d08" "14";
    expect ~problem:"d08b" ~input:"d08" "34";
    expect ~problem:"d09a" ~input:"d09" "1928";
    expect ~problem:"d09b" ~input:"d09" "2858";
  ]

let () = Alcotest.run "AoC examples" [ ("Example", suite) ]
