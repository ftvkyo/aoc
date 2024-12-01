let problem, filename =
  try (Sys.argv.(1), Sys.argv.(2))
  with e ->
    print_endline "Expected problem id and input filename";
    raise e

let answer = Aoc.solve ~problem @@ Arg.read_arg filename
let () = print_endline answer
