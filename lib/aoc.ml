exception ProblemNotFound of string

let solve ~(problem : string) : string array -> string =
  match problem with
  | "d01a" -> D01a.solve
  | "d01b" -> D01b.solve
  | "d02a" -> D02a.solve
  | "d02b" -> D02b.solve
  | "d03a" -> D03a.solve
  | _ -> raise @@ ProblemNotFound problem
