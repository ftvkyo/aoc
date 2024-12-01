exception ProblemNotFound of string

let solve ~(problem : string) : string array -> string =
  match problem with
  | "d01a" -> D01a.solve
  | "d01b" -> D01b.solve
  | _ -> raise @@ ProblemNotFound problem
