exception ProblemNotFound of string

let solve ~(problem : string) : string array -> string =
  match problem with
  | "d01a" -> D01a.solve
  | _ -> raise @@ ProblemNotFound problem
