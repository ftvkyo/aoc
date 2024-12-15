exception ProblemNotFound of string

let solvers =
  [ "d01a", D01a.solve
  ; "d01b", D01b.solve
  ; "d02a", D02a.solve
  ; "d02b", D02b.solve
  ; "d03a", D03a.solve
  ; "d03b", D03b.solve
  ; "d04a", D04a.solve
  ; "d04b", D04b.solve
  ; "d05a", D05a.solve
  ; "d05b", D05b.solve
  ; "d06a", D06a.solve
  ; "d06b", D06b.solve
  ; "d07a", D07a.solve
  ; "d07b", D07b.solve
  ; "d08a", D08a.solve
  ; "d08b", D08b.solve
  ; "d09a", D09a.solve
  ; "d09b", D09b.solve
  ; "d10a", D10a.solve
  ; "d10b", D10b.solve
  ; "d11a", D11a.solve
  ; "d11b", D11b.solve
  ; "d12a", D12a.solve
  ; "d12b", D12b.solve
  ; "d13a", D13a.solve
  ; "d14a", D14a.solve (11, 7)
  ; "d14a-personal", D14a.solve (101, 103)
  ; "d14b", D14b.solve (101, 103)
  ; "d15a", D15a.solve
  ]

let solve ~(problem : string) : string array -> string =
  match List.assoc_opt problem solvers with
  | Some solver -> solver
  | None -> raise @@ ProblemNotFound problem
