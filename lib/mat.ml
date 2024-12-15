type 'a t = 'a array array

let init x y (f : int -> int -> 'a) : 'a t = Array.init_matrix x y f

let make x y (v : 'a) : 'a t = Array.make_matrix x y v

let mat_of ?default (data : string array) =
  let x = Array.length data in
  let y = Array.fold_left max 0 @@ Array.map (fun s -> String.length s) data in
  let get =
    match default with
    | Some default -> ( fun x y -> try String.get data.(x) y with Not_found -> default )
    | None -> fun x y -> String.get data.(x) y
  in
  init x y get

let string_of_mat (m : char t) : string =
  let f acc row = acc ^ "\n" ^ String.of_seq @@ Array.to_seq row in
  Array.fold_left f "" m

let x (m : 'a t) = Array.length m

let y (m : 'a t) = if Array.length m > 0 then Array.length @@ m.(0) else 0

let size (m : 'a t) = x m * y m

let get (m : 'a t) x y = m.(x).(y)

let set (m : 'a t) x y (v : 'a) = Array.set m.(x) y v

let get_opt (m : 'a t) x y = try Option.some @@ get m x y with _ -> None

let set_opt (m : 'a t) x y (v : 'a) = try Option.some @@ set m x y v with _ -> None

let iteri (f : int -> int -> 'a -> unit) (m : 'a t) =
  let f x row = Array.iteri (f x) row in
  Array.iteri f m

let mapi (f : int -> int -> 'a -> 'b) (m : 'a t) : 'b t =
  let f x row = Array.mapi (f x) row in
  Array.mapi f m

let findi (f : int -> int -> 'a -> bool) (m : 'a t) : (int * int * 'a) option =
  let f (x, y) = f x y @@ get m x y in
  let xys =
    let x = x m in
    let y = y m in
    let open Seq in
    let xs = take x @@ ints 0 in
    let ys = take y @@ ints 0 in
    product xs ys
  in
  match Seq.find f xys with
  | Some (x, y) -> Some (x, y, get m x y)
  | None -> None

let fold_left (f : 'acc -> 'a -> 'acc) (acc : 'acc) (m : 'a t) : 'acc =
  let f acc row = Array.fold_left f acc row in
  Array.fold_left f acc m
