open Printf
open Common

(* Convex 90 deg corners and concave 90 deg corners *)
type corners = int * int

type area = Area of int

type region = Info of (corners * area) | Redirect of region ref

let rec attach ~src ~dst =
  if src == dst then
    (* Tried to connect a region when is already in that region *)
    ()
  else
    match !dst with
    | Info dst_info -> (
        let (dst_convex, dst_concave), Area dst_a = dst_info in
        match !src with
        | Info src_info ->
            (* printf "    source is info, taking data and redirecting to destination\n" ; *)
            let (src_convex, src_concave), Area src_a = src_info in
            dst := Info ((dst_convex + src_convex, dst_concave + src_concave), Area (src_a + dst_a)) ;
            src := Redirect dst
        | Redirect src_redirect ->
            (* printf "    source is a redirect, retrying\n" ; *)
            attach ~src:src_redirect ~dst )
    | Redirect _ -> failwith "Destination is a redirect?"

let adjacent x y = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]

let previous x y = [ x - 1, y; x, y - 1 ]

let rec count_convex_corners ?(rotations = 4) (get : int -> int -> char) =
  if rotations <= 0 then 0
  else
    let c = get 0 0 in
    let this_rotation = if c <> get 1 0 && c <> get 0 1 then 1 else 0 in
    let newget x y = get y (-x) in
    this_rotation + count_convex_corners ~rotations:(rotations - 1) newget

let rec count_concave_corners ?(rotations = 4) (get : int -> int -> char) =
  if rotations <= 0 then 0
  else
    let c = get 0 0 in
    let this_rotation = if c = get 1 0 && c = get 0 1 && c <> get 1 1 then 1 else 0 in
    let newget x y = get y (-x) in
    this_rotation + count_concave_corners ~rotations:(rotations - 1) newget

let solve (input : string array) : string =
  let map_plots = matrix_of '.' input in
  let init_regions at_x at_y =
    let at_plot = map_plots#get at_x at_y in
    printf "map[%02d][%02d] = '%c'" at_x at_y at_plot ;
    let get x y = Option.value ~default:'.' @@ map_plots#try_get (at_x + x) (at_y + y) in
    let convex = count_convex_corners get in
    let concave = count_concave_corners get in
    printf " -> on its own, has %d convex & %d concave corners\n" convex concave ;
    ref @@ Info ((convex, concave), Area 1)
  in
  let map_regions = new matrix @@ Array.init_matrix map_plots#x map_plots#y init_regions in
  printf "\nInitialised!\n\n" ;
  let join_plots at_x at_y at_plot =
    printf "map[%02d][%02d] = '%c'\n" at_x at_y at_plot ;
    let current = map_regions#get at_x at_y in
    let iter_previous (prev_x, prev_y) =
      match map_plots#try_get prev_x prev_y with
      | None -> () (* Out of bounds *)
      | Some prev_plot ->
          printf " -> checking previous map[%02d][%02d] = '%c'...\n" prev_x prev_y prev_plot ;
          let prev = map_regions#get prev_x prev_y in
          if at_plot = prev_plot then
            let _ = attach ~src:prev ~dst:current in
            printf " -> attached!\n"
    in
    List.iter iter_previous @@ previous at_x at_y
  in
  map_plots#iteri join_plots ;
  printf "\nCalculated!\n\n" ;
  let total = ref 0 in
  let count_info x y r =
    match !r with
    | Info reg ->
        let (convex, concave), Area a = reg in
        let price = (convex + concave) * a in
        printf "regions[%d][%d] -> convex %d & concave %d, area %d -> price %d\n" x y convex concave a price ;
        total := !total + price
    | Redirect _ -> ()
  in
  map_regions#iteri count_info ;
  Int.to_string !total
