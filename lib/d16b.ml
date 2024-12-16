open Printf

let turn_a (x, y) = -y, x

let turn_b (x, y) = y, -x

let solve input =
  let m = Mat.mat_of input in
  let m = Mat.mapi (fun _ _ c -> if c = '#' then ' ' else c) m in
  let costs = Mat.make (Mat.x m) (Mat.y m) [] in
  let cost_get (x, y, dx, dy) = List.assoc_opt (dx, dy) @@ Mat.get costs x y in
  let cost_set (x, y, dx, dy) cost =
    Mat.set costs x y
    @@ (((dx, dy), cost) :: (List.filter (fun ((odx, ody), _) -> (dx, dy) <> (odx, ody)) @@ Mat.get costs x y))
  in
  let rec visit (x, y) (dx, dy) cost =
    let state = x, y, dx, dy in
    match Mat.get m x y with
    | '.' | 'E' | 'S' ->
        let cost_known = Option.value ~default:Int.max_int @@ cost_get state in
        if cost < cost_known then begin
          cost_set state cost ;
          visit (x + dx, y + dy) (dx, dy) (cost + 1) ;
          visit (x, y) (turn_a (dx, dy)) (cost + 1000) ;
          visit (x, y) (turn_b (dx, dy)) (cost + 1000)
        end
    | ' ' -> ()
    | _ as c -> failwith @@ sprintf "Found '%c'?" c
  in
  let sx, sy, _ = Option.get @@ Mat.findi (fun _ _ c -> c = 'S') m in
  visit (sx, sy) (0, 1) 0 ;
  let rec revisit (x, y) (dx, dy) cost =
    let state = x, y, dx, dy in
    match cost_get state with
    | Some state_cost ->
        if state_cost = cost then begin
          Mat.set m x y 'O' ;
          revisit (x - dx, y - dy) (dx, dy) (state_cost - 1) ;
          revisit (x, y) (turn_b (dx, dy)) (state_cost - 1000) ;
          revisit (x, y) (turn_a (dx, dy)) (state_cost - 1000)
        end
    | None -> ()
  in
  let ex, ey, _ = Option.get @@ Mat.findi (fun _ _ c -> c = 'E') m in
  let e_costs =
    List.map (fun x -> x, Option.get @@ cost_get x) [ ex, ey, 0, 1; ex, ey, 1, 0; ex, ey, 0, -1; ex, ey, -1, 0 ]
  in
  let (ex, ey, edx, edy), ecost =
    List.fold_left
      (fun (cur_state, cur_cost) (nxt_state, nxt_cost) ->
        if cur_cost < nxt_cost then cur_state, cur_cost else nxt_state, nxt_cost )
      (List.hd e_costs) (List.tl e_costs)
  in
  revisit (ex, ey) (edx, edy) ecost ;
  let ms = Mat.to_string m in
  printf "%s\n" ms ;
  let re_o = Str.regexp @@ Str.quote "O" in
  Int.to_string @@ Common.count re_o ms
