open Printf

type perimeter = Perimeter of int

type area = Area of int

type region = Info of (perimeter * area) | Redirect of region ref

let rec attach ~src ~dst =
  if src == dst then
    (* Tried to connect a region when is already in that region *)
    ()
  else
    match !dst with
    | Info dst_info -> (
        let Perimeter dst_p, Area dst_a = dst_info in
        match !src with
        | Info src_info ->
            (* printf "    source is info, taking data and redirecting to destination\n" ; *)
            let Perimeter src_p, Area src_a = src_info in
            dst := Info (Perimeter (src_p + dst_p), Area (src_a + dst_a)) ;
            src := Redirect dst
        | Redirect src_redirect ->
            (* printf "    source is a redirect, retrying\n" ; *)
            attach ~src:src_redirect ~dst )
    | Redirect _ -> failwith "Destination is a redirect?"

let adjacent x y = [ x - 1, y; x + 1, y; x, y - 1; x, y + 1 ]

let previous x y = [ x - 1, y; x, y - 1 ]

let solve (input : string array) : string =
  let map_plots = Mat.mat_of input in
  let init_regions at_x at_y =
    let at_plot = Mat.get map_plots at_x at_y in
    printf "map[%02d][%02d] = '%c'" at_x at_y at_plot ;
    let fold_perimeter p (adj_x, adj_y) =
      let adj_plot = Option.value ~default:'.' @@ Mat.get_opt map_plots adj_x adj_y in
      p + if adj_plot != at_plot then 1 else 0
    in
    let at_plot_info = Perimeter (List.fold_left fold_perimeter 0 @@ adjacent at_x at_y), Area 1 in
    printf " -> on its own, has perimeter %d and area %d\n"
      (let (Perimeter p) = fst at_plot_info in
       p )
      (let (Area a) = snd at_plot_info in
       a ) ;
    ref @@ Info at_plot_info
  in
  let map_regions = Mat.init (Mat.x map_plots) (Mat.y map_plots) init_regions in
  printf "\nInitialised!\n\n" ;
  let join_plots at_x at_y at_plot =
    printf "map[%02d][%02d] = '%c'\n" at_x at_y at_plot ;
    let current = Mat.get map_regions at_x at_y in
    let iter_previous (prev_x, prev_y) =
      match Mat.get_opt map_plots prev_x prev_y with
      | None -> () (* Out of bounds *)
      | Some prev_plot ->
          printf " -> checking previous map[%02d][%02d] = '%c'...\n" prev_x prev_y prev_plot ;
          let prev = Mat.get map_regions prev_x prev_y in
          if at_plot = prev_plot then
            let _ = attach ~src:prev ~dst:current in
            printf " -> attached!\n"
    in
    List.iter iter_previous @@ previous at_x at_y
  in
  Mat.iteri join_plots map_plots ;
  printf "\nCalculated!\n\n" ;
  let total = ref 0 in
  let count_info x y r =
    match !r with
    | Info reg ->
        let Perimeter p, Area a = reg in
        printf "regions[%d][%d] -> perimeter %d & area %d\n" x y p a ;
        total := !total + (p * a)
    | Redirect _ -> ()
  in
  Mat.iteri count_info map_regions ;
  Int.to_string !total
