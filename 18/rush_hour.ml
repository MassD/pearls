type cell = int 
type grid = cell * cell list
type vehicle = int
type move = vehicle * cell
type state = grid

let merge l1 l2 = 
  let rec merge_acc acc = function
    | [], [] -> List.rev acc
    | [], hd::tl | hd::tl, [] -> merge_acc (hd::acc) ([], tl)
    | hd1::tl1, hd2::tl2 when hd1 < hd2 -> merge_acc (hd1::acc) (tl1, hd2::tl2)
    | hd1::tl1, hd2::tl2 -> merge_acc (hd2::acc) (hd1::tl1, tl2)
  in
  merge_acc [] (l1,l2)

let rm_from l1 l2 = 
  let rec merge_rm acc = function
    | [], [] | [], _ -> List.rev acc
    | hd::tl, [] -> merge_rm (hd::acc) (tl, [])
    | hd1::tl1, (hd2::tl2 as to_be_rm) ->
      if hd1 = hd2 then merge_rm acc (tl1, to_be_rm)
      else if hd1 < hd2 then merge_rm (hd1::acc) (tl1, to_be_rm)
      else merge_rm acc (hd1::tl1, tl2)
  in 
  merge_rm [] (l1, l2)

let all_cells = 
  let rec gen acc = function
    | 0 -> acc
    | i when i mod 7 = 0 -> gen acc (i-1)
    | i -> gen (i::acc) (i-1)
  in 
  gen [] 41

let between m n step =
  let rec gen acc i =
    if i < m then acc
    else gen (i::acc) (i-step)
  in 
  gen [] n

let occupied_cells (r,f) = if f-r < 7 then between r f 1 else between r f 7

let all_occupied_cells = List.fold_left (fun acc x -> occupied_cells x |> merge acc) [] 

let free_cells g = all_occupied_cells g |> rm_from all_cells 

let g1 = [(17,18);(1,15);(2, 9);(3, 10);(4, 11);(5, 6);(12, 19);(13, 27);(24, 26);(31, 38);(33, 34);(36, 37);(40, 41)]

let adjs (r, f) = if f - r < 7 then (f+1, r-1) else (f+7, r-7)

let moves g =
  let fcs = free_cells g in
  let possible_moves = 
    List.fold_left (
      fun (i,acc) x -> 
	let pc = adjs x in (i+1, (i,fst pc)::(i, snd pc)::acc)
    ) (0, []) g
  in 
  let good_move c = if c < 1 || c > 41 || not (List.mem c fcs) then false else true in
  snd possible_moves |> List.filter (fun (_,c) -> good_move c) |> List.rev

let adjust (r,f) c =
  if f - r < 7 then if c > f then r+1, c else c, f-1
  else if c < r then c, f-7 else r+7, c

let move g (v,c) =
  let rec mv i = function
    | [] -> []
    | (r,f)::tl ->
      if i = v then (adjust (r,f) c)::tl
      else (r,f)::(mv (i+1) tl)
  in 
  mv 0 g

let solved g = (List.hd g |> snd) = 20


