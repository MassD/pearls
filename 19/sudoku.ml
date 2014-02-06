let b = 
  [|
    [|0;0;4;0;0;5;7;0;0|];
    [|0;0;0;0;0;9;4;0;0|];
    [|3;6;0;0;0;0;0;0;8|];
    [|7;2;0;0;6;0;0;0;0|];
    [|0;0;0;4;0;2;0;0;0|];
    [|0;0;0;0;8;0;0;9;3|];
    [|4;0;0;0;0;0;0;5;6|];
    [|0;0;5;3;0;0;0;0;0|];
    [|0;0;6;1;0;0;9;0;0|];
  |]

let fixed b = 
  let rec collect i j acc =
    if i > 8 then acc
    else if j > 8 then collect (i+1) 0 acc
    else if b.(i).(j) <> 0 then collect i (j+1) ((i,j)::acc)
    else collect i (j+1) acc
  in 
  collect 0 0 []

let candidates b = 
  let a = Array.make_matrix 9 9 [] in
  for i = 0 to 8 do
    for j = 0 to 8 do
      if b.(i).(j) = 0 then a.(i).(j) <- []
      else a.(i).(j) <- [-1]
    done
  done;
  a

let digits = [1;2;3;4;5;6;7;8;9]

let rm l1 l2 = List.fold_left (fun acc x -> if List.mem x l1 then acc else x::acc) [] l2 |> List.rev

let no_dup l =
  let sl = List.sort compare l in 
  List.fold_left (fun (acc, d) x -> if x = d then (acc, d) else (x::acc, x)) ([List.hd sl], List.hd sl) (List.tl sl) |> fst

let bad_digits_row i b = Array.fold_left (fun acc x -> if x <> 0 then x::acc else acc) [] b.(i)

let bad_digits_col j b = 
  let rec col i acc =
    if i >= 8 then acc
    else if b.(i).(j) <> 0 then col (i+1) (b.(i).(j)::acc)
    else col (i+1) acc
  in 
  col 0 []

let bad_digits_mt i j b =
  let rec collect x y acc =
    if x < 0 then collect (x+1) y acc
    else if y < 0 then collect x (y+1) acc
    else if x > i || x > 8 then acc
    else if y > j || y > 8 then collect (x+1) (j-2) acc
    else collect x (y+1) (if b.(i).(j) <> 0 then b.(i).(j)::acc else acc)
  in 
  collect (i-2) (j-2) []

let bad_digits i j b = 
  (bad_digits_row i b) @ (bad_digits_col j b) @ (bad_digits_mt i j b) |> no_dup

let candi_digits i j b = rm (bad_digits i j b) digits

let sudoku b =
  let fixed_places = fixed b in
  let rec move x y =
    if x > 8 then true
    else if y > 8 then move (x+1) 0
    else if List.mem (x,y) fixed_places then move x (y+1)
    else 
      let r = candi_digits x y b |> try_move x y in
      if r then r
      else (b.(x).(y) <- 0;r)
  and try_move i j = function
    | [] -> false
    | hd::tl -> (
      b.(i).(j) <- hd;
      if move i (j+1) then true
      else try_move i j tl
    )
  in 
  move 0 0

	  


  

