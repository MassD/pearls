let by_r1 (r1,_,_) = r1
let by_r2 (_,r2,_) = r2

let csort f n l =
  let a = Array.make n [] in
  List.iter (fun x -> let i = f x in a.(i) <- x::a.(i)) l;
  Array.fold_right (fun x acc -> List.rev_append x acc) a []

let lsdradix_sort l n =
  csort by_r2 n l |> csort by_r1 n

let ranks l n =
  let a = Array.make n 0 in
  let li = List.fold_left (fun (acc,i) x -> (x,i)::acc, i+1) ([], 0) l |> fst in
  List.sort compare li |>
      List.fold_left (
	fun (a, (m, r, c)) x -> 
	  if fst x = fst m then (a.(snd x) <- r; (a, (m, r, c+1)))
	  else (a.(snd x) <- c; (a, (x, c+1, c+1)))
      ) (a, (List.hd li, 0, 0)) |> fst

    
