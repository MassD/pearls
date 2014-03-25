let l = [51; 38; 29; 51; 63; 38]

let rank l =
  if l = [] then []
  else 
    let li = List.fold_left (fun (acc,i) x -> ((x,i)::acc, i+1)) ([],0) l |> fst in
    let sl = List.sort compare li in
    let rec get_rank acc r eq = function
      | _::[] | [] -> acc
      | (hd1,_)::(hd2,j)::tl -> 
	if hd2 > hd1 then get_rank (((hd2,r+1),j)::acc) (r+1) 0 ((hd2,j)::tl)
	else get_rank (((hd2,r-eq),j)::acc) (r+1) (eq+1) ((hd2,j)::tl)
    in 
    let rl = let hd,i = List.hd sl in get_rank [((hd,0), i)] 0 0 sl in
    let ril = List.sort (fun x y -> compare (snd x) (snd y)) rl in
    List.map fst ril
