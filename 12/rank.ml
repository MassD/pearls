let l = [51; 38; 29; 51; 63; 38]

let split n l =
    let u,v,_ = List.fold_left (fun (u,v,i) x -> if i < n then (x::u,v,i+1) else (u,x::v,i+1)) ([],[],0) l in
    List.rev u, List.rev v

let merge l1 l2 len2 =
  let rec mg acc = function
    | [], []-> acc
    | [], (hd,r)::tl| (hd,r)::tl, [] -> mg ((hd,r)::acc) ([],tl)
    | (k1,r1)::tl1, (k2,r2)::tl2 ->
      if k1 < k2 then mg ((k1, r1)::acc) (tl1, (k2,r2+1)::tl2)
      else if k1 = k2 then mg ((k1,r1)::(k2,r2)::acc) (tl1,tl2) 
      else mg ((k2,r2)::acc) ((k1,r1+1)::tl1, tl2)
  in 
  mg [] (l1,l2) |> List.rev

let rec rank len = function
  | [] -> []
  | hd::[] -> [(hd,0)]
  | hd::tl as l -> 
    let l1,l2 = split (len/2) l in
    merge (rank (len/2) l1) (rank (len-len/2) l2)
