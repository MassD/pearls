let by_r1 (_, (r1,_)) = r1
let by_r2 (_, (_,r2)) = r2

let csort f n l =
  let a = Array.make n [] in
  List.iter (fun x -> let i = f x in a.(i) <- x::a.(i)) l;
  Array.fold_right (fun x acc -> List.rev_append x acc) a []

let lsdradix_sort n l =
  csort by_r2 n l |> csort by_r1 n

let update_ranks idxf rf a sorted_li =
  List.fold_left (
	fun (a, (m, r, c)) x -> 
	  if rf x = rf m then (a.(idxf x) <- r; (a, (m, r, c+1)))
	  else (a.(idxf x) <- c; (a, (x, c, c+1)))
      ) (a, (List.hd sorted_li, 0, 0)) sorted_li |> fst

let ranks l n =
  let a = Array.make n 0 in
  let li = List.fold_left (fun (acc,i) x -> (x, i)::acc, i+1) ([], 0) l |> fst in
  List.sort compare li |> update_ranks snd fst a

let install_2nd_rank a k =
  let n = Array.length a in
  let rec install i acc =
    if i >= n then List.rev acc
    else if i+k/2 >= n then install (i+1) ((i,(a.(i),0))::acc)
    else install (i+1) ((i,(a.(i),a.(i+k/2)))::acc)
  in 
  install 0 []

let rank_suffix l =
  let n = List.length l in
  let rs = ranks l n in
  let rec rank_column k a =
    if k > n then a
    else rank_column (2*k) (install_2nd_rank rs k |> lsdradix_sort n |> update_ranks fst snd rs)
  in 
  rank_column 2 rs

let l = ['b';'a';'n';'a';'n';'a']
let l1 = [51;38;29;51;63;38]

exception Wrong_ranks_tails

let tails l =
  let rec tail acc = function
    | [] -> List.rev acc
    | _::tl as l -> tail (l::acc) tl
  in 
  tail [] l

let sorted_suffix rs l =
  if Array.length rs <> List.length l then raise Wrong_ranks_tails
  else 
    let tls = tails l in
    let target = Array.of_list tls in
    let rec arrange i = function
      | [] -> Array.to_list target
      | hd::tl -> (target.(rs.(i)) <- hd;arrange (i+1) tl)
    in 
    arrange 0 tls


  





