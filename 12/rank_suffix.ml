let partition p =
  List.fold_left (
    fun (u, (v, vlen), w) e ->
      let x = fst e |> List.hd in
      if x < p then e::u, (v, vlen), w
      else if x = p then u, (e::v, vlen+1), w
      else u, (v, vlen), e::w
  ) ([], ([], 0), [])

let rec psort = function
  | [] -> []
  | hd::[] -> [([hd], 1)]
  | hd::_ as l ->
    let u, vt, w = partition (fst hd |> List.hd) l in
    (psort u) @ [vt] @ (psort w)

let tails l =
  let rec tls (acc, i) = function
    | [] -> acc
    | hd::tl -> tls ((hd::tl, i)::acc, i-1) tl
  in 
  tls ([], List.length l) l

let l = [2;5;5;2;3;4;1;5;2;4;7;9]

let len_r (u,r) = List.map (fun (x,len) -> len, r) u

let rank l =
  let d, m, _ = List.fold_left (
    fun (d, m, r) (u, len) ->
      if len = 1 then (len_r (u,r) |> List.hd)::d, m, r+1
      else d, List.rev_append (len_r (u,r)) m, r+len
  ) ([], [], 1) l in
  d, m

let ts = tails l

let distinct, mul = psort ts |> rank 

let a = Array.make ((List.length l)+1) (-1)

let rank_to_a = a.(0) <- 0; List.iter (fun (x, r) -> a.(x) <- r) distinct

let sorted_len_mul = List.sort compare mul

let b = Array.make ((List.length l)+1) ((0,0))

exception Cannot_rank_mul

let rank_mul =
  List.map (
    fun (len, r) ->
      let i = len-1 in
      if a.(i) <> -1 then b.(len) <- r, a.(i)
      else if b.(i) <> (0,0) then (
	if r = fst b.(i) then b.(len) <- r, snd b.(i)
	else b.(len) <- r, snd b.(i)
      )
      else raise Cannot_rank_mul;
      len, (r, snd b.(len))
	
  ) sorted_len_mul

let sorted_mul = 
  List.sort (fun x y -> compare (snd x) (snd y)) rank_mul

let combine a =
  List.map (fun (x,len) -> (x,a.(len)))

let rank_suffixes l = 
  if l = [] then []
  else 
    let rec rs r_acc i = function
      | [] -> ()
      | (len, (r,_))::tl -> 
	if r = r_acc then (a.(len) <- r+i; rs r (i+1) tl)
	else (a.(len) <- r; rs r 0 tl)
    in 
    let _, (r,_) = List.hd l in
    rs r 0 l;
    combine a ts

let rank_l = rank_suffixes sorted_mul

let all_sorted = List.sort (fun x y -> compare (snd x) (snd y)) rank_l |> List.map fst 
    
    


