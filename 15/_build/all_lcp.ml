let prepare l n =
  let rec prep a i = function
    | [] | _::[] -> a
    | x1::x2::tl ->
      if x1 = x2 then a.(i).(i+1) <- 1 + (if i = 0 then 0 else a.(i-1).(i))
      else a.(i).(i+1) <- 0;
      prep a (i+1) (x2::tl)
  in 
  prep (Array.make_matrix n n (-1)) 0 l

let update a i j cp = a.(i).(j) <- cp

let rec lcp a b i j =
  if i < 0 || j < 0 then 0
  else if a.(i).(j) <> -1 then a.(i).(j)
  else 
    let i_i1, i1_j = lcp a b i (i+1), lcp a b (i+1) j in
    if i_i1 <> i1_j then
      let m = min i_i1 i1_j in
      update a i j m;
      m
    else 
      let m = i_i1 in
      let cp =
	if m = 0 then
	  if b.(i) <> b.(j) then 0
	  else 1 + (lcp a b (i-1) (j-1))
	else m + lcp a b (i-m) (j-m) 
      in
      update a i j cp;
      cp
  
let rec find_all acc a b i x =
  if i < 0 then acc
  else 
    let lcp_i_x = lcp a b i x in
    find_all (lcp_i_x::acc) a b (i-1) x

let all_lcp l = 
  let n = List.length l in
  let a = prepare l n in
  let b = List.rev l |> Array.of_list in
  n::(find_all [] a b (n-2) (n-1) |> List.rev)

let all_lcp' a l =
  let n = List.length l in
  let b = List.rev l |> Array.of_list in
  n::(find_all [] a b (n-2) (n-1) |> List.rev)

let rec count_step c n a i j =
  if j < 0 then count_step c n a (i-1) (n-1)
  else if i < 0 then c
  else if a.(i).(j) >= 0 then count_step (c+1) n a i (j-1)
  else count_step c n a i (j-1)

let l = ['a'; 'b'; 'a'; 'c'; 'a'; 'b'; 'a'; 'c'; 'a'; 'b']

let all_lcp_bru l = 
  let rec lcp acc l1 l2 =
    match l1, l2 with
      | [], _ | _, [] -> acc
      | hd1::tl1, hd2::tl2 ->
	if hd1 = hd2 then lcp (1+acc) tl1 tl2
	else acc
  in 
  let rec tails acc = function
    | [] -> List.rev acc
    | _::tl as l ->
      tails (l::acc) tl
  in 
  tails [] l |> List.map (lcp 0 l)

let ran_list n = 
  Random.self_init();
  let rec gen acc i =
    if i = 0 then acc
    else gen ((Random.int 1)::acc) (i-1)
  in 
  gen [] n

let _ =
  let n = 10000 in
  let l = ran_list n in
  let t1 = Sys.time () in
  let a = prepare l n in
  let t1' = Sys.time () in
  let al1 = all_lcp' a l in
  let t2 = Sys.time () in
  let al2 = all_lcp_bru l in
  let t3 = Sys.time () in
  Printf.printf "prepare array costs %f s\nall_lcp' costs %f s\nall_lcp_bru costs %f s\ncorrect: %s\n" (t2-.t1) (t2-.t1') (t3-.t2) (if al1 = al2 then "true" else "false")
