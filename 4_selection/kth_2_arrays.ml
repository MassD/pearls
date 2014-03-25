let rm_dup n l =
  let a = Array.make (4*n) 0 in
  List.fold_left (fun acc x -> if a.(x) = 0 then (a.(x) <- 1;x::acc) else acc) [] l

let ran_sorted_list n =
  Random.self_init();
  let rec ran i acc =
    if i = n then acc
    else ran (i+1) ((Random.int (4*n))::acc)
  in
  List.sort compare (ran 0 [])

let print_list l = List.iter (fun x -> Printf.printf "%d; " x) l; print_endline ""
let print_array a = Array.iter (fun x -> Printf.printf "%d; " x) a; print_endline ""

let kth_2_list k l1 l2 =
  let rec kth i acc l1 l2 = 
    if i = k+1 then acc
    else (
      match l1, l2 with
	| [], [] -> None
	| hd::tl, [] | [], hd::tl -> kth (i+1) (Some hd) tl []
	| hd1::tl1, hd2::tl2 when hd1 > hd2 -> kth (i+1) (Some hd2) l1 tl2
	| hd1::tl1, hd2::tl2  -> kth (i+1) (Some hd1) tl1 l2
    )
  in 
  kth 0 None l1 l2

let print_array_ij i j a = 
  Array.iteri (fun k x -> if k >= i && k <= j then Printf.printf "%d; " x else ()) a; print_endline ""

let kth_2_arrays k a b = 
  let sa = Array.length a and sb = Array.length b in 
  if k >= sa+sb then raise Not_found
  else 
    let rec kth k (la, ha) (lb, hb) =
      if la >= ha then b.(k+lb)
      else if lb >= hb then a.(k+la)
      else  
	let ma = (ha+la)/2 and mb = (hb+lb)/2 in
	match a.(ma) < b.(mb), k <= ma+mb-la-lb with
	  | true, true -> kth k (la, ha) (lb, mb)
	  | true, false -> kth (k-ma-1+la) (ma+1, ha) (lb, hb)
	  | false, true -> kth k (la, ma) (lb, hb)
	  | false, false -> kth (k-mb-1+lb) (la, ha) (mb+1, hb)
    in 
    kth k (0, sa) (0, sb)

let _ = 
  Random.self_init();
  let rec ast n = 
    let c = 12 in
    let n1 = 5 + Random.int c and n2 = 5 + Random.int c in 
    let l1 = ran_sorted_list n1 and l2 = ran_sorted_list n2 in
    (*let l3, l4 = [3; 11; 13; 14; 18], [2; 14; 15; 20; 25; 26; 31] in*)
    let a1 = Array.of_list l1 and a2 = Array.of_list l2 in
    let k1 = Random.int ((Array.length a1)+(Array.length a2)) in 
    (*let k2 = 11 in*)
    let k = k1 in
    let kl = 
      match kth_2_list k l1 l2 with
	| None -> raise Not_found
	| Some v -> v
    in 
    let ka = kth_2_arrays k a1 a2 in
    if ka <> kl then (
      print_list l1;
      print_list l2;
      Printf.printf "k = %d, kl = %d, ka = %d\n" k kl ka
    )
    else if n = 0 then ()
    else ast (n-1)
  in 
  ast 10000
