exception Cannot_sum_order

let split n l =
  let l1, l2, _ = List.fold_left (fun (l1,l2,i) x -> if i < n then (x::l1, l2, i+1) else (l1, x::l2, i+1)) ([], [], 0) l in
  List.rev l1, List.rev l2

let merge l1 l2 = 
  let rec mg acc l1 l2 =
    match l1, l2 with
      | [], [] -> acc
      | hd::tl, [] | [], hd::tl -> mg (hd::acc) tl []
      | hd1::tl1, hd2::tl2 -> 
	if hd1 < hd2 then mg (hd1::acc) tl1 l2
	else mg (hd2::acc) l1 tl2
  in List.rev (mg [] l1 l2)

let print_list l = 
  List.iter (fun x -> Printf.printf "%d; " x) l; print_endline ""

let sum_order l1 l2 =
  let rec so l1 l2 n =
    match l1, l2 with
      | [], [] -> []
      | _::_::[], [] | [], _::_::[] -> raise Cannot_sum_order
      | hd1::[], hd2::[] -> [hd1+hd2]
      | hd1::hd2::[], hd3::[] | hd3::[], hd1::hd2::[] ->
	let s1 = hd1 + hd3 and s2 = hd2 + hd3 in
	if s1 < s2 then [s1;s2] else [s2;s1]
      | _, _ ->
	let half = n/2 in
	let rest = n - half in
	let l11, l12 = split half l1 and l21, l22 = split half l2 in
	let r1 = (so l11 l21 half) and r2 = (merge (so l11 l22 half) (so l12 l21 rest)) and r3 = (so l12 l22 rest) in
	print_string "l11 = ";print_list l11;
	print_string "l21 = ";print_list l21;
	print_string "r1 = ";print_list r1;print_endline "";

	print_string "r2 = ";print_list r2;

	print_string "l12 = ";print_list l12;
	print_string "l22 = ";print_list l22;
	print_string "r3 = ";print_list r3;print_endline "";
	r1@r2@r3 
  in 
  so l1 l2 (List.length l1)


