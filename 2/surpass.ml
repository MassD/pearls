let print_list l = List.iter (fun x -> Printf.printf "%c; " x) l; print_endline ""

let print_xc_list l = List.iter (fun (x,c) -> Printf.printf "('%c', %d); " x c) l; print_endline ""

let rec merge acc l1 l2 len2 = 
  match l1, l2 with
  | [], [] -> List.rev acc
  | l, [] | [], l -> List.rev (List.rev_append l acc)
  | (x,cx)::tl1, (y,cy)::tl2 -> 
    if x < y then merge ((x,cx+len2)::acc) tl1 l2 len2
    else merge ((y,cy)::acc) l1 tl2 (len2-1)
    

let split l =
  let n = List.length l in
  let  mid = n/2 in
  let l1, l2, _ =
    List.fold_left (fun (l1, l2, i) x -> if i < mid then (x::l1, l2, i+1) else (l1, x::l2, i+1)) ([], [], 0) l
  in 
  (List.rev l1, List.rev l2, n-mid)

let rec surpass = function
  | [] -> []
  | hd::[] -> [(hd, 0)]
  | l -> 
    let l1, l2, len2 = split l in
    merge [] (surpass l1) (surpass l2) len2

let max l = 
  List.fold_left (fun (m, max) (x, c) -> if max < c then (x, c) else (m, max)) (List.hd l) l

let ran_char_list n = 
  Random.self_init();
  let rec ran acc i =
    if i = n then acc
    else ran ((char_of_int (97+(Random.int 26)))::acc) (i+1)
  in 
  ran [] 0



let _ = 
  (*let l = ran_char_list 10 in*)
  let l = ['w'; 'p'; 'g'; 'c'; 'e'; 'p'; 'u'; 'f'; 'g'; 'j'] in
  print_list l;
  print_xc_list (surpass l);
  let m, max = max (surpass l) in
  Printf.printf "('%c', %d)" m max
