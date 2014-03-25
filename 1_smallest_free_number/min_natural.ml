let print_list l = List.iter (fun x -> Printf.printf "%d; " x) l; print_endline ""

let partition b =
  List.fold_left (
    fun ((us, lus), (vs, lvs)) x -> 
      if x < b then ((x::us, lus+1), (vs, lvs))
      else ((us, lus), (x::vs, lvs+1))
  ) (([], 0), ([], 0))

let min_natural_from a l = 
  let n = List.length l in
  let rec min_from a b = function
  | [] -> a
  | l ->
    let (us, lus), (vs, lvs) = partition b l in
    if lus = b-a then min_from b (b+lvs/2+1) vs
    else min_from a (a+lus/2+1) us
  in 
  min_from a (a+n/2+1) l

let rm_dup_fast range =
  let a = Array.make range 0 in
  List.fold_left (fun acc x -> if a.(x) = 0 then (a.(x) <- 1; x::acc) else acc) [] 

let ran_list n =
  Random.self_init ();
  let range = int_of_float (1.5*.(float_of_int n)) in
  let rec ran_l i acc = 
    if i = 0 then acc
    else ran_l (i-1) ((Random.int range)::acc)
  in
  rm_dup_fast range (ran_l n [])

let rm_dup l =
  let rec rm acc = function
    | [] -> acc
    | hd::tl -> rm (hd::acc) (List.filter (fun x -> if x = hd then false else true) tl)
  in 
  List.rev (rm [] l)



let _ =
  let l = ran_list 10000000 in
  (*let l = [5;0;2;13;9;11;8] in*)
  (*print_list l;*)
  print_endline "list is prepared";
  let start = Sys.time() in
  Printf.printf "min = %d, cost time %f s\n" (min_natural_from 0 l) ((Sys.time ())-.start)
