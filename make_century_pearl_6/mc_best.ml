type num = int
type factor = num list
type term = factor list
type exp = term list

let eval_factor = List.fold_left (fun a x -> x + a*10) 0
let eval_term t = List.fold_left ( * ) 1 (List.map eval_factor t)
let eval_exp e = List.fold_left (+) 0 (List.map eval_term e)

let print_factor fa = Printf.printf "%d" (eval_factor fa)

let rec is_sorted = function
  | [] -> true
  | hd::[] -> true
  | hd1::hd2::tl -> if hd1 <= hd2 then is_sorted (hd2::tl) else false

let extract l = 
  let rec extr acc = function
    | [] -> List.rev acc
    | hd::tl -> extr (List.rev_append hd acc) tl
  in 
  extr [] l

let exp_to_raw exp = extract (extract exp)

let is_exp_sorted exp = is_sorted (exp_to_raw exp)

let rev_concat_map f = List.fold_left (fun acc x -> (List.rev_append (f x) acc)) []

(**

Below is the regular implementation, generate all possible formulas 

*)

let insert x part = 
  let rec ins acc prefix = function
    | [] -> acc
    | hd::tl -> ins ((List.rev_append prefix ((x::hd)::tl))::acc) (hd::prefix) tl
  in 
  ins ([[x]::part]) [] part

let rec partitions = function
  | [] -> [[]]
  | hd::tl -> rev_concat_map (insert hd) (partitions tl)

let expressions l = List.filter is_exp_sorted (rev_concat_map partitions (partitions l))

let mc_regular c l = List.filter (fun x -> x = c) (List.rev_map eval_exp (expressions l))

(**
   Below is the better implementation
*)

let glue x = function
  | (hd::tl1)::tl2 -> 
    [
      [[x]]::(hd::tl1)::tl2; 
      ([x]::hd::tl1)::tl2; 
      ((x::hd)::tl1)::tl2
    ]
  | _ -> [[[[x]]]]

let extend x = function
  | [] -> [[[[x]]]]
  | l -> rev_concat_map (glue x) l

let exps l = List.fold_right (fun x acc -> extend x acc) l []

let mc_better c l = List.filter (fun x -> x = c) (List.rev_map eval_exp (exps l))

(**
   Below is the best implementation
*)

let eval (_, f, t, e) = f*t+e
let ok_filter c (_, e) = if eval e <= c then true else false
let good_filter c (_, e) = if eval e = c then true else false


let glue' x = function
  | (hd::tl1)::tl2, (k, f, t, e) -> 
    [
      [[x]]::(hd::tl1)::tl2 (* x + (hd*tl1) + tl2 *), (10, x, 1, f*t+e); 
      ([x]::hd::tl1)::tl2 (* x*hd*tl1 + tl2 *), (10, x, f*t, e); 
      ((x::hd)::tl1)::tl2 (* (x*10+hd) * tl1 + tl2 *), (k*10, x*k+f, t, e)
    ]
  | _ -> [[[[x]]], (10, x, 1, 0)]

let extend' c x = function
  | [] -> [[[[x]]], (10, x, 1, 0)]
  | l -> rev_concat_map (fun y -> y |> glue' x |> List.filter (ok_filter c)) l

let exps' c l = List.fold_right (fun x acc -> extend' c x acc) l []

let mc_best c l = List.filter (good_filter c)  (exps' c l)


(**

   Compare

*)

let _ = 
  let l = [1;2;3;4;5;6;7;8;9;4;3;2;5;7;9;8] in
  let t1 = Sys.time() in
  let r1 = mc_better 1000 l in
  let t2 = Sys.time() in
  let r2 = mc_best 1000 l in
  let t3 = Sys.time() in
  Printf.printf "better: %f ms\nbest: %f ms\n" (1000.*.(t2 -. t1)) (1000.*.(t3 -. t2))
