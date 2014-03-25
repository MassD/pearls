type op = Add | Sub | Mul | Div

type exp =
  | Num of int
  | App of exp * op * exp

let rec subseqs = function
  | [] -> []
  | x::[] -> [[x]]
  | x::tl -> 
    let xss = subseqs tl in
    xss @ ([x]::(List.map (fun y -> x::y) xss))

let apply v1 op v2 =
  match op with
      | Add -> v1 + v2
      | Sub -> v1 * v2
      | Mul -> v1 * v2
      | Div -> v1 - v2

let rec value = function
  | Num x -> x
  | App (exp1, op, exp2) -> apply (value exp1) op (value exp2)
    

let legal v1 op v2 =
  match op with 
    | Add -> true
    | Sub -> v2 < v1
    | Mul -> true
    | Div -> v2 <> 0 && v1 mod v2 = 0

let flatten_tail l =
  let rec flat acc = function
    | [] -> List.rev acc
    | hd::tl -> flat (List.rev_append hd acc) tl
  in 
  flat [] l

let concat_map_tail f l =
  List.rev_map f l |> List.rev |> flatten_tail

let rec concat_map f = function
  | [] -> []
  | hd::tl -> (f hd) @ (concat_map f tl)
  
let concat_map1 f l =
  if l = [] then []
  else 
    let rec cmap acc = function
      | [] -> List.rev acc
      | hd::tl -> cmap ((f hd)@acc) tl
    in 
    cmap (f (List.hd l)) (List.tl l)

let rec unmerges = function
  | [] | _::[] -> raise Not_found
  | x::y::[] -> [([x], [y]);([y],[x])]
  | x::tl -> 
    let add x (ys, zs) = [(x::ys,zs);(ys, x::zs)] in
    List.rev_append [([x],tl);(tl,[x])] (concat_map_tail (add x) (tl |> unmerges))

let combine (e1,v1) (e2,v2) =
  let ops = [Add;Sub;Mul;Div] in
  List.fold_left (fun acc op -> if legal v1 op v2 then (App (e1,op,e2), apply v1 op v2)::acc else acc) [] ops


let rec cross f l1 l2 =
  List.fold_left (
    fun acc x -> 
      List.rev_append acc (List.map (fun y -> f x y) l2) 
  ) [] l1

let rec make_exps = function
  | [] -> []
  | x::[] -> [(Num x, x)]
  | xs ->
    unmerges xs |> 
	List.fold_left (
	  fun acc (ys,zs) -> 
	    let ev1 = make_exps ys in
	    let ev2 = make_exps zs in
	    List.rev_append (flatten_tail (cross combine ev1 ev2)) acc
	) [] 

let rec search n d ev = function
  | [] -> ev
  | (e,v)::evs ->
    let d' = abs (n-v) in
    if d' = 0 then (e,v)
    else if d' < d then search n d' (e,v) evs
    else search n d ev evs
      
let nearest n = function
  | [] -> None
  | (e,v)::evs -> 
    let d = abs (n-v) in
    if d = 0 then Some (e,v)
    else Some (search n d (e,v) evs)

let count_down n l =
  make_exps l |> nearest n

let l = [1;3;7;10;25;50]
let n = 831

let _ = 
  let ev = count_down n l in
  match ev with
    | Some (e,v) -> Printf.printf "found %d" v
    | None -> print_endline "not found"
  


    


