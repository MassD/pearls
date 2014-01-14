let l = ['c';'a';'l';'c';'u';'l';'u';'s']
let min x y = if x < y then x else y
let min_in l = List.fold_left min (List.hd l) (List.tl l)
let rec rm x = List.filter ((<>) x)
let rec rm_all xl = List.filter (fun x -> not (List.mem x xl)) 

(* rdm = rm_dup_min *)

let rec rdm_0 = function
  | [] -> []
  | x::tl -> 
    if not (List.mem x tl) then x::(rdm_0 tl)
    else min (x::(rm x tl)) (rdm_0 tl)

(* only if List.mem x tl = true && List.mem y tl = true *)

let rec rdm_1 = function
  | [] -> []
  | x::[] -> x::[]
  | x::y::tl ->
    let rest = rdm_1 tl in
    if x < y then 
      let a = x::y::(rm_all [x;y] tl |> rdm_1) in
      let b = x::(rm x tl |> rdm_1) in
      if rest = [] then	min_in [a;b]
      else min_in [a;b;rest]
    else 
      let a = y::(rm y tl |> rdm_1) in
      if rest = [] then a 
      else min_in [a;rest]

let inits l = List.fold_left (fun (init, acc) x -> x::init, (List.rev (x::init))::acc) ([],[]) l |> snd |> List.rev

let min_in' l = List.fold_left (fun m x -> if x = [] || (x > m && m <> []) then m else x) (List.hd l) (List.tl l)

let rec hub ws xs =
  match ws, xs with
    | _, [] -> []
    | [], _ -> rdm_2 xs
    | _ -> List.fold_left (fun acc x -> (x@(rm_all x xs |> rdm_2))::acc) [] (inits ws) |> min_in'
and 
    rdm_2 = function
      | [] -> []
      | x::[] -> x::[]
      | x::y::xs -> if x < y then hub [x;y] xs else hub [y] xs


	
