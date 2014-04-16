(*
  box_all loopless
*)

(* First box_tuple 
   x box y -> (x box y, rev (x box y))
*)
let rec box_tuple xs (ys, sy) =
  match xs with
    | [] -> ys, sy
    | x::tl -> 
      let zs, sz = box_tuple tl (sy, ys) in
      ys @ [x] @ zs, sz @ [x] @ sy




type 'a multiway_tree = Node of 'a * ('a multiway_tree list) 

let rec box_tuple' xs (ys, sy) =
  match xs with
    | [] -> ys, sy
    | x::tl -> 
      let zs, sz = box_tuple' tl (sy, ys) in
      ys @ [Node (x, zs)], sz @ [Node (x, sy)]

let rec box_tuple_all l = List.fold_right box_tuple' l ([],[]) |> fst





type 'a queue = 'a list * 'a list

let rearrange = function
  | [], [] -> [], []
  | l, [] -> [], List.rev l
  | l, r -> l, r

let empty = [], []

let ins x (l,r) = rearrange (x::l, r)

let head = function
  | _, [] -> failwith "empty queue"
  | _, x::_ -> x

let rm = function
  | _, [] -> failwith "empty queue"
  | l, _::r -> rearrange (l, r)

let is_empty (_, r) = r = []

type 'a m_tree_q = Node of 'a * 'a m_tree_q queue

let rec box_tuple_q xs (ys, sy) =
  match xs with
    | [] -> ys, sy
    | x::tl -> 
      let zs, sz = box_tuple_q tl (sy, ys) in
      ins (Node (x, zs)) ys, ins (Node (x, sy)) sz

let rec box_tuple_all_q l = [List.fold_right box_tuple_q l (empty,empty) |> fst]

let rec box_step_q = function
  | [] -> None
  | q::qs when is_empty q -> box_step_q qs
  | q::qs -> 
    let Node (x, mtq) = head q in
    Some (x, mtq::(rm q)::qs)
    
let rec unfold step_f a = 
  match step_f a with
    | None -> []
    | Some (b, a') -> b::unfold step_f a'
