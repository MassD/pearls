(*
  boustrophedon - box
  box [x;y] [a;b;c] = [[a;b;c;][x;][c;b;a;];[y];[a;b;c;]] = [a;b;c;x;c;b;a;y;a;b;c]
*)

(* this is the raw way of boxing, it is not efficient as l2 is always reversed *) 
let rec box l1 l2 =
  match l1 with
    | [] -> l2
    | hd::tl -> l2 @ [hd] @ box tl (List.rev l2)

(* this is more efficient as ys will be reversed only once *) 
let box_better xs ys =
  let rec mix xs (ys, sy) =
    match xs with
      | [] -> ys
      | x::tl -> ys @ [x] @ mix tl (sy, ys)
  in 
  mix xs (ys, List.rev ys)

let box_tail l1 l2 =
  let rec collect acc l2' = function                
    | [], [] -> List.rev acc
    | hd::tl, [] -> collect (hd::acc) [] (tl, l2')
    | l, hd::tl -> collect (hd::acc) (hd::l2') (l, tl)
  in 
  collect [] [] (l1,l2)

(* box all in a list *)

let box_all ll = List.fold_left box_better [] ll

let box_allr ll = List.fold_right box_better ll []

let rec box_allr' = function
  | [] -> []
  | hd::tl -> box_better hd (box_allr tl)


(* ***************************************************************************************************** *)*

(*
  box_all loopless
*)

type 'a multiway_tree = Node of 'a * ('a multiway_tree list) 

let rec preorder_step = function
  | [] -> None
  | []::ms -> preorder_step ms
  | (Node (hd, cms)::ms)::mss -> Some (hd, cms::ms::mss) (* this is constant time, mss is list of list of multiway_trees *)

(* x box y -> (x box y, rev (x box y)) *)
let rec box_tuple xs (ys, sy) =
  match xs with
    | [] -> ys, sy
    | x::tl -> 
      let zs, sz = box_tuple tl (sy, ys) in
      ys @ [x] @ zs, sz @ [x] @ sy

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
