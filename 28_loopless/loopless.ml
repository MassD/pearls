(*
  Chapter 28, loopless functions

  It is related to lazy.
*)

(*
  It is reversed way of fold. 
  
  Fold takes a f ('a -> 'b -> 'a) processes all elements in a list ('b list) one by one and finally give an acc ('a) as result.
  
  Unfold takes a f_step ('a -> ('b, 'a) option). This function takes an 'a, get a 'b out of it together with a leftover 'a.
  Then unfold will concatenate all 'b to a list, until the f_step gives a None.
  
  An important fact of unfold is that the `f_step` need to be constant time O(1)
*)
let rec unfold f a = 
  match f a with
    | None -> []
    | Some (b,a') -> b::unfold f a' (* notice here, it is perfect for lazy, as b is the current output and f a' is next force *)

(* this is an example of unfold: uncons 
   The final result is actually the identical list. 
*)
let uncons_step = function
  | [] -> None
  | hd::tl -> Some (hd, tl)

(* concat a list of list *)
let rec concat_step = function
  | [] -> None
  | []::tll -> concat_step tll
  | (hd::tl)::tll -> Some (hd, tl::tll)

(* forest & preorder traversal *)
type 'a rose_tree = Node of 'a * ('a rose_tree list) 


let rec preorder_normal = function
  | [] -> []
  | (Node (hd, crs))::rs -> hd::(preorder_normal (crs@rs)) (* crs is child roses, rs is roses *)

(* the following is not a good `step_f` as it is not O(1) at ft@rs *)
let preorder_step_bad = function
  | [] -> None
  | Node (hd, crs)::rs -> Some (hd,crs@rs)

(* 
   Here need your attention.
   Since the above preorder involves list concate operation, we need to transform it to :: operation
   1. We can change the parameter to be forest list, instead of forest
   2. So each time we get the Node, we can directly :: it forest child to the forest list
*)
let rec preorder_step = function
  | [] -> None
  | []::rs -> preorder_step rs
  | (Node (hd, crs)::rs)::rss -> Some (hd, crs::rs::rss) (* this is constant time, rss is list of rs (roses) *)

(*
  boustrophedon - box
  box [x;y] [a;b;c] = [[a;b;c;][x;][c;b;a;];[y];[a;b;c;]] = [a;b;c;x;c;b;a;y;a;b;c]
*)
let rec box l1 l2 =
  match l1 with
    | [] -> l2
    | hd::tl -> l2 @ [hd] @ box tl (List.rev l2)

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

let box_all ll = List.fold_left box_better [] ll

let box_allr ll = List.fold_right box_better ll []

let rec box_allr' = function
  | [] -> []
  | hd::tl -> box_better hd (box_allr tl)

(*
  box_all_step
*)



(* x box y -> (x box y, rev (x box y)) *)
let box_tuple xs ys = 
  let rec op1 xs (ys, sy) =
  match xs with
    | [] -> ys, sy
    | x::tl -> 
      let zs, sz = op1 tl (sy, ys) in
      ys @ [x] @ zs, sz @ [x] @ sy
  in 
  op1 xs (ys, List.rev ys)
