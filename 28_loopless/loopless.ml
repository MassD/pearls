(*
  Chapter 28, loopless functions

  It is related to lazy.
*)

(*
  It is reversed way of fold. 
  
  Fold takes a f ('a -> 'b -> 'a) processes all elements in a list ('b list) one by one and finally give an acc ('a) as result.
  
  Unfold takes a f_step ('a -> ('b, 'a) option). This function takes an 'a, get a 'b out of it together with a leftover 'a.
  Then unfold will concatenate all 'b to a list, until the f_step gives a None.
  
  Two important facts of unfold are:
  1. The preparation of initial a (prolog) should be linear at most.
  2. The `f_step` need to be constant time O(1)
*)
let rec unfold f_step a_prolog = 
  match f_step a with
    | None -> []
    | Some (b,a') -> b::unfold f_step a' (* notice here, it is perfect for lazy, as b is the current output and f a' is next force *)

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

(* loopless preorder traversal for binary search tree *)
type 'a bst = Empty | Node of 'a bst * 'a * 'a bst

let rec preorder = function
  | Empty -> []
  | Node (l, x, r) -> x::preorder l @ preorder r

let prepare t = [t]

let inorder_step_f = function
  | [] -> None
  | t::tl -> 
    match t with
      | Empty -> failwith "wrong"
      | Node (Empty, x, Empty) -> Some (x, tl)
      | Node (Empty, x, c) | Node (c, x, Empty) -> Some (x, c::tl)
      | Node (l, x, r) -> Some (x, l::r::tl)


(* Mapping between loopless and lazy *)
