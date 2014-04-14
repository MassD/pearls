(* 
   multiway_tree preorder traversal & loopless
*)

type 'a multiway_tree = Node of 'a * ('a multiway_tree list) 

let rec preorder_normal = function
  | [] -> []
  | (Node (hd, cms))::ms -> hd::(preorder_normal (cms@ms)) (* crs is child tree list, ms is list of multiway_trees *)

(* the following is not a good `step_f` as it is not O(1) at ft@rs *)
let preorder_step_bad = function
  | [] -> None
  | Node (hd, cms)::ms -> Some (hd,cms@ms)

(* 
   Here need your attention.
   Since the above preorder involves list concate operation, we need to transform it to :: operation
   1. We can change the parameter to be forest list, instead of forest
   2. So each time we get the Node, we can directly :: it forest child to the forest list
*)
let rec preorder_step = function
  | [] -> None
  | []::ms -> preorder_step ms
  | (Node (hd, cms)::ms)::mss -> Some (hd, cms::ms::mss) (* this is constant time, mss is list of list of multiway_trees *)
