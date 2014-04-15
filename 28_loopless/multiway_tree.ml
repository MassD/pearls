(* 
   multiway_tree preorder traversal & loopless
   preorder is that always the root goes first
*)

type 'a multiway_tree = Node of 'a * ('a multiway_tree list) 

let mt1 = Node (1, [Node (2, [Node (3,[]); Node (4,[])]);Node (5,[])])

let rec preorder_root (Node (x, mtl)) = x::(List.map (fun mt -> preorder_root mt) mtl |> List.flatten)

(* On list of multiway_trees, so called forest *)
let rec preorder_mt_list = function
  | [] -> []
  | (Node (hd, cms))::ms -> hd::(preorder_mt_list (cms@ms)) (* crs is child tree list, ms is list of multiway_trees *)


(* the following is not a good `step_f` as it is not O(1) at ft@rs *)
let preorder_step_bad = function
  | [] -> None
  | Node (hd, cms)::ms -> Some (hd,cms@ms)

(* 
   Since the above preorder involves list concate operation, we need to transform it to :: operation
   1. We can change the parameter to be list of list of multiway_trees
   2. So each time we get the Node, we can directly :: its child multiway_tree list to the list of list
*)
let rec preorder_step = function
  | [] -> None
  | []::ms -> preorder_step ms
  | (Node (hd, cms)::ms)::mss -> Some (hd, cms::ms::mss) (* this is constant time, mss is list of list of multiway_trees *)
