type tree = Leaf of int | Fork of tree * tree

let max x y = if x < y then y else x
let concat_map f l = List.fold_left (fun acc x -> List.rev_append (f x) acc) [] l |> List.rev

let rec cost = function
  | Leaf v -> v
  | Fork (l, r) -> 1 + (max (cost l) (cost r)) 

let rec prefix x = function
  | Leaf v as t-> [Fork (Leaf x, t)]
  | Fork (l, r) as t -> 
    (Fork (Leaf x, t)) :: (prefix x l |> List.rev_map (fun l -> Fork (l,r)) |> List.rev)

let trees l = List.fold_right (
  fun x -> function 
    | [] -> [Leaf x]
    | acc -> concat_map (prefix x) acc
) l []

let _ =
  trees [1;2;3;4;5;6] |> List.length


(**

forest way

*)
exception Cannot_fold1

let fold_left_1 f = function
  | [] -> raise Cannot_foldl1
  | hd::tl -> List.fold_left f hd tl

let rollup = fold_left_1 (fun acc x -> Fork acc x) 


