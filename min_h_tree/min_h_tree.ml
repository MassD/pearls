type tree = Leaf of int | Fork of tree * tree

let max x y = if x < y then y else x
let concat_map f l = l |> List.fold_left (fun acc x -> List.rev_append (f x) acc) [] |> List.rev

let rec cost = function
  | Leaf v -> v
  | Fork (l, r) -> 1 + (max (cost l) (cost r)) 

let fork_left ll r = List.rev_map (fun x -> Fork (x,r)) ll |> List.rev
let rec prefix x = function
  | Leaf v as t-> [Fork (Leaf x, t)]
  | Fork (l, r) as t -> (Fork (Leaf x, t)) :: (fork_left (prefix x l) r)

let trees l = List.fold_right (
  fun x -> function 
    | [] -> [Leaf x]
    | acc -> concat_map (prefix x) acc
) l []

let _ =
  trees [1;2;3;4;5;6;7;8;9;10;1;2;3;4;5;6;7;8;9;10] |> List.length
