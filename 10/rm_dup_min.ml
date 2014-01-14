let min x y = if x < y then x else y
let min_in l = List.fold_left min (List.hd l) (List.tl l)
let rec rm x = List.filter ((<>) x)

let rec rm_dup_min' = function
  | [] -> []
  | x::tl -> 
    if not (List.mem x tl) then x::(rm_dup_min' tl)
    else min (x::(rm x tl)) (rm_dup_min' tl)

