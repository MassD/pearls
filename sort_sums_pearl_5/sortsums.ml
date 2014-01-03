let negate = List.map (fun x -> -x)

let sub l (acc,(i,j)) x =
    fst (List.fold_left (fun (acc, j) y -> (((x-y),i,j)::acc, j+1)) (acc, 0) l), (i+1, 0)

let subs l1 l2 =
  fst (List.fold_left (sub l2) ([], (0, 0)) l1)

let tag k (x, (i, j)) = x, (k, i, j)


