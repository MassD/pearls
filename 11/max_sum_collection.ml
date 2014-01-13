let max (x,y) = if x > y then x else y

let max_sum_seg l = List.fold_left (fun (c,m) x -> let n = max x (c+x) in (n, max n m)) (List.hd l,min_int) (List.tl l) |> snd

let max_sumsnns = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd1::hd2::tl ->
    List.fold_left (fun (e0, e1) x -> max (e0, e1), max (e0, 0)+x) (hd1, hd2) tl |> max



