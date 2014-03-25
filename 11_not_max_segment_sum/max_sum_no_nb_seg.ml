let max (x,y) = if x > y then x else y

let msnns = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd1::hd2::tl ->
    List.fold_left (fun (e0, e1) x -> max (e0, e1), max (e0, 0)+x) (hd1, hd2) tl |> max
