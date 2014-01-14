let max_t (x,y) = if x > y then x else y
let max x y = if x > y then x else y

let max_sum_seg l = List.fold_left (fun (c,m) x -> let n = max x (c+x) in (n, max n m)) (List.hd l,min_int) (List.tl l) |> snd


let max_sum_nn = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd1::hd2::tl ->
    List.fold_left (fun (e0, e1) x -> max_t (e0, e1), max_t (e0, 0)+x) (hd1, hd2) tl |> max_t


let fourth (_,_,_,x) = x
let max_in = List.fold_left max min_int
let h (e,s,m,n) x = (e, (max s e) + x, max m s, max n ((max m n) + x))
let start  = function [x;y;z] -> (0, max_in [x+y+z;y+z;z], max_in [x;x+y;y], x+z) | _ -> raise Not_found
let max_sum_n = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd1::hd2::[] -> max hd1 hd2
  | hd1::hd2::hd3::tl -> List.fold_left h (start [hd1;hd2;hd3]) tl |> fourth
