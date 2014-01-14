let max_t (x,y) = if x > y then x else y
let max x y = if x > y then x else y

(**
   There are several keys:
   
   1. If we try to add x, after addition, the sum is even smaller than x, then we should dump the previous sum and take x only.
   
   2. Depending on the rules, we can build a number of limited states and there is a final state which is the answer.
   3. Everytime an x joins, considering x or not considering x may needs to changes to current possible states. We accumulate all possible states and work out what the final state is.
   4. The whole process is to assume an initial state (complete) and then keeping adding x and change all possible states and finally arrive at the answer.

   http://stackoverflow.com/questions/21033548/maximum-non-segment-sum/21096586
*)

(**
   For a seg (continuous), we basically will have two possible states:

   1. Currently adding sum state (c)
   2. Prevously max stopped-sum state (m)

   We would like to add x to c. 
   If c > 0, then c will contribute to the future sum, so we add and previous m stays the same.
   if c < 0, then c won't contribute, and we have a new stopped-sum state. And we need to compare it will the prevoius m.
*)
let max_sum_seg' l = List.fold_left (fun (c,m) x -> if c > 0 then (c+x, m) else (x, max c m)) (List.hd l,min_int) (List.tl l) |> max_t

(**
   The idea of this implementation is the same. but neater. And we always try to update m.
*)
let max_sum_seg' l = List.fold_left (fun (c,m) x -> let n = max x (c+x) in (n, max n m)) (List.hd l,min_int) (List.tl l) |> snd


(**
   For non-continuous sequences, we can image if a number appears, then 1, otherwise 0.
   for example, for [3;1;2;4]'s sub sequence [3;1;4] is actually [1;1;0;1] mapping to the origin list.

   For non-neighbours, we have two states: ending with 0 and ending with 1 (e0 and e1).

   Not adding x -> e0 -> new e0
   Not adding x -> e1 -> new e0
   
   adding x -> e0 -> new e1

   Note that because here is non-neighbouring, so e1 must be transitted from e0 only

   Each of those transition, we only need to reserve the max ones. 
   So, new_e0 = max e0 e1, new_e1 = x + (max e0 0). For new_e1, if e0 is negative, then we will start freshly from x

*)
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
