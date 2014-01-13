let max_sum_seq l =
  let max (s1,sq1) (s2,sq2) =
    if sq2 = [] || s2 < s1 then (s1,sq1)
    else (s2,sq2)
  in 
  let s1, s2  = 
    List.fold_left (
    fun ((acc,seq),c) x ->
      if seq = [] || acc < 0  then ((x,[x]), max (acc, seq) c)
      else if x > 0 then ((x+acc,x::seq),c)
      else ((x+acc,x::seq),max (acc,seq) c)
  ) ((0,[]),(0,[])) l 
  in
  let max,max_seq = max s1 s2 in
  (max, List.rev max_seq)

let _ = max_sum_seq [-1;-1;-2;-4]

let max x y = if x > y then x else y
let mss l = List.fold_left (fun (c,m) x -> let n = max x (c+x) in (n, max n m)) (List.hd l,min_int) (List.tl l) |> snd

let l = [-10;-2;-5;-2;-4;-6]
