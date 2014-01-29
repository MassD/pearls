let rec drop n = function
  | [] -> []
  | _::tl as l -> 
    if n <= 0 then l
    else drop (n-1) tl

let rec max_tail = function
  | [] -> []
  | hd::tl as l -> step (0,1,l) (l, tl)
and step (p,q,ys) = function
  | _, [] | [], _ -> ys
  | w::ws, x::xs -> 
    if w < x then max_tail (drop (q-(p mod q)) (w::ws))
    else if w = x then step (p+1, q, ys) (ws, xs)
    else step (0, p+q+1, ys) (ys, xs)
