let partition p l =
  let rec aux (lc, left, right) = function
    | [] -> lc, left, right
    | x::xs ->
       if x < p then aux (lc+1, x::left, right) xs
       else aux (lc, left, x::right) xs
  in 
  aux (0, [], []) l

let min_free l =
  let rec aux lo = function
    | [] -> lo
    | p::tl -> 
       let lc, left, right = partition p tl in
       if lc + lo < p then aux lo left
       else aux (p+1) right
  in 
  aux 0 l

Printf.printf "%B" (min_free [8;23;9;0;12;11;1;10;13;7;41;4;14;21;5;17;3;19;2;6] == 15)
