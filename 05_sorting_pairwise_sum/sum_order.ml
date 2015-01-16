(*
  Note the code is not doing O(n^2) sorting pairwise sum. 
  It is very hard to understand Lambert's algorithm on this, at least for me.
  Also, I can't really precisely understand the text in the book
  
  However, this code is close to what the book tries to say, just it is O(n^2logn)
  
  I asked the question here: http://stackoverflow.com/questions/27987481/lamberts-on2-algorithm-on-pairwise-sum-sort
*)


exception Cannot_sum_order

let half l =
  let half_len = List.length l / 2 in
  let rec aux a i = function
    | [] -> List.rev a, []
    | x::xs -> if i < half_len then aux (x::a) (i+1) xs else List.rev a, xs

let rec sort_pairwise_sum l1 l2 =
  match l1, l2 with
    | l, [] | [], l -> l
    | x::[], y::[] -> [x+y]
    | x::xs, y::ys ->
      let la, lb = half l1 and lc, ld = half l2 in
      sort_pairwise_sum la lc @ List.merge (sort_pairwise_sum la ld) (sort_pairwise_sum lb lc) @ sort_pairwise_sum lb ld
