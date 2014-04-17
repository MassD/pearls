(*
  Johnson Trotter permutation algorithm

  generating one permutation via O(1) step opration

  http://tropenhitze.wordpress.com/2010/01/25/steinhaus-johnson-trotter-permutation-algorithm-explained-and-implemented-in-java/

  1. The algorithm works by placing the numbers 1…n in the increasing order and associating LEFT < as the direction for each of them

  2. Find the largest mobile integer and swap it with the adjacent element on its direction without changing the direction of any of these two.

  3. In doing so, if the largest mobile integer has reached a spot where it’s no more mobile, proceed with the next largest integer if it’s mobile (or with the next …). There’s a catch. Read step 4.

  4. After each swapping, check if there’s any number, larger than the current largest mobile integer. If there’s one or more, change the direction of all of them. (see the example shown bellow for clarity).

  5. The algorithm terminates when there are no more mobile integers.
*)

type direction = L | R

let flip d = if d = L then R else L

let dummy n = Array.init n (fun i -> i+1, L)

let is_mobile a i =
  let len = Array.length a in
  let x,d = a.(i) in
  if d = L && i > 0 && fst a.(i-1) < x then true
  else if d = R && i < len -1 && fst a.(i+1) < x then true
  else false

let max_mobile a = 
  let len = Array.length a in
  let rec find (m,d,j) i =
    if i < len then
      let x,d = a.(i) in
      if x > m && is_mobile a i then find (x,d,i) (i+1) 
      else find (m,d,j) (i+1)
    else 
      m,d,j
  in 
  let m,d,j = find (0,L,0) 0 in
  if m = 0 then None
  else Some j

let flip_bigger a x = 
  let rec try_flip i =
    if i >= Array.length a then ()
    else
      let y,d = a.(i) in
      if y > x then a.(i) <- y,flip d
      else ();
      try_flip (i+1)
  in 
  try_flip 0

let swap a i j = let tmp = a.(i) in a.(i) <- a.(j);a.(j) <- tmp

let move a i =
  if is_mobile a i then 
    let x,d = a.(i) in
    (match d with
      | L -> swap a i (i-1)
      | R -> swap a i (i+1));
    flip_bigger a x
  else ()

let map_to_list a = Array.map fst a |> Array.to_list

let permutation n = 
  let a = dummy n in
  let rec generate acc = function
    | None -> acc
    | Some i -> move a i; generate (map_to_list a ::acc) (max_mobile a)
  in 
  generate [map_to_list a] (Some ((Array.length a)-1))
