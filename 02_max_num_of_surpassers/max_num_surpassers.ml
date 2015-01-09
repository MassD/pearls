let split by_n l =
  let rec aux left i = function
    | [] -> i, List.rev left, []
    | x::xs when i < by_n -> aux (x::left) (i+1) xs
    | _ as l -> i, List.rev left, l
  in 
  aux [] 0 l

let merge left (right, rc) =
  let rec aux acc rc = function
    | l, [] | [], l -> List.rev_append acc l
    | ((x, ns)::xs as l1), ((y, ny)::ys as l2) ->
      if x < y then
	aux ((x, ns+rc)::acc) rc (xs, l2)
      else 
	aux ((y, ny)::acc) (rc-1) (l1, ys)
  in 
  aux [] rc (left, right)

let rec solve len = function
  | [] | _::[] as l-> l
  | l -> 
    let c, left, right = split (len/2) l in
    let rc = len - c in
    merge (solve c left) (solve rc right, rc)

let num_surpassers l = List.map ~f:(fun x -> x, 0) l |> solve (List.length l)

let max_ns l = List.fold ~f:(fun m (_, ns) -> max m ns) ~init:0 l

let max_num_surpassers l = num_surpassers l |> max_ns


let l =  ['g'; 'e'; 'n'; 'e'; 'r'; 'a'; 't'; 'i'; 'n'; 'g']

let _ = print_endline (if max_num_surpassers l = 6 then "test passed" else "test failed")
