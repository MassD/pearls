let max x y = if x > y then x else y
let max_in = List.fold_left max min_int 
let sum = List.fold_left (+) 0

let rec markings = function
  | [] -> [[]]
  | hd::tl -> 
    let m = markings tl in
    let f x b y = (x,b)::y in
    List.map (f hd true) m @ List.map (f hd false) m

type state = E | S | M | N

let step s x =
  match s, x with
    | E, false -> E
    | E, true -> S
    | S, false -> M
    | S, true -> S
    | M, false -> M
    | M, true -> N
    | N, false -> N
    | N, true -> N

let non_seg l = 
  List.fold_left (fun s x -> step s (snd x)) E l == N

let extract = List.map (fun l -> List.filter snd l |> List.map fst)

let non_seg_markings l = markings l |> List.filter non_seg |> extract

let msns' l = non_seg_markings l |> List.map sum |> max_in

let pick l q =
  markings l |> List.filter (fun x -> List.fold_left (fun s x -> step s (snd x)) E x == q) |> extract


(** Best way **)

let fourth (_,_,_,x) = x

let h (e,s,m,n) x = (e, (max s e) + x, max m s, max n ((max m n) + x))

let start [x;y;z] = (0, max_in [x+y+z;y+z;z], max_in [x;x+y;y], x+z)

let msns = function
  | [] -> raise Not_found
  | hd::[] -> hd
  | hd1::hd2::[] -> max hd1 hd2
  | hd1::hd2::hd3::tl -> List.fold_left h (start [hd1;hd2;hd3]) tl |> fourth

