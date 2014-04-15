(*
  boustrophedon - box
  box [x;y] [a;b;c] = [[a;b;c;][x;][c;b;a;];[y];[a;b;c;]] = [a;b;c;x;c;b;a;y;a;b;c]
*)

(* this is the raw way of boxing, it is not efficient as l2 is always reversed *) 
let rec box l1 l2 =
  match l1 with
    | [] -> l2
    | hd::tl -> l2 @ [hd] @ box tl (List.rev l2)

(* this is more efficient as ys will be reversed only once 
   But still, because of `ys @` part, it will anyway read through ys/sy all the time
*) 
let box_better xs ys =
  let rec mix xs (ys, sy) =
    match xs with
      | [] -> ys
      | x::tl -> ys @ [x] @ mix tl (sy, ys)
  in 
  mix xs (ys, List.rev ys)

let box_tail l1 l2 =
  let rec collect acc l2' = function                
    | [], [] -> List.rev acc
    | hd::tl, [] -> collect (hd::acc) [] (tl, l2')
    | l, hd::tl -> collect (hd::acc) (hd::l2') (l, tl)
  in 
  collect [] [] (l1,l2)

(* box all in a list *)

let box_all ll = List.fold_left box_better [] ll

let box_allr ll = List.fold_right box_better ll []

let rec box_allr' = function
  | [] -> []
  | hd::tl -> box_better hd (box_allr tl)
