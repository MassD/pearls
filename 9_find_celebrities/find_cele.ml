(**

http://stackoverflow.com/questions/20983260/linear-algorithm-for-finding-the-celebrity-group-not-single-celebrity?noredirect=1#comment31567146_20983260

Note: There must be a celebrity group

*)

let l = [0;1;2;3];;

let relations n =
  Random.self_init();
  let a = Array.make_matrix n n false in
  for i = 0 to n-1 do
    for j = 0 to n-1 do 
      if i = j then a.(i).(j) <- true
      else a.(i).(j) <- Random.bool()
    done
  done;
  if n = 4 then (
    a.(0).(2) <- true;
    a.(0).(3) <- true;
    a.(1).(2) <- true;
    a.(1).(3) <- true;
    a.(2).(3) <- true;
    a.(2).(0) <- false;
    a.(2).(1) <- false;
    a.(3).(2) <- true;
    a.(3).(0) <- false;
    a.(3).(1) <- false;
  )
  else ();
  a

let r = relations 4

let know r i j = r.(i).(j)

let rec subseqs = function
  | [] -> [[]]
  | hd::tl -> 
    let sub = subseqs tl in
    (List.map (fun x -> hd::x) sub) @ sub

let rec x_know_s r x = function
  | [] -> true
  | hd::tl -> if know r x hd then x_know_s r x tl else false

let rec s_know_s r s1 s2 = 
  match s1 with
    | [] -> true
    | hd::tl -> if x_know_s r hd s2 then s_know_s r tl s2 else false
	
let rec is_cs r ps cs = (s_know_s r ps cs) && (s_know_s r cs cs)

let find_cs r ps = List.filter (is_cs r ps) (subseqs ps) |> List.hd

let not_member p = List.fold_left (fun acc c -> if acc && know r p c && not (know r c p) then true else false) true

let member p ps css = (List.fold_left (fun acc c -> if acc && know r p c then true else false) true css) && (List.fold_left (fun acc x -> if acc && know r x p then true else false) true ps)

let rec find_cs' r = function
  | [] -> [[]]
  | p::ps ->  
    let css = find_cs' r ps in
    (List.map (fun x -> p::x) (List.filter (member p ps) css)) @ (List.filter (not_member p) css)
