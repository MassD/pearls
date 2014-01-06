let l = [1;2;3]

let rec subseqs = function
  | [] -> [[]]
  | hd::tl -> 
    let sub = subseqs tl in
    (List.map (fun x -> hd::x) sub) @ sub
