exception Cannot_compute
exception Cannot_glue

let rec candidates n ops =
  if n = 0 then [[]]
  else 
    List.fold_left (fun acc op -> List.rev_append acc (List.map (fun x -> op::x) (candidates (n-1) ops))) [] ops

let glue l opl =
  let rec aggr acc_l acc_opl = function
    | hd::[], [] -> (List.rev (hd::acc_l), List.rev acc_opl)
    | hd1::hd2::tl, None::optl -> aggr acc_l acc_opl (((hd1*10+hd2)::tl), optl)
    | hd::tl, (Some c)::optl -> aggr (hd::acc_l) ((Some c)::acc_opl) (tl, optl)
    | _ -> raise Cannot_glue
  in 
  aggr [] [] (l, opl)

let compute l opl =
  let new_l, new_opl = glue l opl in
  let rec comp = function
    | hd::[], [] -> hd 
    | hd::tl, (Some '+')::optl -> hd + (comp (tl, optl))
    | hd1::hd2::tl, (Some '-')::optl -> hd1 + (comp ((-hd2)::tl, optl))
    | hd1::hd2::tl, (Some '*')::optl -> comp (((hd1*hd2)::tl), optl)
    | hd1::hd2::tl, (Some '/')::optl -> comp (((hd1/hd2)::tl), optl)
    | _, _ -> raise Cannot_compute
  in 
  comp (new_l, new_opl)

let make_century l ops =
  List.filter (fun x -> fst x = 100) (
    List.fold_left (fun acc x -> ((compute l x), (l, x))::acc) [] (candidates ((List.length l)-1) ops))

let rec print_solution = function
    | hd::[], [] -> Printf.printf "%d\n" hd 
    | hd::tl, (Some op)::optl -> Printf.printf "%d %c " hd op; print_solution (tl, optl)
    | hd1::hd2::tl, None::optl -> print_solution (((hd1*10+hd2)::tl), optl)
    | _, _ -> ()


