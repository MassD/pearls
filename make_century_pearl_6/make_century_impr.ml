exception Cannot_compute
exception Cannot_glue
exception No_need_continue

let rec candidates n ops =
  if n = 0 then [[]]
  else 
    List.fold_left (fun acc op -> List.rev_append acc (List.map (fun x -> op::x) (candidates (n-1) ops))) [] ops

let glue c l opl =
  let rec aggr acc_l acc_opl = function
    | hd::[], [] -> (List.rev (hd::acc_l), List.rev acc_opl)
    | hd1::hd2::tl, None::optl -> 
      let a = hd1*10+hd2 in
      if a > c then raise No_need_continue
      else aggr acc_l acc_opl ((a::tl), optl)
    | hd::tl, (Some c)::optl -> aggr (hd::acc_l) ((Some c)::acc_opl) (tl, optl)
    | _ -> raise Cannot_glue
  in 
  aggr [] [] (l, opl)

let ok a c = if a <= c then a else raise No_need_continue

let compute c l opl =
  let new_l, new_opl = try (glue c l opl) with No_need_continue -> [], [] in
  let rec comp = function
    | [], [] -> raise No_need_continue
    | hd::[], [] -> ok hd c
    | hd::tl, (Some '+')::optl -> hd + (ok  (comp (tl, optl)) c)
    | hd1::hd2::tl, (Some '-')::optl -> hd1 + (ok  (comp ((-hd2)::tl, optl)) c)
    | hd1::hd2::tl, (Some '*')::optl -> ok (comp (((hd1*hd2)::tl), optl)) c
    | hd1::hd2::tl, (Some '/')::optl -> ok (comp (((hd1/hd2)::tl), optl)) c
    | _, _ -> raise Cannot_compute
  in 
  let r = try (comp (new_l, new_opl)) with No_need_continue -> c-1 in
  if r = c then Some opl 
  else None

let make_century l ops c =
    List.fold_left (
      fun acc x -> 
	match compute c l x with
	  | None -> acc
	  | Some v -> (v, (l, x))::acc
    ) [] (candidates ((List.length l)-1) ops)

let rec print_solution = function
    | hd::[], [] -> Printf.printf "%d\n" hd 
    | hd::tl, (Some op)::optl -> Printf.printf "%d %c " hd op; print_solution (tl, optl)
    | hd1::hd2::tl, None::optl -> print_solution (((hd1*10+hd2)::tl), optl)
    | _, _ -> ()


