let rec drop n = function
  | [] -> []
  | _::tl as l -> 
    if n <= 0 then l
    else drop (n-1) tl

let rec max_tail = function
  | [] -> []
  | hd::tl as l ->
    probe_max l (l, tl, 1, 0)
and probe_max m (wl, xl, w_x_dist, x_shift) =
  match wl, xl with
    | _, [] | [], _ -> m
    | w::ws, x::xs ->
      if w > x then probe_max m (m, xs, w_x_dist+1, 0)
      else if w = x then probe_max m (ws, xs, w_x_dist+1, x_shift+1)
      else max_tail (drop (w_x_dist - x_shift) m)

(**

-w-0-0-0-0-0-0 : wl
  -x-0-0-0-0-0 : xl

if w > x, wl is always the m, xl keep moving, distance between w and x increased by 1

if w < x, new possible m need to start from somewhere in xl, wl will be dumped

if w = x, then wl and xl keep moving together, until w > x or w < x

*)
