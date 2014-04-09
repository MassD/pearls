

let rec unfold f a = 
  match f a with
    | None -> []
    | Some (b,a') -> b::unfold f a'
