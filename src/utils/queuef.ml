
include Queue
    
let find t ~f =
  with_return (fun r -> (iter(fun x -> if f x then r.return (Some x)) t; None))
    
let find_map t ~f =
  with_return (fun r ->
    (iter (fun x -> match f x with | None -> () | Some _ as res -> r.return res) t;
     None))
    (* the first element is in the bottom *)  
let to_list_rev q =
  fold (fun acc v -> v::acc) [] q 

let of_list l =
  let q = create () in
  let _ = List.iter (fun x -> push x q) l in 
  q
    
let rev q=
  of_list (to_list_rev q )
    


