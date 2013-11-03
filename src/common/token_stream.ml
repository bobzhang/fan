

(* get_cur_loc *must* be used first *)  
let cur_loc (strm:Tokenf.stream) =
  match Streamf.peek strm with
  | Some r -> Tokenf.get_loc r 
  | None -> Locf.ghost 


let prev_loc strm =
  match Streamf.get_last strm with
  |Some l -> Tokenf.get_loc l
  |None -> Locf.ghost
