


(** Return a location from ocamllex buffer. *)
let from_lexbuf lb =
  let loc_start = Lexing.lexeme_start_p lb
  and loc_end  = Lexing.lexeme_end_p lb in
  {FLoc.loc_start  ;
   loc_end   ;
   loc_ghost  = false }

let of_positions s e =
  {FLoc.loc_start = s; loc_end = e ; loc_ghost = false}


let join_end (x:FLoc.t) = {x with loc_start = x.loc_end}

(** The stop pos becomes equal to the start pos. *)
let join (x:FLoc.t) = { x with loc_end = x.loc_start }
    
    
