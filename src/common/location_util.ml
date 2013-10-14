


(** Return a location from ocamllex buffer. *)
let from_lexbuf (lb:Lexing.lexbuf) = {
  Locf.loc_start   = lb.lex_start_p ;
  loc_end = lb.lex_curr_p    ;
  loc_ghost  = false }

    
let of_positions s e =
  {Locf.loc_start = s; loc_end = e ; loc_ghost = false}

let (--) = of_positions


let join_end (x:Locf.t) = {x with loc_start = x.loc_end}

(** The stop pos becomes equal to the start pos. *)
let join (x:Locf.t) = { x with loc_end = x.loc_start }
    
    
