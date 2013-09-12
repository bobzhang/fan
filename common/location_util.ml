


(** Return a location from ocamllex buffer. *)
let from_lexbuf lb =
  let loc_start = Lexing.lexeme_start_p lb
  and loc_end  = Lexing.lexeme_end_p lb in
  {FLoc.loc_start  ;
   loc_end   ;
   loc_ghost  = false }

let of_positions s e =
  {FLoc.loc_start = s; loc_end = e ; loc_ghost = false}