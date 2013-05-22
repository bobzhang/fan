

(*************************************************************************)
(** antiquot filter for FAst without locations, slightly different from [Ant] *)
(*************************************************************************)    
(* when the antiquotation appears in the pattern position,
   its final context is [pat] *)  


open FAst
  
val antiquot_expander :
  parse_pat:(loc -> string -> pat) ->
  parse_exp:(loc -> string -> exp) -> Objs.map
