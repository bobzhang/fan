open FAst



(*************************************************************************)    
(* when the antiquotation appears in the pattern position,
   its final context is [pat] *)  
val antiquot_expander : parse_pat:(loc -> string -> pat) ->
  parse_exp:(loc -> string -> exp) ->  Objs.map
