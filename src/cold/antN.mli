(** This module ipmlements the antiquot filter for quasiquotation without locations
    @author Hongbo Zhang *)




open Astf
(** simliar to [Ant.antiquot_expander]*)  
val antiquot_expander :
  parse_pat:(loc -> string -> pat) ->
  parse_exp:(loc -> string -> exp) -> Objs.map
