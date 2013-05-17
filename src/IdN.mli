
open AstN

  
val tvar_of_ident : vid -> string

val map_to_string : vid -> string

val to_string : ident -> string

val to_vid : ident -> vid

val ident_map :
    (string -> string) ->
      vid -> [> `Dot of vid * [> `Lid of string ] | `Lid of string ]


val ident_map_of_ident : (string -> vid) -> vid -> vid

