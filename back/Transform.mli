open Ast
  
val transform : FSig.full_id_transform -> vid -> exp
val basic_transform :
  [< `Fun of string -> string | `Post of string | `Pre of string ] ->
  string -> string
val right_transform :
  [< `Exp of string -> exp
   | `Fun of string -> string
   | `Post of string
   | `Pre of string ] ->
  string -> exp
