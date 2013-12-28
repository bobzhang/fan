
open Astf
  
val exp : Locf.t -> string -> exp
val pat : Locf.t -> string -> pat

val ep : Locf.t -> string -> ep
val ident : Locf.t -> string -> ident



    


val expand_exp : Tokenf.quot -> exp
val expand_stru : Tokenf.quot -> stru
