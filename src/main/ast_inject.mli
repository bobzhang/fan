


(** Internal use: injection language by codetemplate *)

open Astf

type key = string
      
val register_inject_exp : key * exp -> unit

val register_inject_stru : key * stru -> unit

val register_inject_clfield : key * clfield -> unit
  
