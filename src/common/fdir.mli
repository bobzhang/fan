
(**
   register 
 *)
val dir_table : (Tokenf.name , unit Tokenf.expand_fun ) Hashtbl.t



val register : Tokenf.name * unit Tokenf.expand_fun -> unit

val handle_quot : Tokenf.quot -> unit    

