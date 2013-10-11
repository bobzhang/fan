
(**
   register 
 *)
val dir_table : (Ftoken.name , unit Ftoken.expand_fun ) Hashtbl.t



val register : Ftoken.name * unit Ftoken.expand_fun -> unit

val handle_quot : Ftoken.quot -> unit    

