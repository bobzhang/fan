
(**
   register 
 *)
val dir_table : (Ftoken.name , FLoc.t -> string -> unit) Hashtbl.t



val register : Ftoken.name * (FLoc.t -> string -> unit) -> unit

val handle_dir : FLoc.t -> Ftoken.name * string -> unit    
