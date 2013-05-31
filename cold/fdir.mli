

val dir_table : (string, FLoc.t -> string -> unit) Hashtbl.t

(* val handle : FLoc.t -> FToken.quotation -> unit *)

val register : string * (FLoc.t -> string -> unit) -> unit

val handle_dir : FLoc.t -> string * string -> unit    
