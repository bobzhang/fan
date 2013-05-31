

val dir_table : (string, string -> unit) Hashtbl.t

val handle : FLoc.t -> FToken.quotation -> unit

val register_dir : string * (string -> unit) -> unit
