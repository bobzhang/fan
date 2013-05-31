

(** Fan parser management *)
  
type key = string

type effect = unit -> unit

val applied_parsers : (key * effect) Queue.t

val registered_parsers : (key, effect) Hashtbl.t


(** apply the parser, the place where side effect happens *)
val use_parsers : key list -> unit
    
(** do the parser registration *)
val register_parser : key * effect -> unit
