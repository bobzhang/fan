
type t

val mk : unit -> t

val define : Tokenf.filter_plugin -> t -> unit

val filter : t ->  Tokenf.stream -> Tokenf.stream 

val take_list : t -> (string * Locf.t) list 

val take_stream : t -> (string * Locf.t) Streamf.t 

