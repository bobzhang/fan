
type t

val mk : unit -> t

val define : FanTokenFilter.t -> t -> unit

val filter : t -> (Tokenf.t * Locf.t) Fstream.t  -> (Tokenf.t * Locf.t) Fstream.t 

val take_list : t -> (string * Locf.t) list 

val take_stream : t -> (string * Locf.t) Fstream.t 

