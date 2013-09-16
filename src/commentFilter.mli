
type t

val mk : unit -> t

val define : FanTokenFilter.t -> t -> unit

val filter : t -> (Ftoken.t * FLoc.t) Fstream.t  -> (Ftoken.t * FLoc.t) Fstream.t 

val take_list : t -> (string * FLoc.t) list 

val take_stream : t -> (string * FLoc.t) Fstream.t 

