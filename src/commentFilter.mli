
type t

val mk : unit -> t

val define : FanTokenFilter.t -> t -> unit

val filter : t -> (FToken.t * FLoc.t) XStream.t  -> (FToken.t * FLoc.t) XStream.t 

val take_list : t -> (string * FLoc.t) list 

val take_stream : t -> (string * FLoc.t) XStream.t 

