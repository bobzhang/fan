
type t

val mk : unit -> t

val define : FanToken.Filter.t -> t -> unit

val filter : t -> (FanToken.t * FanLoc.t) XStream.t  -> (FanToken.t * FanLoc.t) XStream.t 

val take_list : t -> (string * FanLoc.t) list 

val take_stream : t -> (string * FanLoc.t) XStream.t 

