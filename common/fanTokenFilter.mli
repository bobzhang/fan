



type t = { is_kwd : string -> bool; mutable filter : FToken.filter; }

val mk : is_kwd:(string -> bool) -> t

val filter : t -> (FToken.t * FLoc.t) LibUtil.XStream.t -> FToken.stream

val set_filter : t -> (FToken.filter -> FToken.filter) -> unit

