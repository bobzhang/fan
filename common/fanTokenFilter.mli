



type t = { is_kwd : string -> bool; mutable filter : FToken.filter; }

val mk : is_kwd:(string -> bool) -> t

val filter : t -> (FToken.t * FLoc.t) LibUtil.XStream.t -> FToken.stream

val define_filter : t -> (FToken.filter -> FToken.filter) -> unit

val keyword_added : 'a -> 'b -> 'c -> unit

val keyword_removed : 'a -> 'b -> unit
