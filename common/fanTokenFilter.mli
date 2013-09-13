



type t = {
    mutable kwds : LibUtil.SSet.t;
    mutable filter : FToken.filter;
  }

(* val mk : is_kwd:(string -> bool) -> t *)

val filter : t -> (FToken.t * FLoc.t) XStream.t -> FToken.stream

val set_filter : t -> (FToken.filter -> FToken.filter) -> unit

