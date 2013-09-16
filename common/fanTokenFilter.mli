



type t = {
    mutable kwds : LibUtil.SSet.t;
    mutable filter : Ftoken.filter;
  }

(* val mk : is_kwd:(string -> bool) -> t *)

val filter : t -> (Ftoken.t * FLoc.t) Fstream.t -> Ftoken.stream

val set_filter : t -> (Ftoken.filter -> Ftoken.filter) -> unit

