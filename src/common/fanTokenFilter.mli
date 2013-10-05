



type t = {
    mutable kwds : Setf.String.t;
    mutable filter : Ftoken.filter;
  }


val filter : t -> (Ftoken.t * FLoc.t) Fstream.t -> Ftoken.stream

val set_filter : t -> (Ftoken.filter -> Ftoken.filter) -> unit

