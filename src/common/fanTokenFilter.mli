



type t = {
    mutable kwds : Setf.String.t;
    mutable filter : Tokenf.filter;
  }


val filter : t -> Tokenf.stream  -> Tokenf.stream

val set_filter : t -> (Tokenf.filter -> Tokenf.filter) -> unit

