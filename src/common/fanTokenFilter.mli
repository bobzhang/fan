



type t = {
    mutable kwds : Setf.String.t;
    mutable filter : Tokenf.filter;
  }


val filter : t -> (Tokenf.t * Locf.t) Streamf.t -> Tokenf.stream

val set_filter : t -> (Tokenf.filter -> Tokenf.filter) -> unit

