

(** Util module for [Location.t], i.e, [Locf.t] *)
val from_lexbuf : Lexing.lexbuf -> Locf.t
val of_positions : Locf.position -> Locf.position -> Locf.t
val (--) : Locf.position -> Locf.position -> Locf.t
val join_end : Locf.t -> Locf.t
val join : Locf.t -> Locf.t
