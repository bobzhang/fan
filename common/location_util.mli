

(** Util module for [Location.t], i.e, [FLoc.t] *)
val from_lexbuf : Lexing.lexbuf -> FLoc.t
val of_positions : FLoc.position -> FLoc.position -> FLoc.t
val join_end : FLoc.t -> FLoc.t
val join : FLoc.t -> FLoc.t
