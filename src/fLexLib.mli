(** Utilities for Fan's lexer *)

val lexing_store : char XStream.t -> string -> int -> int
(* val from_context : *)
(*   context -> ([> FToken.t ] * FLoc.t) XStream.t *)
val from_lexbuf : Lexing.lexbuf -> ([> FToken.t ] * FLoc.t) XStream.t
val setup_loc : Lexing.lexbuf -> FLoc.t -> unit

val from_string :  FLoc.t -> string -> ([> FToken.t ] * FLoc.t) XStream.t

val from_stream :  FLoc.t ->  char XStream.t -> ([> FToken.t ] * FLoc.t) XStream.t

val mk : unit ->  FLoc.t ->  char XStream.t -> ([> FToken.t ] * FLoc.t) XStream.t
      
val clean : (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t

val strict_clean :  (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t
    
val debug_from_string :  string -> unit

val debug_from_file : string -> unit
