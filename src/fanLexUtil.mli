
val lexing_store : char XStream.t -> string -> int -> int
(* val from_context : *)
(*   context -> ([> FanToken.t ] * FanLoc.t) XStream.t *)
val from_lexbuf : Lexing.lexbuf -> ([> FanToken.t ] * FanLoc.t) XStream.t
val setup_loc : Lexing.lexbuf -> FanLoc.t -> unit

val from_string :  FanLoc.t -> string -> ([> FanToken.t ] * FanLoc.t) XStream.t

val from_stream :  FanLoc.t ->  char XStream.t -> ([> FanToken.t ] * FanLoc.t) XStream.t

val mk : unit ->  FanLoc.t ->  char XStream.t -> ([> FanToken.t ] * FanLoc.t) XStream.t
      
val clean : (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t

val strict_clean :  (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t
    
val debug_from_string :  string -> unit

val debug_from_file : string -> unit
