

(** The lexer for lexer DDSL *)
  
val token : Lexing.lexbuf -> Ftoken.t * FLoc.t


val from_lexbuf : Lexing.lexbuf -> Ftoken.stream 

val from_stream : FLoc.t -> char XStream.t -> Ftoken.stream
