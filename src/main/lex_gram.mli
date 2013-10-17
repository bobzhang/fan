

(** The lexer for lexer DDSL *)
  
val token : Lexing.lexbuf -> Ftoken.t * Locf.t


val from_lexbuf : Lexing.lexbuf -> Ftoken.stream 

val from_stream : Locf.t -> char Fstream.t -> Ftoken.stream
