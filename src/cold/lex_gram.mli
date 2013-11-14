

(** The lexer for lexer DDSL *)
  
val token : Lexing.lexbuf -> Tokenf.t 

val from_stream : Locf.t -> char Streamf.t -> Tokenf.stream
