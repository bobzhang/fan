
(** Fan's lexer using [lex] DDSL *)  


val token :  Lexing.lexbuf ->   Tokenf.t



(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream : Tokenf.t Streamf.t Tokenf.lex


(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  string -> Tokenf.t Streamf.t

val from_lexbuf : Lexing.lexbuf -> Tokenf.stream
