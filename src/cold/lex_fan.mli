
(** Fan's lexer using [lex] DDSL *)  



(** The [low_keys] is exported so the user could tweak the original language
    with some new keys
    see [make_token]
 *)
val low_keys : string list 

val make_token : string list -> Lexing.lexbuf -> Tokenf.t
    
val token :  Lexing.lexbuf ->   Tokenf.t




(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream : Locf.t -> char Streamf.t -> Tokenf.t Streamf.t


(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  string -> Tokenf.t Streamf.t

val from_lexbuf : Lexing.lexbuf -> Tokenf.stream
