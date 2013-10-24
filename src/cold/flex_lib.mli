(** Utilities for Fan's lexer *)

val lexing_store : char Streamf.t -> string -> int -> int







(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  Locf.t -> string -> Tokenf.stream 

(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream :  Locf.t ->  char Streamf.t -> Tokenf.stream 

      
val clean : Tokenf.stream -> Tokenf.stream

val strict_clean :  Tokenf.stream -> Tokenf.stream 
    



val debug_from_string :  string -> unit

val debug_from_file : string -> unit

val list_of_string : ?verbose:bool -> string -> Tokenf.t list 
val get_tokens : string ->  Tokenf.t  list
