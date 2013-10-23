(** Utilities for Fan's lexer *)

val lexing_store : char Streamf.t -> string -> int -> int







(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  Locf.t -> string -> ( Tokenf.t  * Locf.t) Streamf.t

(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream :  Locf.t ->  char Streamf.t -> ( Tokenf.t  * Locf.t) Streamf.t

      
val clean : (Tokenf.t * 'b) Streamf.t -> (Tokenf.t * 'b) Streamf.t

val strict_clean :  (Tokenf.t * 'b) Streamf.t -> (Tokenf.t * 'b) Streamf.t
    



val debug_from_string :  string -> unit

val debug_from_file : string -> unit

val list_of_string : ?verbose:bool -> string -> ( Tokenf.t  * Locf.t) list
val get_tokens : string ->  Tokenf.t  list
