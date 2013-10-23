(** Utilities for Fan's lexer *)

val lexing_store : char Fstream.t -> string -> int -> int







(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  Locf.t -> string -> ( Ftoken.t  * Locf.t) Fstream.t

(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream :  Locf.t ->  char Fstream.t -> ( Ftoken.t  * Locf.t) Fstream.t

      
val clean : (Ftoken.t * 'b) Fstream.t -> (Ftoken.t * 'b) Fstream.t

val strict_clean :  (Ftoken.t * 'b) Fstream.t -> (Ftoken.t * 'b) Fstream.t
    



val debug_from_string :  string -> unit

val debug_from_file : string -> unit

val list_of_string : ?verbose:bool -> string -> ( Ftoken.t  * Locf.t) list
val get_tokens : string ->  Ftoken.t  list
