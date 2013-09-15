(** Utilities for Fan's lexer *)

val lexing_store : char XStream.t -> string -> int -> int







(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  FLoc.t -> string -> ([> Ftoken.t ] * FLoc.t) XStream.t

(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream :  FLoc.t ->  char XStream.t -> ([> Ftoken.t ] * FLoc.t) XStream.t

      
val clean : (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t

val strict_clean :  (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t
    



val debug_from_string :  string -> unit

val debug_from_file : string -> unit

val list_of_string : ?verbose:bool -> string -> ( Ftoken.t  * FLoc.t) list
val get_tokens : string ->  Ftoken.t  list
