(** Utilities for Fan's lexer *)

val lexing_store : char XStream.t -> string -> int -> int







(** In initial stage
    [Lexing.lexeme_start_p] returns
    {[ Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 ]}
    for a string input or a channel input (from_string, from_channel).
 *)    
val from_lexbuf : Lexing.lexbuf -> ([> FToken.t ] * FLoc.t) XStream.t

(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)          
val from_string :  FLoc.t -> string -> ([> FToken.t ] * FLoc.t) XStream.t

(** the stack is cleared to clear the previous error message
    call [from_lexbuf] internally *)    
val from_stream :  FLoc.t ->  char XStream.t -> ([> FToken.t ] * FLoc.t) XStream.t

      
val clean : (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t

val strict_clean :  (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t
    



val debug_from_string :  string -> unit

val debug_from_file : string -> unit
val list_of_string : string -> ([> FToken.t ] * FLoc.t) list
