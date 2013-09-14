
(** Fan's lexer using [lex] DDSL *)  


  
(** To store some context information:
    loc       : position of the beginning of a string, quotation and comment *)        
type context = {
    loc        : FLoc.position ;
    (* only record the start position when enter into a quotation or antiquotation
       everytime token is used, the loc is updated to be [lexeme_start_p] *)
    buffer     : Buffer.t
  }



val turn_on_quotation_debug: unit -> unit

val turn_off_quotation_debug: unit -> unit

val clear_stack: unit -> unit

val show_stack: unit -> unit



val token :  Lexing.lexbuf ->  [> FToken.t ]

(** called by [token]
    argument [context] contains a raw [buffer] and  the [loc] is pointed to the
    starting [position].

    For context, its buffer contains the comment string, its location is
    *not valid* anymore.

    The "(*" prefix should not be there
 *)    
val lex_comment : context -> Lexing.lexbuf -> unit

(**
   called by [lex_antiquot], [lex_quotation], [token]
   argument [context] contains a raw [buffer] and  the [loc] is pointed to the
   starting [position].

   For context, its buffer contains the comment string, its location is
   *not valid* anymore.
   The "\"" should not be there *)    
val lex_string : context -> Lexing.lexbuf -> unit

val lex_antiquot : context -> Lexing.lexbuf -> unit

val lex_quotation : context -> Lexing.lexbuf -> unit    

(** In initial stage
    [Lexing.lexeme_start_p] returns
    {[ Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 ]}
    for a string input or a channel input (from_string, from_channel).
 *)    
val from_lexbuf : Lexing.lexbuf -> ([> FToken.t ] * FLoc.t) XStream.t




