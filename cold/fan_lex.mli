
(** Fan's lexer using [lex] DDSL *)  
(* open Lexing *)
type lex_error =

  | Illegal_character of char
  | Illegal_escape of string
  | Illegal_quotation of string
  | Illegal_antiquote        
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_quotation
  | Unterminated_antiquot
  | Unterminated_string_in_comment
  | Unterminated_string_in_quotation
  | Unterminated_string_in_antiquot
  | Comment_start
  | Comment_not_end
  | Literal_overflow of string


(** To store some context information:
    loc       : position of the beginning of a string, quotation and comment *)        
type context = {
    loc        : FLoc.position ;
    (* only record the start position when enter into a quotation or antiquotation
       everytime token is used, the loc is updated to be [lexeme_start_p] *)
    buffer     : Buffer.t
  }

exception Lexing_error of lex_error
val turn_on_quotation_debug: unit -> unit
val turn_off_quotation_debug: unit -> unit
val clear_stack: unit -> unit
val show_stack: unit -> unit


val token:  Lexing.lexbuf ->  [> FToken.t ]

val lex_comment: context -> Lexing.lexbuf -> unit

val lex_string: context -> Lexing.lexbuf -> unit

val lex_quotation: context -> Lexing.lexbuf -> unit    

(** In initial stage
    [Lexing.lexeme_start_p] returns
    {[ Lexing.pos_fname = ""; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 ]}
    for a string input or a channel input (from_string, from_channel).
 *)    
val from_lexbuf : Lexing.lexbuf -> ([> FToken.t ] * FLoc.t) XStream.t




