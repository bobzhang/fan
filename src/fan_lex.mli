
(** Fan's lexer using [lex] DDSL *)  
open Lexing
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
type context =
    { loc        : FLoc.position ;
      antiquots  : bool ;
      lexbuf     : lexbuf ;
      buffer     : Buffer.t }
        
exception Lexing_error of lex_error
val turn_on_quotation_debug: unit -> unit
val turn_off_quotation_debug: unit -> unit
val clear_stack: unit -> unit
val show_stack: unit -> unit

val mk_quotation:
  (context -> Lexing.lexbuf -> 'a) ->
  context ->
  name:FToken.name ->
  loc:string ->
  shift:int -> retract:int -> [> `QUOTATION of (FToken.name*string*int*string) ]
val update_loc:
  ?file:string ->
  ?absolute:bool -> ?retract:int -> ?line:int -> context -> unit

val err: lex_error -> FLoc.t -> 'a

val warn: lex_error -> FLoc.t -> unit

val token: context ->  Lexing.lexbuf ->  [> FToken.t ]

val comment: context -> Lexing.lexbuf -> unit

val string: context -> Lexing.lexbuf -> unit




