open Lexing
type lex_error =
    Illegal_character of char
  | Illegal_escape of string
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
        
type context =
    { loc        : FanLoc.position    ;
      in_comment : bool     ;
      quotations : bool     ;
      antiquots  : bool     ;
      lexbuf     : lexbuf   ;
      buffer     : Buffer.t }
        
exception Lexing_error of lex_error
val turn_on_quotation_debug: unit -> unit
val turn_off_quotation_debug: unit -> unit
val clear_stack: unit -> unit
val show_stack: unit -> unit

val mk_quotation:
  (context -> Lexing.lexbuf -> 'a) ->
  context ->
  name:string ->
  loc:string ->
  shift:int -> retract:int -> [> `QUOTATION of FanToken.quotation ]
val update_loc:
  ?file:string ->
  ?absolute:bool -> ?retract:int -> ?line:int -> context -> unit
val err: lex_error -> FanLoc.t -> 'a
val warn: lex_error -> FanLoc.t -> unit

val token: context ->  Lexing.lexbuf ->  [> FanToken.t ]

val comment: context -> Lexing.lexbuf -> unit

val string: context -> Lexing.lexbuf -> unit

val symbolchar_star:  string ->  context -> Lexing.lexbuf ->  [> FanToken.t ]
val default_context: Lexing.lexbuf -> context   
val with_curr_loc: (context -> Lexing.lexbuf -> 'a) -> context -> 'a

(* val from_context :  context ->  ([> FanSig.token] * FanLoc.t)XStream.t *)
(* val from_lexbuf :  ?quotations:bool ->  Lexing.lexbuf -> *)
(*   ([> FanSig.token ] * FanLoc.t) XStream.t *)
(* val setup_loc : Lexing.lexbuf -> FanLoc.t -> unit *)
(* val from_string : *)
(*   ?quotations:bool -> FanLoc.t ->  string ->  ([> FanSig.token] * FanLoc.t) XStream.t *)
(* val from_stream : *)
(*     ?quotations:bool -> FanLoc.t -> char XStream.t -> ([> FanSig.token ] * FanLoc.t) XStream.t *)
(* val mk : *)
(*   unit -> FanLoc.t ->  char XStream.t -> ([> FanSig.token ] * FanLoc.t) XStream.t *)
      
