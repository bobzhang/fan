

type lex_error  =
  | Illegal_character of char
  | Illegal_escape    of string
  | Illegal_quotation of string
  | Illegal_antiquote 
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_quotation
  | Unterminated_antiquot
  | Comment_start
  | Comment_not_end
      
exception Lexing_error of lex_error

val print_lex_error : Format.formatter -> lex_error -> unit

val lex_error_to_string : lex_error -> string

val opt_char_len : 'a option -> int
val print_opt_char : Format.formatter -> char option -> unit
val clear_stack : unit -> unit
module CStack : sig
  type 'a t
  val push : char option -> char option t -> unit
  val pop : char option t -> char option
end

(** FIXME: here only one copy of opt_char which makes it to call multiple lexers
    impossible *)    
val opt_char : char option Stack.t 

(** To store some context information:
    loc       : position of the beginning of a string, quotation and comment *)        
type context = {
    loc        : FLoc.position ;
    (* only record the start position when enter into a quotation or antiquotation
       everytime token is used, the loc is updated to be [lexeme_start_p] *)
    buffer     : Buffer.t
  }




val default_cxt : Lexing.lexbuf -> context
    

val store : context -> Lexing.lexbuf -> unit

val with_store :
    (context -> Lexing.lexbuf -> 'a) -> context -> Lexing.lexbuf -> 'a

val buff_contents : context -> string

val move_curr_p : int -> Lexing.lexbuf -> unit

val with_curr_loc :
    (context -> Lexing.lexbuf -> 'a) -> context -> Lexing.lexbuf -> 'a

val mk_quotation :
  (context -> Lexing.lexbuf -> 'a) ->
  context ->
  Lexing.lexbuf ->
  name:Ftoken.name ->
  loc:string option ->
  shift:int -> retract:int -> [> `Quot of Ftoken.quot ] * FLoc.t

val update_loc :
  ?file:string ->
  ?absolute:bool -> ?retract:int -> ?line:int -> Lexing.lexbuf -> unit

val err : lex_error -> FLoc.t -> 'a

val warn : lex_error -> FLoc.t -> unit

(** called by [token]
    argument [context] contains a raw [buffer] and  the [loc] is pointed to the
    starting [position].

    For context, its buffer contains the comment string, its location is
    *not valid* anymore.

    The "(*" prefix should not be there

    (* shgo *) is returned 
 *)    
val lex_comment : context -> Lexing.lexbuf -> unit

(**
   called by [lex_antiquot], [lex_quotation], [token]
   argument [context] contains a raw [buffer] and  the [loc] is pointed to the
   starting [position].

   For context, its buffer contains the comment string, its location is
   *not valid* anymore.
   The "\"" should not be there,.

   remember it return a raw string, which would be evaled in the ast level

   abho is returned without trailing "\""
 *)    
val lex_string : context -> Lexing.lexbuf -> unit

(**
   called by [token]
   argument [context] contains a raw [buffer] and  the [loc] is pointed to the
   starting [position].

   For context, its buffer contains the comment string, its location is
   *not valid* anymore.
   The "$" should not be there. "()" is returned for $()
   
 *)    
val lex_antiquot : context -> Lexing.lexbuf -> unit

(**
   called by [lex_antiquot], [token]
 *)    
val lex_quotation : context -> Lexing.lexbuf -> unit    
