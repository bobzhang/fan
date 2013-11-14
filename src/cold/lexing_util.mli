

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


val lexing_store : char Streamf.t -> string -> int -> int
    
val print_lex_error : Format.formatter -> lex_error -> unit

val lex_error_to_string : lex_error -> string

(** To store some context information:
    loc       : position of the beginning of a string, quotation and comment *)        
type context = {
    mutable loc        : Locf.position list ;
    (* only record the start position when enter into a quotation or antiquotation
       everytime token is used, the loc is updated to be [lexeme_start_p] *)
    buffer     : Buffer.t
  }

val from_lexbuf : Lexing.lexbuf -> Locf.t     
val new_cxt : unit -> context
    
val store : context -> Lexing.lexbuf -> unit

val with_store :
     context -> Lexing.lexbuf ->
       (context -> Lexing.lexbuf -> 'a) -> 'a

val buff_contents : context -> string

val move_curr_p : int -> Lexing.lexbuf -> unit


val push_loc_cont :
    context -> Lexing.lexbuf -> (context -> Lexing.lexbuf -> 'a) -> 'a

val pop_loc :
    context -> unit
        

val update_loc :
  ?file:string ->
  ?absolute:bool -> ?retract:int -> ?line:int -> Lexing.lexbuf -> unit

val err : lex_error -> Locf.t -> 'a

val warn : lex_error -> Locf.t -> unit

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


(**
   called by [lex_antiquot], [token]
 *)    
val lex_quotation : context -> Lexing.lexbuf -> unit    



val adapt_to_stream : (Lexing.lexbuf -> 'a) -> Locf.t -> char Streamf.t -> 'a Streamf.t

val clean : Tokenf.stream -> Tokenf.stream    
(* local variables: *)
(* compile-command: "cd ../main_annot && pmake lexing_util.cmo" *)
(* end: *)
