
open Tokenf
  
type 'a t = 'a Gentry.t

type assoc = [ `LA | `NA | `RA ]

type position =
    [ `After of string
    | `Before of string
    | `First
    | `Last
    | `Level of string ]

val filter: stream -> stream      

(** Basially a filter attached to the stream lexer *)    
type gram = Gstructure.gram = {
  annot:string;
  gfilter : Tokenf.filter_plugin;
}

module Action : sig
  type t = Gaction.t
  val mk : 'a -> t
  val get : t -> 'a
  val apply : t -> 'a -> 'b
  val apply2 : t -> 'a -> 'b -> 'c
end

      
type word =
   [`Any
   |`A of string
   |`Empty]
and data = (int * word) (* FIXME duplicate in gram_def *)      
type descr = data
      

      
type token_pattern = ((Tokenf.t -> bool) * descr * string )
type terminal = 
    [ `Keyword of string
    | `Token of token_pattern ]

type entry = Gstructure.entry 
and desc = Gstructure.desc
and level =Gstructure.level 
and symbol =
    (* The concrete representation is needed here
       at least for polymorphic variant, otherwise the
       compile does not know how to destruct symbol
     *)
    [`Nterm of entry
    | `Snterml of (entry * string) (* the second argument is the level name *)
    | `List0 of symbol
    | `List0sep of (symbol * symbol)
    | `List1 of symbol
    | `List1sep of (symbol * symbol)
    | `Try of symbol
    | `Peek of symbol
    | `Self
    | terminal
   ]

and tree = Gstructure.tree 
and node = Gstructure.node 

type anno_action = int * symbol list * string * Action.t       

type production = symbol list * (string * Action.t)
      
type label = string option

(* FIXME duplicate with Grammar/Gstructure *)      
type olevel =label * assoc option * production list
type extend_statment = position option * olevel list
type single_extend_statement = position option * olevel      
type delete_statment = symbol list

      
val name: 'a t -> string

val print: Format.formatter -> 'a t -> unit
    
val dump: Format.formatter -> 'a t -> unit

val trace_parser: bool ref

val parse_origin_tokens:  'a t -> stream -> 'a
      
val setup_parser:  'a t ->  (stream -> 'a) -> unit
    
val clear: 'a t -> unit

val entry_first : 'a t -> string list
    
val mk_action: 'a -> Action.t

val obj: 'a t -> entry         

val repr: entry -> 'a t
    
val gram: gram

(** create a standalone gram
    {[

    {:new| (g:Gramf.t)
    include_quot
    |}
    ]}
 *)
val create_lexer:
    ?filter:Tokenf.filter ->
      annot:string -> keywords: string list -> unit -> gram

val mk_dynamic: gram -> string -> 'a t

val gram_of_entry: 'a t -> gram
    
val mk: string -> 'a t

val of_parser:  string ->  (stream -> 'a) ->  'a t

val get_filter: unit -> Tokenf.filter_plugin


val lex_string: Locf.t -> string -> Tokenf.stream


val parse:  'a t -> Locf.t -> char Streamf.t -> 'a

val parse_string:
    ?lexer:(Locf.t -> char Streamf.t -> Tokenf.stream ) -> 
    ?loc:Locf.t -> 'a t  -> string -> 'a
      
val debug_origin_token_stream: 'a t -> Tokenf.t Streamf.t -> 'a

val debug_filtered_token_stream: 'a t -> Tokenf.t Streamf.t -> 'a

val parse_string_safe:  ?loc:Locf.t -> 'a t ->  string -> 'a

val wrap_stream_parser: ?loc:Locf.t -> (loc:Locf.t -> 'a -> 'b) -> 'a -> 'b


val delete_rule:  'a t -> symbol list -> unit


val extend:  'a t -> extend_statment -> unit
val unsafe_extend:  'a t -> extend_statment -> unit

val extend_single: 'a t -> single_extend_statement -> unit
val unsafe_extend_single: 'a t -> single_extend_statement -> unit    



    
val levels_of_entry: 'a t -> level list option

    
val token_stream_of_string : string -> stream



val parse_include_file : 'a t -> string -> 'a    
