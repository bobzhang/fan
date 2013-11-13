
open Tokenf
  
type 'a t = 'a Gentry.t

type assoc =  bool (* [ `LA | `NA | `RA ] *)

type position = int
    (* [ `After of string *)
    (* | `Before of string *)
    (* | `First *)
    (* | `Last *)
    (* | `Level of string ] *)

val filter: stream -> stream      

(** Basially a filter attached to the stream lexer *)    
type gram = Gdefs.gram = {
  annot:string;
  gfilter : Tokenf.filter_plugin;
}

type action = Gaction.t


type entry = Gdefs.entry 

and level =Gdefs.level 
and symbol =
    (* The concrete representation is needed here
       at least for polymorphic variant, otherwise the
       compile does not know how to destruct symbol
     *)
  | Nterm of entry
  | Snterml of (entry * int) (* the second argument is the level name *)
  | List0 of symbol
  | List0sep of (symbol * symbol)
  | List1 of symbol
  | List1sep of (symbol * symbol)
  | Try of symbol
  | Peek of symbol
  | Self
  | Token of Tokenf.pattern


and tree = Gdefs.tree 
and node = Gdefs.node 

type anno_action = {
    arity : int ;
    symbols : symbol list ;
    annot : string ;
    fn : action}

type production = {
    symbols : symbol list ;
    annot : string ;
    fn : action }

      
type label = int option

(* FIXME duplicate with Grammar/Gdefs *)      
type olevel  = {
    label : label ;
    assoc : assoc option;
    productions : production list
  }
      
type extend_statment = {
    pos : position option ;
    olevels : olevel list
  }
type single_extend_statement = olevel
  (*   { *)
  (*   pos : position option ; *)
  (*   olevel : olevel *)
  (* } *)
      
type delete_statment = symbol list

      
val name: 'a t -> string

val print: Format.formatter -> 'a t -> unit
    
val dump: Format.formatter -> 'a t -> unit

val trace_parser: bool ref

val parse_origin_tokens:  'a t -> stream -> 'a
      
    
val clear: 'a t -> unit

val entry_first : 'a t -> string list
    
val mk_action: 'a -> (* Action.t *) action

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
    ?filter:Tokenf.filter option ->
      annot:string -> keywords: string list -> unit -> gram

val mk_dynamic: gram -> string -> 'a t

val gram_of_entry: 'a t -> gram
    
val mk: string -> 'a t

val get_filter: unit -> Tokenf.filter_plugin

val filter_of_gram : 'a t -> Tokenf.filter_plugin

      
val debug_origin_token_stream : 'a t -> Tokenf.t Streamf.t -> 'a

val debug_filtered_token_stream : 'a t -> Tokenf.t Streamf.t -> 'a



val wrap_stream_parser : ?loc:Locf.t -> (loc:Locf.t -> 'a -> 'b) -> 'a -> 'b


val delete_rule :  'a t -> symbol list -> unit
val extend_single : 'a t -> single_extend_statement -> unit
val unsafe_extend_single : 'a t -> single_extend_statement -> unit    



