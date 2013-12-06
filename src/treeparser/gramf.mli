
open Tokenf
  
type 'a t = 'a Gentry.t



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
    lassoc : bool ;
    productions : production list
  }
      

(* type single_extend_statement = olevel *)
type 'a single_extend_statement = {
    entry : 'a t ;
    olevel : olevel
  }
      
type delete_statment = symbol list

      
val name: 'a t -> string

val print: 'a t Formatf.t 
    
val dump: 'a t Formatf.t

val trace_parser: bool ref

val parse_origin_tokens:  'a t -> stream -> 'a
      
val parse_tokens_eoi : 'a t  -> Tokenf.stream -> 'a
    
val clear: 'a t -> unit

val entry_first : 'a t -> string list
    
val mk_action: 'a -> action

val obj: 'a t -> entry         

val repr: entry -> 'a t
    


(** create a standalone gram
    {[

    {:new| (g:Gramf.t)
    include_quot
    |}
    ]}
 *)
    
val mk: string -> 'a t

      
val debug_origin_token_stream : 'a t -> Tokenf.t Streamf.t -> 'a

val wrap_stream_parser : ?loc:Locf.t -> (loc:Locf.t -> 'a -> 'b) -> 'a -> 'b
    

val delete_rule :  'a t -> symbol list -> unit

val extend_single : 'a single_extend_statement -> unit

val protects :
    'a single_extend_statement list -> (unit -> 'b) -> 'b




