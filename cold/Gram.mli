open LibUtil
open FanToken
  
type 'a t 

type assoc = [ `LA | `NA | `RA ]

type position =
    [ `After of string
    | `Before of string
    | `First
    | `Last
    | `Level of string ]
      
(* type token_stream = (FanToken.token * FanLoc.t) XStream.t *)
      
type gram =
  Grammar.Structure.gram = {
  gfilter : FanTokenFilter.t;
  gkeywords : (string, int ref) Hashtbl.t;
  glexer : FanLoc.t -> char XStream.t -> FanToken.stream ;
}

module Action :
  sig
    type t = Grammar.Structure.Action.t
    val mk : 'a -> t
    val get : t -> 'a
    val getf : t -> 'a -> 'b
    val getf2 : t -> 'a -> 'b -> 'c
  end



type description = [ `Antiquot | `Normal ]

type descr = description * string

type token_pattern = (FanToken.t -> bool) * descr

type entry = Grammar.Structure.entry 
and desc = Grammar.Structure.desc
and level = Grammar.Structure.level 
and symbol = Grammar.Structure.symbol
and tree = Grammar.Structure.tree 
and node =  Grammar.Structure.node 

type anno_action = int * Action.t       
type production = symbol list * anno_action
type label = string option

(* FIXME duplicate with Grammar/Structure *)      
type olevel =label * assoc option * production list
type extend_statment = position option * olevel list
type single_extend_statement = position option * olevel      
type delete_statment = symbol list

type ('a,'b,'c)fold  = 'b t-> symbol list-> ('a XStream.t  -> 'b) -> 'a XStream.t  -> 'c

type ('a,'b,'c) foldsep  = 'b t -> symbol list -> ('a XStream.t -> 'b) -> ('a XStream.t -> unit) -> 'a XStream.t -> 'c
      
val name: 'a t -> string

val print: Format.formatter -> 'a t -> unit
    
val dump: Format.formatter -> 'a t -> unit

val trace_parser: bool ref

val action_parse:  'a t ->  stream -> Action.t

val parse_origin_tokens:  'a t -> stream -> 'a
      
val setup_parser:  'a t ->  (stream -> 'a) -> unit
    
val clear: 'a t -> unit

val using: gram -> string -> unit

val mk_action: 'a -> Action.t

val string_of_token:[> FanToken.t ] -> string

val obj: 'a t -> entry         
val repr: entry -> 'a t
    
val removing: gram -> string -> unit

val gram: gram
(* create a standalone gram *)
val create_gram: unit -> gram

val mk_dynamic: gram -> string -> 'a t

val gram_of_entry: 'a t -> gram
    
val mk: string -> 'a t

val of_parser:  string ->  (stream -> 'a) ->  'a t

val get_filter: unit -> FanTokenFilter.t

val lex: FanLoc.t -> char XStream.t -> (FanToken.t * FanLoc.t) XStream.t

val lex_string: FanLoc.t -> string -> (FanToken.t * FanLoc.t) XStream.t

val filter:  stream -> stream

val parse:  'a t -> FanLoc.t -> char XStream.t -> 'a

val parse_string:  ?loc:FanLoc.t -> 'a t  -> string -> 'a
      
val debug_origin_token_stream: 'a t -> FanToken.t XStream.t -> 'a

val debug_filtered_token_stream: 'a t -> FanToken.t XStream.t -> 'a

val parse_string_safe:  ?loc:FanLoc.t -> 'a t ->  string -> 'a

val wrap_stream_parser: ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

val parse_file_with: rule:'a t -> string -> 'a

val delete_rule:  'a t -> symbol list -> unit

val srules: production list  ->  [> `Stree of tree ]

val sfold0:  ('a -> 'b -> 'b) ->  'b -> 'c -> 'd -> ('e XStream.t -> 'a) -> 'e XStream.t -> 'b


val sfold1:  ('a -> 'b -> 'b) ->  'b -> 'c -> 'd -> ('e XStream.t -> 'a) -> 'e XStream.t -> 'b
      
val sfold0sep:
    ('a -> 'b -> 'b) ->  'b -> 'a t -> symbol list -> ('c XStream.t -> 'a) ->
      ('c XStream.t -> unit) ->
        'c XStream.t -> 'b

val sfold1sep:
    ('a -> 'b -> 'b) ->  'b -> 'a t -> symbol list -> (stream -> 'a) ->
      (stream -> unit) ->
        stream -> 'b
            
val extend:  'a t -> extend_statment -> unit
val extend_single: 'a t -> single_extend_statement -> unit

val eoi_entry: 'a t -> 'a t

    
val levels_of_entry: 'a t -> level list option

(* val find_level: ?position:position ->  'a t -> level *)
    
val token_stream_of_string: string -> stream

val name_of_entry: 'a t -> string    
