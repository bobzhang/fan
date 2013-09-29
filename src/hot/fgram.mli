open LibUtil
open Ftoken
  
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
  gfilter : FanTokenFilter.t;
}

module Action :
  sig
    type t = Gaction.t
    val mk : 'a -> t
    val get : t -> 'a
    val getf : t -> 'a -> 'b
    val getf2 : t -> 'a -> 'b -> 'c
  end



(* type description = [ `Antiquot | `Normal ] *)

type loc = FLoc.t
type ant = [ `Ant of (loc* FanUtil.anti_cxt)]
type vid = [ `Dot of (vid* vid) | `Lid of string | `Uid of string | ant] 
type any = [ `Any]
type alident = [ `Lid of string | ant] 
type auident = [ `Uid of string | ant] 
type ident =
  [ `Dot of (ident* ident) | `Apply of (ident* ident) | alident | auident] 
type literal =
  [ `Chr of string | `Int of string | `Int32 of string | `Int64 of string
  | `Flo of string | `Nativeint of string | `Str of string]
(** a simplified pattern *)      
type pat =
  [ vid | `App of (pat* pat) | `Vrn of string | `Com of (pat* pat)
  | `Sem of (pat* pat) | `Par of pat | any | `Record of rec_pat | literal
  | `Alias of (pat* alident) | `ArrayEmpty | `Array of pat
  | `Bar of (pat* pat)
  | `PaRng of (pat* pat) ] 

and rec_pat =
  [ `RecBind of (ident* pat) | `Sem of (rec_pat* rec_pat) | any | ant] 
      
      
type descr = pat
      

      
type token_pattern = ((Ftoken.t -> bool) * descr * string )

type entry = Gstructure.entry 
and desc = Gstructure.desc
and level =Gstructure.level 
and symbol = Gstructure.symbol
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

(* val using: gram -> string -> unit *)

val mk_action: 'a -> Action.t

val string_of_token:[> Ftoken.t ] -> string

val obj: 'a t -> entry         
val repr: entry -> 'a t
    
(* val removing: gram -> string -> unit *)

val gram: gram

(** create a standalone gram
    {[

    {:new| (g:Fgram.t)
    include_quot
    |}
    ]}
 *)
val create_lexer:
    ?filter:Ftoken.filter ->
      annot:string -> keywords: string list -> unit -> gram

val mk_dynamic: gram -> string -> 'a t

val gram_of_entry: 'a t -> gram
    
val mk: string -> 'a t

val of_parser:  string ->  (stream -> 'a) ->  'a t

val get_filter: unit -> FanTokenFilter.t


val lex_string: FLoc.t -> string -> Ftoken.stream


val parse:  'a t -> FLoc.t -> char Fstream.t -> 'a

val parse_string:
    ?lexer:(FLoc.t -> char Fstream.t -> Ftoken.stream ) -> 
    ?loc:FLoc.t -> 'a t  -> string -> 'a
      
val debug_origin_token_stream: 'a t -> Ftoken.t Fstream.t -> 'a

val debug_filtered_token_stream: 'a t -> Ftoken.t Fstream.t -> 'a

val parse_string_safe:  ?loc:FLoc.t -> 'a t ->  string -> 'a

val wrap_stream_parser: ?loc:FLoc.t -> (loc:FLoc.t -> 'a -> 'b) -> 'a -> 'b

(* val parse_file_with: rule:'a t -> string -> 'a *)

val delete_rule:  'a t -> symbol list -> unit

(* val srules: production list  ->  [> `Stree of tree ] *)

val sfold0:  ('a -> 'b -> 'b) ->  'b -> 'c -> 'd -> ('e Fstream.t -> 'a) -> 'e Fstream.t -> 'b


val sfold1:  ('a -> 'b -> 'b) ->  'b -> 'c -> 'd -> ('e Fstream.t -> 'a) -> 'e Fstream.t -> 'b
      
val sfold0sep:
    ('a -> 'b -> 'b) ->  'b -> 'a t -> symbol list -> ('c Fstream.t -> 'a) ->
      ('c Fstream.t -> unit) ->
        'c Fstream.t -> 'b

val sfold1sep:
    ('a -> 'b -> 'b) ->  'b -> 'a t -> symbol list -> (stream -> 'a) ->
      (stream -> unit) ->
        stream -> 'b
            

val extend:  'a t -> extend_statment -> unit
val unsafe_extend:  'a t -> extend_statment -> unit

val extend_single: 'a t -> single_extend_statement -> unit
val unsafe_extend_single: 'a t -> single_extend_statement -> unit    

val eoi_entry: 'a t -> 'a t

    
val levels_of_entry: 'a t -> level list option

(* val find_level: ?position:position ->  'a t -> level *)
    
val token_stream_of_string : string -> stream



val parse_include_file : 'a t -> string -> 'a    
