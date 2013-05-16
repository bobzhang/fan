open Gstructure

type 'a t = private entry

val name : 'a t -> string

val print : Format.formatter -> 'a t -> unit

val dump : Format.formatter -> 'a t -> unit

val trace_parser : bool ref

val mk_dynamic : gram -> string -> 'a t

val action_parse : 'a t -> FanToken.stream -> Gaction.t

val of_parser :
  gram -> string -> (FanToken.stream -> 'a) -> 'a t

val setup_parser : 'a t -> (FanToken.stream -> 'a) -> unit

val clear : 'a t -> unit

val obj : 'a t -> entry

val repr : entry -> 'a t

val parse_origin_tokens : 'a t -> FanToken.stream -> 'a

val filter_and_parse_tokens : 'a t -> (FanToken.t * FanLoc.t) XStream.t -> 'a

val glexer :
  FanLoc.t -> char XStream.t -> (FanToken.t * FanLoc.t) XStream.t
val lex :
  FanLoc.t -> char XStream.t -> (FanToken.t * FanLoc.t) XStream.t


val lex_string : FanLoc.t -> string -> (FanToken.t * FanLoc.t) XStream.t

val parse_string : ?loc:FanLoc.t -> 'a t -> string -> 'a

val parse : 'a t -> FanLoc.t -> char XStream.t -> 'a
    
val name_of_entry : 'a t -> string
val gram_of_entry : 'a t -> gram

(** mutate the [estart] and [econtinue]
   The previous version is lazy. We should find a way to exploit both in the future *)    
val extend :
  'a t -> position  option * olevel list -> unit

val extend_single :
  'a t -> position  option * olevel  -> unit
    
val delete_rule : 'a t -> symbol list -> unit

val symb_failed :  'b t ->  'a -> symbol -> symbol -> string

val symb_failed_txt :  'a t -> symbol -> symbol -> string

val parser_of_symbol : 'a t ->
  symbol -> int -> (Gaction.t * FanLoc.t) FanToken.parse
val levels_of_entry : 'a t -> Gstructure.level list option      
    
