
(** management for Fan's parser entry *)
open Gstructure

type 'a t (* = private entry *)

val name : 'a t -> string

(** *)
val print : Format.formatter -> 'a t -> unit
(** *)
val dump : Format.formatter -> 'a t -> unit

val trace_parser : bool ref

val mk_dynamic : gram -> string -> 'a t

val action_parse : 'a t -> Ftoken.stream -> Gaction.t

val of_parser :
  gram -> string -> (Ftoken.stream -> 'a) -> 'a t

val setup_parser : 'a t -> (Ftoken.stream -> 'a) -> unit

val clear : 'a t -> unit

val obj : 'a t -> entry

val repr : entry -> 'a t

val parse_origin_tokens : 'a t -> Ftoken.stream -> 'a

val filter_and_parse_tokens : 'a t -> (Ftoken.t * FLoc.t) XStream.t -> 'a

val glexer :
  FLoc.t -> char XStream.t -> (Ftoken.t * FLoc.t) XStream.t
val lex :
  FLoc.t -> char XStream.t -> (Ftoken.t * FLoc.t) XStream.t


val lex_string : FLoc.t -> string -> (Ftoken.t * FLoc.t) XStream.t

val parse_string : ?loc:FLoc.t -> 'a t -> string -> 'a

val parse : 'a t -> FLoc.t -> char XStream.t -> 'a
    
val name_of_entry : 'a t -> string
val gram_of_entry : 'a t -> gram


(** mutate the [estart] and [econtinue]
   The previous version is lazy. We should find a way to exploit both in the future *)    
val extend :
  'a t -> position  option * olevel list -> unit
val unsafe_extend :
    'a t -> position  option * olevel list -> unit

val extend_single :
  'a t -> position  option * olevel  -> unit

val unsafe_extend_single :
  'a t -> position  option * olevel  -> unit
      
val delete_rule : 'a t -> symbol list -> unit

val symb_failed :  'b t ->  'a -> symbol -> symbol -> string

val symb_failed_txt :  'a t -> symbol -> symbol -> string

val parser_of_symbol : 'a t -> symbol  -> (Gaction.t * FLoc.t) Ftoken.parse
val levels_of_entry : 'a t -> Gstructure.level list option      
    
val copy : 'a t -> 'a t

val eoi_entry : 'a t -> 'a t
