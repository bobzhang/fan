
(** management for Fan's parser entry *)
open Gdefs

type 'a t (* = private entry *)

(** get the name of the entry *)
val name : 'a t -> string

(** print the entry *)
val print : Format.formatter -> 'a t -> unit
(** dump the entry *)
val dump : Format.formatter -> 'a t -> unit

val trace_parser : bool ref

(** the entry would not share the lexer filter with [glexer] used by fan *)    
val mk_dynamic : (* gram -> *) string -> 'a t

val clear : 'a t -> unit

val obj : 'a t -> entry

val repr : entry -> 'a t


val get_levels :  'a t -> Gdefs.level list
    
val fresh_with_levels : 'a t -> Gdefs.level list -> unit
(**  The main entrance to consume the parser,
     it call [action_parse] internally, which would call [entry.start 0 ],
     the filter of the gram is not applied  *)  
val parse_tokens : 'a t -> Tokenf.stream -> 'a


val parse_tokens_eoi : 'a t  -> Tokenf.stream -> 'a
(** mutate the [estart] and [econtinue]
   The previous version is lazy. We should find a way to exploit both in the future *)    
(* val extend : *)
(*   'a t -> position  option * olevel list -> unit *)
(* val unsafe_extend : *)
(*     'a t -> position  option * olevel list -> unit *)

type 'a single_extend_statement = {
    entry : 'a t ;
    olevel : Gdefs.olevel
  }
val extend_single :  'a single_extend_statement  -> unit

val protects :
    'a single_extend_statement list -> (unit -> 'b) -> 'b

(* val unsafe_extend_single : *)
(*   'a t -> Gdefs.olevel  -> unit *)
      
val delete_rule : 'a t -> symbol list -> unit

val symb_failed :  'b t ->  'a -> symbol -> symbol -> string

val symb_failed_txt :  'a t -> symbol -> symbol -> string

val parser_of_symbol : 'a t -> symbol  -> (Gaction.t * Locf.t) Tokenf.parse

    
val copy : 'a t -> 'a t


val map : name:string -> ('a -> 'b) -> 'a t -> 'b t     
val entry_first : 'a t -> string list
