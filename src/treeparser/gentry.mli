
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

(* val gram_of_entry : 'a t -> gram *)


(**  The main entrance to consume the parser,
     it call [action_parse] internally, which would call [entry.start 0 ],
     the filter of the gram is not applied  *)  
val parse_origin_tokens : 'a t -> Tokenf.stream -> 'a

(** The same as [parse_origin_tokens] except that filter is applied *)    
(* val filter_and_parse_tokens : 'a t -> Tokenf.stream -> 'a *)

(** The default lexer, i.e, [Flex_lib.form_stream],
     *)
(* val lex_string : Locf.t -> string -> Tokenf.stream *)

(** It would call the default lexer [gfilter], however, the filter
    is parameterized by the entry *)    
(* val parse_string : *)
(*     ?lexer:(Locf.t -> char Streamf.t -> Tokenf.stream ) -> *)
(*       ?loc:Locf.t -> 'a t -> string -> 'b *)

(* val filter_of_gram : 'a t -> Tokenf.filter_plugin *)

    
(** call the [gfilter], and use [glexer] *)
(* val parse : 'a t -> Locf.t -> char Streamf.t -> 'a *)
    




(** mutate the [estart] and [econtinue]
   The previous version is lazy. We should find a way to exploit both in the future *)    
(* val extend : *)
(*   'a t -> position  option * olevel list -> unit *)
(* val unsafe_extend : *)
(*     'a t -> position  option * olevel list -> unit *)

val extend_single :
  'a t -> Gdefs.single_extend_statement  -> unit

val unsafe_extend_single :
  'a t -> Gdefs.single_extend_statement  -> unit
      
val delete_rule : 'a t -> symbol list -> unit

val symb_failed :  'b t ->  'a -> symbol -> symbol -> string

val symb_failed_txt :  'a t -> symbol -> symbol -> string

val parser_of_symbol : 'a t -> symbol  -> (Gaction.t * Locf.t) Tokenf.parse

    
val copy : 'a t -> 'a t


val map : name:string -> ('a -> 'b) -> 'a t -> 'b t     
val entry_first : 'a t -> string list
