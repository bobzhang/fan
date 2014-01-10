
(** Manage Fan's quotation *)
  
open Astf

val dump_names_tbl : unit -> unit

val resolve_name : Tokenf.name -> Tokenf.name option

val paths : Tokenf.domain list ref
    
val current_loc_name : string option ref

(** [add name exp] adds the quotation [name] associated with the  expander [exp]. *)
val add : Tokenf.name -> 'a Dyn_tag.t  -> 'a Tokenf.expand_fun  -> unit

    
(** [default] holds the default quotation name. *)
val default: Tokenf.name option ref

val set_default: Tokenf.name -> unit


(** [default_tbl] mapping [position] to the default quotation name
    it has higher precedence over default  *)

val map: Tokenf.name Mapf.String.t ref

    
(** [default_at_pos] set the default quotation name for specific pos*)
val default_at_pos: string -> Tokenf.name -> unit
    
val clear_map: unit -> unit
    
val clear_default: unit -> unit
    
val expand : Tokenf.quot -> 'a Dyn_tag.t-> 'a



(** [dump_file] optionally tells Fan to dump the
    result of an expander if this result is syntactically incorrect.
    If [None] (default), this result is not dumped. If [Some fname], the
    result is dumped in the file [fname]. *)
val dump_file : string option  ref



(** The raw quotation expander, register its type, laguage namespace and
    an expansion function *)
val add : Tokenf.name -> 'a Dyn_tag.t -> 'a Tokenf.expand_fun -> unit    
(** theoretically you can use [mexp] which lift it into any type you can
   but we made a restriction here.
   [exp_filter] and [pat_filter] default to an id 
*)
val add_quotation:
    lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
    exp_filter:(ep -> exp) ->
      pat_filter:(ep -> pat) ->
        mexp:(Locf.t -> 'c -> ep) ->
          mpat:(Locf.t -> 'c -> ep) -> Tokenf.name -> 'c Gramf.t -> unit


val make_parser :
    lexer:Tokenf.stream Tokenf.lex ->
      'a Gramf.t -> Locf.t -> string option -> string -> 'a
(* FIXME revised parser *cannot* parse name:string -> unit*)
(*************************************************************)
(* Registration: requies  optional lexer, domain name and    *)
(*   parser entry                                            *)
(*************************************************************)    
val of_stru :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> entry:Astf.stru Gramf.t ->
    unit -> unit
val of_pat :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> entry:Astf.pat Gramf.t
    -> unit -> unit
val of_clfield :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> entry:Astf.clfield Gramf.t
    -> unit -> unit
val of_case :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> entry:Astf.case Gramf.t
    -> unit -> unit 
val of_exp :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> entry:Astf.exp Gramf.t
    -> unit -> unit
        
val of_ep :
  lexer:(Location.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> entry:Astf.ep Gramf.t ->
    unit -> unit

(*************************************************************)
(* the same as above, allows a filter plugin though          *)    
(*************************************************************)        
val of_pat_with_filter :
    lexer:(loc -> char Streamf.t -> Tokenf.stream) ->
      name:Tokenf.name -> entry:'a Gramf.t -> filter:('a -> pat)
        -> unit -> unit 
val of_stru_with_filter :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name ->
  entry:'a Gramf.t -> filter:('a -> Astf.stru)
    -> unit -> unit
val of_clfield_with_filter :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name ->
  entry:'a Gramf.t -> filter:('a -> Astf.clfield)
    -> unit -> unit 
val of_case_with_filter :
  lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  name:Tokenf.name -> 
  entry:'a Gramf.t -> filter:('a -> Astf.case)
    -> unit -> unit
val of_exp_with_filter :
  lexer:(loc -> char Streamf.t -> Tokenf.stream)
  -> name:Tokenf.name
    -> entry:'a Gramf.t -> filter:('a -> exp)
      -> unit -> unit
              

val register : Tokenf.name * unit Tokenf.expand_fun -> unit

val handle_quot : Tokenf.quot -> unit    

val register_unit_parser :
    lexer:Tokenf.stream Tokenf.lex -> Tokenf.name * unit Gramf.t -> unit

val dump_directives : unit -> unit
