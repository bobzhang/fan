
(** Manage Fan's quotation *)
  
open FAst

val resolve_name : (* Locf.t -> *) Ftoken.name -> Ftoken.name option

val paths : Ftoken.domains list ref
    
val current_loc_name : string option ref

(** [add name exp] adds the quotation [name] associated with the  expander [exp]. *)
val add : Ftoken.name -> 'a Dyn_tag.t  -> 'a Ftoken.expand_fun  -> unit

    
(** [default] holds the default quotation name. *)
val default: Ftoken.name option ref

val set_default: Ftoken.name -> unit


(** [default_tbl] mapping [position] to the default quotation name
    it has higher precedence over default  *)

val map: Ftoken.name Mapf.String.t ref

    
(** [default_at_pos] set the default quotation name for specific pos*)
val default_at_pos: string -> Ftoken.name -> unit
    
val clear_map: unit -> unit
    
val clear_default: unit -> unit
    
val expand : Ftoken.quot -> 'a Dyn_tag.t-> 'a



(** [dump_file] optionally tells Fan to dump the
    result of an expander if this result is syntactically incorrect.
    If [None] (default), this result is not dumped. If [Some fname], the
    result is dumped in the file [fname]. *)
val dump_file : string option  ref



(** The raw quotation expander, register its type, laguage namespace and
    an expansion function *)
val add : Ftoken.domains * string -> 'a Dyn_tag.t -> 'a Ftoken.expand_fun -> unit    
(** theoretically you can use [mexp] which lift it into any type you can
   but we made a restriction here.
   [exp_filter] and [pat_filter] default to an id 
*)
val add_quotation:
    exp_filter:(ep -> exp) ->
      pat_filter:(ep -> pat) ->
        mexp:(Locf.t -> 'c -> ep) ->
          mpat:(Locf.t -> 'c -> ep) -> Ftoken.name -> 'c Fgram.t -> unit



(* FIXME revised parser *cannot* parse name:string -> unit*)
(*************************************************************)
(* Registration: requies  optional lexer, domain name and    *)
(*   parser entry                                            *)
(*************************************************************)    
val of_stru :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string -> entry:FAst.stru Gentry.t ->
    unit -> unit
val of_pat :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string -> entry:FAst.pat Gentry.t
    -> unit -> unit
val of_clfield :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string -> entry:FAst.clfield Gentry.t
    -> unit -> unit
val of_case :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string -> entry:FAst.case Gentry.t
    -> unit -> unit 
val of_exp :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string -> entry:FAst.exp Gentry.t
    -> unit -> unit 


(*************************************************************)
(* the same as above, allows a filter plugin though          *)    
(*************************************************************)        
val of_pat_with_filter :
    ?lexer:(loc -> char Fstream.t -> Ftoken.stream) ->
      name:Ftoken.domains * string -> entry:'a Gentry.t -> filter:('a -> pat)
        -> unit -> unit 
val of_stru_with_filter :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string ->
  entry:'a Gentry.t -> filter:('a -> FAst.stru)
    -> unit -> unit
val of_clfield_with_filter :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string ->
  entry:'a Gentry.t -> filter:('a -> FAst.clfield)
    -> unit -> unit 
val of_case_with_filter :
  ?lexer:(Locf.t -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string ->
  entry:'a Gentry.t -> filter:('a -> FAst.case)
    -> unit -> unit
val of_exp_with_filter :
  ?lexer:(loc -> char Fstream.t -> Ftoken.stream) ->
  name:Ftoken.domains * string -> entry:'a Gentry.t -> filter:('a -> exp)
    -> unit -> unit
              

