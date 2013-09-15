
(** Manage Fan's quotation *)
  
open FAst




val resolve_name : FLoc.t -> Ftoken.name -> Ftoken.name

val paths : Ftoken.domains list ref
    
(** The [loc] is the initial location. The option string is the optional name
    for the [location variable]. The string is the quotation contents. 
    expand fun accepts [location] and [location label] and string   
    to generate an arbitrary value of type ['a] *)                     
type 'a expand_fun  = FLoc.t -> string option  -> string -> 'a


val current_loc_name : string option ref

(** [add name exp] adds the quotation [name] associated with the  expander [exp]. *)
val add : Ftoken.name -> 'a FDyn.tag  -> 'a expand_fun  -> unit


(** [find name] returns the expander of the given quotation name. *)
(* val find : string -> 'a FDyn.tag  -> 'a expand_fun  *)

    
(** [default] holds the default quotation name. *)
val default: Ftoken.name ref

val set_default: Ftoken.name -> unit

(* val resolve_name: Ftoken.name -> Ftoken.name     *)
(** [default_tbl] mapping [position] to the default quotation name
    it has higher precedence over default  *)
(* val default_tbl : (string, string)Hashtbl.t   *)
val map: Ftoken.name LibUtil.SMap.t ref

    
(** [default_at_pos] set the default quotation name for specific pos*)
val default_at_pos: string -> Ftoken.name -> unit
    
val clear_map: unit -> unit
    
val clear_default: unit -> unit
    
(** [parse_quotation_result parse_function loc position_tag quotation quotation_result]
  It's a parser wrapper, this function handles the error reporting for you. *)
(* val parse_quotation_result: *)
(*     (FLoc.t -> string -> 'a) -> FLoc.t -> Ftoken.quotation -> string -> string -> 'a *)

(** function translating quotation names; default = identity *)
(* val translate : (string -> string) ref *)


val expand : FLoc.t -> Ftoken.quot -> 'a FDyn.tag -> 'a



(** [dump_file] optionally tells Fan to dump the
    result of an expander if this result is syntactically incorrect.
    If [None] (default), this result is not dumped. If [Some fname], the
    result is dumped in the file [fname]. *)
val dump_file : string option  ref


    
(** theoretically you can use [mexp] which lift it into any type you can
   but we made a restriction here *)
val add_quotation:
    exp_filter:(ep -> exp) ->
      pat_filter:(ep -> pat) ->
        mexp:(FLoc.t -> 'c -> ep) ->
          mpat:(FLoc.t -> 'c -> ep) -> Ftoken.name -> 'c Fgram.t -> unit



(* FIXME revised parser can not parse name:string -> unit*)
val of_exp: name:Ftoken.name -> entry: exp Fgram.t  -> unit

val of_pat: name:Ftoken.name -> entry: pat Fgram.t  -> unit

val of_clfield: name:Ftoken.name -> entry: clfield Fgram.t  -> unit

val of_case: name:Ftoken.name -> entry: case Fgram.t  -> unit

val of_stru: name:Ftoken.name -> entry: stru Fgram.t  -> unit

val make_parser: 'a Fgram.t -> FLoc.t -> string option -> string -> 'a


val of_stru_with_filter: name:Ftoken.name ->
  entry:stru Fgram.t -> filter:(stru -> stru) -> unit

val of_pat_with_filter :
  name:Ftoken.name -> entry:pat Fgram.t -> filter:(pat -> pat) -> unit

val of_clfield_with_filter :
  name:Ftoken.name
  -> entry:clfield Fgram.t -> filter:(clfield -> clfield) -> unit

val of_case_with_filter :
  name:Ftoken.name
  -> entry:case Fgram.t -> filter:(case -> case) -> unit

val of_exp_with_filter :
    name:Ftoken.name
  -> entry:exp Fgram.t -> filter:(exp -> exp) -> unit
        
