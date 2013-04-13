open Ast


(** The [loc] is the initial location. The option string is the optional name
      for the location variable. The string is the quotation contents. *)
type 'a expand_fun  = FanLoc.t -> string option  -> string -> 'a


val current_loc_name: string option ref

(** [add name exp] adds the quotation [name] associated with the  expander [exp]. *)
val add : FanToken.name -> 'a FanDyn.tag  -> 'a expand_fun  -> unit


(** [find name] returns the expander of the given quotation name. *)
(* val find : string -> 'a FanDyn.tag  -> 'a expand_fun  *)

    
(** [default] holds the default quotation name. *)
val default: FanToken.name ref

val set_default: FanToken.name -> unit

(* val resolve_name: FanToken.name -> FanToken.name     *)
(** [default_tbl] mapping [position] to the default quotation name
    it has higher precedence over default  *)
(* val default_tbl : (string, string)Hashtbl.t   *)
val map: FanToken.name LibUtil.SMap.t ref

    
(** [default_at_pos] set the default quotation name for specific pos*)
val default_at_pos: string -> FanToken.name -> unit
    
val clear_map: unit -> unit
    
val clear_default: unit -> unit
    
(** [parse_quotation_result parse_function loc position_tag quotation quotation_result]
  It's a parser wrapper, this function handles the error reporting for you. *)
val parse_quotation_result:
    (FanLoc.t -> string -> 'a) -> FanLoc.t -> FanToken.quotation -> string -> string -> 'a

(** function translating quotation names; default = identity *)
(* val translate : (string -> string) ref *)

val expand : FanLoc.t -> FanToken.quotation -> 'a FanDyn.tag  -> 'a

val expand_quotation :
    FanLoc.t ->
      expander:(FanLoc.t -> string option -> string -> 'a) ->
        string -> FanToken.quotation -> 'a

(** [dump_file] optionally tells Fan to dump the
    result of an expander if this result is syntactically incorrect.
    If [None] (default), this result is not dumped. If [Some fname], the
    result is dumped in the file [fname]. *)
val dump_file : string option  ref


    
(* theoretically you can use [mexp] which lift it into any type you can *)
val add_quotation:
    exp_filter:(ep(* 'a *) -> exp) ->
      pat_filter:(ep(* 'b *) -> pat) ->
        mexp:(FanLoc.t -> 'c -> ep(* 'a *)) ->
          mpat:(FanLoc.t -> 'c -> ep(* 'b *)) -> FanToken.name -> 'c Gram.t -> unit



(* BUG, revised parser can not parse name:string -> unit*)
val of_exp: name:FanToken.name -> entry: exp Gram.t  -> unit

val of_pat: name:FanToken.name -> entry: pat Gram.t  -> unit

val of_clfield: name:FanToken.name -> entry: clfield Gram.t  -> unit

val of_case: name:FanToken.name -> entry: case Gram.t  -> unit

val of_stru: name:FanToken.name -> entry: stru Gram.t  -> unit

val make_parser: 'a Gram.t -> FanLoc.t -> string option -> string -> 'a


val of_stru_with_filter: name:FanToken.name ->
  entry:stru Gram.t -> filter:(stru -> stru) -> unit

val of_pat_with_filter :
  name:FanToken.name -> entry:pat Gram.t -> filter:(pat -> pat) -> unit

val of_clfield_with_filter :
  name:FanToken.name
  -> entry:clfield Gram.t -> filter:(clfield -> clfield) -> unit

val of_case_with_filter :
  name:FanToken.name
  -> entry:case Gram.t -> filter:(case -> case) -> unit

val of_exp_with_filter :
    name:FanToken.name
  -> entry:exp Gram.t -> filter:(exp -> exp) -> unit
        
