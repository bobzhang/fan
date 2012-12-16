open Ast

(** The [loc] is the initial location. The option string is the optional name
      for the location variable. The string is the quotation contents. *)
type 'a expand_fun  = FanLoc.t -> string option  -> string -> 'a


(** [add name exp] adds the quotation [name] associated with the  expander [exp]. *)
val add : string -> 'a DynAst.tag  -> 'a expand_fun  -> unit


(** [find name] returns the expander of the given quotation name. *)
(* val find : string -> 'a DynAst.tag  -> 'a expand_fun  *)

    
(** [default] holds the default quotation name. *)
val default : string ref 

(** [default_tbl] mapping position to the default quotation name
    it has higher precedence over default  *)
val default_tbl : (string, string)Hashtbl.t  

(** [default_at_pos] set the default quotation name for specific pos*)
val default_at_pos: string -> string -> unit
    

(** [parse_quotation_result parse_function loc position_tag quotation quotation_result]
  It's a parser wrapper, this function handles the error reporting for you. *)
val parse_quotation_result:
    (FanLoc.t -> string -> 'a) -> FanLoc.t -> FanToken.quotation -> string -> string -> 'a

(** function translating quotation names; default = identity *)
val translate : (string -> string) ref

val expand : FanLoc.t -> FanToken.quotation -> 'a DynAst.tag  -> 'a


(** [dump_file] optionally tells Camlp4 to dump the
    result of an expander if this result is syntactically incorrect.
    If [None] (default), this result is not dumped. If [Some fname], the
    result is dumped in the file [fname]. *)
val dump_file : string option  ref 

val add_quotation:
    expr_filter:('a -> Lib.Meta.Ast.expr) ->
      patt_filter:('b -> Lib.Meta.Ast.patt) ->
        mexpr:(FanLoc.t -> 'c -> 'a) ->
          mpatt:(FanLoc.t -> 'c -> 'b) -> string -> 'c Gram.t -> unit
(* val add_quotation: string ->  'a Gram.t ->  (FanLoc.t -> 'a -> Ast.expr) -> (FanLoc.t -> 'a -> Ast.patt) -> unit *)



(* BUG, revised parser can not parse name:string -> unit*)
val of_expr: name:string -> entry: expr Gram.t  -> unit

val of_patt: name:string -> entry: patt Gram.t  -> unit

val of_class_str_item: name:string -> entry: class_str_item Gram.t  -> unit

val of_match_case: name:string -> entry: match_case Gram.t  -> unit

val of_str_item: name:string -> entry: str_item Gram.t  -> unit

val make_parser: 'a Gram.t -> FanLoc.t -> string option -> string -> 'a

val of_str_item: name:string -> entry:str_item Gram.t -> unit

val of_str_item_with_filter: name:string ->
  entry:str_item Gram.t -> filter:(str_item -> str_item) -> unit


module MetaLocQuotation :
  sig
    val meta_loc_expr : loc -> loc -> expr
    val meta_loc_patt : loc -> 'a -> patt
  end
val antiquot_expander : parse_patt:(loc -> string -> patt) ->  parse_expr:(loc -> string -> expr) ->  Camlp4Ast.map
