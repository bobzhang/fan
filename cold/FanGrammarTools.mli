open FanGrammar
open Ast
val print_warning : FanLoc.t -> string -> unit

val prefix : string

val grammar_module_name : ident ref
val gm : unit -> ident
val mk_entry :
  name:name ->
  pos:expr option -> levels:level list -> entry
val mk_level :
  label:string option ->
  assoc:expr option -> rules:rule list -> level
val mk_rule :
  prod:symbol list -> action:expr option -> rule
val mk_symbol :
  ?pattern:patt option ->
  text:text -> styp:styp -> symbol
val string_of_patt : patt -> string

val check_not_tok : symbol -> unit
    
val new_type_var: unit -> string

val gensym: unit -> int ref
val gen_lid: unit -> string
    
val retype_rule_list_without_patterns:  loc -> rule list -> rule list

    
val make_ctyp : styp -> string -> ctyp

val text_of_action :
  loc ->
  symbol list -> ?action:expr -> string   -> string -> expr
val mk_srules :
  loc ->
  string ->
  rule list -> string -> (text list * expr) list
val make_expr : name -> string -> text -> expr
val make_expr_rules :
    loc -> name -> (text list * expr) list -> string -> expr

(* val expr_of_delete_rule : *)
val expr_delete_rule:
  loc -> name -> symbol list list  -> expr
      
val mk_name : loc -> ident -> name
val mk_slist :
  loc ->
  bool -> symbol option -> symbol -> text
val text_of_entry :   entry -> expr
val let_in_of_extend :
  loc ->
  ident option -> name list option -> expr -> expr
val text_of_functorial_extend :
  loc ->
  ident option ->
  name list option -> entry list -> expr
val mk_tok :
  loc ->
  ?restrict:expr ->
  pattern:patt -> styp -> symbol
val sfold :
  ?sep:symbol ->
  loc ->
  string list ->
  expr -> expr -> symbol -> symbol
