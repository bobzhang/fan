open FanGrammar
val print_warning : FanLoc.t -> string -> unit
val split_ext : bool ref
val prefix : string
val meta_action : bool ref
val grammar_module_name : Ast.ident ref
val gm : unit -> Ast.ident
val mk_entry :
  name:name ->
  pos:Ast.expr option -> levels:level list -> entry
val mk_level :
  label:string option ->
  assoc:Ast.expr option -> rules:rule list -> level
val mk_rule :
  prod:symbol list -> action:Ast.expr option -> rule
val mk_symbol :
  ?pattern:Ast.patt option ->
  used:string list ->
  text:text -> styp:styp -> symbol
val string_of_patt : Camlp4Ast.Ast.patt -> string

val check_not_tok : symbol -> unit
    
val new_type_var : unit -> string
val used_of_rule_list : rule list -> string list
val retype_rule_list_without_patterns :
  Ast.loc -> rule list -> rule list
exception NotneededTyping
val make_ctyp : styp -> string -> Ast.ctyp option
val make_ctyp_patt :
  styp -> string -> Ast.patt -> Ast.patt
val make_ctyp_expr :
  styp -> string -> Ast.expr -> Ast.expr
val text_of_action :
  Ast.loc ->
  symbol list -> string -> Ast.expr option -> string -> Ast.expr
val srules :
  Ast.loc ->
  string ->
  rule list -> string -> (text list * Ast.expr) list
val make_expr : name -> string -> text -> Ast.expr
val make_expr_rules :
  loc ->
  name ->
  (text list * Ast.expr) list -> string -> Ast.expr
val expr_of_delete_rule :
  Ast.loc -> name -> symbol list -> Ast.expr * Ast.expr
val mk_name : Ast.loc -> Ast.ident -> name
val slist :
  loc ->
  bool -> symbol option -> symbol -> text
val text_of_entry :
  Ast.loc -> entry -> Ast.expr * Ast.expr * Ast.expr
val let_in_of_extend :
  Ast.loc ->
  Ast.ident option -> name list option -> Ast.expr -> Ast.expr
val text_of_functorial_extend :
  Ast.loc ->
  Ast.ident option ->
  name list option -> entry list -> Ast.expr
val mk_tok :
  Ast.loc ->
  ?restrict:Ast.expr ->
  pattern:Ast.patt -> styp -> symbol
val sfold :
  ?sep:symbol ->
  loc ->
  string list ->
  Ast.expr -> Ast.expr -> symbol -> symbol
