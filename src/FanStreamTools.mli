type spat_comp =
    SpTrm of FanLoc.t * Ast.patt * Ast.expr option
  | SpNtr of FanLoc.t * Ast.patt * Ast.expr
  | SpStr of FanLoc.t * Ast.patt
type sexp_comp = SeTrm of FanLoc.t * Ast.expr | SeNtr of FanLoc.t * Ast.expr
val grammar_module_name : string ref
val gm : unit -> string
val strm_n : string
val peek_fun : Ast.loc -> Ast.expr
val junk_fun : Ast.loc -> Ast.expr
val empty : Ast.loc -> Ast.expr
val is_raise : Ast.expr -> bool
val is_raise_failure : Ast.expr -> bool
val handle_failure : Ast.expr -> bool
val is_constr_apply : Ast.expr -> bool
val subst : string -> Ast.expr -> Ast.expr
val subst_binding : string -> Ast.binding -> Ast.binding
val stream_pattern_component : Ast.expr -> Ast.expr -> spat_comp -> Ast.expr
val stream_pattern :
  Ast.loc ->
  Ast.patt option ->
  Ast.expr ->
  (Ast.expr option -> Ast.expr) ->
  (spat_comp * Ast.expr option) list -> Ast.expr
val stream_patterns_term :
  Ast.loc ->
  (unit -> Ast.expr) ->
  (Ast.patt * Ast.expr option * Ast.loc *
   (spat_comp * Ast.expr option) list * Ast.patt option * Ast.expr)
  list -> Ast.expr
val group_terms :
  ((spat_comp * 'a option) list * 'b * 'c) list ->
  (Ast.patt * Ast.expr option * FanLoc.t * (spat_comp * 'a option) list *
   'b * 'c)
  list * ((spat_comp * 'a option) list * 'b * 'c) list
val parser_cases :
  Ast.loc ->
  ((spat_comp * Ast.expr option) list * Ast.patt option * Ast.expr) list ->
  Ast.expr
val cparser :
  Ast.loc ->
  Ast.patt option ->
  ((spat_comp * Ast.expr option) list * Ast.patt option * Ast.expr) list ->
  Ast.expr
val cparser_match :
  Ast.loc ->
  Ast.expr ->
  Ast.patt option ->
  ((spat_comp * Ast.expr option) list * Ast.patt option * Ast.expr) list ->
  Ast.expr
val not_computing : Ast.expr -> bool
val is_cons_apply_not_computing : Ast.expr -> bool
val slazy : Ast.loc -> Ast.expr -> Ast.expr
val cstream : Ast.loc -> sexp_comp list -> Ast.expr
