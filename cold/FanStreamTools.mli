type spat_comp =
    SpTrm of FanLoc.t * Ast.patt * Ast.exp option
  | SpNtr of FanLoc.t * Ast.patt * Ast.exp
  | SpStr of FanLoc.t * Ast.patt
type sexp_comp = SeTrm of FanLoc.t * Ast.exp | SeNtr of FanLoc.t * Ast.exp
val grammar_module_name : string ref
val gm : unit -> string
val strm_n : string
val peek_fun : Ast.loc -> Ast.exp
val junk_fun : Ast.loc -> Ast.exp
val empty : Ast.loc -> Ast.exp
val is_raise : Ast.exp -> bool
val is_raise_failure : Ast.exp -> bool
val handle_failure : Ast.exp -> bool
val is_constr_apply : Ast.exp -> bool
val subst : string -> Ast.exp -> Ast.exp
val subst_binding : string -> Ast.binding -> Ast.binding
val stream_pattern_component : Ast.exp -> Ast.exp -> spat_comp -> Ast.exp
val stream_pattern :
  Ast.loc ->
  Ast.patt option ->
  Ast.exp ->
  (Ast.exp option -> Ast.exp) ->
  (spat_comp * Ast.exp option) list -> Ast.exp
val stream_patterns_term :
  Ast.loc ->
  (unit -> Ast.exp) ->
  (Ast.patt * Ast.exp option * Ast.loc *
   (spat_comp * Ast.exp option) list * Ast.patt option * Ast.exp)
  list -> Ast.exp
val group_terms :
  ((spat_comp * 'a option) list * 'b * 'c) list ->
  (Ast.patt * Ast.exp option * FanLoc.t * (spat_comp * 'a option) list *
   'b * 'c)
  list * ((spat_comp * 'a option) list * 'b * 'c) list
val parser_cases :
  Ast.loc ->
  ((spat_comp * Ast.exp option) list * Ast.patt option * Ast.exp) list ->
  Ast.exp
val cparser :
  Ast.loc ->
  Ast.patt option ->
  ((spat_comp * Ast.exp option) list * Ast.patt option * Ast.exp) list ->
  Ast.exp
val cparser_match :
  Ast.loc ->
  Ast.exp ->
  Ast.patt option ->
  ((spat_comp * Ast.exp option) list * Ast.patt option * Ast.exp) list ->
  Ast.exp
val not_computing : Ast.exp -> bool
val is_cons_apply_not_computing : Ast.exp -> bool
val slazy : Ast.loc -> Ast.exp -> Ast.exp
val cstream : Ast.loc -> sexp_comp list -> Ast.exp
