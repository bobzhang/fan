open Ast
module MetaLocQuotation :
  sig
    val meta_loc_expr : loc -> loc -> expr
    val meta_loc_patt : loc -> 'a -> patt
  end

val antiquot_expander : parse_patt:(loc -> string -> patt) ->
  parse_expr:(loc -> string -> expr) ->  FanAst.map
