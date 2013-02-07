open Ast
module MetaLocQuotation : FanAst.META_LOC

val antiquot_expander : parse_patt:(loc -> string -> patt) ->
  parse_expr:(loc -> string -> expr) ->  FanAst.map
