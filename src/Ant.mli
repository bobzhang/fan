open Ast

module LocExpr: FanAst.META_LOC
module LocPatt: FanAst.META_LOC    

val antiquot_expander : parse_patt:(loc -> string -> patt) ->
  parse_expr:(loc -> string -> expr) ->  Objs.map
