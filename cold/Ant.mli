open Ast

(* module LocExpr: FanAst.META_LOC *)
(* module LocPatt: FanAst.META_LOC     *)

val antiquot_expander : parse_pat:(loc -> string -> pat) ->
  parse_exp:(loc -> string -> exp) ->  Objs.map
