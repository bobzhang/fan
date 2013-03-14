

open Ast
val gen_tuple_abbrev: 
arity:int ->
  annot:ctyp ->
  destination:FSig.destination ->
  Ast.ident ->
  Ast.expr ->
  case
