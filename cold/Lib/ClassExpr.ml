open Camlp4Ast
let rec fa al =
  function | Ast.CeApp (_loc,ce,a) -> fa (a :: al) ce | ce -> (ce, al)