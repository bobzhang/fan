open Camlp4Ast
let rec view_app al =
  function | Ast.CeApp (_loc,ce,a) -> view_app (a :: al) ce | ce -> (ce, al)