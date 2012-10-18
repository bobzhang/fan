open Camlp4Ast

let rec fa =
 fun al ->
  function
  | Ast.CeApp (_, ce, a) -> (fa ( ( a ) :: al  ) ce)
  | ce -> (ce, al)
