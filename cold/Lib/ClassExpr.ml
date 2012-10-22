open Camlp4Ast
let rec  fa (al) =
  
  function
  | Ast.CeApp(_ , ce , a ) -> (fa ( a :: al  ) ce )
  | ce -> (ce , al )
