let app mt1 mt2 =
  match (mt1, mt2) with
  | ((i1 : Ast.module_type),(i2 : Ast.module_type)) ->
      (`App (_loc, i1, i2) : Ast.module_type )
  | _ -> invalid_arg "Fan_module_type app"

let acc mt1 mt2 =
  match (mt1, mt2) with
  | ((i1 : Ast.module_type),(i2 : Ast.module_type)) ->
      (`Dot (_loc, i1, i2) : Ast.module_type )
  | _ -> invalid_arg "ModuleType.acc"