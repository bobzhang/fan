let app mt1 mt2 =
  match (mt1, mt2) with
  | ((i1 : Ast.mtyp),(i2 : Ast.mtyp)) -> (`Apply (_loc, i1, i2) : Ast.mtyp )
  | _ -> invalid_arg "Fan_mtyp app"

let acc mt1 mt2 =
  match (mt1, mt2) with
  | ((i1 : Ast.mtyp),(i2 : Ast.mtyp)) -> (`Dot (_loc, i1, i2) : Ast.mtyp )
  | _ -> invalid_arg "ModuleType.acc"