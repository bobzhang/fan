open Camlp4Ast
let app0 mt1 mt2 =
  match (mt1, mt2) with
  | (Ast.MtId (_loc,i1),Ast.MtId (_,i2)) ->
      Ast.MtId (_loc, (Ast.IdApp (_loc, i1, i2)))
  | _ -> raise Stream.Failure
let acc0 mt1 mt2 =
  match (mt1, mt2) with
  | (Ast.MtId (_loc,i1),Ast.MtId (_,i2)) ->
      Ast.MtId (_loc, (Ast.IdAcc (_loc, i1, i2)))
  | _ -> raise Stream.Failure
let app mt1 mt2 =
  match (mt1, mt2) with
  | (Ast.MtId (_loc,i1),Ast.MtId (_,i2)) ->
      Ast.MtId (_loc, (Ast.IdApp (_loc, i1, i2)))
  | _ -> invalid_arg "Fan_module_type app"
let acc mt1 mt2 =
  match (mt1, mt2) with
  | (Ast.MtId (_loc,i1),Ast.MtId (_,i2)) ->
      Ast.MtId (_loc, (Ast.IdAcc (_loc, i1, i2)))
  | _ -> invalid_arg "ModuleType.acc"