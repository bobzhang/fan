open Camlp4Ast
let rec  normalize_acc =
  
  function
  | Ast.IdAcc(_loc,i1,i2) ->
    Ast.ExAcc ((_loc,( (normalize_acc i1) ),( (normalize_acc i2) )))
  | Ast.IdApp(_loc,i1,i2) ->
    Ast.ExApp ((_loc,( (normalize_acc i1) ),( (normalize_acc i2) )))
  | (((Ast.IdAnt(_loc,_) |Ast.IdUid(_loc,_)) |Ast.IdLid(_loc,_)) as i) ->
    Ast.ExId ((_loc,i))
let rec  to_lid =
  
  function
  | Ast.IdAcc(_,_,i) -> (to_lid i)
  | Ast.IdLid(_,lid) -> lid
  | _ -> assert false
let rec  tvar_of_ident =
  
  function
  | (Ast.IdLid(_,x) |Ast.IdUid(_,x)) -> x
  | Ast.IdAcc(_,Ast.IdUid(_,x),xs) ->
    (x ^ ( ("__" ^ ( (tvar_of_ident xs) )) ))
  | _ -> (failwith "internal error in the Grammar extension")