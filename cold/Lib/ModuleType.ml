open Camlp4Ast
let app =
 fun mt1 ->
  fun mt2 ->
   (match (mt1 , mt2) with
    | (Ast.MtId (_loc , i1) , Ast.MtId (_ , i2)) ->
       (Ast.MtId (_loc , ( (Ast.IdApp (_loc , i1 , i2)) )))
    | _ -> (raise Stream.Failure ))
let acc =
 fun mt1 ->
  fun mt2 ->
   (match (mt1 , mt2) with
    | (Ast.MtId (_loc , i1) , Ast.MtId (_ , i2)) ->
       (Ast.MtId (_loc , ( (Ast.IdAcc (_loc , i1 , i2)) )))
    | _ -> (raise Stream.Failure ))