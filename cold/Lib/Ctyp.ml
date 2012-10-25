open Camlp4Ast
let rec  fa (al) =
  
  (function
  | Ast.TyApp(_,f,a) -> (fa ( a::al ) f) | f -> (f,al))
let rec  to_var_list =
  
  (function
  | Ast.TyApp(_,t1,t2) -> (( (to_var_list t1) ) @ ( (to_var_list t2) ))
    | Ast.TyQuo(_,s) -> [s] | _ -> assert false)
let  list_of_opt (ot) (acc) =
  
  (match ot with
  | Ast.TyNil(_) -> acc | t -> (list_of_ctyp t acc))
let rec  name_tags =
  
  (function
  | Ast.TyApp(_,t1,t2) -> (( (name_tags t1) ) @ ( (name_tags t2) ))
    | Ast.TyVrn(_,s) -> [s] | _ -> assert false)
let rec  to_generalized =
  
  (function
  | Ast.TyArr(_,t1,t2) ->
    
    let  (tl,rt) = (to_generalized t2) in (( t1::tl ),rt) | t -> ([] ,t))