open Camlp4Ast
open LibUtil
open Format
let rec fa al =
  function | Ast.TyApp (_loc,f,a) -> fa (a :: al) f | f -> (f, al)
let rec to_var_list =
  function
  | Ast.TyApp (_loc,t1,t2) -> (to_var_list t1) @ (to_var_list t2)
  | Ast.TyQuo (_loc,s) -> [s]
  | _ -> assert false
let list_of_opt ot acc =
  match ot with | Ast.TyNil _loc -> acc | t -> list_of_ctyp t acc
let rec name_tags =
  function
  | Ast.TyApp (_loc,t1,t2) -> (name_tags t1) @ (name_tags t2)
  | Ast.TyVrn (_loc,s) -> [s]
  | _ -> assert false
let rec to_generalized =
  function
  | Ast.TyArr (_loc,t1,t2) ->
      let (tl,rt) = to_generalized t2 in ((t1 :: tl), rt)
  | t -> ([], t)
let to_string =
  ref
    (fun _  ->
       failwith "Ctyp.to_string foward declaration, not implemented yet")
let eprint: (Ast.ctyp -> unit) ref =
  ref
    (fun _  -> failwith "Ctyp.eprint foward declaration, not implemented yet")
let _loc = FanLoc.ghost
let arrow a b = Ast.TyArr (_loc, a, b)
let (|->) = arrow
let sta a b = Ast.TySta (_loc, a, b)
let sta_of_list = List.reduce_right sta
let arrow_of_list = List.reduce_right arrow
let app_arrow lst acc = List.fold_right arrow lst acc
let tuple_sta_of_list =
  function
  | [] -> invalid_arg "tuple_sta__of_list while list is empty"
  | x::[] -> x
  | xs -> Ast.TyTup (_loc, (sta_of_list xs))
let (<+) names ty =
  List.fold_right
    (fun name  acc  -> Ast.TyArr (_loc, (Ast.TyQuo (_loc, name)), acc)) names
    ty
let (+>) params base = List.fold_right arrow params base
let name_length_of_tydcl =
  function
  | Ast.TyDcl (_,name,tyvars,_,_) -> (name, (List.length tyvars))
  | tydcl ->
      invalid_arg
        ((sprintf "name_length_of_tydcl {|%s|}\n") &
           (to_string.contents tydcl))