open FSig
open LibUtil
open AstLoc
open Ast
let stru_from_module_types ~f:(aux : named_type -> typedecl) 
  (x : module_types) =
  (let _loc = FanLoc.ghost in
   match x with
   | [] -> (`StExp (_loc, (`Id (_loc, (`Uid (_loc, "()"))))) : Ast.stru )
   | _ ->
       let xs: stru list =
         List.map
           (function
            | `Mutual tys ->
                (`Type (_loc, (and_of_list (List.map aux tys))) : Ast.stru )
            | `Single ty -> (`Type (_loc, (aux ty)) : Ast.stru )) x in
       sem_of_list xs : stru )
let stru_from_ty ~f:(f : string -> stru)  (x : module_types) =
  (let tys: string list =
     List.concat_map
       (fun x  ->
          match x with
          | `Mutual tys -> List.map (fun ((x,_) : named_type)  -> x) tys
          | `Single (x,_) -> [x]) x in
   sem_of_list (List.map f tys) : stru )