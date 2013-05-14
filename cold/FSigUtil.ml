open LibUtil

open AstLib

open Ast

let stru_from_mtyps ~f:(aux : FSig.named_type -> typedecl)  (x : FSig.mtyps)
  =
  (let _loc = FanLoc.ghost in
   match x with
   | [] -> None
   | _ ->
       let xs: stru list =
         List.map
           (function
            | `Mutual tys ->
                (`Type (_loc, (and_of_list (List.map aux tys))) : Ast.stru )
            | `Single ty -> (`Type (_loc, (aux ty)) : Ast.stru )) x in
       Some (sem_of_list xs) : stru option )

let stru_from_ty ~f:(f : string -> stru)  (x : FSig.mtyps) =
  (let tys: string list =
     List.concat_map
       (function
        | `Mutual tys -> List.map (fun ((x,_) : FSig.named_type)  -> x) tys
        | `Single (x,_) -> [x]) x in
   sem_of_list (List.map f tys) : stru )