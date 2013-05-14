open LibUtil
open AstLib
open Ast
  
let stru_from_mtyps ~f:(aux:FSig.named_type -> typedecl)
    (x:FSig.mtyps) : stru option =
  let _loc = FanLoc.ghost in with stru
  match x with
  | [] -> None
  | _ ->
      let xs : stru list   =
        (List.map
           (function
             |`Mutual tys -> {| type $(and_of_list (List.map aux tys)) |}
             |`Single ty ->
                 {| type $(aux ty)|} ) x ) in
      Some (sem_of_list xs )

let stru_from_ty ~f:(f:string -> stru) (x:FSig.mtyps) : stru  =     
  let tys : string list  =
    List.concat_map
      (function
        |`Mutual tys -> List.map (fun ((x,_):FSig.named_type) -> x ) tys
        |`Single (x,_) -> [x] ) x in
  sem_of_list (List.map f tys)



















