open FSig;
open LibUtil;
open AstLoc;
open Ast;
let stru_from_mtyps ~f:(aux:named_type -> typedecl)
    (x:mtyps) : stru =
  let _loc = FanLoc.ghost in
  match x with
  [ [] -> {:stru'| let _ = () |}
  | _ ->
      let xs : list stru  = (List.map
       (fun
         [`Mutual tys -> {:stru'| type $(and_of_list (List.map aux tys)) |}
         |`Single ty ->
             {:stru'| type $(aux ty)|}] ) x ) in
      sem_of_list xs] ;

let stru_from_ty ~f:(f:string -> stru) (x:mtyps) : stru  =     
  let tys : list string =
    List.concat_map
      (fun x -> match x with
      [`Mutual tys -> List.map (fun ((x,_):named_type) -> x ) tys
      |`Single (x,_) -> [x] ]) x in
  sem_of_list (List.map f tys);



















