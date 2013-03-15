open FSig;

open AstLoc;

let stru_from_module_types ~f:(aux:named_type -> typedecl)
    (x:module_types) : stru =
  let _loc = FanLoc.ghost in
  match x with
  [ [] -> {:stru| let _ = () |}
  | _ ->
      let xs : list stru  = (List.map
       (fun
         [`Mutual tys -> {:stru| type $(and_of_list (List.map aux tys)) |}
         |`Single ty ->
             {:stru| type $(aux ty)|}] ) x ) in
      sem_of_list xs] ;




















