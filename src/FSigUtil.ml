open FSig;

open AstLoc;

let str_item_from_module_types ~f:(aux:named_type -> typedecl)
    (x:module_types) : str_item =
  let _loc = FanLoc.ghost in
  match x with
  [ [] -> {:str_item| let _ = () |}
  | _ ->
      let xs : list str_item  = (List.map
       (fun
         [`Mutual tys -> {:str_item| type $(and_of_list (List.map aux tys)) |}
         |`Single ty ->
             {:str_item| type $(aux ty)|}] ) x ) in
      sem_of_list xs] ;




















