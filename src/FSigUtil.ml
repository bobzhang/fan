open FSig;
open FanAst;
let str_item_from_module_types ~f:(aux:named_type -> ctyp)
    (x:module_types) : str_item =
  let _loc = FanLoc.ghost in
  sem_of_list
    (List.map
       (fun
         [`Mutual tys -> {:str_item| type $(and_of_list (List.map aux tys)) |}
         |`Single ty ->
             {:str_item| type $(aux ty)|}] ) x );




















