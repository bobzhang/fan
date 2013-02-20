open FSig
open FanAst
let str_item_from_module_types ~f:(aux : named_type -> typedecl) 
  (x : module_types) =
  (let _loc = FanLoc.ghost in
   let xs: str_item list =
     List.map
       (function
        | `Mutual tys -> `Type (_loc, (and_of_list (List.map aux tys)))
        | `Single ty -> `Type (_loc, (aux ty))) x in
   sem_of_list xs : str_item )