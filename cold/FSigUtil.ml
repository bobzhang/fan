open FSig
open FanAst
let str_item_from_module_types ~f:(aux : named_type -> ctyp) 
  (x : module_types) =
  (let _loc = FanLoc.ghost in
   sem_of_list
     (List.map
        (function
         | `Mutual tys -> `Type (_loc, (and_of_list (List.map aux tys)))
         | `Single ty -> `Type (_loc, (aux ty))) x) : str_item )