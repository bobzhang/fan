open FSig
open AstLoc
let str_item_from_module_types ~f:(aux : named_type -> typedecl) 
  (x : module_types) =
  (let _loc = FanLoc.ghost in
   match x with
   | [] -> `StExp (_loc, (`Id (_loc, (`Uid (_loc, "()")))))
   | _ ->
       let xs: str_item list =
         List.map
           (function
            | `Mutual tys -> `Type (_loc, (and_of_list (List.map aux tys)))
            | `Single ty -> `Type (_loc, (aux ty))) x in
       sem_of_list1 xs : str_item )