(* {:pat| (#$id:x as y)|} *)
(* let of_stru = function *)
(*   |`Type(_,x) -> x *)
(*   | t -> FanLoc.errorf (loc_of t) "Ctyp.of_stru %s" (ObjsN.dump_stru t)  *)
(*
  @raise Invalid_argument  when the input is not a type declaration

  {[
  
  (fun [ <:stru<type .$x$. >> -> ty_name_of_tydcl x  |> eprint ])
  <:stru< type list 'a  = [A of int | B of 'a] >>;

  list 'a
  ]}
 *)  
(* let ty_name_of_tydcl  (x:typedecl) = *)
(*   let _loc = FanLoc.ghost in *)
(*   match x with  *)
(*   | `TyDcl (_, `Lid(_,name), tyvars, _, _) -> *)
(*       let tyvars = *)
(*         match tyvars with *)
(*         | `None _ -> [] *)
(*         |`Some(_,xs) -> (list_of_com xs [] :>  ctyp list)  in *)
(*       appl_of_list ( {| $lid:name |} :: tyvars) *)
(*   | tydcl -> *)
(*       failwithf "ctyp_of_tydcl{|%s|}\n" (ObjsN.dump_typedecl tydcl) *)
        

