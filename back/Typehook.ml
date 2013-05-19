




  
(* Filter type definitions from mli file
   for simplicity, we only care about `toplevel type definitions'
   nested type definitions in module are not considered.
   {:sigi| type $x |}
 *)  
(* let filter_type_defs ?qualified () = object *)
(*   inherit Objs.map as super; *)
(*   val mutable type_defs = None ; *)
(*   method! sigi = with sigi fun *)
(*     [ *)
(*      ( {| val $_ : $_ |} | {| include $_ |} | {| external $_ : $_ = $_ |} *)
(*      | {|exception $_ |}  | {| class $_ |}  | {| class type $_ |} *)
(*      | {| # $_ |}  | {| module $_:$_ |}    | {| module type $_ = $_ |} *)
(*      | {| module rec $_ |}  | {| open $_ |} ) -> *)
(*          {| |} (\* For sigi, keep does not make sense. *\) *)
(*      | {@_| type $((`TyDcl (_loc,name,vars, ctyp, constraints) as x)) |} -> begin *)
(*          let res = *)
(*            match ctyp with *)
(*            [`TyEq(_,_,ctyp) -> Ctyp.qualified_app_list ctyp | _ -> None] in *)
(*          let x =  *)
(*            match (res (\* Ctyp.qualified_app_list ctyp *\), qualified)with *)
(*            [(Some ({:ident|$i.$_ |},ls),Some q) when *)
(*                 (Id.eq i q && Ctyp.eq_list ls vars )-> *)
(*                    (\* type u 'a = Loc.u 'a *\)        *)
(*                   `TyDcl _loc name vars {:ctyp||} constraints *)
(*                |(_,_) -> super#typedecl x ] in  *)
(*              let y = {:stru| type $x  |} in *)
(*              let () =  type_defs <- Some {:stru| $type_defs ; $y |} in       *)
(*              {| type $x |}   *)
(*      end *)
(*      | {| type $ty |} -> (\* `And case *\) begin *)
(*          let x = super#typedecl ty in *)
(*          let () = type_defs <- Some {:stru| $type_defs ; $({:stru|type $x |}) |} in *)
(*          {|type $x |}  *)
(*          end *)
(*      | x -> super#sigi x]; *)
(*   method! ident = fun *)
(*     [ {:ident| $x.$y |} as i -> *)
(*       match qualified with *)
(*       [Some q when Id.eq q  x -> super#ident y *)
(*       |_ -> super#ident i] *)
(*     | i -> super#ident i]; *)
(*   method! type_info = fun *)
(*     [ `TyMan(_loc,_,p1,ctyp)(\* {:ctyp| $_ == $ctyp |} *\) -> *)
(*       `TyRepr (_loc,p1,super#type_repr ctyp) *)
(*     | ty -> super#type_info ty]; *)
(*   method get_type_defs = type_defs; *)
(* end; *)
