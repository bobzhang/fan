
(* open FanAst; *)
#default_quotation "mtyp'";;



let app  mt1 mt2 =
  match (mt1, mt2) with
  [ ({| $(id:i1) |}, {@_| $id:i2 |}) -> {| $(id: {:ident| ($i1 $i2) |}) |}
  | _ -> invalid_arg "Fan_mtyp app" ];
(**
   This is the place where [App] makes sense
   {[
   app {| A |} {| B |};

   MtId  (App  (Uid  "A") (Uid  "B"))
   ]}
   Here we need define [mtyp_app], since
   {[
     |	App of loc * ident* ident
     |	App of loc * exp * exp
   ]}
   but for module_exp
   {[
     |	Id of loc * ident
   |	App of loc * module_exp * module_exp
   ]}
   since we require that for mtyp_app operation, only
   Id can be used as app operation.
*)      


let acc mt1 mt2 =
  match (mt1, mt2) with
  [ ({| $id:i1 |}, {@_| $id:i2 |}) -> {| $(id:{:ident| $i1.$i2 |}) |}
  | _ -> invalid_arg "ModuleType.acc"];
    
(**
   {[
   acc {| A |} {| B |};
   MtId  (IdAcc  (Uid  "A") (Uid  "B"))
   ]}
 *)      
















    
