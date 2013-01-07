
(* open FanAst; *)
#default_quotation "module_type";;

let app0 mt1 mt2 =
  match (mt1, mt2) with
  [ ({| $id:i1 |}, {@_| $id:i2 |}) ->
    {| $(id:{:ident| ($i1 $i2) |}) |}
  | _ -> raise Stream.Failure ]; (* FIXME raise Stream.Failure *)


let acc0 mt1 mt2 =
  match (mt1, mt2) with
  [ ({| $id:i1 |}, {@_| $id:i2 |}) ->  {| $(id:{:ident| $i1.$i2 |}) |}
  | _ -> raise Stream.Failure ];


let app  mt1 mt2 =
  match (mt1, mt2) with
  [ ({| $(id:i1) |}, {@_| $id:i2 |}) -> {| $(id: {:ident| ($i1 $i2) |}) |}
  | _ -> invalid_arg "Fan_module_type app" ];
(**
   This is the place where [IdApp] makes sense
   {[
   app {| A |} {| B |};

   MtId  (IdApp  (Uid  "A") (Uid  "B"))
   ]}
   Here we need define [module_type_app], since
   {[
     |	IdApp of loc * ident* ident
     |	ExApp of loc * expr * expr
   ]}
   but for module_expr
   {[
     |	Id of loc * ident
     |	MeApp of loc * module_expr * module_expr
   ]}
   since we require that for module_type_app operation, only
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
















    
