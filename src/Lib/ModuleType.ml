
open Camlp4Ast;

let app mt1 mt2 =
  match (mt1, mt2) with
  [ ({:module_type@_loc| $id:i1 |}, {:module_type| $id:i2 |}) ->
    {:module_type| $(id:{:ident| $i1 $i2 |}) |}
  | _ -> raise Stream.Failure ]; (* FIXME raise Stream.Failure *)


let acc mt1 mt2 =
  match (mt1, mt2) with
  [ ({:module_type@_loc| $id:i1 |}, {:module_type| $id:i2 |}) ->
    {:module_type| $(id:{:ident| $i1.$i2 |}) |}
  | _ -> raise Stream.Failure ];
