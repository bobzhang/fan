open Camlp4Ast;
#default_quotation "class_expr";;
(* INCLUDE "src/Lib/CommonStructure.ml"; *)

let rec view_app al =  fun
  [ {| $ce $a |} ->view_app [a :: al] ce
  | ce -> (ce, al) ];

