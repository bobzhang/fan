open Camlp4Ast;

let rec fa al =  fun
  [ {:class_expr| $ce $a |}
    ->fa [a :: al] ce
  | ce -> (ce, al) ];

