
open StdLib;
include AstN;
(* {:fans|keep off; *)
(*  derive ((\* Map2 Fold2 Map Fold Print OPrint *\) OEq ); |}; *)

{:fans|keep off; derive ( (* MetaExpr MetaPatt *) OEq OPrint MetaExpr MetaPatt);|};

{:ocaml| {:include| "src/AstN.ml" |}; |};

(* let meta_ant = StdMeta.meta_ant; *)
module Expr = struct
  open StdMeta.Expr;
  __MetaExpr__;
end;
  
module Patt = struct
  open StdMeta.Patt;
  __MetaPatt__; 
end;














