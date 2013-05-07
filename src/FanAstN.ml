
open StdLib;
include AstN;
(* {:fans|keep off; *)
(*  derive ((\* Map2 Fold2 Map Fold Print OPrint *\) OEq ); |}; *)

{:fans|keep off; derive ( OEq OPrint MetaExpr);|};;

{:ocaml| {:include| "src/AstN.ml" |} |};;

__MetaExpr__;














