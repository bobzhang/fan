open Ast;
(* module Ast = FanAst; *)
open LibUtil;
#default_quotation "expr";;


DEFINE GETLOC(x) = loc_of_expr(x);


module Expr = struct
  INCLUDE "src/MetaTemplate.ml"; (* FIXME INCLUDE as a langauge :default *)
end;
#default_quotation "patt"  ;;


DEFINE GETLOC(x) = loc_of_patt(x);
module Patt = struct
  INCLUDE "src/MetaTemplate.ml";
end;


