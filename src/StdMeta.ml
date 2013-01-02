
module Ast = Camlp4Ast;
open LibUtil;
#default_quotation "expr";;


DEFINE MKARRAY = Lib.Expr.mkarray;
DEFINE MKLIST = Lib.Expr.mklist;

module Expr = struct
  INCLUDE "src/MetaTemplate.ml"; (* FIXME INCLUDE as a langauge :default *)
end;
#default_quotation "patt"  ;;

DEFINE MKARRAY = Lib.Patt.mkarray;
DEFINE MKLIST = Lib.Patt.mklist;

module Patt = struct
  INCLUDE "src/MetaTemplate.ml";
end;
