
(*
  We split it in another file, because
  the generated code based on this file has a dependency on
  camlp4
 *)
<:fan< lang "fan" ;>>;

open Camlp4.PreCast;
open Lib_common;
module MetaExpr= struct
  type t = Ast.expr;
  open Fan_expr;
  << lang_at "expr" "expr"; >> ;
  <:include_ml<"meta_template.ml";  >>;
end;

module MetaPatt = struct
  open Fan_patt;
  type t = Ast.patt;
  <<
  lang_at "expr" "patt"; >> ;
  <:include_ml< "meta_template.ml";  >>;
end;

  


















