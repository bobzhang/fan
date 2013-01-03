open Ast;
(* module Ast = FanAst; *)
open LibUtil;
#default_quotation "expr";;

(* external loc_of_ctyp : ctyp -> FanLoc.t = "%field0"; *)
(* external loc_of_patt : patt -> FanLoc.t = "%field0"; *)
(* external loc_of_expr : expr -> FanLoc.t = "%field0"; *)
(* external loc_of_module_type : module_type -> FanLoc.t = "%field0"; *)
(* external loc_of_module_expr : module_expr -> FanLoc.t = "%field0"; *)
(* external loc_of_sig_item : sig_item -> FanLoc.t = "%field0"; *)
(* external loc_of_str_item : str_item -> FanLoc.t = "%field0"; *)
(* external loc_of_class_type : class_type -> FanLoc.t = "%field0"; *)
(* external loc_of_class_sig_item : class_sig_item -> FanLoc.t = "%field0"; *)
(* external loc_of_class_expr : class_expr -> FanLoc.t = "%field0"; *)
(* external loc_of_class_str_item : class_str_item -> FanLoc.t = "%field0"; *)
(* external loc_of_with_constr : with_constr -> FanLoc.t = "%field0"; *)
(* external loc_of_binding : binding -> FanLoc.t = "%field0"; *)
(* external loc_of_rec_binding : rec_binding -> FanLoc.t = "%field0"; *)
(* external loc_of_module_binding : module_binding -> FanLoc.t = "%field0"; *)
(* external loc_of_match_case : match_case -> FanLoc.t = "%field0"; *)
(* external loc_of_ident : ident -> FanLoc.t = "%field0"; *)

DEFINE GETLOC(x) = loc_of_expr(x);


module Expr = struct
  INCLUDE "src/MetaTemplate.ml"; (* FIXME INCLUDE as a langauge :default *)
end;
#default_quotation "patt"  ;;


DEFINE GETLOC(x) = loc_of_patt(x);
module Patt = struct
  INCLUDE "src/MetaTemplate.ml";
end;
