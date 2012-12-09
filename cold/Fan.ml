include PreCast.Make(struct
  
  end)
open AstQuotation
open Lib.Meta
open Syntax
let _ =
  add_quotation "sig_item" sig_item_quot ~mexpr:ME.meta_sig_item
    ~mpatt:MP.meta_sig_item ~expr_filter ~patt_filter
let _ =
  add_quotation "str_item" str_item_quot ~mexpr:ME.meta_str_item
    ~mpatt:MP.meta_str_item ~expr_filter ~patt_filter
let _ =
  add_quotation "ctyp" ctyp_quot ~mexpr:ME.meta_ctyp ~mpatt:MP.meta_ctyp
    ~expr_filter ~patt_filter
let _ =
  add_quotation "patt" patt_quot ~mexpr:ME.meta_patt ~mpatt:MP.meta_patt
    ~expr_filter ~patt_filter
let _ =
  add_quotation "expr" expr_quot ~mexpr:ME.meta_expr ~mpatt:MP.meta_expr
    ~expr_filter ~patt_filter
let _ =
  add_quotation "module_type" module_type_quot ~mexpr:ME.meta_module_type
    ~mpatt:MP.meta_module_type ~expr_filter ~patt_filter
let _ =
  add_quotation "module_expr" module_expr_quot ~mexpr:ME.meta_module_expr
    ~mpatt:MP.meta_module_expr ~expr_filter ~patt_filter
let _ =
  add_quotation "class_type" class_type_quot ~mexpr:ME.meta_class_type
    ~mpatt:MP.meta_class_type ~expr_filter ~patt_filter
let _ =
  add_quotation "class_expr" class_expr_quot ~mexpr:ME.meta_class_expr
    ~mpatt:MP.meta_class_expr ~expr_filter ~patt_filter
let _ =
  add_quotation "class_sig_item" class_sig_item_quot
    ~mexpr:ME.meta_class_sig_item ~mpatt:MP.meta_class_sig_item ~expr_filter
    ~patt_filter
let _ =
  add_quotation "class_str_item" class_str_item_quot
    ~mexpr:ME.meta_class_str_item ~mpatt:MP.meta_class_str_item ~expr_filter
    ~patt_filter
let _ =
  add_quotation "with_constr" with_constr_quot ~mexpr:ME.meta_with_constr
    ~mpatt:MP.meta_with_constr ~expr_filter ~patt_filter
let _ =
  add_quotation "binding" binding_quot ~mexpr:ME.meta_binding
    ~mpatt:MP.meta_binding ~expr_filter ~patt_filter
let _ =
  add_quotation "rec_binding" rec_binding_quot ~mexpr:ME.meta_rec_binding
    ~mpatt:MP.meta_rec_binding ~expr_filter ~patt_filter
let _ =
  add_quotation "match_case" match_case_quot ~mexpr:ME.meta_match_case
    ~mpatt:MP.meta_match_case ~expr_filter ~patt_filter
let _ =
  add_quotation "module_binding" module_binding_quot
    ~mexpr:ME.meta_module_binding ~mpatt:MP.meta_module_binding ~expr_filter
    ~patt_filter
let _ =
  add_quotation "ident" ident_quot ~mexpr:ME.meta_ident ~mpatt:MP.meta_ident
    ~expr_filter ~patt_filter
let _ =
  add_quotation "rec_flag" rec_flag_quot ~mexpr:ME.meta_rec_flag
    ~mpatt:MP.meta_rec_flag ~expr_filter ~patt_filter
let _ =
  add_quotation "private_flag" private_flag_quot ~mexpr:ME.meta_private_flag
    ~mpatt:MP.meta_private_flag ~expr_filter ~patt_filter
let _ =
  add_quotation "row_var_flag" row_var_flag_quot ~mexpr:ME.meta_row_var_flag
    ~mpatt:MP.meta_row_var_flag ~expr_filter ~patt_filter
let _ =
  add_quotation "mutable_flag" mutable_flag_quot ~mexpr:ME.meta_mutable_flag
    ~mpatt:MP.meta_mutable_flag ~expr_filter ~patt_filter
let _ =
  add_quotation "virtual_flag" virtual_flag_quot ~mexpr:ME.meta_virtual_flag
    ~mpatt:MP.meta_virtual_flag ~expr_filter ~patt_filter
let _ =
  add_quotation "override_flag" override_flag_quot
    ~mexpr:ME.meta_override_flag ~mpatt:MP.meta_override_flag ~expr_filter
    ~patt_filter
let _ =
  add_quotation "direction_flag" direction_flag_quot
    ~mexpr:ME.meta_direction_flag ~mpatt:MP.meta_direction_flag ~expr_filter
    ~patt_filter
let _ =
  add "str" DynAst.expr_tag
    (fun _loc  _loc_option  s  -> Ast.ExStr (_loc, s))
let _ =
  add "str" DynAst.str_item_tag
    (fun _loc  _loc_option  s  -> Ast.StExp (_loc, (Ast.ExStr (_loc, s))))
let _ =
  Options.add
    ("-dlang", (FanArg.Set_string AstQuotation.default),
      " Set the default language")