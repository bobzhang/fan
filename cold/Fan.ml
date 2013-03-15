open FanOps
open FanAst
open Filters
include PreCast
open AstQuotation
open Lib.Meta
open Syntax
open LibUtil
open AstQuotation
let d = `Absolute ["Fan"; "Lang"]
let _ =
  of_str_item_with_filter ~name:(d, "ocaml") ~entry:str_items
    ~filter:(fun s  ->
               let _loc = loc_of s in
               let v = `Struct (_loc, s) in
               let module_expr = (Typehook.traversal ())#module_expr v in
               let code =
                 match module_expr with
                 | `Struct (_loc,item) -> item
                 | _ -> failwith "can not find items back " in
               if Typehook.show_code.contents
               then
                 (try FanBasic.p_str_item Format.std_formatter code
                  with
                  | _ ->
                      prerr_endline &
                        ("There is a printer bugOur code generator may still work when Printer is brokenPlz send bug report to "
                           ^ FanConfig.bug_main_address));
               code)
let _ = of_expr ~name:(d, "fans") ~entry:Typehook.fan_quots
let _ = of_expr ~name:(d, "save") ~entry:Typehook.save_quot
let _ = of_str_item ~name:(d, "include") ~entry:Typehook.include_quot
let d = `Absolute ["Fan"; "Lang"; "Macro"]
let _ =
  of_expr_with_filter ~name:(d, "expr") ~entry:expr
    ~filter:(AstMacros.macro_expander#expr)
let _ =
  of_class_str_item_with_filter ~name:(d, "class_str_item")
    ~entry:class_str_item ~filter:(AstMacros.macro_expander#class_str_item)
let _ =
  of_str_item_with_filter ~name:(d, "str_item") ~entry:str_item
    ~filter:(AstMacros.macro_expander#str_item)
let d = `Absolute ["Fan"; "Lang"; "Meta"]
let _ =
  add_quotation (d, "sig_item") sig_item_quot ~mexpr:(Filters.me#sig_item)
    ~mpatt:(Filters.mp#sig_item) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "str_item") str_item_quot ~mexpr:(Filters.me#str_item)
    ~mpatt:(Filters.mp#str_item) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "ctyp") ctyp_quot ~mexpr:(Filters.me#ctyp)
    ~mpatt:(Filters.mp#ctyp) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "patt") patt_quot ~mexpr:(Filters.me#patt)
    ~mpatt:(Filters.mp#patt) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "expr") expr_quot ~mexpr:(Filters.me#expr)
    ~mpatt:(Filters.mp#expr) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "module_type") module_type_quot
    ~mexpr:(Filters.me#module_type) ~mpatt:(Filters.mp#module_type)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "module_expr") module_expr_quot
    ~mexpr:(Filters.me#module_expr) ~mpatt:(Filters.mp#module_expr)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "class_type") class_type_quot
    ~mexpr:(Filters.me#class_type) ~mpatt:(Filters.mp#class_type)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "class_expr") class_expr_quot
    ~mexpr:(Filters.me#class_expr) ~mpatt:(Filters.mp#class_expr)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "class_sig_item") class_sig_item_quot
    ~mexpr:(Filters.me#class_sig_item) ~mpatt:(Filters.mp#class_sig_item)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "class_str_item") class_str_item_quot
    ~mexpr:(Filters.me#class_str_item) ~mpatt:(Filters.mp#class_str_item)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "with_constr") with_constr_quot
    ~mexpr:(Filters.me#with_constr) ~mpatt:(Filters.mp#with_constr)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "binding") binding_quot ~mexpr:(Filters.me#binding)
    ~mpatt:(Filters.mp#binding) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "rec_expr") rec_expr_quot ~mexpr:(Filters.me#rec_expr)
    ~mpatt:(Filters.mp#rec_expr) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "case") case_quot ~mexpr:(Filters.me#case)
    ~mpatt:(Filters.mp#case) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "module_binding") module_binding_quot
    ~mexpr:(Filters.me#module_binding) ~mpatt:(Filters.mp#module_binding)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "ident") ident_quot ~mexpr:(Filters.me#ident)
    ~mpatt:(Filters.mp#ident) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "rec_flag") rec_flag_quot ~mexpr:(Filters.me#rec_flag)
    ~mpatt:(Filters.mp#rec_flag) ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "private_flag") private_flag_quot
    ~mexpr:(Filters.me#private_flag) ~mpatt:(Filters.mp#private_flag)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "row_var_flag") row_var_flag_quot
    ~mexpr:(Filters.me#row_var_flag) ~mpatt:(Filters.mp#row_var_flag)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "mutable_flag") mutable_flag_quot
    ~mexpr:(Filters.me#mutable_flag) ~mpatt:(Filters.mp#mutable_flag)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "virtual_flag") virtual_flag_quot
    ~mexpr:(Filters.me#virtual_flag) ~mpatt:(Filters.mp#virtual_flag)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "override_flag") override_flag_quot
    ~mexpr:(Filters.me#override_flag) ~mpatt:(Filters.mp#override_flag)
    ~expr_filter ~patt_filter
let _ =
  add_quotation (d, "direction_flag") direction_flag_quot
    ~mexpr:(Filters.me#direction_flag) ~mpatt:(Filters.mp#direction_flag)
    ~expr_filter ~patt_filter
let _ =
  add ((`Absolute ["Fan"; "Lang"]), "str") DynAst.expr_tag
    (fun _loc  _loc_option  s  -> `Str (_loc, s))
let _ =
  add ((`Absolute ["Fan"; "Lang"]), "str") DynAst.str_item_tag
    (fun _loc  _loc_option  s  -> `StExp (_loc, (`Str (_loc, s))))
let _ =
  Options.add
    ("-dlang",
      (FanArg.String
         (fun s  ->
            AstQuotation.default := (FanToken.resolve_name ((`Sub []), s)))),
      " Set the default language")
let d = `Absolute ["Fan"; "Lang"; "Meta"; "N"]
open ParserRevise
open ParserMacro
open ParserGrammar
open ParserStream
open ParserLex
open AstInjection
open AstTypeGen
open CodeTemplate