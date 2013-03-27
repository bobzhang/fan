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
  of_stru_with_filter ~name:(d, "ocaml") ~entry:strus
    ~filter:(fun s  ->
               let _loc = loc_of s in
               let v = `Struct (_loc, s) in
               let module_exp = (Typehook.traversal ())#module_exp v in
               let code =
                 match module_exp with
                 | `Struct (_loc,item) -> item
                 | _ -> failwith "can not find items back " in
               if Typehook.show_code.contents
               then
                 (try FanBasic.p_stru Format.std_formatter code
                  with
                  | _ ->
                      prerr_endline &
                        ("There is a printer bugOur code generator may still work when Printer is brokenPlz send bug report to "
                           ^ FanConfig.bug_main_address));
               code)
let _ = of_exp ~name:(d, "fans") ~entry:Typehook.fan_quots
let _ = of_exp ~name:(d, "save") ~entry:Typehook.save_quot
let _ = of_stru ~name:(d, "include") ~entry:Typehook.include_quot
let d = `Absolute ["Fan"; "Lang"; "Macro"]
let _ =
  of_exp_with_filter ~name:(d, "exp") ~entry:exp
    ~filter:(AstMacros.macro_expander#exp)
let _ =
  of_cstru_with_filter ~name:(d, "cstru") ~entry:cstru
    ~filter:(AstMacros.macro_expander#cstru)
let _ =
  of_stru_with_filter ~name:(d, "stru") ~entry:stru
    ~filter:(AstMacros.macro_expander#stru)
let d = `Absolute ["Fan"; "Lang"; "Meta"]
let _ =
  add_quotation (d, "sig_item") sig_item_quot ~mexp:(Filters.me#sig_item)
    ~mpat:(Filters.mp#sig_item) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "stru") stru_quot ~mexp:(Filters.me#stru)
    ~mpat:(Filters.mp#stru) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "ctyp") ctyp_quot ~mexp:(Filters.me#ctyp)
    ~mpat:(Filters.mp#ctyp) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "pat") pat_quot ~mexp:(Filters.me#pat)
    ~mpat:(Filters.mp#pat) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "exp") exp_quot ~mexp:(Filters.me#exp)
    ~mpat:(Filters.mp#exp) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "module_type") module_type_quot
    ~mexp:(Filters.me#module_type) ~mpat:(Filters.mp#module_type) ~exp_filter
    ~pat_filter
let _ =
  add_quotation (d, "module_exp") module_exp_quot
    ~mexp:(Filters.me#module_exp) ~mpat:(Filters.mp#module_exp) ~exp_filter
    ~pat_filter
let _ =
  add_quotation (d, "class_type") class_type_quot
    ~mexp:(Filters.me#class_type) ~mpat:(Filters.mp#class_type) ~exp_filter
    ~pat_filter
let _ =
  add_quotation (d, "class_exp") class_exp_quot ~mexp:(Filters.me#class_exp)
    ~mpat:(Filters.mp#class_exp) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "class_sig_item") class_sig_item_quot
    ~mexp:(Filters.me#class_sig_item) ~mpat:(Filters.mp#class_sig_item)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "cstru") cstru_quot ~mexp:(Filters.me#cstru)
    ~mpat:(Filters.mp#cstru) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "with_constr") with_constr_quot
    ~mexp:(Filters.me#with_constr) ~mpat:(Filters.mp#with_constr) ~exp_filter
    ~pat_filter
let _ =
  add_quotation (d, "binding") binding_quot ~mexp:(Filters.me#binding)
    ~mpat:(Filters.mp#binding) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "rec_exp") rec_exp_quot ~mexp:(Filters.me#rec_exp)
    ~mpat:(Filters.mp#rec_exp) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "case") case_quot ~mexp:(Filters.me#case)
    ~mpat:(Filters.mp#case) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "module_binding") module_binding_quot
    ~mexp:(Filters.me#module_binding) ~mpat:(Filters.mp#module_binding)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "ident") ident_quot ~mexp:(Filters.me#ident)
    ~mpat:(Filters.mp#ident) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "rec_flag") rec_flag_quot ~mexp:(Filters.me#rec_flag)
    ~mpat:(Filters.mp#rec_flag) ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "private_flag") private_flag_quot
    ~mexp:(Filters.me#private_flag) ~mpat:(Filters.mp#private_flag)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "row_var_flag") row_var_flag_quot
    ~mexp:(Filters.me#row_var_flag) ~mpat:(Filters.mp#row_var_flag)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "mutable_flag") mutable_flag_quot
    ~mexp:(Filters.me#mutable_flag) ~mpat:(Filters.mp#mutable_flag)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "virtual_flag") virtual_flag_quot
    ~mexp:(Filters.me#virtual_flag) ~mpat:(Filters.mp#virtual_flag)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "override_flag") override_flag_quot
    ~mexp:(Filters.me#override_flag) ~mpat:(Filters.mp#override_flag)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "direction_flag") direction_flag_quot
    ~mexp:(Filters.me#direction_flag) ~mpat:(Filters.mp#direction_flag)
    ~exp_filter ~pat_filter
let _ =
  add_quotation (d, "or_ctyp") constructor_declarations
    ~mexp:(Filters.me#or_ctyp) ~mpat:(Filters.me#or_ctyp) ~exp_filter
    ~pat_filter
let _ =
  add_quotation (d, "row_field") row_field ~mexp:(Filters.me#row_field)
    ~mpat:(Filters.mp#row_field) ~exp_filter ~pat_filter
let efilter str e =
  let e = exp_filter e in
  let _loc = loc_of e in
  `Constraint
    (_loc, e,
      (`Id (_loc, (`Dot (_loc, (`Uid (_loc, "Ast")), (`Lid (_loc, str)))))))
let pfilter str e =
  let p = pat_filter e in
  let _loc = loc_of p in
  `Constraint
    (_loc, p,
      (`Id (_loc, (`Dot (_loc, (`Uid (_loc, "Ast")), (`Lid (_loc, str)))))))
let _ =
  add_quotation (d, "sig_item'") sig_item_quot ~mexp:(Filters.me#sig_item)
    ~mpat:(Filters.mp#sig_item) ~exp_filter:(efilter "sig_item")
    ~pat_filter:(pfilter "sig_item")
let _ =
  add_quotation (d, "stru'") stru_quot ~mexp:(Filters.me#stru)
    ~mpat:(Filters.mp#stru) ~exp_filter:(efilter "stru")
    ~pat_filter:(pfilter "stru")
let _ =
  add_quotation (d, "ctyp'") ctyp_quot ~mexp:(Filters.me#ctyp)
    ~mpat:(Filters.mp#ctyp) ~exp_filter:(efilter "ctyp")
    ~pat_filter:(pfilter "ctyp")
let _ =
  add_quotation (d, "pat'") pat_quot ~mexp:(Filters.me#pat)
    ~mpat:(Filters.mp#pat) ~exp_filter:(efilter "pat")
    ~pat_filter:(pfilter "pat")
let _ =
  add_quotation (d, "ep'") exp_quot ~mexp:(Filters.me#exp)
    ~mpat:(Filters.mp#exp) ~exp_filter:(efilter "ep")
    ~pat_filter:(pfilter "ep")
let _ =
  add_quotation (d, "exp'") exp_quot ~mexp:(Filters.me#exp)
    ~mpat:(Filters.mp#exp) ~exp_filter:(efilter "exp")
    ~pat_filter:(pfilter "exp")
let _ =
  add_quotation (d, "module_type'") module_type_quot
    ~mexp:(Filters.me#module_type) ~mpat:(Filters.mp#module_type)
    ~exp_filter:(efilter "module_type") ~pat_filter:(pfilter "module_type")
let _ =
  add_quotation (d, "module_exp'") module_exp_quot
    ~mexp:(Filters.me#module_exp) ~mpat:(Filters.mp#module_exp)
    ~exp_filter:(efilter "module_exp") ~pat_filter:(pfilter "module_exp")
let _ =
  add_quotation (d, "class_type'") class_type_quot
    ~mexp:(Filters.me#class_type) ~mpat:(Filters.mp#class_type)
    ~exp_filter:(efilter "class_type") ~pat_filter:(pfilter "class_type")
let _ =
  add_quotation (d, "class_exp'") class_exp_quot ~mexp:(Filters.me#class_exp)
    ~mpat:(Filters.mp#class_exp) ~exp_filter:(efilter "class_exp")
    ~pat_filter:(pfilter "class_exp")
let _ =
  add_quotation (d, "class_sig_item'") class_sig_item_quot
    ~mexp:(Filters.me#class_sig_item) ~mpat:(Filters.mp#class_sig_item)
    ~exp_filter:(efilter "class_sig_item")
    ~pat_filter:(pfilter "class_sig_item")
let _ =
  add_quotation (d, "cstru'") cstru_quot ~mexp:(Filters.me#cstru)
    ~mpat:(Filters.mp#cstru) ~exp_filter:(efilter "cstru")
    ~pat_filter:(pfilter "cstru")
let _ =
  add_quotation (d, "with_constr'") with_constr_quot
    ~mexp:(Filters.me#with_constr) ~mpat:(Filters.mp#with_constr)
    ~exp_filter:(efilter "with_constr") ~pat_filter:(pfilter "with_constr")
let _ =
  add_quotation (d, "binding'") binding_quot ~mexp:(Filters.me#binding)
    ~mpat:(Filters.mp#binding) ~exp_filter:(efilter "binding")
    ~pat_filter:(pfilter "binding")
let _ =
  add_quotation (d, "rec_exp'") rec_exp_quot ~mexp:(Filters.me#rec_exp)
    ~mpat:(Filters.mp#rec_exp) ~exp_filter:(efilter "rec_exp")
    ~pat_filter:(pfilter "rec_exp")
let _ =
  add_quotation (d, "case'") case_quot ~mexp:(Filters.me#case)
    ~mpat:(Filters.mp#case) ~exp_filter:(efilter "case")
    ~pat_filter:(pfilter "case")
let _ =
  add_quotation (d, "module_binding'") module_binding_quot
    ~mexp:(Filters.me#module_binding) ~mpat:(Filters.mp#module_binding)
    ~exp_filter:(efilter "module_binding")
    ~pat_filter:(pfilter "module_binding")
let _ =
  add_quotation (d, "ident'") ident_quot ~mexp:(Filters.me#ident)
    ~mpat:(Filters.mp#ident) ~exp_filter:(efilter "ident")
    ~pat_filter:(pfilter "ident")
let _ =
  add_quotation (d, "or_ctyp'") constructor_declarations
    ~mexp:(Filters.me#or_ctyp) ~mpat:(Filters.me#or_ctyp)
    ~exp_filter:(efilter "or_ctyp") ~pat_filter:(pfilter "or_ctyp")
let _ =
  add_quotation (d, "row_field'") row_field ~mexp:(Filters.me#row_field)
    ~mpat:(Filters.mp#row_field) ~exp_filter:(efilter "row_field")
    ~pat_filter:(pfilter "row_field")
let _ =
  add ((`Absolute ["Fan"; "Lang"]), "str") FanDyn.exp_tag
    (fun _loc  _loc_option  s  -> `Str (_loc, s))
let _ =
  add ((`Absolute ["Fan"; "Lang"]), "str") FanDyn.stru_tag
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