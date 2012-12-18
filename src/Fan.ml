


include PreCast;

open AstQuotation;
open Lib.Meta;
open Syntax;
open LibUtil;
module MetaQAst = Camlp4Ast.Meta.Make MetaLocQuotation;
module ME = MetaQAst.Expr;
module MP = MetaQAst.Patt;







of_str_item_with_filter ~name:"ocaml" ~entry:str_items
  ~filter:(fun s ->
    let _loc = Ast.loc_of_str_item s in 
    let v =  {:module_expr| struct $s end |} in
    let module_expr =
      (Typehook.traversal ())#module_expr v in
    let code = match module_expr with
    [ {:module_expr| struct $item end |}  -> item
    | _ -> failwith "can not find items back " ]  in
    begin
      if !Typehook.show_code then
        try
          FanBasic.p_str_item Format.std_formatter code
        with
          [_ -> begin
            prerr_endline &
            "There is a printer bug\
             Our code generator may still work when \
             Printer is broken\
             Plz send bug report to " ^ FanConfig.bug_main_address;
          end]
      else ();
      code
    end);

(* {:ocaml| type u = int; |}; *)
(* {:fan_quot| show_code on |}; *)

of_expr ~name:"fans" ~entry:Typehook.fan_quots ;
(* of_expr ~name:"macro.expr" ~entry: *)
of_expr_with_filter ~
    name:"macro.expr" ~entry:expr ~filter:(AstMacros.macro_expander#expr);

of_class_str_item_with_filter ~name:"macro.class_str_item" ~entry:class_str_item
    ~filter:(AstMacros.macro_expander#class_str_item);

of_str_item_with_filter ~name:"macro.str_item" ~entry:str_item
    ~filter:(AstMacros.macro_expander#str_item);
  
add_quotation "sig_item" sig_item_quot
    ~mexpr:ME.meta_sig_item
    ~mpatt:MP.meta_sig_item
    ~expr_filter ~patt_filter ;
add_quotation "str_item" str_item_quot
    ~mexpr:ME.meta_str_item
    ~mpatt:MP.meta_str_item
    ~expr_filter
    ~patt_filter ;
add_quotation "ctyp" ctyp_quot
    ~mexpr:ME.meta_ctyp
    ~mpatt:MP.meta_ctyp
    ~expr_filter
    ~patt_filter ;
add_quotation "patt" patt_quot
    ~mexpr:ME.meta_patt
    ~mpatt:MP.meta_patt
    ~expr_filter
    ~patt_filter ;
add_quotation "expr" expr_quot
    ~mexpr:ME.meta_expr
    ~mpatt:MP.meta_expr
    ~expr_filter
    ~patt_filter ;
add_quotation "module_type" module_type_quot
    ~mexpr:ME.meta_module_type
    ~mpatt:MP.meta_module_type
    ~expr_filter
    ~patt_filter ;
add_quotation "module_expr" module_expr_quot
    ~mexpr:ME.meta_module_expr
    ~mpatt:MP.meta_module_expr
    ~expr_filter
    ~patt_filter ;

add_quotation "class_type" class_type_quot
    ~mexpr:ME.meta_class_type ~mpatt:MP.meta_class_type
    ~expr_filter
    ~patt_filter ;
  
add_quotation "class_expr" class_expr_quot
    ~mexpr:ME.meta_class_expr
    ~mpatt:MP.meta_class_expr
    ~expr_filter
    ~patt_filter ;
  
add_quotation "class_sig_item" class_sig_item_quot
    ~mexpr:ME.meta_class_sig_item
    ~mpatt:MP.meta_class_sig_item
    ~expr_filter
    ~patt_filter ;

add_quotation "class_str_item" class_str_item_quot
    ~mexpr:ME.meta_class_str_item
    ~mpatt:MP.meta_class_str_item
    ~expr_filter
    ~patt_filter ;
  
add_quotation "with_constr" with_constr_quot
    ~mexpr:ME.meta_with_constr
    ~mpatt:MP.meta_with_constr
    ~expr_filter
    ~patt_filter ;
  
add_quotation "binding" binding_quot
    ~mexpr:ME.meta_binding
    ~mpatt:MP.meta_binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation "rec_binding" rec_binding_quot
    ~mexpr:ME.meta_rec_binding
    ~mpatt:MP.meta_rec_binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation "match_case" match_case_quot
    ~mexpr:ME.meta_match_case
    ~mpatt:MP.meta_match_case
    ~expr_filter
    ~patt_filter ;
  
add_quotation "module_binding" module_binding_quot
    ~mexpr:ME.meta_module_binding
    ~mpatt:MP.meta_module_binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation "ident" ident_quot
    ~mexpr:ME.meta_ident
    ~mpatt:MP.meta_ident
    ~expr_filter
    ~patt_filter ;
  
add_quotation "rec_flag" rec_flag_quot
    ~mexpr:ME.meta_rec_flag
    ~mpatt:MP.meta_rec_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation "private_flag" private_flag_quot
    ~mexpr:ME.meta_private_flag
    ~mpatt:MP.meta_private_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation "row_var_flag" row_var_flag_quot
    ~mexpr:ME.meta_row_var_flag
    ~mpatt:MP.meta_row_var_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation "mutable_flag" mutable_flag_quot
    ~mexpr:ME.meta_mutable_flag
    ~mpatt:MP.meta_mutable_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation "virtual_flag" virtual_flag_quot
    ~mexpr:ME.meta_virtual_flag ~mpatt:MP.meta_virtual_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation "override_flag" override_flag_quot
    ~mexpr:ME.meta_override_flag
    ~mpatt:MP.meta_override_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation "direction_flag" direction_flag_quot
    ~mexpr:ME.meta_direction_flag
    ~mpatt:MP.meta_direction_flag
    ~expr_filter
    ~patt_filter ;

add "str" DynAst.expr_tag
  (fun _loc _loc_option s -> {:expr|$str:s|});

add "str" DynAst.str_item_tag
  (fun _loc _loc_option s -> {:str_item| $(exp:{:expr|$str:s|}) |});
  
Options.add ("-dlang", FanArg.Set_string AstQuotation.default," Set the default language");

  
open ParserListComprehension;
open ParserRevise;
open ParserMacro;
open ParserGrammar;
open ParserDebug;
open ParserStream;
open ParserLex;  
open AstInjection;
