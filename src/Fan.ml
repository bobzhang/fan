

open FanAst;
open Filters;  
include PreCast;

open AstQuotation;
open Lib.Meta;
open Syntax;
open LibUtil;
open AstQuotation;







let d = `Absolute ["Fan";"Lang"];


of_str_item_with_filter
  ~name:(d,"ocaml") ~entry:str_items
  ~filter:(fun s ->
    let _loc = loc_of s in 
    let v =  {:module_expr| struct $s end |} in
    (* let _ = Format.eprintf "@[%a@]@." Ast2pt.print_str_item s in *)
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
          end];
      code
    end);

of_expr ~name:(d,"fans") ~entry:Typehook.fan_quots ;
of_expr ~name:(d,"save") ~entry:Typehook.save_quot;
  
of_str_item ~name:(d,"include") ~entry:Typehook.include_quot;
let d = `Absolute ["Fan";"Lang";"Macro"];
  
of_expr_with_filter
    ~name:(d,"expr") ~entry:expr ~filter:(AstMacros.macro_expander#expr);

of_class_str_item_with_filter
    ~name:(d,"class_str_item") ~entry:class_str_item
    ~filter:(AstMacros.macro_expander#class_str_item);

of_str_item_with_filter
    ~name:(d,"str_item") ~entry:str_item
    ~filter:(AstMacros.macro_expander#str_item);


let d = `Absolute ["Fan";"Lang";"Meta"];

add_quotation (d,"sig_item") sig_item_quot
    ~mexpr:ME.meta_sig_item
    ~mpatt:MP.meta_sig_item
    ~expr_filter ~patt_filter ;
add_quotation (d,"str_item") str_item_quot
    ~mexpr:ME.meta_str_item
    ~mpatt:MP.meta_str_item
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"ctyp") ctyp_quot
    ~mexpr:ME.meta_ctyp
    ~mpatt:MP.meta_ctyp
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"patt") patt_quot
    ~mexpr:ME.meta_patt
    ~mpatt:MP.meta_patt
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"expr") expr_quot
    ~mexpr:ME.meta_expr
    ~mpatt:MP.meta_expr
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"module_type") module_type_quot
    ~mexpr:ME.meta_module_type
    ~mpatt:MP.meta_module_type
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"module_expr") module_expr_quot
    ~mexpr:ME.meta_module_expr
    ~mpatt:MP.meta_module_expr
    ~expr_filter
    ~patt_filter ;

add_quotation (d,"class_type") class_type_quot
    ~mexpr:ME.meta_class_type ~mpatt:MP.meta_class_type
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"class_expr") class_expr_quot
    ~mexpr:ME.meta_class_expr
    ~mpatt:MP.meta_class_expr
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"class_sig_item") class_sig_item_quot
    ~mexpr:ME.meta_class_sig_item
    ~mpatt:MP.meta_class_sig_item
    ~expr_filter
    ~patt_filter ;

add_quotation (d,"class_str_item") class_str_item_quot
    ~mexpr:ME.meta_class_str_item
    ~mpatt:MP.meta_class_str_item
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"with_constr") with_constr_quot
    ~mexpr:ME.meta_with_constr
    ~mpatt:MP.meta_with_constr
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"binding") binding_quot
    ~mexpr:ME.meta_binding
    ~mpatt:MP.meta_binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"rec_expr") rec_expr_quot
    ~mexpr:ME.meta_rec_expr
    ~mpatt:MP.meta_rec_expr
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"match_case") match_case_quot
    ~mexpr:ME.meta_match_case
    ~mpatt:MP.meta_match_case
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"module_binding") module_binding_quot
    ~mexpr:ME.meta_module_binding
    ~mpatt:MP.meta_module_binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"ident") ident_quot
    ~mexpr:ME.meta_ident
    ~mpatt:MP.meta_ident
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"rec_flag") rec_flag_quot
    ~mexpr:ME.meta_rec_flag
    ~mpatt:MP.meta_rec_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"private_flag") private_flag_quot
    ~mexpr:ME.meta_private_flag
    ~mpatt:MP.meta_private_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"row_var_flag") row_var_flag_quot
    ~mexpr:ME.meta_row_var_flag
    ~mpatt:MP.meta_row_var_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"mutable_flag") mutable_flag_quot
    ~mexpr:ME.meta_mutable_flag
    ~mpatt:MP.meta_mutable_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"virtual_flag") virtual_flag_quot
    ~mexpr:ME.meta_virtual_flag ~mpatt:MP.meta_virtual_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"override_flag") override_flag_quot
    ~mexpr:ME.meta_override_flag
    ~mpatt:MP.meta_override_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"direction_flag") direction_flag_quot
    ~mexpr:ME.meta_direction_flag
    ~mpatt:MP.meta_direction_flag
    ~expr_filter
    ~patt_filter ;

add (`Absolute ["Fan";"Lang"],"str") DynAst.expr_tag
  (fun _loc _loc_option s -> {:expr|$str:s|});

add (`Absolute ["Fan";"Lang"],"str") DynAst.str_item_tag
  (fun _loc _loc_option s -> {:str_item| $(exp:{:expr|$str:s|}) |});
  
Options.add
    ("-dlang",
     FanArg.String (fun s ->
       AstQuotation.default := FanToken.resolve_name (`Sub [],s))
       ," Set the default language");

let d = `Absolute ["Fan"; "Lang"; "Meta";"N"] ;
(*
  (* temporarily turned off, since it's not the core of Fan *)
add_quotation (d,"expr") expr_quot
    ~mexpr:(fun loc pexpr -> FanAstN.Expr.meta_expr loc (strip_loc_expr pexpr))
    ~mpatt:(fun loc ppatt -> FanAstN.Patt.meta_expr loc (strip_loc_expr ppatt))
    ~expr_filter
    ~patt_filter;
*)
(* let a = {:N.expr|a + b |}; *)
(* let a = {:expr|a + b|}; *)
  
(* Try to force linking for convenience *)  
open ParserListComprehension;
open ParserRevise;
open ParserMacro;
open ParserGrammar;
open ParserDebug;
open ParserStream;
open ParserLex;  
open AstInjection;
open AstTypeGen;
open CodeTemplate;

(* open FanEval; *)
