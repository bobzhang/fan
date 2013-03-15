
open FanOps;
open FanAst;
open Filters;  
include PreCast;

open AstQuotation;
open Lib.Meta;
open Syntax;
open LibUtil;
open AstQuotation;







let d = `Absolute ["Fan";"Lang"];


of_stru_with_filter
  ~name:(d,"ocaml") ~entry:strus
  ~filter:(fun s ->
    let _loc = loc_of s in 
    let v =  `Struct(_loc,s)in
    (* let _ = Format.eprintf "@[%a@]@." Ast2pt.print_stru s in *)
    let module_expr =
      (Typehook.traversal ())#module_expr v in
    let code = match module_expr with
    [ `Struct(_loc,item)  -> item
    | _ -> failwith "can not find items back " ]  in
    begin
      if !Typehook.show_code then
        try
          FanBasic.p_stru Format.std_formatter code
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

of_stru ~name:(d,"include") ~entry:Typehook.include_quot;
let d = `Absolute ["Fan";"Lang";"Macro"];
  
of_expr_with_filter
    ~name:(d,"expr") ~entry:expr ~filter:(AstMacros.macro_expander#expr);

of_cstru_with_filter
    ~name:(d,"cstru") ~entry:cstru
    ~filter:(AstMacros.macro_expander#cstru);

of_stru_with_filter
    ~name:(d,"stru") ~entry:stru
    ~filter:(AstMacros.macro_expander#stru);


let d = `Absolute ["Fan";"Lang";"Meta"];



add_quotation (d,"sig_item") sig_item_quot
    ~mexpr:Filters.me#sig_item
    ~mpatt:Filters.mp#sig_item
    ~expr_filter ~patt_filter ;
add_quotation (d,"stru") stru_quot
    ~mexpr:Filters.me#stru
    ~mpatt:Filters.mp#stru
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"ctyp") ctyp_quot
    ~mexpr:Filters.me#ctyp
    ~mpatt:Filters.mp#ctyp
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"patt") patt_quot
    ~mexpr:Filters.me#patt
    ~mpatt:Filters.mp#patt
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"expr") expr_quot
    ~mexpr:Filters.me#expr
    ~mpatt:Filters.mp#expr
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"module_type") module_type_quot
    ~mexpr:Filters.me#module_type
    ~mpatt:Filters.mp#module_type
    ~expr_filter
    ~patt_filter ;
add_quotation (d,"module_expr") module_expr_quot
    ~mexpr:Filters.me#module_expr
    ~mpatt:Filters.mp#module_expr
    ~expr_filter
    ~patt_filter ;

add_quotation (d,"class_type") class_type_quot
    ~mexpr:Filters.me#class_type ~mpatt:Filters.mp#class_type
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"class_expr") class_expr_quot
    ~mexpr:Filters.me#class_expr
    ~mpatt:Filters.mp#class_expr
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"class_sig_item") class_sig_item_quot
    ~mexpr:Filters.me#class_sig_item
    ~mpatt:Filters.mp#class_sig_item
    ~expr_filter
    ~patt_filter ;

add_quotation (d,"cstru") cstru_quot
    ~mexpr:Filters.me#cstru
    ~mpatt:Filters.mp#cstru
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"with_constr") with_constr_quot
    ~mexpr:Filters.me#with_constr
    ~mpatt:Filters.mp#with_constr
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"binding") binding_quot
    ~mexpr:Filters.me#binding
    ~mpatt:Filters.mp#binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"rec_expr") rec_expr_quot
    ~mexpr:Filters.me#rec_expr
    ~mpatt:Filters.mp#rec_expr
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"case") case_quot
    ~mexpr:Filters.me#case
    ~mpatt:Filters.mp#case
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"module_binding") module_binding_quot
    ~mexpr:Filters.me#module_binding
    ~mpatt:Filters.mp#module_binding
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"ident") ident_quot
    ~mexpr:Filters.me#ident
    ~mpatt:Filters.mp#ident
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"rec_flag") rec_flag_quot
    ~mexpr:Filters.me#rec_flag
    ~mpatt:Filters.mp#rec_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"private_flag") private_flag_quot
    ~mexpr:Filters.me#private_flag
    ~mpatt:Filters.mp#private_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"row_var_flag") row_var_flag_quot
    ~mexpr:Filters.me#row_var_flag
    ~mpatt:Filters.mp#row_var_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"mutable_flag") mutable_flag_quot
    ~mexpr:Filters.me#mutable_flag
    ~mpatt:Filters.mp#mutable_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"virtual_flag") virtual_flag_quot
    ~mexpr:Filters.me#virtual_flag ~mpatt:Filters.mp#virtual_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"override_flag") override_flag_quot
    ~mexpr:Filters.me#override_flag
    ~mpatt:Filters.mp#override_flag
    ~expr_filter
    ~patt_filter ;
  
add_quotation (d,"direction_flag") direction_flag_quot
    ~mexpr:Filters.me#direction_flag
    ~mpatt:Filters.mp#direction_flag
    ~expr_filter
    ~patt_filter ;

add (`Absolute ["Fan";"Lang"],"str") DynAst.expr_tag
  (fun _loc _loc_option s -> {:expr|$str:s|});

add (`Absolute ["Fan";"Lang"],"str") DynAst.stru_tag
  (fun _loc _loc_option s ->
    `StExp(_loc,{:expr|$str:s|})(* {:stru| $(exp:{:expr|$str:s|}) |} *));
  
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
(* open ParserListComprehension; *)
open ParserRevise;
open ParserMacro;
open ParserGrammar;

open ParserStream;
open ParserLex;  
open AstInjection;
open AstTypeGen;
open CodeTemplate;

(* open FanEval; *)
