
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
    let module_exp =
      (Typehook.traversal ())#module_exp v in
    let code = match module_exp with
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

of_exp ~name:(d,"fans") ~entry:Typehook.fan_quots ;
of_exp ~name:(d,"save") ~entry:Typehook.save_quot;

of_stru ~name:(d,"include") ~entry:Typehook.include_quot;
let d = `Absolute ["Fan";"Lang";"Macro"];
  
of_exp_with_filter
    ~name:(d,"exp") ~entry:exp ~filter:(AstMacros.macro_expander#exp);

of_cstru_with_filter
    ~name:(d,"cstru") ~entry:cstru
    ~filter:(AstMacros.macro_expander#cstru);

of_stru_with_filter
    ~name:(d,"stru") ~entry:stru
    ~filter:(AstMacros.macro_expander#stru);


let d = `Absolute ["Fan";"Lang";"Meta"];



add_quotation (d,"sig_item") sig_item_quot
    ~mexp:Filters.me#sig_item
    ~mpatt:Filters.mp#sig_item
    ~exp_filter ~patt_filter ;
add_quotation (d,"stru") stru_quot
    ~mexp:Filters.me#stru
    ~mpatt:Filters.mp#stru
    ~exp_filter
    ~patt_filter ;
add_quotation (d,"ctyp") ctyp_quot
    ~mexp:Filters.me#ctyp
    ~mpatt:Filters.mp#ctyp
    ~exp_filter
    ~patt_filter ;
add_quotation (d,"patt") patt_quot
    ~mexp:Filters.me#patt
    ~mpatt:Filters.mp#patt
    ~exp_filter
    ~patt_filter ;
add_quotation (d,"exp") exp_quot
    ~mexp:Filters.me#exp
    ~mpatt:Filters.mp#exp
    ~exp_filter
    ~patt_filter ;
add_quotation (d,"module_type") module_type_quot
    ~mexp:Filters.me#module_type
    ~mpatt:Filters.mp#module_type
    ~exp_filter
    ~patt_filter ;
add_quotation (d,"module_exp") module_exp_quot
    ~mexp:Filters.me#module_exp
    ~mpatt:Filters.mp#module_exp
    ~exp_filter
    ~patt_filter ;

add_quotation (d,"class_type") class_type_quot
    ~mexp:Filters.me#class_type ~mpatt:Filters.mp#class_type
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"class_exp") class_exp_quot
    ~mexp:Filters.me#class_exp
    ~mpatt:Filters.mp#class_exp
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"class_sig_item") class_sig_item_quot
    ~mexp:Filters.me#class_sig_item
    ~mpatt:Filters.mp#class_sig_item
    ~exp_filter
    ~patt_filter ;

add_quotation (d,"cstru") cstru_quot
    ~mexp:Filters.me#cstru
    ~mpatt:Filters.mp#cstru
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"with_constr") with_constr_quot
    ~mexp:Filters.me#with_constr
    ~mpatt:Filters.mp#with_constr
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"binding") binding_quot
    ~mexp:Filters.me#binding
    ~mpatt:Filters.mp#binding
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"rec_exp") rec_exp_quot
    ~mexp:Filters.me#rec_exp
    ~mpatt:Filters.mp#rec_exp
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"case") case_quot
    ~mexp:Filters.me#case
    ~mpatt:Filters.mp#case
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"module_binding") module_binding_quot
    ~mexp:Filters.me#module_binding
    ~mpatt:Filters.mp#module_binding
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"ident") ident_quot
    ~mexp:Filters.me#ident
    ~mpatt:Filters.mp#ident
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"rec_flag") rec_flag_quot
    ~mexp:Filters.me#rec_flag
    ~mpatt:Filters.mp#rec_flag
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"private_flag") private_flag_quot
    ~mexp:Filters.me#private_flag
    ~mpatt:Filters.mp#private_flag
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"row_var_flag") row_var_flag_quot
    ~mexp:Filters.me#row_var_flag
    ~mpatt:Filters.mp#row_var_flag
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"mutable_flag") mutable_flag_quot
    ~mexp:Filters.me#mutable_flag
    ~mpatt:Filters.mp#mutable_flag
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"virtual_flag") virtual_flag_quot
    ~mexp:Filters.me#virtual_flag ~mpatt:Filters.mp#virtual_flag
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"override_flag") override_flag_quot
    ~mexp:Filters.me#override_flag
    ~mpatt:Filters.mp#override_flag
    ~exp_filter
    ~patt_filter ;
  
add_quotation (d,"direction_flag") direction_flag_quot
    ~mexp:Filters.me#direction_flag
    ~mpatt:Filters.mp#direction_flag
    ~exp_filter
    ~patt_filter ;

add (`Absolute ["Fan";"Lang"],"str") DynAst.exp_tag
  (fun _loc _loc_option s -> {:exp|$str:s|});

add (`Absolute ["Fan";"Lang"],"str") DynAst.stru_tag
  (fun _loc _loc_option s ->
    `StExp(_loc,{:exp|$str:s|})(* {:stru| $(exp:{:exp|$str:s|}) |} *));
  
Options.add
    ("-dlang",
     FanArg.String (fun s ->
       AstQuotation.default := FanToken.resolve_name (`Sub [],s))
       ," Set the default language");

let d = `Absolute ["Fan"; "Lang"; "Meta";"N"] ;
(*
  (* temporarily turned off, since it's not the core of Fan *)
add_quotation (d,"exp") exp_quot
    ~mexp:(fun loc pexp -> FanAstN.Expr.meta_exp loc (strip_loc_exp pexp))
    ~mpatt:(fun loc ppatt -> FanAstN.Patt.meta_exp loc (strip_loc_exp ppatt))
    ~exp_filter
    ~patt_filter;
*)
(* let a = {:N.exp|a + b |}; *)
(* let a = {:exp|a + b|}; *)
  
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
