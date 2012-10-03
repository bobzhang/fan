open Format;
open Camlp4.PreCast;
open Syntax;
open Fan_basic;
open Lib_common;
open Fan_config;
open Fan_dynamic_plugins;
(**
   This module export 4 languages
   [fan] a meta language
   [fan_str_item] a language to splice str_item
   [fan_expr] a language to splice expr
   [fan_class_str_item] a language to splice class_str_item 

   Fan is a simple directive language for parsing type computation
   we need different DSL since they are expanded in different
   positions by   the camlp4 engine
 *)
module MGram = MakeGram Lexer ;
module M = Make(MGram);
value mk = MGram.Entry.mk ;
value fan_quots = mk "fan";
value fan_quot = mk "fan_quot";
value fan_str_item = mk "fan_str_item";
value fan_expr = mk "fan_expr";
value fan_class_str_item = mk "fan_class_str_item" ;
value fan_ctyp = mk "fan_ctyp";

(*
   {[
   [ Tag_ctyp -> "ctyp"
    | Tag_patt -> "patt"
    | Tag_expr -> "expr"
    | Tag_module_type -> "module_type"
    | Tag_sig_item -> "sig_item"
    | Tag_with_constr -> "with_constr"
    | Tag_module_expr -> "module_expr"
    | Tag_str_item -> "str_item"
    | Tag_class_type -> "class_type"
    | Tag_class_sig_item -> "class_sig_item"
    | Tag_class_expr -> "class_expr"
    | Tag_class_str_item -> "class_str_item"
    | Tag_match_case -> "match_case"
    | Tag_ident -> "ident"
    | Tag_binding -> "binding"
    | Tag_rec_binding -> "rec_binding"
    | Tag_module_binding -> "module_binding" ];
   ]}
   Camlp4 support those tags. Take care to use them.
 *)


  
EXTEND MGram GLOBAL: fan_quots fan_quot 
    fan_str_item fan_expr
    fan_class_str_item fan_ctyp  ;
  fan_quots:
    ["top"
       [ strs = LIST0 [x = fan_quot; ";" -> x ] ->
         <:expr< do { .$list:strs$. } >> ] ];
  fan_quot:
    ["top"
       [ "lang"; quot=STRING-> do{
         Quotation.default.val:= quot;
         unit_literal }
       | "lang_at"; tag=STRING; quot=STRING->do{
          eprintf "Set: quotation expander %s at position %s@." quot tag ;
          Quotation.default_at_pos tag quot;
          unit_literal 
       }
       | "lang_clear" -> do{
         Quotation.default.val := "";
         Hashtbl.clear Quotation.default_tbl;
         unit_literal
        } ] ];
  fan_str_item:[[file=STRING; c=STRING -> str_item_of_file (file,c) ] ];
  fan_expr: [[ file=STRING; c=STRING-> expr_of_file (file,c) ]];
  fan_class_str_item: [[ file=STRING; c=STRING->  class_str_item_of_file (file,c) ]];
  fan_ctyp: [[file=STRING;c=STRING ->  ctyp_of_file (file,c) ]];
END ; 
  

let open M in do {
add_quotation_of_expr
  ~name:"fan" ~entry:fan_quots;
add_quotation_of_str_item ~name:"fan_str_item" ~entry:fan_str_item;
add_quotation_of_expr ~name:"fan_expr" ~entry:fan_expr;
add_quotation_of_class_str_item ~name:"fan_class_str_item"
  ~entry:fan_class_str_item;

};


