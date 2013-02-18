open LibUtil;
open StdLib;
open Ast;


let strip_loc_list f lst =
  List.map f lst ;
let strip_loc_ant ant = ant ;
  
{:fans|keep off;
derive(Map2 Fold2 OIter Map Fold  OPrint OEq Strip Print);
|};

{:ocaml|INCLUDE "src/Ast.ml"; |};


let dump = new print;

let dump_ctyp = to_string_of_printer dump#ctyp;
let dump_name_ctyp = to_string_of_printer dump#name_ctyp;  
let dump_with_constr = to_string_of_printer dump#with_constr;
let dump_module_type = to_string_of_printer dump#module_type;
let dump_expr = to_string_of_printer dump#expr;
let dump_patt = to_string_of_printer dump#patt;
let dump_class_type = to_string_of_printer dump#class_type;
let dump_class_expr = to_string_of_printer dump#class_expr;
let dump_ident = to_string_of_printer dump#ident;
let dump_match_case = to_string_of_printer dump#match_case;
let dump_rec_expr = to_string_of_printer dump#rec_expr;  
let dump_str_item = to_string_of_printer dump#str_item;
let dump_sig_item = to_string_of_printer dump#sig_item;
let dump_module_binding  = to_string_of_printer dump#module_binding;
let dump_module_expr = to_string_of_printer dump#module_expr;  
let dump_class_sig_item = to_string_of_printer dump#class_sig_item;
let dump_class_str_item = to_string_of_printer dump#class_str_item;  

  
