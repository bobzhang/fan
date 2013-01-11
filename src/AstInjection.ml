open Ast;
open LibUtil;

type key = string;

let inject_expr_tbl: Hashtbl.t key expr = Hashtbl.create 40; 
let inject_str_item_tbl: Hashtbl.t key str_item = Hashtbl.create 40;
let inject_class_str_item_tbl: Hashtbl.t key class_str_item = Hashtbl.create 40;

let register_inject_expr (k,f)=
  Hashtbl.replace inject_expr_tbl k f;
let register_inject_str_item (k,f)=
  Hashtbl.replace inject_str_item_tbl k f;
let register_inject_class_str_item (k,f) =
  Hashtbl.replace inject_class_str_item_tbl k f;



{:extend.create|Gram
  inject_expr inject_str_item inject_class_str_item
|};
  
{:extend| Gram
  inject_expr:
  [`Lid x ->
     try Hashtbl.find inject_expr_tbl x 
     with [Not_found -> failwithf "inject.expr %s not found" x ]]

  inject_str_item:
  [`Lid x ->
     try Hashtbl.find inject_str_item_tbl x
     with [Not_found -> failwithf "inject.expr %s not found" x ]]

  inject_class_str_item:
  [`Lid x ->
     try Hashtbl.find inject_class_str_item_tbl x
     with [Not_found -> failwithf "inject.expr %s not found" x ]]
|};

let open AstQuotation in begin
  of_expr ~name:"inject.expr" ~entry:inject_expr;
  of_str_item ~name:"inject.str_item" ~entry:inject_str_item;
  of_class_str_item ~name:"inject.class_str_item" ~entry:inject_class_str_item; 
end;  
