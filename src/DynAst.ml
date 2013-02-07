(* {:fans|keep on ; derive (Print); |}; *)
open Ast;
(* {:ocaml| *)
type tag 'a =
 [ Ctyp
 | Patt
 | Expr
 | Module_type
 | Sig_item
 | With_constr
 | Module_expr
 | Str_item
 | Class_type
 | Class_sig_item
 | Class_expr
 | Class_str_item
 | Match_case
 | Ident
 | Binding
 | Rec_expr
 | Module_binding ];
(* |}; *)
let string_of_tag = fun
 [ Ctyp -> "ctyp"
 | Patt -> "patt"
 | Expr -> "expr"
 | Module_type -> "module_type"
 | Sig_item -> "sig_item"
 | With_constr -> "with_constr"
 | Module_expr -> "module_expr"
 | Str_item -> "str_item"
 | Class_type -> "class_type"
 | Class_sig_item -> "class_sig_item"
 | Class_expr -> "class_expr"
 | Class_str_item -> "class_str_item"
 | Match_case -> "match_case"
 | Ident -> "ident"
 | Binding -> "binding"
 | Rec_expr -> "rec_expr"
 | Module_binding -> "module_binding" ];
  
let ctyp_tag : tag ctyp = Ctyp;
let patt_tag : tag patt = Patt;
let expr_tag : tag expr = Expr;
let module_type_tag : tag module_type = Module_type;
let sig_item_tag : tag sig_item = Sig_item;
let with_constr_tag : tag with_constr = With_constr;
let module_expr_tag : tag module_expr = Module_expr;
let str_item_tag : tag str_item = Str_item;
let class_type_tag : tag class_type = Class_type;
let class_sig_item_tag : tag class_sig_item = Class_sig_item;
let class_expr_tag : tag class_expr = Class_expr;
let class_str_item_tag : tag class_str_item = Class_str_item;
let match_case_tag : tag match_case = Match_case;
let ident_tag : tag ident = Ident;
let binding_tag : tag binding = Binding;
let rec_expr_tag : tag rec_expr  = Rec_expr;
let module_binding_tag : tag module_binding = Module_binding;

type dyn;
  
external dyn_tag : tag 'a -> tag dyn = "%identity";

module Pack(X : sig type t 'a; end) = struct
  type pack = (tag dyn * Obj.t);
  exception Pack_error;
  let pack tag (v:X.t 'a) = (dyn_tag tag, Obj.repr v);
  let unpack : tag 'a -> pack -> X.t 'a = fun
    tag (tag', obj) -> 
      if dyn_tag tag = tag' then (Obj.obj obj : X.t 'a) else raise Pack_error;
  let print_tag : Format.formatter -> pack -> unit = fun
     f (tag, _) ->
      Format.pp_print_string f (string_of_tag tag);
end;

