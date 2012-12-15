(* {:fans|<+ "Print"; |}; *)
(* {:ocaml| *)(* |}; *)
type tag 'a =
 [ Tag_ctyp
 | Tag_patt
 | Tag_expr
 | Tag_module_type
 | Tag_sig_item
 | Tag_with_constr
 | Tag_module_expr
 | Tag_str_item
 | Tag_class_type
 | Tag_class_sig_item
 | Tag_class_expr
 | Tag_class_str_item
 | Tag_match_case
 | Tag_ident
 | Tag_binding
 | Tag_rec_binding
 | Tag_module_binding ];

let string_of_tag = fun
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
  
let ctyp_tag : tag Ast.ctyp = Tag_ctyp;
let patt_tag : tag Ast.patt = Tag_patt;
let expr_tag : tag Ast.expr = Tag_expr;
let module_type_tag : tag Ast.module_type = Tag_module_type;
let sig_item_tag : tag Ast.sig_item = Tag_sig_item;
let with_constr_tag : tag Ast.with_constr = Tag_with_constr;
let module_expr_tag : tag Ast.module_expr = Tag_module_expr;
let str_item_tag : tag Ast.str_item = Tag_str_item;
let class_type_tag : tag Ast.class_type = Tag_class_type;
let class_sig_item_tag : tag Ast.class_sig_item = Tag_class_sig_item;
let class_expr_tag : tag Ast.class_expr = Tag_class_expr;
let class_str_item_tag : tag Ast.class_str_item = Tag_class_str_item;
let match_case_tag : tag Ast.match_case = Tag_match_case;
let ident_tag : tag Ast.ident = Tag_ident;
let binding_tag : tag Ast.binding = Tag_binding;
let rec_binding_tag : tag Ast.rec_binding  = Tag_rec_binding;
let module_binding_tag : tag Ast.module_binding = Tag_module_binding;

type dyn;
external dyn_tag : tag 'a -> tag dyn = "%identity";

module Pack(X : sig type t 'a; end) = struct
 (* These Obj.* hacks should be avoided with GADTs *)
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

