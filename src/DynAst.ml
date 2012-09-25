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

value string_of_tag = fun
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
  
value ctyp_tag : tag Ast.ctyp = Tag_ctyp;
value patt_tag : tag Ast.patt = Tag_patt;
value expr_tag : tag Ast.expr = Tag_expr;
value module_type_tag : tag Ast.module_type = Tag_module_type;
value sig_item_tag : tag Ast.sig_item = Tag_sig_item;
value with_constr_tag : tag Ast.with_constr = Tag_with_constr;
value module_expr_tag : tag Ast.module_expr = Tag_module_expr;
value str_item_tag : tag Ast.str_item = Tag_str_item;
value class_type_tag : tag Ast.class_type = Tag_class_type;
value class_sig_item_tag : tag Ast.class_sig_item = Tag_class_sig_item;
value class_expr_tag : tag Ast.class_expr = Tag_class_expr;
value class_str_item_tag : tag Ast.class_str_item = Tag_class_str_item;
value match_case_tag : tag Ast.match_case = Tag_match_case;
value ident_tag : tag Ast.ident = Tag_ident;
value binding_tag : tag Ast.binding = Tag_binding;
value rec_binding_tag : tag Ast.rec_binding  = Tag_rec_binding;
value module_binding_tag : tag Ast.module_binding = Tag_module_binding;

type dyn;
external dyn_tag : tag 'a -> tag dyn = "%identity";

module Pack(X : sig type t 'a; end) = struct
 (* These Obj.* hacks should be avoided with GADTs *)
  type pack = (tag dyn * Obj.t);
  exception Pack_error;
  value pack tag (v:X.t 'a) = (dyn_tag tag, Obj.repr v);
  value unpack : tag 'a -> pack -> X.t 'a = fun
    tag (tag', obj) -> 
      if dyn_tag tag = tag' then (Obj.obj obj : X.t 'a) else raise Pack_error;
  value print_tag : Format.formatter -> pack -> unit = fun
     f (tag, _) ->
      Format.pp_print_string f (string_of_tag tag);
end;

