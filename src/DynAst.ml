{:fans|keep on ;|};
{:ocaml|
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
 | Rec_binding
 | Module_binding ];
|};
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
 | Rec_binding -> "rec_binding"
 | Module_binding -> "module_binding" ];
  
let ctyp_tag : tag Ast.ctyp = Ctyp;
let patt_tag : tag Ast.patt = Patt;
let expr_tag : tag Ast.expr = Expr;
let module_type_tag : tag Ast.module_type = Module_type;
let sig_item_tag : tag Ast.sig_item = Sig_item;
let with_constr_tag : tag Ast.with_constr = With_constr;
let module_expr_tag : tag Ast.module_expr = Module_expr;
let str_item_tag : tag Ast.str_item = Str_item;
let class_type_tag : tag Ast.class_type = Class_type;
let class_sig_item_tag : tag Ast.class_sig_item = Class_sig_item;
let class_expr_tag : tag Ast.class_expr = Class_expr;
let class_str_item_tag : tag Ast.class_str_item = Class_str_item;
let match_case_tag : tag Ast.match_case = Match_case;
let ident_tag : tag Ast.ident = Ident;
let binding_tag : tag Ast.binding = Binding;
let rec_binding_tag : tag Ast.rec_binding  = Rec_binding;
let module_binding_tag : tag Ast.module_binding = Module_binding;

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

