
(* {:fans|keep off; derive(DynAst); |}; *)
(* {:ocaml|{:include| "src/Ast.ml" |}; |}; *)

(* {:fans|keep on ; derive (Print); |}; *)
open Ast;
(* {:ocaml| *)
type 'a tag  =
 [ Ctyp
 | Pat
 | Expr
 | Module_type
 | Sig_item
 | With_constr
 | Module_exp
 | Stru
 | Class_type
 | Class_sig_item
 | Class_exp
 | Cstru
 | Case
 | Ident
 | Binding
 | Rec_exp
 | Module_binding ];
(* |}; *)
let string_of_tag = fun
 [ Ctyp -> "ctyp"
 | Pat -> "pat"
 | Expr -> "exp"
 | Module_type -> "module_type"
 | Sig_item -> "sig_item"
 | With_constr -> "with_constr"
 | Module_exp -> "module_exp"
 | Stru -> "stru"
 | Class_type -> "class_type"
 | Class_sig_item -> "class_sig_item"
 | Class_exp -> "class_exp"
 | Cstru -> "cstru"
 | Case -> "case"
 | Ident -> "ident"
 | Binding -> "binding"
 | Rec_exp -> "rec_exp"
 | Module_binding -> "module_binding" ];
  
let ctyp_tag : tag ctyp = Ctyp;
let pat_tag : tag pat = Pat;
let exp_tag : tag exp = Expr;
let module_type_tag : tag module_type = Module_type;
let sig_item_tag : tag sig_item = Sig_item;
let with_constr_tag : tag with_constr = With_constr;
let module_exp_tag : tag module_exp = Module_exp;
let stru_tag : tag stru = Stru;
let class_type_tag : tag class_type = Class_type;
let class_sig_item_tag : tag class_sig_item = Class_sig_item;
let class_exp_tag : tag class_exp = Class_exp;
let cstru_tag : tag cstru = Cstru;
let case_tag : tag case = Case;
let ident_tag : tag ident = Ident;
let binding_tag : tag binding = Binding;
let rec_exp_tag : tag rec_exp  = Rec_exp;
let module_binding_tag : tag module_binding = Module_binding;

type dyn;
  
external dyn_tag : tag 'a -> tag dyn = "%identity";

module Pack(X : sig type 'a t ; end) = struct
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

