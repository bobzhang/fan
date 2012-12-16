type 'a tag =  
  | Ctyp
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
  | Module_binding 
let string_of_tag =
  function
  | Ctyp  -> "ctyp"
  | Patt  -> "patt"
  | Expr  -> "expr"
  | Module_type  -> "module_type"
  | Sig_item  -> "sig_item"
  | With_constr  -> "with_constr"
  | Module_expr  -> "module_expr"
  | Str_item  -> "str_item"
  | Class_type  -> "class_type"
  | Class_sig_item  -> "class_sig_item"
  | Class_expr  -> "class_expr"
  | Class_str_item  -> "class_str_item"
  | Match_case  -> "match_case"
  | Ident  -> "ident"
  | Binding  -> "binding"
  | Rec_binding  -> "rec_binding"
  | Module_binding  -> "module_binding"
let ctyp_tag: Ast.ctyp tag = Ctyp
let patt_tag: Ast.patt tag = Patt
let expr_tag: Ast.expr tag = Expr
let module_type_tag: Ast.module_type tag = Module_type
let sig_item_tag: Ast.sig_item tag = Sig_item
let with_constr_tag: Ast.with_constr tag = With_constr
let module_expr_tag: Ast.module_expr tag = Module_expr
let str_item_tag: Ast.str_item tag = Str_item
let class_type_tag: Ast.class_type tag = Class_type
let class_sig_item_tag: Ast.class_sig_item tag = Class_sig_item
let class_expr_tag: Ast.class_expr tag = Class_expr
let class_str_item_tag: Ast.class_str_item tag = Class_str_item
let match_case_tag: Ast.match_case tag = Match_case
let ident_tag: Ast.ident tag = Ident
let binding_tag: Ast.binding tag = Binding
let rec_binding_tag: Ast.rec_binding tag = Rec_binding
let module_binding_tag: Ast.module_binding tag = Module_binding
type dyn  
external dyn_tag : 'a tag -> dyn tag = "%identity"
module Pack(X:sig type 'a t   end) = struct
  type pack = (dyn tag* Obj.t)  exception Pack_error
  let pack tag (v : 'a X.t) = ((dyn_tag tag), (Obj.repr v))
  let unpack: 'a tag -> pack -> 'a X.t =
    fun tag  (tag',obj)  ->
      if (dyn_tag tag) = tag'
      then (Obj.obj obj : 'a X.t )
      else raise Pack_error
  let print_tag: Format.formatter -> pack -> unit =
    fun f  (tag,_)  -> Format.pp_print_string f (string_of_tag tag)
  end