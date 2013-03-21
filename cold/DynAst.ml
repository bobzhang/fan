open Ast
type 'a tag =  
  | Ctyp
  | Patt
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
  | Module_binding 
let string_of_tag =
  function
  | Ctyp  -> "ctyp"
  | Patt  -> "patt"
  | Expr  -> "exp"
  | Module_type  -> "module_type"
  | Sig_item  -> "sig_item"
  | With_constr  -> "with_constr"
  | Module_exp  -> "module_exp"
  | Stru  -> "stru"
  | Class_type  -> "class_type"
  | Class_sig_item  -> "class_sig_item"
  | Class_exp  -> "class_exp"
  | Cstru  -> "cstru"
  | Case  -> "case"
  | Ident  -> "ident"
  | Binding  -> "binding"
  | Rec_exp  -> "rec_exp"
  | Module_binding  -> "module_binding"
let ctyp_tag: ctyp tag = Ctyp
let patt_tag: patt tag = Patt
let exp_tag: exp tag = Expr
let module_type_tag: module_type tag = Module_type
let sig_item_tag: sig_item tag = Sig_item
let with_constr_tag: with_constr tag = With_constr
let module_exp_tag: module_exp tag = Module_exp
let stru_tag: stru tag = Stru
let class_type_tag: class_type tag = Class_type
let class_sig_item_tag: class_sig_item tag = Class_sig_item
let class_exp_tag: class_exp tag = Class_exp
let cstru_tag: cstru tag = Cstru
let case_tag: case tag = Case
let ident_tag: ident tag = Ident
let binding_tag: binding tag = Binding
let rec_exp_tag: rec_exp tag = Rec_exp
let module_binding_tag: module_binding tag = Module_binding
type dyn  
external dyn_tag : 'a tag -> dyn tag = "%identity"
module Pack(X:sig type 'a t   end) =
  struct
    type pack = (dyn tag* Obj.t) 
    exception Pack_error
    let pack tag (v : 'a X.t) = ((dyn_tag tag), (Obj.repr v))
    let unpack: 'a tag -> pack -> 'a X.t =
      fun tag  (tag',obj)  ->
        if (dyn_tag tag) = tag'
        then (Obj.obj obj : 'a X.t )
        else raise Pack_error
    let print_tag: Format.formatter -> pack -> unit =
      fun f  (tag,_)  -> Format.pp_print_string f (string_of_tag tag)
  end
