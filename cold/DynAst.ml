type 'a tag =  
  | Tag_ctyp
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
  | Tag_module_binding 
let string_of_tag =
  (function
  | Tag_ctyp  ->   "ctyp"
  | Tag_patt  ->   "patt"
  | Tag_expr  ->   "expr"
  | Tag_module_type  ->   "module_type"
  | Tag_sig_item  ->   "sig_item"
  | Tag_with_constr  ->   "with_constr"
  | Tag_module_expr  ->   "module_expr"
  | Tag_str_item  ->   "str_item"
  | Tag_class_type  ->   "class_type"
  | Tag_class_sig_item  ->   "class_sig_item"
  | Tag_class_expr  ->   "class_expr"
  | Tag_class_str_item  ->   "class_str_item"
  | Tag_match_case  ->   "match_case"
  | Tag_ident  ->   "ident"
  | Tag_binding  ->   "binding"
  | Tag_rec_binding  ->   "rec_binding"
  | Tag_module_binding  ->   "module_binding")
let ctyp_tag = (Tag_ctyp : Ast.ctyp  tag  )
let patt_tag = (Tag_patt : Ast.patt  tag  )
let expr_tag = (Tag_expr : Ast.expr  tag  )
let module_type_tag = (Tag_module_type : Ast.module_type  tag  )
let sig_item_tag = (Tag_sig_item : Ast.sig_item  tag  )
let with_constr_tag = (Tag_with_constr : Ast.with_constr  tag  )
let module_expr_tag = (Tag_module_expr : Ast.module_expr  tag  )
let str_item_tag = (Tag_str_item : Ast.str_item  tag  )
let class_type_tag = (Tag_class_type : Ast.class_type  tag  )
let class_sig_item_tag = (Tag_class_sig_item : Ast.class_sig_item  tag  )
let class_expr_tag = (Tag_class_expr : Ast.class_expr  tag  )
let class_str_item_tag = (Tag_class_str_item : Ast.class_str_item  tag  )
let match_case_tag = (Tag_match_case : Ast.match_case  tag  )
let ident_tag = (Tag_ident : Ast.ident  tag  )
let binding_tag = (Tag_binding : Ast.binding  tag  )
let rec_binding_tag = (Tag_rec_binding : Ast.rec_binding  tag  )
let module_binding_tag = (Tag_module_binding : Ast.module_binding  tag  )
type  dyn  
external dyn_tag : ('a tag  ->  dyn  tag )  = "%identity"
module Pack (X:sig type 'a t   end) =
  struct type  pack = ( dyn  tag * Obj.t )  exception Pack_error
    let pack (tag) ((v : 'a X.t )) = (( (dyn_tag tag) ),( (Obj.repr v) ))
    let unpack =
      ((fun (tag) ->
         (fun ((tag',obj)) ->
           if (( (dyn_tag tag) ) = tag') then begin
           ((Obj.obj obj) :'a X.t  )
           end else begin (raise Pack_error )
           end)) :('a tag  -> ( pack  -> 'a X.t ) )  )
    let print_tag =
      ((fun (f) ->
         (fun ((tag,_)) ->
           (Format.pp_print_string f ( (string_of_tag tag) ))))
        :( Format.formatter  -> ( pack  ->  unit ) )  ) end