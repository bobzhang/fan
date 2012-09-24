module Make =
 functor (Ast : Sig.Camlp4Ast) ->
  (struct
    module Ast = Ast

    type 'a tag =
       Tag_ctyp
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
     function
     | Tag_ctyp -> "ctyp"
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
     | Tag_module_binding -> "module_binding"

    let ctyp_tag = (Tag_ctyp)

    let patt_tag = (Tag_patt)

    let expr_tag = (Tag_expr)

    let module_type_tag = (Tag_module_type)

    let sig_item_tag = (Tag_sig_item)

    let with_constr_tag = (Tag_with_constr)

    let module_expr_tag = (Tag_module_expr)

    let str_item_tag = (Tag_str_item)

    let class_type_tag = (Tag_class_type)

    let class_sig_item_tag = (Tag_class_sig_item)

    let class_expr_tag = (Tag_class_expr)

    let class_str_item_tag = (Tag_class_str_item)

    let match_case_tag = (Tag_match_case)

    let ident_tag = (Tag_ident)

    let binding_tag = (Tag_binding)

    let rec_binding_tag = (Tag_rec_binding)

    let module_binding_tag = (Tag_module_binding)

    type dyn

    external dyn_tag : ('a tag -> dyn tag) = "%identity"

    module Pack =
     functor (X : sig type 'a t
 end) ->
      struct
       type pack = (dyn tag * Obj.t)

       exception Pack_error

       let pack = fun tag -> fun v -> (( (dyn_tag tag) ), ( (Obj.repr v) ))

       let unpack =
        fun (tag :
          'a tag) ->
         fun (tag', obj) ->
          if (( (dyn_tag tag) ) = tag') then ( ((Obj.obj obj) : 'a X.t) )
          else (raise Pack_error )

       let print_tag =
        fun f ->
         fun (tag, _) -> (Format.pp_print_string f ( (string_of_tag tag) ))

      end

   end : (Sig.DynAst with module Ast = Ast))
