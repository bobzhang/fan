open Ast
module Make :
  functor (S : FSig.Config) ->
    sig
      val mapi_expr : (ctyp -> expr) -> int -> ctyp -> FSig.ty_info
      val tuple_expr_of_ctyp :
        (ctyp -> expr LibUtil.ErrorMonad.result) ->
        ctyp -> expr
      val normal_simple_expr_of_ctyp :
        (string, 'a) Hashtbl.t -> ctyp -> expr LibUtil.ErrorMonad.result
      val obj_simple_expr_of_ctyp :
        ctyp -> expr LibUtil.ErrorMonad.result
      val expr_of_ctyp :
        (ctyp -> expr) ->
        ctyp -> expr LibUtil.ErrorMonad.result
      val mk_prefix : ctyp list -> expr -> expr
      val fun_of_tydcl :
        (ctyp -> expr LibUtil.ErrorMonad.result) ->
        (ctyp -> expr LibUtil.ErrorMonad.result) ->
        ctyp -> expr
      val binding_of_tydcl :
        (ctyp -> expr LibUtil.ErrorMonad.result) ->
        'a -> ctyp -> binding
      val str_item_of_module_types :
        ?module_name:string ->
        ((string, unit) Hashtbl.t ->
         ctyp -> expr LibUtil.ErrorMonad.result) ->
        FSig.module_types -> str_item
      val obj_of_module_types :
        ?module_name:string ->
        string ->
        string ->
        (ctyp -> expr LibUtil.ErrorMonad.result) ->
        FSig.k -> FSig.module_types -> str_item
    end
