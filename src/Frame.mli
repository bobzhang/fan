open Ast

val check : string list -> unit
val mapi_expr :
  arity:int ->
  names:string list ->
  (ctyp -> expr) -> int -> ctyp -> FSig.ty_info
val tuple_expr_of_ctyp :
  arity:int ->
  names:string list ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  (ctyp -> expr LibUtil.ErrorMonad.result) ->
  ctyp -> expr
val normal_simple_expr_of_ctyp :
  arity:int ->
  names:string list ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  right_type_id:FSig.full_id_transform ->
  left_type_id:FSig.basic_id_transform ->
  right_type_variable:FSig.rhs_basic_id_transform ->
  (string, 'a) Hashtbl.t -> ctyp -> expr LibUtil.ErrorMonad.result
val obj_simple_expr_of_ctyp :
  right_type_id:FSig.full_id_transform ->
  left_type_variable:FSig.basic_id_transform ->
  right_type_variable:FSig.rhs_basic_id_transform ->
  names:string list ->
  arity:int ->
  mk_tuple:(FSig.ty_info list -> expr) ->
  ctyp -> expr LibUtil.ErrorMonad.result
val expr_of_ctyp :
  ?cons_transform:(string -> string) ->
  arity:int ->
  names:string list ->
  trail:(FSig.vrn * int -> match_case) ->
  mk_variant:(string -> FSig.ty_info list -> expr) ->
  (ctyp -> expr) -> ctyp -> expr LibUtil.ErrorMonad.result
val mk_prefix :
  ctyp list ->
  expr ->
  names:string list ->
  left_type_variable:FSig.basic_id_transform ->
  expr
val fun_of_tydcl :
  names:string list ->
  arity:int ->
  left_type_variable:FSig.basic_id_transform ->
  mk_record:(FSig.record_col list -> expr) ->
  (ctyp -> expr LibUtil.ErrorMonad.result) ->
  (ctyp -> expr LibUtil.ErrorMonad.result) ->
  [> `TyDcl of 'a * 'b * ctyp list * ctyp * 'c ] -> expr
val binding_of_tydcl :
  ?cons_transform:(string -> string) ->
  (ctyp -> expr LibUtil.ErrorMonad.result) ->
  ctyp ->
  arity:int ->
  names:string list ->
  trail:(FSig.vrn * int -> match_case) ->
  mk_variant:(string -> FSig.ty_info list -> expr) ->
  left_type_id:FSig.basic_id_transform ->
  left_type_variable:FSig.basic_id_transform ->
  mk_record:(FSig.record_col list -> expr) -> binding
      
val str_item_of_module_types :
  ?module_name:string ->
  ?cons_transform:(string -> string) ->
  arity:int ->
  names:string list ->
  trail:(FSig.vrn * int -> match_case) ->
  mk_variant:(string -> FSig.ty_info list -> expr) ->
  left_type_id:FSig.basic_id_transform ->
  left_type_variable:FSig.basic_id_transform ->
    mk_record:(FSig.record_col list -> expr) ->
      ((string, unit) Hashtbl.t -> ctyp -> expr LibUtil.ErrorMonad.result) ->
  FSig.module_types -> str_item
      
val obj_of_module_types :
  ?cons_transform:(string -> string) ->
    ?module_name:string ->
  arity:int ->
  names:string list ->
  trail:(FSig.vrn * int -> match_case) ->
  left_type_variable:FSig.basic_id_transform ->
  mk_record:(FSig.record_col list -> expr) ->
  mk_variant:(string -> FSig.ty_info list -> expr) ->
  string ->
  string ->
  (ctyp -> expr LibUtil.ErrorMonad.result) ->
  FSig.k -> FSig.module_types -> str_item
  
