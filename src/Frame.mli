open Ast

val check : string list -> unit
val mapi_exp :
  ?arity:int ->
  ?names:string list ->
  f:(ctyp -> exp) -> int -> ctyp -> FSig.ty_info
val tuple_exp_of_ctyp :
  ?arity:int ->
  ?names:string list ->
  mk_tuple:(FSig.ty_info list -> exp) ->
  (ctyp -> exp ) ->
  ctyp -> exp

val exp_of_variant:
    ?cons_transform:(string -> string) ->
  ?arity:int ->
  ?names:string list ->
  default:(FSig.vrn * int -> case option) ->
  mk_variant:(string -> FSig.ty_info list -> exp) ->
  destination:FSig.destination -> 
  (ctyp -> exp) -> ctyp -> row_field(* ctyp  *)->  exp
      
val normal_simple_exp_of_ctyp :
  ?arity:int ->
  ?names:string list ->
  mk_tuple:(FSig.ty_info list -> exp) ->
  right_type_id:FSig.full_id_transform ->
  left_type_id:FSig.basic_id_transform ->
  right_type_variable:FSig.rhs_basic_id_transform ->
  (string, 'a) Hashtbl.t -> ctyp -> exp 
val obj_simple_exp_of_ctyp :
  right_type_id:FSig.full_id_transform ->
  left_type_variable:FSig.basic_id_transform ->
  right_type_variable:FSig.rhs_basic_id_transform ->
  ?names:string list ->
  ?arity:int ->
  mk_tuple:(FSig.ty_info list -> exp) ->
  ctyp -> exp 
val exp_of_ctyp :
  ?cons_transform:(string -> string) ->
  ?arity:int ->
  ?names:string list ->
  default:(FSig.vrn * int -> case option) ->
  mk_variant:(string -> FSig.ty_info list -> exp) ->
  (ctyp -> exp) -> or_ctyp -> exp 
val mk_prefix :
  opt_decl_params ->
  exp ->
  ?names:string list ->
  left_type_variable:FSig.basic_id_transform ->
  exp
val fun_of_tydcl :
    ?names:string list ->
    ?arity:int ->
    left_type_variable:FSig.basic_id_transform ->
    mk_record:(FSig.record_col list -> exp) ->
    destination:FSig.destination ->
      result_type:ctyp -> 
      (ctyp -> exp ) ->
        (or_ctyp -> exp ) ->
          (ctyp -> row_field -> exp) ->  (* labeld as variant *)
            typedecl -> exp
val binding_of_tydcl :
  ?cons_transform:(string -> string) ->
  (ctyp -> exp ) ->
  typedecl ->
  ?arity:int ->
  ?names:string list ->
  default:(FSig.vrn * int -> case option) ->
  mk_variant:(string -> FSig.ty_info list -> exp) ->
  left_type_id:FSig.basic_id_transform ->
  left_type_variable:FSig.basic_id_transform ->
  mk_record:(FSig.record_col list -> exp) ->
  (* destination:FSig.destination -> *)
    binding
      
val stru_of_module_types :
  ?module_name:string ->
  ?cons_transform:(string -> string) ->
  ?arity:int ->
  ?names:string list ->
  default:(FSig.vrn * int -> case option) ->
  mk_variant:(string -> FSig.ty_info list -> exp) ->
  left_type_id:FSig.basic_id_transform ->
  left_type_variable:FSig.basic_id_transform ->
  mk_record:(FSig.record_col list -> exp) ->
  (* destination:FSig.destination ->  *)
  ((string, unit) Hashtbl.t -> ctyp -> exp ) ->
  FSig.module_types -> stru
      
val obj_of_module_types :
  ?cons_transform:(string -> string) ->
    ?module_name:string ->
  ?arity:int ->
  ?names:string list ->
  default:(FSig.vrn * int -> case option) ->
  left_type_variable:FSig.basic_id_transform ->
  mk_record:(FSig.record_col list -> exp) ->
  mk_variant:(string -> FSig.ty_info list -> exp) ->
  (* destination:FSig.destination  -> *)
     string ->  string ->  (ctyp -> exp ) -> 
  FSig.kind -> FSig.module_types -> stru
  
