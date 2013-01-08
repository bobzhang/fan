open Ast 

val to_var_list : ctyp -> string list

val list_of_opt : ctyp -> ctyp list -> ctyp list

val name_tags : ctyp -> string list

val to_generalized : ctyp -> ctyp list * ctyp

val to_string : (ctyp -> string) ref

val eprint : (ctyp -> unit) ref

(* val _loc : FanLoc.t *)

val app : ctyp -> ctyp -> ctyp

val comma : ctyp -> ctyp -> ctyp

(* val ( <$ ) : ctyp -> ctyp -> ctyp *)

val apply : ctyp -> ctyp list -> ctyp

val sem : ctyp -> ctyp -> ctyp
val list_of_app : ctyp -> ctyp list
val list_of_com : ctyp -> ctyp list
val list_of_sem : ctyp -> ctyp list
val view_app : ctyp list -> ctyp -> ctyp * ctyp list
val app_of_list : ctyp list -> ctyp
val com_of_list : ctyp list -> ctyp
val sem_of_list : ctyp list -> ctyp
val tuple_of_list : ctyp list -> ctyp
val arrow : ctyp -> ctyp -> ctyp
val ( |-> ) : ctyp -> ctyp -> ctyp
val sta : ctyp -> ctyp -> ctyp
val sta_of_list : ctyp list -> ctyp
val arrow_of_list : ctyp list -> ctyp
val app_arrow : ctyp list -> ctyp -> ctyp
val tuple_sta_of_list : ctyp list -> ctyp
val ( <+ ) : string list -> ctyp -> ctyp
val ( +> ) : ctyp list -> ctyp -> ctyp
val name_length_of_tydcl : ctyp -> string * int
val gen_quantifiers : arity:int -> int -> ctyp
val of_id_len : off:int -> ident * int -> ctyp
val of_name_len : off:int -> string * int -> ctyp
val ty_name_of_tydcl : ctyp -> ctyp
val gen_ty_of_tydcl : off:int -> ctyp -> ctyp
val list_of_record : ctyp -> FSig.col list
val gen_tuple_n : ctyp -> int -> ctyp
val repeat_arrow_n : ctyp -> int -> ctyp
val mk_method_type :
  number:int ->
  prefix:string list -> ident * int -> FSig.obj_dest -> ctyp
val mk_method_type_of_name :
  number:int ->
  prefix:string list -> string * int -> FSig.obj_dest -> ctyp
val mk_obj : string -> string -> class_str_item -> str_item
val is_recursive : ctyp -> bool
val qualified_app_list : ctyp -> (ident * ctyp list) option
val is_abstract : ctyp -> bool
val abstract_list : ctyp -> int option
val eq : ctyp -> ctyp -> bool
val eq_list : ctyp list -> ctyp list -> bool
(* val mk_transform_type_eq : *)
(*   unit -> FanAst.map *)
  
val transform_module_types : FSig.module_types ->
  (string * ident * int) list * FSig.module_types

val reduce_data_ctors :
  ctyp ->
    'a -> (string -> ctyp list -> 'a -> 'a) -> 'a LibUtil.ErrorMonad.result

(* @raise Invalid_argument *)        
val of_str_item: str_item -> ctyp 

val view_adt: ctyp -> FSig.branch list
val view_variant: ctyp -> FSig.vbranch list    
