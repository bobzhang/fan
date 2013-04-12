open Ast
val mk_variant_eq : 'a -> FSig.ty_info list -> exp
val mk_tuple_eq : FSig.ty_info list -> exp
val mk_record_eq : FSig.record_col list -> exp
val mk_variant_meta_exp : string -> FSig.ty_info list -> exp
val mk_record_meta_exp : FSig.record_col list -> exp
val mk_tuple_meta_exp : FSig.ty_info list -> exp
val gen_meta_exp : FSig.mtyps -> stru
val mk_variant_meta_pat : string -> FSig.ty_info list -> exp
val mk_record_meta_pat : FSig.record_col list -> exp
val mk_tuple_meta_pat : FSig.ty_info list -> exp


val extract : FSig.ty_info list -> exp list

val mkfmt : string -> string -> string -> string list -> exp

val mk_variant_print : string -> FSig.ty_info list -> exp

val mk_tuple_print : FSig.ty_info list -> exp

val mk_record_print : FSig.record_col list -> exp

val gen_eq : FSig.mtyps -> stru

val gen_fold : FSig.mtyps -> stru

val gen_fold2 : FSig.mtyps -> stru

val gen_map : FSig.mtyps -> stru

val gen_map2 : FSig.mtyps -> stru

val gen_meta_pat : FSig.mtyps -> stru

val gen_print : FSig.mtyps -> stru

val gen_print_obj : FSig.mtyps -> stru
