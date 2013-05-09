val gen_eq : FSig.mtyps -> Ast.stru
val gen_eqobj : FSig.mtyps -> Ast.stru
val gen_fold : FSig.mtyps -> Ast.stru
val gen_fold2 : FSig.mtyps -> Ast.stru
val gen_map : FSig.mtyps -> Ast.stru
val gen_map2 : FSig.mtyps -> Ast.stru
val gen_strip : FSig.mtyps -> Ast.stru
val mk_variant : string -> FSig.ty_info list -> Ast.exp
val mk_record : FSig.record_col list -> Ast.exp
val mk_tuple : FSig.ty_info list -> Ast.exp
val gen_meta_exp : FSig.mtyps -> Ast.stru
val gen_meta : FSig.mtyps -> Ast.stru
val extract : FSig.ty_info list -> Ast.exp list
val mkfmt : string -> string -> string -> string list -> Ast.exp
val mk_variant_print : string -> FSig.ty_info list -> Ast.exp
val mk_tuple_print : FSig.ty_info list -> Ast.exp
val mk_record_print : FSig.record_col list -> Ast.exp
val gen_print : FSig.mtyps -> Ast.stru
val gen_print_obj : FSig.mtyps -> Ast.stru
val mk_variant_iter : 'a -> FSig.ty_info list -> Ast.exp
val mk_tuple_iter : FSig.ty_info list -> Ast.exp
val mk_record_iter : FSig.record_col list -> [> `Seq of FanLoc.t * Ast.exp ]
val gen_iter : FSig.mtyps -> Ast.stru
val generate : FSig.mtyps -> Ast.stru
