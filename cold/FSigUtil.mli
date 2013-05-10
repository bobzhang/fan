
val stru_from_mtyps:
    f:(FSig.named_type -> Ast.typedecl) -> FSig.mtyps -> Ast.stru option

val stru_from_ty:
    f:(string -> Ast.stru) -> FSig.mtyps -> Ast.stru
