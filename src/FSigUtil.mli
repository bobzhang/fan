
val stru_from_module_types:
    f:(FSig.named_type -> Ast.typedecl) -> FSig.module_types -> Ast.stru

val stru_from_ty:
    f:(string -> Ast.stru) -> FSig.module_types -> Ast.stru
