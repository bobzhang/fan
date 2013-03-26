val stru_from_module_types:
    f:(FSig.named_type -> AstLoc.typedecl) -> FSig.module_types -> AstLoc.stru

val stru_from_ty:
    f:(string -> AstLoc.stru) -> FSig.module_types -> AstLoc.stru
