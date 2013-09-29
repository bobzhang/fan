let rec meta_binding _loc =
  function
    | Ast.BiAnt (x0,x1) -> Ast.ExAnt (x0, x1)
    (* | Ast.BiEq (x0,x1,x2) -> *)
    (*     Ast.ExApp *)
    (*       (_loc, *)
    (*        (Ast.ExApp *)
    (*           (_loc, *)
    (*            (Ast.ExApp *)
    (*               (_loc, *)
    (*                (Ast.ExId *)
    (*                   (_loc, *)
    (*                    (Ast.IdAcc *)
    (*                       (_loc, (Ast.IdUid (_loc, "Ast")), *)
    (*                        (Ast.IdUid (_loc, "BiEq")))))), *)
    (*                (meta_loc _loc x0))), (meta_patt _loc x1))), *)
    (*        (meta_expr _loc x2)) *)
and meta_class_expr _loc =
  function
    | Ast.CeAnt (x0,x1) -> Ast.ExAnt (x0, x1)
    | Ast.CeEq (x0,x1,x2) ->
        Ast.ExApp
          (_loc,
           (Ast.ExApp
              (_loc,
               (* (Ast.ExApp *)
               (*    (_loc, *)
               (*     (Ast.ExId *)
               (*        (_loc, *)
               (*         (Ast.IdAcc *)
               (*            (_loc, (Ast.IdUid (_loc, "Ast")), *)
               (*             (Ast.IdUid (_loc, "CeEq")))))), *)
               (*     (meta_loc _loc x0))) ,*)
               (meta_class_expr _loc x1))),
           (meta_class_expr _loc x2))
          
