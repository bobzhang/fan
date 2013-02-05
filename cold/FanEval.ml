let s2s s = (Ast2pt.phrase s : Parsetree.toplevel_phrase )
let eval_ast fmt ast =
  let _snap = Btype.snapshot () in
  try
    Env.reset_cache_toplevel ();
    ignore (Toploop.execute_phrase true fmt (s2s ast))
  with | x -> (Errors.report_error fmt x; exit 2)