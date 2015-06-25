let s2s s = (Ast2pt.phrase s : Parsetree.toplevel_phrase )
let eval_ast fmt ast =
  try ignore (Toploop.execute_phrase false fmt (s2s ast))
  with | x -> (Errors.report_error fmt x; exit 2)