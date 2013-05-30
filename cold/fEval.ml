let s2s s = (Ast2pt.phrase s : Parsetree.toplevel_phrase )

let eval_ast fmt ast =
  let _snap = Btype.snapshot () in
  try
    begin
      Env.reset_cache (); ignore (Toploop.execute_phrase true fmt (s2s ast))
    end
  with | x -> begin Errors.report_error fmt x; exit 2 end