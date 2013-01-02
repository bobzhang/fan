let s2s s = (Ast2pt.phrase s : Parsetree.toplevel_phrase )
let eval_ast fmt ast =
  let _snap = Btype.snapshot () in
  try
    Env.reset_cache_toplevel ();
    ignore (Toploop.execute_phrase true fmt (s2s ast))
  with | x -> (Errors.report_error fmt x; exit 2)
let _ =
  AstQuotation.of_str_item_with_filter ~name:"eval"
    ~entry:PreCast.Syntax.str_items
    ~filter:(fun s  ->
               eval_ast Format.err_formatter s;
               (let _loc = FanLoc.ghost in Ast.StNil _loc))