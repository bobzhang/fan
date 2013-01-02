
let s2s s : Parsetree.toplevel_phrase =(Ast2pt.phrase s);
let eval_ast fmt ast =
  let _snap = Btype.snapshot ()  in
    try
      ( begin
        Env.reset_cache_toplevel ();
        ignore (Toploop.execute_phrase true fmt (s2s ast))
      end)
    with  x -> (Errors.report_error fmt x; exit 2);

AstQuotation.of_str_item_with_filter
        ~name:"eval" ~entry:PreCast.Syntax.str_items
        ~filter:(fun s -> begin 
          eval_ast Format.err_formatter s;
          let _loc = FanLoc.ghost ;
            {:str_item||}
        end);
        


















