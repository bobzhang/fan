

(** eval the ast, only for byte code *)

let s2s s : Parsetree.toplevel_phrase =
  Ast2pt.phrase s
    
let eval_ast fmt ast =
  (* let _snap = Btype.snapshot ()  in *)
    try
      ( (* begin *)
        (* Env.reset_cache (); *)
        ignore (Toploop.execute_phrase false fmt (s2s ast))
      )
    with  x -> (Errors.report_error fmt x; exit 2)



        
