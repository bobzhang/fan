let cstream = Compile_stream.cstream
let exp = Syntaxf.exp
let stream_exp = Gramf.mk "stream_exp"
let stream_exp_comp = Gramf.mk "stream_exp_comp"
let stream_exp_comp_list = Gramf.mk "stream_exp_comp_list"
let _ =
  Gramf.extend_single (stream_exp : 'stream_exp Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "!";
          `Stoken
            (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
              "`Uid n")],
           ("Ref.protect Compile_stream.grammar_module_name n\n  (fun _  -> Compile_stream.empty _loc)\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Uid ({ txt = n;_} : Tokenf.txt) ->
                       (Ref.protect Compile_stream.grammar_module_name n
                          (fun _  -> Compile_stream.empty _loc) : 'stream_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
        ([`Skeyword "!";
         `Stoken
           (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
             "`Uid n");
         `Snterm
           (Gramf.obj (stream_exp_comp_list : 'stream_exp_comp_list Gramf.t ))],
          ("Ref.protect Compile_stream.grammar_module_name n (fun _  -> cstream _loc sel)\n",
            (Gramf.mk_action
               (fun (sel : 'stream_exp_comp_list)  (__fan_1 : Tokenf.t)  _ 
                  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Uid ({ txt = n;_} : Tokenf.txt) ->
                      (Ref.protect Compile_stream.grammar_module_name n
                         (fun _  -> cstream _loc sel) : 'stream_exp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_1))))));
        ([`Snterm
            (Gramf.obj
               (stream_exp_comp_list : 'stream_exp_comp_list Gramf.t ))],
          ("cstream _loc sel\n",
            (Gramf.mk_action
               (fun (sel : 'stream_exp_comp_list)  (_loc : Locf.t)  ->
                  (cstream _loc sel : 'stream_exp )))));
        ([],
          ("Compile_stream.empty _loc\n",
            (Gramf.mk_action
               (fun (_loc : Locf.t)  ->
                  (Compile_stream.empty _loc : 'stream_exp )))))]));
  Gramf.extend_single (stream_exp_comp : 'stream_exp_comp Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(Trm (_loc, e) : Compile_stream.sexp_comp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  ->
                   ((Trm (_loc, e) : Compile_stream.sexp_comp ) : 'stream_exp_comp )))));
        ([`Skeyword "'"; `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
          ("Ntr (_loc, e)\n",
            (Gramf.mk_action
               (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                  (Ntr (_loc, e) : 'stream_exp_comp )))))]));
  Gramf.extend_single (stream_exp_comp_list : 'stream_exp_comp_list Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (stream_exp_comp : 'stream_exp_comp Gramf.t ));
          `Skeyword ";";
          `Sself],
           ("se :: sel\n",
             (Gramf.mk_action
                (fun (sel : 'stream_exp_comp_list)  _ 
                   (se : 'stream_exp_comp)  (_loc : Locf.t)  -> (se ::
                   sel : 'stream_exp_comp_list )))));
        ([`Snterm (Gramf.obj (stream_exp_comp : 'stream_exp_comp Gramf.t ));
         `Skeyword ";"],
          ("[se]\n",
            (Gramf.mk_action
               (fun _  (se : 'stream_exp_comp)  (_loc : Locf.t)  ->
                  ([se] : 'stream_exp_comp_list )))));
        ([`Snterm (Gramf.obj (stream_exp_comp : 'stream_exp_comp Gramf.t ))],
          ("[se]\n",
            (Gramf.mk_action
               (fun (se : 'stream_exp_comp)  (_loc : Locf.t)  ->
                  ([se] : 'stream_exp_comp_list )))))]))
let _ = Ast_quotation.of_exp ~name:(Ns.lang, "stream") ~entry:stream_exp ()