let cstream = Compile_stream.cstream
let exp = Syntaxf.exp
let stream_exp = Fgram.mk "stream_exp"
let stream_exp_comp = Fgram.mk "stream_exp_comp"
let stream_exp_comp_list = Fgram.mk "stream_exp_comp_list"
let _ =
  Fgram.extend_single (stream_exp : 'stream_exp Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "!";
          `Stoken
            (((function | `Uid _ -> true | _ -> false)), ("Uid", `Any),
              "`Uid _")],
           ("Ref.protect Compile_stream.grammar_module_name n\n  (fun _  -> Compile_stream.empty _loc)\n",
             (Fgram.mk_action
                (fun (__fan_1 : [> Ftoken.t])  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Uid n ->
                       (Ref.protect Compile_stream.grammar_module_name n
                          (fun _  -> Compile_stream.empty _loc) : 'stream_exp )
                   | _ ->
                       failwith
                         "Ref.protect Compile_stream.grammar_module_name n\n  (fun _  -> Compile_stream.empty _loc)\n"))));
        ([`Skeyword "!";
         `Stoken
           (((function | `Uid _ -> true | _ -> false)), ("Uid", `Any),
             "`Uid _");
         `Snterm
           (Fgram.obj (stream_exp_comp_list : 'stream_exp_comp_list Fgram.t ))],
          ("Ref.protect Compile_stream.grammar_module_name n (fun _  -> cstream _loc sel)\n",
            (Fgram.mk_action
               (fun (sel : 'stream_exp_comp_list)  (__fan_1 : [> Ftoken.t]) 
                  _  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Uid n ->
                      (Ref.protect Compile_stream.grammar_module_name n
                         (fun _  -> cstream _loc sel) : 'stream_exp )
                  | _ ->
                      failwith
                        "Ref.protect Compile_stream.grammar_module_name n (fun _  -> cstream _loc sel)\n"))));
        ([`Snterm
            (Fgram.obj
               (stream_exp_comp_list : 'stream_exp_comp_list Fgram.t ))],
          ("cstream _loc sel\n",
            (Fgram.mk_action
               (fun (sel : 'stream_exp_comp_list)  (_loc : Locf.t)  ->
                  (cstream _loc sel : 'stream_exp )))));
        ([],
          ("Compile_stream.empty _loc\n",
            (Fgram.mk_action
               (fun (_loc : Locf.t)  ->
                  (Compile_stream.empty _loc : 'stream_exp )))))]));
  Fgram.extend_single (stream_exp_comp : 'stream_exp_comp Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(Trm (_loc, e) : Compile_stream.sexp_comp )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  ->
                   ((Trm (_loc, e) : Compile_stream.sexp_comp ) : 'stream_exp_comp )))));
        ([`Skeyword "'"; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
          ("Ntr (_loc, e)\n",
            (Fgram.mk_action
               (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                  (Ntr (_loc, e) : 'stream_exp_comp )))))]));
  Fgram.extend_single (stream_exp_comp_list : 'stream_exp_comp_list Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (stream_exp_comp : 'stream_exp_comp Fgram.t ));
          `Skeyword ";";
          `Sself],
           ("se :: sel\n",
             (Fgram.mk_action
                (fun (sel : 'stream_exp_comp_list)  _ 
                   (se : 'stream_exp_comp)  (_loc : Locf.t)  -> (se ::
                   sel : 'stream_exp_comp_list )))));
        ([`Snterm (Fgram.obj (stream_exp_comp : 'stream_exp_comp Fgram.t ));
         `Skeyword ";"],
          ("[se]\n",
            (Fgram.mk_action
               (fun _  (se : 'stream_exp_comp)  (_loc : Locf.t)  ->
                  ([se] : 'stream_exp_comp_list )))));
        ([`Snterm (Fgram.obj (stream_exp_comp : 'stream_exp_comp Fgram.t ))],
          ("[se]\n",
            (Fgram.mk_action
               (fun (se : 'stream_exp_comp)  (_loc : Locf.t)  ->
                  ([se] : 'stream_exp_comp_list )))))]))
let _ = Ast_quotation.of_exp ~name:(Ns.lang, "stream") ~entry:stream_exp ()