let cstream = Compile_stream.cstream
let exp = Syntaxf.exp
let stream_exp = Gramf.mk "stream_exp"
let stream_exp_comp = Gramf.mk "stream_exp_comp"
let stream_exp_comp_list = Gramf.mk "stream_exp_comp_list"
let _ =
  Gramf.extend_single (stream_exp : 'stream_exp Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "!";
           `Token
             (((function | `Uid _ -> true | _ -> false)),
               ({ tag = `Uid; word = Any; tag_name = "Uid" } : Tokenf.descr ))],
            ("Ref.protect Compile_stream.grammar_module_name n\n  (fun _  -> Compile_stream.empty _loc)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | ({ txt = n;_} : Tokenf.txt) ->
                        (Ref.protect Compile_stream.grammar_module_name n
                           (fun _  -> Compile_stream.empty _loc) : 'stream_exp )))));
         ([`Keyword "!";
          `Token
            (((function | `Uid _ -> true | _ -> false)),
              ({ tag = `Uid; word = Any; tag_name = "Uid" } : Tokenf.descr ));
          `Nterm
            (Gramf.obj
               (stream_exp_comp_list : 'stream_exp_comp_list Gramf.t ))],
           ("Ref.protect Compile_stream.grammar_module_name n (fun _  -> cstream _loc sel)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sel : 'stream_exp_comp_list) 
                   ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | ({ txt = n;_} : Tokenf.txt) ->
                       (Ref.protect Compile_stream.grammar_module_name n
                          (fun _  -> cstream _loc sel) : 'stream_exp )))));
         ([`Nterm
             (Gramf.obj
                (stream_exp_comp_list : 'stream_exp_comp_list Gramf.t ))],
           ("cstream _loc sel\n",
             (Gramf.mk_action
                (fun ~__fan_0:(sel : 'stream_exp_comp_list)  (_loc : Locf.t) 
                   -> (cstream _loc sel : 'stream_exp )))));
         ([],
           ("Compile_stream.empty _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  ->
                   (Compile_stream.empty _loc : 'stream_exp )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (stream_exp_comp : 'stream_exp_comp Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(Trm (_loc, e) : Compile_stream.sexp_comp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                    ((Trm (_loc, e) : Compile_stream.sexp_comp ) : 'stream_exp_comp )))));
         ([`Keyword "'"; `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("Ntr (_loc, e)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (Ntr (_loc, e) : 'stream_exp_comp )))))]) : Gramf.olevel ));
  Gramf.extend_single (stream_exp_comp_list : 'stream_exp_comp_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (stream_exp_comp : 'stream_exp_comp Gramf.t ));
           `Keyword ";";
           `Self],
            ("se :: sel\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(sel : 'stream_exp_comp_list)  ~__fan_1:_ 
                    ~__fan_0:(se : 'stream_exp_comp)  (_loc : Locf.t)  -> (se
                    :: sel : 'stream_exp_comp_list )))));
         ([`Nterm (Gramf.obj (stream_exp_comp : 'stream_exp_comp Gramf.t ));
          `Keyword ";"],
           ("[se]\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(se : 'stream_exp_comp) 
                   (_loc : Locf.t)  -> ([se] : 'stream_exp_comp_list )))));
         ([`Nterm (Gramf.obj (stream_exp_comp : 'stream_exp_comp Gramf.t ))],
           ("[se]\n",
             (Gramf.mk_action
                (fun ~__fan_0:(se : 'stream_exp_comp)  (_loc : Locf.t)  ->
                   ([se] : 'stream_exp_comp_list )))))]) : Gramf.olevel ))
let _ = Ast_quotation.of_exp ~name:(Ns.lang, "stream") ~entry:stream_exp ()
