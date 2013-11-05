let cparser = Compile_stream.cparser
let cstream = Compile_stream.cstream
let exp = Syntaxf.exp
let a_lident = Syntaxf.a_lident
let pat = Syntaxf.pat
open FAst
let parser_ipat = Gramf.mk "parser_ipat"
let parser_exp = Gramf.mk "parser_exp"
let stream_pat_comp = Gramf.mk "stream_pat_comp"
let stream_pat_comp_err = Gramf.mk "stream_pat_comp_err"
let stream_pat_comp_err_list = Gramf.mk "stream_pat_comp_err_list"
let stream_pat = Gramf.mk "stream_pat"
let parser_case = Gramf.mk "parser_case"
let parser_case_list = Gramf.mk "parser_case_list"
let _ =
  Gramf.extend_single (parser_exp : 'parser_exp Gramf.t )
    (None,
      ((None, None,
         [([`Nterm
              (Gramf.obj (parser_case_list : 'parser_case_list Gramf.t ))],
            ("match n with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser _loc pcl)\n| None  -> cparser _loc pcl\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(pcl : 'parser_case_list)  (_loc : Locf.t)  ->
                    let n = None in
                    (match n with
                     | Some o ->
                         Ref.protect Compile_stream.grammar_module_name o
                           (fun _  -> cparser _loc pcl)
                     | None  -> cparser _loc pcl : 'parser_exp )))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (`Uid, `Any),
               "Uid");
          `Nterm (Gramf.obj (parser_case_list : 'parser_case_list Gramf.t ))],
           ("match n with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser _loc pcl)\n| None  -> cparser _loc pcl\n",
             (Gramf.mk_action
                (fun ~__fan_1:(pcl : 'parser_case_list) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = n;_} : Tokenf.txt) ->
                       let n = Some n in
                       ((match n with
                         | Some o ->
                             Ref.protect Compile_stream.grammar_module_name o
                               (fun _  -> cparser _loc pcl)
                         | None  -> cparser _loc pcl) : 'parser_exp )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (parser_ipat : 'parser_ipat Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("(i : alident  :>pat)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                    ((i : alident  :>pat) : 'parser_ipat )))));
         ([`Keyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Any _loc : FAst.pat ) : 'parser_ipat )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (parser_case_list : 'parser_case_list Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "|";
           `List0sep
             ((`Nterm (Gramf.obj (parser_case : 'parser_case Gramf.t ))),
               (`Keyword "|"))],
            ("pcl\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(pcl : 'parser_case list)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (pcl : 'parser_case_list )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (parser_case : 'parser_case Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (stream_pat : 'stream_pat Gramf.t ));
           `Keyword "->";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(sp, None, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(sp : 'stream_pat)  (_loc : Locf.t)  ->
                    ((sp, None, e) : 'parser_case )))))]) : Gramf.olevel ));
  Gramf.extend_single (stream_pat : 'stream_pat Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ))],
            ("[(spc, None)]\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                    ([(spc, None)] : 'stream_pat )))));
         ([`Nterm (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ));
          `Keyword ";";
          `Nterm
            (Gramf.obj
               (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gramf.t ))],
           ("(spc, None) :: sp\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sp : 'stream_pat_comp_err_list)  ~__fan_1:_ 
                   ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                   ((spc, None) :: sp : 'stream_pat )))));
         ([],
           ("[]\n",
             (Gramf.mk_action (fun (_loc : Locf.t)  -> ([] : 'stream_pat )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (stream_pat_comp : 'stream_pat_comp Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword "when";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("When (_loc, p, (Some e))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:(p : 'pat) 
                    (_loc : Locf.t)  ->
                    (When (_loc, p, (Some e)) : 'stream_pat_comp )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("When (_loc, p, None)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                   (When (_loc, p, None) : 'stream_pat_comp )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("Match (_loc, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:(p : 'pat) 
                   (_loc : Locf.t)  ->
                   (Match (_loc, p, e) : 'stream_pat_comp )))));
         ([`Keyword "'"; `Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("Str (_loc, p)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (Str (_loc, p) : 'stream_pat_comp )))))]) : Gramf.olevel ));
  Gramf.extend_single (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ));
           `Keyword "??";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(spc, (Some e))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                    ((spc, (Some e)) : 'stream_pat_comp_err )))));
         ([`Nterm (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ))],
           ("(spc, None)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                   ((spc, None) : 'stream_pat_comp_err )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm
              (Gramf.obj
                 (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t ))],
            ("[spc]\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(spc : 'stream_pat_comp_err)  (_loc : Locf.t) 
                    -> ([spc] : 'stream_pat_comp_err_list )))));
         ([`Nterm
             (Gramf.obj (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t ));
          `Keyword ";"],
           ("[spc]\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(spc : 'stream_pat_comp_err) 
                   (_loc : Locf.t)  -> ([spc] : 'stream_pat_comp_err_list )))));
         ([`Nterm
             (Gramf.obj (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t ));
          `Keyword ";";
          `Self],
           ("spc :: sp\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sp : 'stream_pat_comp_err_list)  ~__fan_1:_ 
                   ~__fan_0:(spc : 'stream_pat_comp_err)  (_loc : Locf.t)  ->
                   (spc :: sp : 'stream_pat_comp_err_list )))))]) : Gramf.olevel ))
let () = Ast_quotation.of_exp ~name:(Ns.lang, "parser") ~entry:parser_exp ()
