let cparser = Compile_stream.cparser
let cstream = Compile_stream.cstream
let exp = Syntaxf.exp
let a_lident = Syntaxf.a_lident
let pat = Syntaxf.pat
open FAst
let parser_ipat = Fgram.mk "parser_ipat"
let parser_exp = Fgram.mk "parser_exp"
let stream_pat_comp = Fgram.mk "stream_pat_comp"
let stream_pat_comp_err = Fgram.mk "stream_pat_comp_err"
let stream_pat_comp_err_list = Fgram.mk "stream_pat_comp_err_list"
let stream_pat = Fgram.mk "stream_pat"
let parser_case = Fgram.mk "parser_case"
let parser_case_list = Fgram.mk "parser_case_list"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let uid: 'uid Fgram.t = grammar_entry_create "uid" in
  Fgram.extend_single (uid : 'uid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)), ("Uid", `Any),
               "`Uid _")],
           ("n\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid n -> (n : 'uid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (parser_exp : 'parser_exp Fgram.t )
    (None,
      (None, None,
        [([`Sopt (`Snterm (Fgram.obj (uid : 'uid Fgram.t )));
          `Snterm (Fgram.obj (parser_case_list : 'parser_case_list Fgram.t ))],
           ("match name with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser _loc pcl)\n| None  -> cparser _loc pcl\n",
             (Fgram.mk_action
                (fun (pcl : 'parser_case_list)  (name : 'uid option) 
                   (_loc : Locf.t)  ->
                   (match name with
                    | Some o ->
                        Ref.protect Compile_stream.grammar_module_name o
                          (fun _  -> cparser _loc pcl)
                    | None  -> cparser _loc pcl : 'parser_exp )))))]));
  Fgram.extend_single (parser_ipat : 'parser_ipat Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("(i : alident  :>pat)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i : alident  :>pat) : 'parser_ipat )))));
        ([`Skeyword "_"],
          ("(`Any _loc : FAst.pat )\n",
            (Fgram.mk_action
               (fun _  (_loc : Locf.t)  ->
                  ((`Any _loc : FAst.pat ) : 'parser_ipat )))))]));
  Fgram.extend_single (parser_case_list : 'parser_case_list Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Slist0sep
            ((`Snterm (Fgram.obj (parser_case : 'parser_case Fgram.t ))),
              (`Skeyword "|"))],
           ("pcl\n",
             (Fgram.mk_action
                (fun (pcl : 'parser_case list)  _  (_loc : Locf.t)  ->
                   (pcl : 'parser_case_list )))))]));
  Fgram.extend_single (parser_case : 'parser_case Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (stream_pat : 'stream_pat Fgram.t ));
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(sp, None, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (sp : 'stream_pat)  (_loc : Locf.t)  ->
                   ((sp, None, e) : 'parser_case )))))]));
  Fgram.extend_single (stream_pat : 'stream_pat Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ))],
           ("[(spc, None)]\n",
             (Fgram.mk_action
                (fun (spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                   ([(spc, None)] : 'stream_pat )))));
        ([`Snterm (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ));
         `Skeyword ";";
         `Snterm
           (Fgram.obj
              (stream_pat_comp_err_list : 'stream_pat_comp_err_list Fgram.t ))],
          ("(spc, None) :: sp\n",
            (Fgram.mk_action
               (fun (sp : 'stream_pat_comp_err_list)  _ 
                  (spc : 'stream_pat_comp)  (_loc : Locf.t)  -> ((spc, None)
                  :: sp : 'stream_pat )))));
        ([],
          ("[]\n",
            (Fgram.mk_action (fun (_loc : Locf.t)  -> ([] : 'stream_pat )))))]));
  Fgram.extend_single (stream_pat_comp : 'stream_pat_comp Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword "when";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("When (_loc, p, (Some e))\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                   (When (_loc, p, (Some e)) : 'stream_pat_comp )))));
        ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
          ("When (_loc, p, None)\n",
            (Fgram.mk_action
               (fun (p : 'pat)  (_loc : Locf.t)  ->
                  (When (_loc, p, None) : 'stream_pat_comp )))));
        ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
         `Skeyword "=";
         `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
          ("Match (_loc, p, e)\n",
            (Fgram.mk_action
               (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                  (Match (_loc, p, e) : 'stream_pat_comp )))));
        ([`Skeyword "'"; `Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
          ("Str (_loc, p)\n",
            (Fgram.mk_action
               (fun (p : 'pat)  _  (_loc : Locf.t)  ->
                  (Str (_loc, p) : 'stream_pat_comp )))))]));
  Fgram.extend_single (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ));
          `Skeyword "??";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(spc, (Some e))\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (spc : 'stream_pat_comp)  (_loc : Locf.t)
                    -> ((spc, (Some e)) : 'stream_pat_comp_err )))));
        ([`Snterm (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ))],
          ("(spc, None)\n",
            (Fgram.mk_action
               (fun (spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                  ((spc, None) : 'stream_pat_comp_err )))))]));
  Fgram.extend_single
    (stream_pat_comp_err_list : 'stream_pat_comp_err_list Fgram.t )
    (None,
      (None, None,
        [([`Snterm
             (Fgram.obj (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t ))],
           ("[spc]\n",
             (Fgram.mk_action
                (fun (spc : 'stream_pat_comp_err)  (_loc : Locf.t)  ->
                   ([spc] : 'stream_pat_comp_err_list )))));
        ([`Snterm
            (Fgram.obj (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t ));
         `Skeyword ";"],
          ("[spc]\n",
            (Fgram.mk_action
               (fun _  (spc : 'stream_pat_comp_err)  (_loc : Locf.t)  ->
                  ([spc] : 'stream_pat_comp_err_list )))));
        ([`Snterm
            (Fgram.obj (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t ));
         `Skeyword ";";
         `Sself],
          ("spc :: sp\n",
            (Fgram.mk_action
               (fun (sp : 'stream_pat_comp_err_list)  _ 
                  (spc : 'stream_pat_comp_err)  (_loc : Locf.t)  -> (spc ::
                  sp : 'stream_pat_comp_err_list )))))]))
let () = Ast_quotation.of_exp ~name:(Ns.lang, "parser") ~entry:parser_exp ()