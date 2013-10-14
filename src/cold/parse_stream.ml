open FAst
open! Fsyntax
open Compile_stream
let parser_ipat = Fgram.mk "parser_ipat"
let stream_exp_comp = Fgram.mk "stream_exp_comp"
let stream_exp_comp_list = Fgram.mk "stream_exp_comp_list"
let stream_pat_comp = Fgram.mk "stream_pat_comp"
let stream_pat_comp_err = Fgram.mk "stream_pat_comp_err"
let stream_pat_comp_err_list = Fgram.mk "stream_pat_comp_err_list"
let stream_pat = Fgram.mk "stream_pat"
let parser_case = Fgram.mk "parser_case"
let parser_case_list = Fgram.mk "parser_case_list"
let stream_exp = Fgram.mk "stream_exp"
let apply () =
  let grammar_entry_create x = Fgram.mk x in
  let uid: 'uid Fgram.t = grammar_entry_create "uid" in
  Fgram.extend_single (uid : 'uid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
           ("n\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> Ftoken.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid n -> (n : 'uid )
                   | _ -> failwith "n\n"))))]));
  Fgram.extend_single (exp : 'exp Fgram.t )
    ((Some (`Level "top")),
      (None, None,
        [([`Skeyword "parser";
          `Sopt (`Snterm (Fgram.obj (uid : 'uid Fgram.t )));
          `Sopt (`Snterm (Fgram.obj (parser_ipat : 'parser_ipat Fgram.t )));
          `Snterm (Fgram.obj (parser_case_list : 'parser_case_list Fgram.t ))],
           ("match name with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser _loc po pcl)\n| None  -> cparser _loc po pcl\n",
             (Fgram.mk_action
                (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                   (name : 'uid option)  _  (_loc : Locf.t)  ->
                   (match name with
                    | Some o ->
                        Ref.protect Compile_stream.grammar_module_name o
                          (fun _  -> cparser _loc po pcl)
                    | None  -> cparser _loc po pcl : 'exp )))));
        ([`Skeyword "match";
         `Sself;
         `Skeyword "with";
         `Skeyword "parser";
         `Sopt (`Snterm (Fgram.obj (uid : 'uid Fgram.t )));
         `Sopt (`Snterm (Fgram.obj (parser_ipat : 'parser_ipat Fgram.t )));
         `Snterm (Fgram.obj (parser_case_list : 'parser_case_list Fgram.t ))],
          ("match name with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser_match _loc e po pcl)\n| None  -> cparser_match _loc e po pcl\n",
            (Fgram.mk_action
               (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                  (name : 'uid option)  _  _  (e : 'exp)  _  (_loc : Locf.t) 
                  ->
                  (match name with
                   | Some o ->
                       Ref.protect Compile_stream.grammar_module_name o
                         (fun _  -> cparser_match _loc e po pcl)
                   | None  -> cparser_match _loc e po pcl : 'exp )))))]));
  Fgram.extend_single (stream_exp : 'stream_exp Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "!";
          `Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
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
           (((function | `Uid _ -> true | _ -> false)),
             (`App ((`Vrn "Uid"), `Any)), "`Uid _");
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
           ("SpWhen (_loc, p, (Some e))\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                   (SpWhen (_loc, p, (Some e)) : 'stream_pat_comp )))));
        ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
          ("SpWhen (_loc, p, None)\n",
            (Fgram.mk_action
               (fun (p : 'pat)  (_loc : Locf.t)  ->
                  (SpWhen (_loc, p, None) : 'stream_pat_comp )))));
        ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
         `Skeyword "=";
         `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
          ("SpMatch (_loc, p, e)\n",
            (Fgram.mk_action
               (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                  (SpMatch (_loc, p, e) : 'stream_pat_comp )))));
        ([`Skeyword "'"; `Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
          ("SpStr (_loc, p)\n",
            (Fgram.mk_action
               (fun (p : 'pat)  _  (_loc : Locf.t)  ->
                  (SpStr (_loc, p) : 'stream_pat_comp )))))]));
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
                  sp : 'stream_pat_comp_err_list )))))]));
  Fgram.extend_single (stream_exp_comp : 'stream_exp_comp Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("SeTrm (_loc, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  ->
                   (SeTrm (_loc, e) : 'stream_exp_comp )))));
        ([`Skeyword "'"; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
          ("SeNtr (_loc, e)\n",
            (Fgram.mk_action
               (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                  (SeNtr (_loc, e) : 'stream_exp_comp )))))]));
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
let fill_parsers =
  let applied = ref false in
  fun ()  -> if not applied.contents then (apply (); applied := true)
let () = Ast_parsers.register_parser ("stream", fill_parsers)