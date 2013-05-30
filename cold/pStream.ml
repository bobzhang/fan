open FAst

open Fsyntax

open LibUtil

open FStreamGen

let parser_ipat = Fgram.mk "parser_ipat"

let stream_exp_comp = Fgram.mk "stream_exp_comp"

let stream_exp_comp_list = Fgram.mk "stream_exp_comp_list"

let stream_pat_comp = Fgram.mk "stream_pat_comp"

let stream_pat_comp_err = Fgram.mk "stream_pat_comp_err"

let stream_pat_comp_err_list = Fgram.mk "stream_pat_comp_err_list"

let stream_begin = Fgram.mk "stream_begin"

let stream_pat = Fgram.mk "stream_pat"

let parser_case = Fgram.mk "parser_case"

let parser_case_list = Fgram.mk "parser_case_list"

let stream_exp = Fgram.mk "stream_exp"

let apply () =
  begin
    Fgram.extend_single (exp : 'exp Fgram.t )
      ((Some (`Level "top")),
        (None, None,
          [([`Skeyword "parser";
            `Sopt
              (Fgram.srules
                 [([`Stoken
                      (((function | `Uid _ -> true | _ -> false)),
                        (`Normal, "`Uid _"))],
                    ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Uid n -> (n : 'e__1 ) | _ -> failwith \"n\n\")\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid n -> (n : 'e__1 )
                            | _ -> failwith "n\n"))))]);
            `Sopt (`Snterm (Fgram.obj (parser_ipat : 'parser_ipat Fgram.t )));
            `Snterm
              (Fgram.obj (parser_case_list : 'parser_case_list Fgram.t ))],
             ("Fgram.mk_action\n  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) \n     (name : 'e__1 option)  _  (_loc : FLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FStreamGen.grammar_module_name o\n            (fun _  -> cparser _loc po pcl)\n      | None  -> cparser _loc po pcl : 'exp ))\n",
               (Fgram.mk_action
                  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                     (name : 'e__1 option)  _  (_loc : FLoc.t)  ->
                     (match name with
                      | Some o ->
                          Ref.protect FStreamGen.grammar_module_name o
                            (fun _  -> cparser _loc po pcl)
                      | None  -> cparser _loc po pcl : 'exp )))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Skeyword "parser";
           `Sopt
             (Fgram.srules
                [([`Stoken
                     (((function | `Uid _ -> true | _ -> false)),
                       (`Normal, "`Uid _"))],
                   ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Uid n -> (n : 'e__2 ) | _ -> failwith \"n\n\")\n",
                     (Fgram.mk_action
                        (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                           match __fan_0 with
                           | `Uid n -> (n : 'e__2 )
                           | _ -> failwith "n\n"))))]);
           `Sopt (`Snterm (Fgram.obj (parser_ipat : 'parser_ipat Fgram.t )));
           `Snterm
             (Fgram.obj (parser_case_list : 'parser_case_list Fgram.t ))],
            ("Fgram.mk_action\n  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) \n     (name : 'e__2 option)  _  _  (e : 'exp)  _  (_loc : FLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FStreamGen.grammar_module_name o\n            (fun _  -> cparser_match _loc e po pcl)\n      | None  -> cparser_match _loc e po pcl : 'exp ))\n",
              (Fgram.mk_action
                 (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                    (name : 'e__2 option)  _  _  (e : 'exp)  _ 
                    (_loc : FLoc.t)  ->
                    (match name with
                     | Some o ->
                         Ref.protect FStreamGen.grammar_module_name o
                           (fun _  -> cparser_match _loc e po pcl)
                     | None  -> cparser_match _loc e po pcl : 'exp )))))]));
    Fgram.extend_single (stream_exp : 'stream_exp Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "!";
            `Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
             ("Fgram.mk_action\n  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Uid n ->\n         (Ref.protect FStreamGen.grammar_module_name n\n            (fun _  -> FStreamGen.empty _loc) : 'stream_exp )\n     | _ ->\n         failwith\n           \"Ref.protect FStreamGen.grammar_module_name n\n  (fun _  -> FStreamGen.empty _loc)\n\")\n",
               (Fgram.mk_action
                  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                     match __fan_1 with
                     | `Uid n ->
                         (Ref.protect FStreamGen.grammar_module_name n
                            (fun _  -> FStreamGen.empty _loc) : 'stream_exp )
                     | _ ->
                         failwith
                           "Ref.protect FStreamGen.grammar_module_name n\n  (fun _  -> FStreamGen.empty _loc)\n"))));
          ([`Skeyword "!";
           `Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
           `Snterm
             (Fgram.obj
                (stream_exp_comp_list : 'stream_exp_comp_list Fgram.t ))],
            ("Fgram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Uid n ->\n         (Ref.protect FStreamGen.grammar_module_name n\n            (fun _  -> cstream _loc sel) : 'stream_exp )\n     | _ ->\n         failwith\n           \"Ref.protect FStreamGen.grammar_module_name n (fun _  -> cstream _loc sel)\n\")\n",
              (Fgram.mk_action
                 (fun (sel : 'stream_exp_comp_list)  (__fan_1 : [> FToken.t])
                     _  (_loc : FLoc.t)  ->
                    match __fan_1 with
                    | `Uid n ->
                        (Ref.protect FStreamGen.grammar_module_name n
                           (fun _  -> cstream _loc sel) : 'stream_exp )
                    | _ ->
                        failwith
                          "Ref.protect FStreamGen.grammar_module_name n (fun _  -> cstream _loc sel)\n"))));
          ([`Snterm
              (Fgram.obj
                 (stream_exp_comp_list : 'stream_exp_comp_list Fgram.t ))],
            ("Fgram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  (_loc : FLoc.t)  ->\n     (cstream _loc sel : 'stream_exp ))\n",
              (Fgram.mk_action
                 (fun (sel : 'stream_exp_comp_list)  (_loc : FLoc.t)  ->
                    (cstream _loc sel : 'stream_exp )))));
          ([],
            ("Fgram.mk_action\n  (fun (_loc : FLoc.t)  -> (FStreamGen.empty _loc : 'stream_exp ))\n",
              (Fgram.mk_action
                 (fun (_loc : FLoc.t)  ->
                    (FStreamGen.empty _loc : 'stream_exp )))))]));
    Fgram.extend_single (parser_ipat : 'parser_ipat Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
             ("Fgram.mk_action\n  (fun (i : 'a_lident)  (_loc : FLoc.t)  ->\n     ((i : alident  :>pat) : 'parser_ipat ))\n",
               (Fgram.mk_action
                  (fun (i : 'a_lident)  (_loc : FLoc.t)  ->
                     ((i : alident  :>pat) : 'parser_ipat )))));
          ([`Skeyword "_"],
            ("Fgram.mk_action\n  (fun _  (_loc : FLoc.t)  -> ((`Any _loc : FAst.pat ) : 'parser_ipat ))\n",
              (Fgram.mk_action
                 (fun _  (_loc : FLoc.t)  ->
                    ((`Any _loc : FAst.pat ) : 'parser_ipat )))))]));
    Fgram.extend_single (parser_case_list : 'parser_case_list Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "|";
            `Slist0sep
              ((`Snterm (Fgram.obj (parser_case : 'parser_case Fgram.t ))),
                (`Skeyword "|"))],
             ("Fgram.mk_action\n  (fun (pcl : 'parser_case list)  _  (_loc : FLoc.t)  ->\n     (pcl : 'parser_case_list ))\n",
               (Fgram.mk_action
                  (fun (pcl : 'parser_case list)  _  (_loc : FLoc.t)  ->
                     (pcl : 'parser_case_list )))))]));
    Fgram.extend_single (parser_case : 'parser_case Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (stream_pat : 'stream_pat Fgram.t ));
            `Skeyword "->";
            `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
             ("Fgram.mk_action\n  (fun (e : 'exp)  _  (sp : 'stream_pat)  (_loc : FLoc.t)  ->\n     ((sp, None, e) : 'parser_case ))\n",
               (Fgram.mk_action
                  (fun (e : 'exp)  _  (sp : 'stream_pat)  (_loc : FLoc.t)  ->
                     ((sp, None, e) : 'parser_case )))))]));
    Fgram.extend_single (stream_begin : 'stream_begin Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "[<";
            `Sopt
              (Fgram.srules
                 [([`Skeyword "!";
                   `Stoken
                     (((function | `Uid _ -> true | _ -> false)),
                       (`Normal, "`Uid _"))],
                    ("Fgram.mk_action\n  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->\n     match __fan_1 with | `Uid n -> (n : 'e__3 ) | _ -> failwith \"n\n\")\n",
                      (Fgram.mk_action
                         (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t) 
                            ->
                            match __fan_1 with
                            | `Uid n -> (n : 'e__3 )
                            | _ -> failwith "n\n"))))])],
             ("Fgram.mk_action\n  (fun (name : 'e__3 option)  _  (_loc : FLoc.t)  -> (name : 'stream_begin ))\n",
               (Fgram.mk_action
                  (fun (name : 'e__3 option)  _  (_loc : FLoc.t)  ->
                     (name : 'stream_begin )))))]));
    Fgram.extend_single (stream_pat : 'stream_pat Fgram.t )
      (None,
        (None, None,
          [([`Snterm
               (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ))],
             ("Fgram.mk_action\n  (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->\n     ([(spc, None)] : 'stream_pat ))\n",
               (Fgram.mk_action
                  (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->
                     ([(spc, None)] : 'stream_pat )))));
          ([`Snterm (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ));
           `Skeyword ";";
           `Snterm
             (Fgram.obj
                (stream_pat_comp_err_list : 'stream_pat_comp_err_list Fgram.t ))],
            ("Fgram.mk_action\n  (fun (sp : 'stream_pat_comp_err_list)  _  (spc : 'stream_pat_comp) \n     (_loc : FLoc.t)  -> ((spc, None) :: sp : 'stream_pat ))\n",
              (Fgram.mk_action
                 (fun (sp : 'stream_pat_comp_err_list)  _ 
                    (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->
                    ((spc, None) :: sp : 'stream_pat )))));
          ([],
            ("Fgram.mk_action (fun (_loc : FLoc.t)  -> ([] : 'stream_pat ))\n",
              (Fgram.mk_action (fun (_loc : FLoc.t)  -> ([] : 'stream_pat )))))]));
    Fgram.extend_single (stream_pat_comp : 'stream_pat_comp Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
            `Skeyword "when";
            `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
             ("Fgram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->\n     (SpWhen (_loc, p, (Some e)) : 'stream_pat_comp ))\n",
               (Fgram.mk_action
                  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->
                     (SpWhen (_loc, p, (Some e)) : 'stream_pat_comp )))));
          ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
            ("Fgram.mk_action\n  (fun (p : 'pat)  (_loc : FLoc.t)  ->\n     (SpWhen (_loc, p, None) : 'stream_pat_comp ))\n",
              (Fgram.mk_action
                 (fun (p : 'pat)  (_loc : FLoc.t)  ->
                    (SpWhen (_loc, p, None) : 'stream_pat_comp )))));
          ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
           `Skeyword "=";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("Fgram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->\n     (SpMatch (_loc, p, e) : 'stream_pat_comp ))\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->
                    (SpMatch (_loc, p, e) : 'stream_pat_comp )))));
          ([`Skeyword "'"; `Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
            ("Fgram.mk_action\n  (fun (p : 'pat)  _  (_loc : FLoc.t)  ->\n     (SpStr (_loc, p) : 'stream_pat_comp ))\n",
              (Fgram.mk_action
                 (fun (p : 'pat)  _  (_loc : FLoc.t)  ->
                    (SpStr (_loc, p) : 'stream_pat_comp )))))]));
    Fgram.extend_single (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t )
      (None,
        (None, None,
          [([`Snterm
               (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ));
            `Skeyword "??";
            `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
             ("Fgram.mk_action\n  (fun (e : 'exp)  _  (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->\n     ((spc, (Some e)) : 'stream_pat_comp_err ))\n",
               (Fgram.mk_action
                  (fun (e : 'exp)  _  (spc : 'stream_pat_comp) 
                     (_loc : FLoc.t)  ->
                     ((spc, (Some e)) : 'stream_pat_comp_err )))));
          ([`Snterm (Fgram.obj (stream_pat_comp : 'stream_pat_comp Fgram.t ))],
            ("Fgram.mk_action\n  (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->\n     ((spc, None) : 'stream_pat_comp_err ))\n",
              (Fgram.mk_action
                 (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->
                    ((spc, None) : 'stream_pat_comp_err )))))]));
    Fgram.extend_single
      (stream_pat_comp_err_list : 'stream_pat_comp_err_list Fgram.t )
      (None,
        (None, None,
          [([`Snterm
               (Fgram.obj
                  (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t ))],
             ("Fgram.mk_action\n  (fun (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->\n     ([spc] : 'stream_pat_comp_err_list ))\n",
               (Fgram.mk_action
                  (fun (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->
                     ([spc] : 'stream_pat_comp_err_list )))));
          ([`Snterm
              (Fgram.obj
                 (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t ));
           `Skeyword ";"],
            ("Fgram.mk_action\n  (fun _  (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->\n     ([spc] : 'stream_pat_comp_err_list ))\n",
              (Fgram.mk_action
                 (fun _  (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->
                    ([spc] : 'stream_pat_comp_err_list )))));
          ([`Snterm
              (Fgram.obj
                 (stream_pat_comp_err : 'stream_pat_comp_err Fgram.t ));
           `Skeyword ";";
           `Sself],
            ("Fgram.mk_action\n  (fun (sp : 'stream_pat_comp_err_list)  _  (spc : 'stream_pat_comp_err) \n     (_loc : FLoc.t)  -> (spc :: sp : 'stream_pat_comp_err_list ))\n",
              (Fgram.mk_action
                 (fun (sp : 'stream_pat_comp_err_list)  _ 
                    (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  -> (spc ::
                    sp : 'stream_pat_comp_err_list )))))]));
    Fgram.extend_single (stream_exp_comp : 'stream_exp_comp Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
             ("Fgram.mk_action\n  (fun (e : 'exp)  (_loc : FLoc.t)  -> (SeTrm (_loc, e) : 'stream_exp_comp ))\n",
               (Fgram.mk_action
                  (fun (e : 'exp)  (_loc : FLoc.t)  ->
                     (SeTrm (_loc, e) : 'stream_exp_comp )))));
          ([`Skeyword "'"; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("Fgram.mk_action\n  (fun (e : 'exp)  _  (_loc : FLoc.t)  ->\n     (SeNtr (_loc, e) : 'stream_exp_comp ))\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (_loc : FLoc.t)  ->
                    (SeNtr (_loc, e) : 'stream_exp_comp )))))]));
    Fgram.extend_single
      (stream_exp_comp_list : 'stream_exp_comp_list Fgram.t )
      (None,
        (None, None,
          [([`Snterm
               (Fgram.obj (stream_exp_comp : 'stream_exp_comp Fgram.t ));
            `Skeyword ";";
            `Sself],
             ("Fgram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  _  (se : 'stream_exp_comp) \n     (_loc : FLoc.t)  -> (se :: sel : 'stream_exp_comp_list ))\n",
               (Fgram.mk_action
                  (fun (sel : 'stream_exp_comp_list)  _ 
                     (se : 'stream_exp_comp)  (_loc : FLoc.t)  -> (se ::
                     sel : 'stream_exp_comp_list )))));
          ([`Snterm (Fgram.obj (stream_exp_comp : 'stream_exp_comp Fgram.t ));
           `Skeyword ";"],
            ("Fgram.mk_action\n  (fun _  (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->\n     ([se] : 'stream_exp_comp_list ))\n",
              (Fgram.mk_action
                 (fun _  (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->
                    ([se] : 'stream_exp_comp_list )))));
          ([`Snterm (Fgram.obj (stream_exp_comp : 'stream_exp_comp Fgram.t ))],
            ("Fgram.mk_action\n  (fun (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->\n     ([se] : 'stream_exp_comp_list ))\n",
              (Fgram.mk_action
                 (fun (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->
                    ([se] : 'stream_exp_comp_list )))))]))
  end

let _ = AstParsers.register_parser ("stream", apply)