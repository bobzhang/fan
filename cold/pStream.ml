open FAst

open Syntax

open LibUtil

open FanStreamTools

let parser_ipat = Gram.mk "parser_ipat"

let stream_exp_comp = Gram.mk "stream_exp_comp"

let stream_exp_comp_list = Gram.mk "stream_exp_comp_list"

let stream_pat_comp = Gram.mk "stream_pat_comp"

let stream_pat_comp_err = Gram.mk "stream_pat_comp_err"

let stream_pat_comp_err_list = Gram.mk "stream_pat_comp_err_list"

let stream_begin = Gram.mk "stream_begin"

let stream_pat = Gram.mk "stream_pat"

let parser_case = Gram.mk "parser_case"

let parser_case_list = Gram.mk "parser_case_list"

let stream_exp = Gram.mk "stream_exp"

let apply () =
  begin
    Gram.extend_single (exp : 'exp Gram.t )
      ((Some (`Level "top")),
        (None, None,
          [([`Skeyword "parser";
            `Sopt
              (Gram.srules
                 [([`Stoken
                      (((function | `Uid _ -> true | _ -> false)),
                        (`Normal, "`Uid _"))],
                    ("Gram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Uid n -> (n : 'e__1 ) | _ -> failwith \"n\n\")\n",
                      (Gram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid n -> (n : 'e__1 )
                            | _ -> failwith "n\n"))))]);
            `Sopt (`Snterm (Gram.obj (parser_ipat : 'parser_ipat Gram.t )));
            `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
             ("Gram.mk_action\n  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) \n     (name : 'e__1 option)  _  (_loc : FLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FanStreamTools.grammar_module_name o\n            (fun _  -> cparser _loc po pcl)\n      | None  -> cparser _loc po pcl : 'exp ))\n",
               (Gram.mk_action
                  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                     (name : 'e__1 option)  _  (_loc : FLoc.t)  ->
                     (match name with
                      | Some o ->
                          Ref.protect FanStreamTools.grammar_module_name o
                            (fun _  -> cparser _loc po pcl)
                      | None  -> cparser _loc po pcl : 'exp )))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Skeyword "parser";
           `Sopt
             (Gram.srules
                [([`Stoken
                     (((function | `Uid _ -> true | _ -> false)),
                       (`Normal, "`Uid _"))],
                   ("Gram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Uid n -> (n : 'e__2 ) | _ -> failwith \"n\n\")\n",
                     (Gram.mk_action
                        (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                           match __fan_0 with
                           | `Uid n -> (n : 'e__2 )
                           | _ -> failwith "n\n"))))]);
           `Sopt (`Snterm (Gram.obj (parser_ipat : 'parser_ipat Gram.t )));
           `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
            ("Gram.mk_action\n  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) \n     (name : 'e__2 option)  _  _  (e : 'exp)  _  (_loc : FLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FanStreamTools.grammar_module_name o\n            (fun _  -> cparser_match _loc e po pcl)\n      | None  -> cparser_match _loc e po pcl : 'exp ))\n",
              (Gram.mk_action
                 (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                    (name : 'e__2 option)  _  _  (e : 'exp)  _ 
                    (_loc : FLoc.t)  ->
                    (match name with
                     | Some o ->
                         Ref.protect FanStreamTools.grammar_module_name o
                           (fun _  -> cparser_match _loc e po pcl)
                     | None  -> cparser_match _loc e po pcl : 'exp )))))]));
    Gram.extend_single (stream_exp : 'stream_exp Gram.t )
      (None,
        (None, None,
          [([`Skeyword "!";
            `Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
             ("Gram.mk_action\n  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Uid n ->\n         (Ref.protect FanStreamTools.grammar_module_name n\n            (fun _  -> FanStreamTools.empty _loc) : 'stream_exp )\n     | _ ->\n         failwith\n           \"Ref.protect FanStreamTools.grammar_module_name n\n  (fun _  -> FanStreamTools.empty _loc)\n\")\n",
               (Gram.mk_action
                  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                     match __fan_1 with
                     | `Uid n ->
                         (Ref.protect FanStreamTools.grammar_module_name n
                            (fun _  -> FanStreamTools.empty _loc) : 'stream_exp )
                     | _ ->
                         failwith
                           "Ref.protect FanStreamTools.grammar_module_name n\n  (fun _  -> FanStreamTools.empty _loc)\n"))));
          ([`Skeyword "!";
           `Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
           `Snterm
             (Gram.obj (stream_exp_comp_list : 'stream_exp_comp_list Gram.t ))],
            ("Gram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Uid n ->\n         (Ref.protect FanStreamTools.grammar_module_name n\n            (fun _  -> cstream _loc sel) : 'stream_exp )\n     | _ ->\n         failwith\n           \"Ref.protect FanStreamTools.grammar_module_name n (fun _  -> cstream _loc sel)\n\")\n",
              (Gram.mk_action
                 (fun (sel : 'stream_exp_comp_list)  (__fan_1 : [> FToken.t])
                     _  (_loc : FLoc.t)  ->
                    match __fan_1 with
                    | `Uid n ->
                        (Ref.protect FanStreamTools.grammar_module_name n
                           (fun _  -> cstream _loc sel) : 'stream_exp )
                    | _ ->
                        failwith
                          "Ref.protect FanStreamTools.grammar_module_name n (fun _  -> cstream _loc sel)\n"))));
          ([`Snterm
              (Gram.obj
                 (stream_exp_comp_list : 'stream_exp_comp_list Gram.t ))],
            ("Gram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  (_loc : FLoc.t)  ->\n     (cstream _loc sel : 'stream_exp ))\n",
              (Gram.mk_action
                 (fun (sel : 'stream_exp_comp_list)  (_loc : FLoc.t)  ->
                    (cstream _loc sel : 'stream_exp )))));
          ([],
            ("Gram.mk_action\n  (fun (_loc : FLoc.t)  -> (FanStreamTools.empty _loc : 'stream_exp ))\n",
              (Gram.mk_action
                 (fun (_loc : FLoc.t)  ->
                    (FanStreamTools.empty _loc : 'stream_exp )))))]));
    Gram.extend_single (parser_ipat : 'parser_ipat Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
             ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FLoc.t)  ->\n     ((i : alident  :>pat) : 'parser_ipat ))\n",
               (Gram.mk_action
                  (fun (i : 'a_lident)  (_loc : FLoc.t)  ->
                     ((i : alident  :>pat) : 'parser_ipat )))));
          ([`Skeyword "_"],
            ("Gram.mk_action\n  (fun _  (_loc : FLoc.t)  -> ((`Any _loc : FAst.pat ) : 'parser_ipat ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FLoc.t)  ->
                    ((`Any _loc : FAst.pat ) : 'parser_ipat )))))]));
    Gram.extend_single (parser_case_list : 'parser_case_list Gram.t )
      (None,
        (None, None,
          [([`Skeyword "|";
            `Slist0sep
              ((`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))),
                (`Skeyword "|"))],
             ("Gram.mk_action\n  (fun (pcl : 'parser_case list)  _  (_loc : FLoc.t)  ->\n     (pcl : 'parser_case_list ))\n",
               (Gram.mk_action
                  (fun (pcl : 'parser_case list)  _  (_loc : FLoc.t)  ->
                     (pcl : 'parser_case_list )))))]));
    Gram.extend_single (parser_case : 'parser_case Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (stream_pat : 'stream_pat Gram.t ));
            `Skeyword "->";
            `Snterm (Gram.obj (exp : 'exp Gram.t ))],
             ("Gram.mk_action\n  (fun (e : 'exp)  _  (sp : 'stream_pat)  (_loc : FLoc.t)  ->\n     ((sp, None, e) : 'parser_case ))\n",
               (Gram.mk_action
                  (fun (e : 'exp)  _  (sp : 'stream_pat)  (_loc : FLoc.t)  ->
                     ((sp, None, e) : 'parser_case )))))]));
    Gram.extend_single (stream_begin : 'stream_begin Gram.t )
      (None,
        (None, None,
          [([`Skeyword "[<";
            `Sopt
              (Gram.srules
                 [([`Skeyword "!";
                   `Stoken
                     (((function | `Uid _ -> true | _ -> false)),
                       (`Normal, "`Uid _"))],
                    ("Gram.mk_action\n  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->\n     match __fan_1 with | `Uid n -> (n : 'e__3 ) | _ -> failwith \"n\n\")\n",
                      (Gram.mk_action
                         (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t) 
                            ->
                            match __fan_1 with
                            | `Uid n -> (n : 'e__3 )
                            | _ -> failwith "n\n"))))])],
             ("Gram.mk_action\n  (fun (name : 'e__3 option)  _  (_loc : FLoc.t)  -> (name : 'stream_begin ))\n",
               (Gram.mk_action
                  (fun (name : 'e__3 option)  _  (_loc : FLoc.t)  ->
                     (name : 'stream_begin )))))]));
    Gram.extend_single (stream_pat : 'stream_pat Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ))],
             ("Gram.mk_action\n  (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->\n     ([(spc, None)] : 'stream_pat ))\n",
               (Gram.mk_action
                  (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->
                     ([(spc, None)] : 'stream_pat )))));
          ([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ));
           `Skeyword ";";
           `Snterm
             (Gram.obj
                (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gram.t ))],
            ("Gram.mk_action\n  (fun (sp : 'stream_pat_comp_err_list)  _  (spc : 'stream_pat_comp) \n     (_loc : FLoc.t)  -> ((spc, None) :: sp : 'stream_pat ))\n",
              (Gram.mk_action
                 (fun (sp : 'stream_pat_comp_err_list)  _ 
                    (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->
                    ((spc, None) :: sp : 'stream_pat )))));
          ([],
            ("Gram.mk_action (fun (_loc : FLoc.t)  -> ([] : 'stream_pat ))\n",
              (Gram.mk_action (fun (_loc : FLoc.t)  -> ([] : 'stream_pat )))))]));
    Gram.extend_single (stream_pat_comp : 'stream_pat_comp Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
            `Skeyword "when";
            `Snterm (Gram.obj (exp : 'exp Gram.t ))],
             ("Gram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->\n     (SpWhen (_loc, p, (Some e)) : 'stream_pat_comp ))\n",
               (Gram.mk_action
                  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->
                     (SpWhen (_loc, p, (Some e)) : 'stream_pat_comp )))));
          ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
            ("Gram.mk_action\n  (fun (p : 'pat)  (_loc : FLoc.t)  ->\n     (SpWhen (_loc, p, None) : 'stream_pat_comp ))\n",
              (Gram.mk_action
                 (fun (p : 'pat)  (_loc : FLoc.t)  ->
                    (SpWhen (_loc, p, None) : 'stream_pat_comp )))));
          ([`Snterm (Gram.obj (pat : 'pat Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->\n     (SpMatch (_loc, p, e) : 'stream_pat_comp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (p : 'pat)  (_loc : FLoc.t)  ->
                    (SpMatch (_loc, p, e) : 'stream_pat_comp )))));
          ([`Skeyword "'"; `Snterm (Gram.obj (pat : 'pat Gram.t ))],
            ("Gram.mk_action\n  (fun (p : 'pat)  _  (_loc : FLoc.t)  ->\n     (SpStr (_loc, p) : 'stream_pat_comp ))\n",
              (Gram.mk_action
                 (fun (p : 'pat)  _  (_loc : FLoc.t)  ->
                    (SpStr (_loc, p) : 'stream_pat_comp )))))]));
    Gram.extend_single (stream_pat_comp_err : 'stream_pat_comp_err Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ));
            `Skeyword "??";
            `Snterm (Gram.obj (exp : 'exp Gram.t ))],
             ("Gram.mk_action\n  (fun (e : 'exp)  _  (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->\n     ((spc, (Some e)) : 'stream_pat_comp_err ))\n",
               (Gram.mk_action
                  (fun (e : 'exp)  _  (spc : 'stream_pat_comp) 
                     (_loc : FLoc.t)  ->
                     ((spc, (Some e)) : 'stream_pat_comp_err )))));
          ([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ))],
            ("Gram.mk_action\n  (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->\n     ((spc, None) : 'stream_pat_comp_err ))\n",
              (Gram.mk_action
                 (fun (spc : 'stream_pat_comp)  (_loc : FLoc.t)  ->
                    ((spc, None) : 'stream_pat_comp_err )))))]));
    Gram.extend_single
      (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gram.t )
      (None,
        (None, None,
          [([`Snterm
               (Gram.obj (stream_pat_comp_err : 'stream_pat_comp_err Gram.t ))],
             ("Gram.mk_action\n  (fun (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->\n     ([spc] : 'stream_pat_comp_err_list ))\n",
               (Gram.mk_action
                  (fun (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->
                     ([spc] : 'stream_pat_comp_err_list )))));
          ([`Snterm
              (Gram.obj (stream_pat_comp_err : 'stream_pat_comp_err Gram.t ));
           `Skeyword ";"],
            ("Gram.mk_action\n  (fun _  (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->\n     ([spc] : 'stream_pat_comp_err_list ))\n",
              (Gram.mk_action
                 (fun _  (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  ->
                    ([spc] : 'stream_pat_comp_err_list )))));
          ([`Snterm
              (Gram.obj (stream_pat_comp_err : 'stream_pat_comp_err Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (sp : 'stream_pat_comp_err_list)  _  (spc : 'stream_pat_comp_err) \n     (_loc : FLoc.t)  -> (spc :: sp : 'stream_pat_comp_err_list ))\n",
              (Gram.mk_action
                 (fun (sp : 'stream_pat_comp_err_list)  _ 
                    (spc : 'stream_pat_comp_err)  (_loc : FLoc.t)  -> (spc ::
                    sp : 'stream_pat_comp_err_list )))))]));
    Gram.extend_single (stream_exp_comp : 'stream_exp_comp Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (exp : 'exp Gram.t ))],
             ("Gram.mk_action\n  (fun (e : 'exp)  (_loc : FLoc.t)  -> (SeTrm (_loc, e) : 'stream_exp_comp ))\n",
               (Gram.mk_action
                  (fun (e : 'exp)  (_loc : FLoc.t)  ->
                     (SeTrm (_loc, e) : 'stream_exp_comp )))));
          ([`Skeyword "'"; `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FLoc.t)  ->\n     (SeNtr (_loc, e) : 'stream_exp_comp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (_loc : FLoc.t)  ->
                    (SeNtr (_loc, e) : 'stream_exp_comp )))))]));
    Gram.extend_single (stream_exp_comp_list : 'stream_exp_comp_list Gram.t )
      (None,
        (None, None,
          [([`Snterm (Gram.obj (stream_exp_comp : 'stream_exp_comp Gram.t ));
            `Skeyword ";";
            `Sself],
             ("Gram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  _  (se : 'stream_exp_comp) \n     (_loc : FLoc.t)  -> (se :: sel : 'stream_exp_comp_list ))\n",
               (Gram.mk_action
                  (fun (sel : 'stream_exp_comp_list)  _ 
                     (se : 'stream_exp_comp)  (_loc : FLoc.t)  -> (se ::
                     sel : 'stream_exp_comp_list )))));
          ([`Snterm (Gram.obj (stream_exp_comp : 'stream_exp_comp Gram.t ));
           `Skeyword ";"],
            ("Gram.mk_action\n  (fun _  (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->\n     ([se] : 'stream_exp_comp_list ))\n",
              (Gram.mk_action
                 (fun _  (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->
                    ([se] : 'stream_exp_comp_list )))));
          ([`Snterm (Gram.obj (stream_exp_comp : 'stream_exp_comp Gram.t ))],
            ("Gram.mk_action\n  (fun (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->\n     ([se] : 'stream_exp_comp_list ))\n",
              (Gram.mk_action
                 (fun (se : 'stream_exp_comp)  (_loc : FLoc.t)  ->
                    ([se] : 'stream_exp_comp_list )))))]))
  end

let _ = AstParsers.register_parser ("stream", apply)