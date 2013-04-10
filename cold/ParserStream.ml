open Ast

open Syntax

open LibUtil

open FanStreamTools

let apply () =
  let grammar_entry_create = Gram.mk in
  let parser_ipat: 'parser_ipat Gram.t = grammar_entry_create "parser_ipat"
  and stream_exp_comp: 'stream_exp_comp Gram.t =
    grammar_entry_create "stream_exp_comp"
  and stream_exp_comp_list: 'stream_exp_comp_list Gram.t =
    grammar_entry_create "stream_exp_comp_list"
  and stream_pat_comp: 'stream_pat_comp Gram.t =
    grammar_entry_create "stream_pat_comp"
  and stream_pat_comp_err: 'stream_pat_comp_err Gram.t =
    grammar_entry_create "stream_pat_comp_err"
  and stream_pat_comp_err_list: 'stream_pat_comp_err_list Gram.t =
    grammar_entry_create "stream_pat_comp_err_list"
  and stream_begin: 'stream_begin Gram.t =
    grammar_entry_create "stream_begin"
  and stream_end: 'stream_end Gram.t = grammar_entry_create "stream_end"
  and stream_pat: 'stream_pat Gram.t = grammar_entry_create "stream_pat"
  and parser_case: 'parser_case Gram.t = grammar_entry_create "parser_case"
  and parser_case_list: 'parser_case_list Gram.t =
    grammar_entry_create "parser_case_list"
  and stream_exp: 'stream_exp Gram.t = grammar_entry_create "stream_exp"
  and stream_quot: 'stream_quot Gram.t = grammar_entry_create "stream_quot" in
  Gram.extend_single (exp : 'exp Gram.t )
    ((Some (`Level "top")),
      (None, None,
        [([`Skeyword "parser";
          `Sopt
            (Gram.srules
               [([`Stoken
                    (((function | `Uid _ -> true | _ -> false)),
                      (`Normal, "`Uid _"))],
                  ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid n -> (n : 'e__1 ) | _ -> failwith \"n\n\")\n",
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `Uid n -> (n : 'e__1 )
                          | _ -> failwith "n\n"))))]);
          `Sopt (`Snterm (Gram.obj (parser_ipat : 'parser_ipat Gram.t )));
          `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
           ("Gram.mk_action\n  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) \n     (name : 'e__1 option)  _  (_loc : FanLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FanStreamTools.grammar_module_name o\n            (fun _  -> cparser _loc po pcl)\n      | None  -> cparser _loc po pcl : 'exp ))\n",
             (Gram.mk_action
                (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                   (name : 'e__1 option)  _  (_loc : FanLoc.t)  ->
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
                 ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid n -> (n : 'e__2 ) | _ -> failwith \"n\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid n -> (n : 'e__2 )
                         | _ -> failwith "n\n"))))]);
         `Sopt (`Snterm (Gram.obj (parser_ipat : 'parser_ipat Gram.t )));
         `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
          ("Gram.mk_action\n  (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) \n     (name : 'e__2 option)  _  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FanStreamTools.grammar_module_name o\n            (fun _  -> cparser_match _loc e po pcl)\n      | None  -> cparser_match _loc e po pcl : 'exp ))\n",
            (Gram.mk_action
               (fun (pcl : 'parser_case_list)  (po : 'parser_ipat option) 
                  (name : 'e__2 option)  _  _  (e : 'exp)  _ 
                  (_loc : FanLoc.t)  ->
                  (match name with
                   | Some o ->
                       Ref.protect FanStreamTools.grammar_module_name o
                         (fun _  -> cparser_match _loc e po pcl)
                   | None  -> cparser_match _loc e po pcl : 'exp )))))]));
  Gram.extend_single (exp : 'exp Gram.t )
    ((Some (`Level "simple")),
      (None, None,
        [([`Snterm (Gram.obj (stream_begin : 'stream_begin Gram.t ));
          `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ))],
           ("Gram.mk_action\n  (fun _  (name : 'stream_begin)  (_loc : FanLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FanStreamTools.grammar_module_name o\n            (fun _  -> FanStreamTools.empty _loc)\n      | None  -> FanStreamTools.empty _loc : 'exp ))\n",
             (Gram.mk_action
                (fun _  (name : 'stream_begin)  (_loc : FanLoc.t)  ->
                   (match name with
                    | Some o ->
                        Ref.protect FanStreamTools.grammar_module_name o
                          (fun _  -> FanStreamTools.empty _loc)
                    | None  -> FanStreamTools.empty _loc : 'exp )))));
        ([`Snterm (Gram.obj (stream_begin : 'stream_begin Gram.t ));
         `Snterm
           (Gram.obj (stream_exp_comp_list : 'stream_exp_comp_list Gram.t ));
         `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ))],
          ("Gram.mk_action\n  (fun _  (sel : 'stream_exp_comp_list)  (name : 'stream_begin) \n     (_loc : FanLoc.t)  ->\n     (match name with\n      | Some o ->\n          Ref.protect FanStreamTools.grammar_module_name o\n            (fun _  -> cstream _loc sel)\n      | None  -> cstream _loc sel : 'exp ))\n",
            (Gram.mk_action
               (fun _  (sel : 'stream_exp_comp_list)  (name : 'stream_begin) 
                  (_loc : FanLoc.t)  ->
                  (match name with
                   | Some o ->
                       Ref.protect FanStreamTools.grammar_module_name o
                         (fun _  -> cstream _loc sel)
                   | None  -> cstream _loc sel : 'exp )))))]));
  Gram.extend_single (parser_ipat : 'parser_ipat Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i : alident  :>pat) : 'parser_ipat ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                   ((i : alident  :>pat) : 'parser_ipat )))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'parser_ipat ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'parser_ipat )))))]));
  Gram.extend_single (parser_case_list : 'parser_case_list Gram.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Slist0sep
            ((`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))),
              (`Skeyword "|"));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (pcl : 'parser_case list)  _  (_loc : FanLoc.t)  ->\n     (pcl : 'parser_case_list ))\n",
             (Gram.mk_action
                (fun _  (pcl : 'parser_case list)  _  (_loc : FanLoc.t)  ->
                   (pcl : 'parser_case_list )))));
        ([`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))],
          ("Gram.mk_action\n  (fun (pc : 'parser_case)  (_loc : FanLoc.t)  -> ([pc] : 'parser_case_list ))\n",
            (Gram.mk_action
               (fun (pc : 'parser_case)  (_loc : FanLoc.t)  ->
                  ([pc] : 'parser_case_list )))))]));
  Gram.extend_single (parser_case : 'parser_case Gram.t )
    (None,
      (None, None,
        [([`Skeyword "[<";
          `Snterm (Gram.obj (stream_pat : 'stream_pat Gram.t ));
          `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ));
          `Sopt (`Snterm (Gram.obj (parser_ipat : 'parser_ipat Gram.t )));
          `Skeyword "->";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (po : 'parser_ipat option)  _  (sp : 'stream_pat)  _ \n     (_loc : FanLoc.t)  -> ((sp, po, e) : 'parser_case ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (po : 'parser_ipat option)  _ 
                   (sp : 'stream_pat)  _  (_loc : FanLoc.t)  ->
                   ((sp, po, e) : 'parser_case )))))]));
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
                  ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with | `Uid n -> (n : 'e__3 ) | _ -> failwith \"n\n\")\n",
                    (Gram.mk_action
                       (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t) 
                          ->
                          match __fan_1 with
                          | `Uid n -> (n : 'e__3 )
                          | _ -> failwith "n\n"))))])],
           ("Gram.mk_action\n  (fun (name : 'e__3 option)  _  (_loc : FanLoc.t)  ->\n     (name : 'stream_begin ))\n",
             (Gram.mk_action
                (fun (name : 'e__3 option)  _  (_loc : FanLoc.t)  ->
                   (name : 'stream_begin )))))]));
  Gram.extend_single (stream_end : 'stream_end Gram.t )
    (None,
      (None, None,
        [([`Skeyword ">]"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'stream_end ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (() : 'stream_end )))))]));
  Gram.extend_single (stream_quot : 'stream_quot Gram.t )
    (None,
      (None, None,
        [([`Skeyword "'"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'stream_quot ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (() : 'stream_quot )))))]));
  Gram.extend_single (stream_exp : 'stream_exp Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action (fun (e : 'exp)  (_loc : FanLoc.t)  -> (e : 'stream_exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  (_loc : FanLoc.t)  -> (e : 'stream_exp )))))]));
  Gram.extend_single (stream_pat : 'stream_pat Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ))],
           ("Gram.mk_action\n  (fun (spc : 'stream_pat_comp)  (_loc : FanLoc.t)  ->\n     ([(spc, None)] : 'stream_pat ))\n",
             (Gram.mk_action
                (fun (spc : 'stream_pat_comp)  (_loc : FanLoc.t)  ->
                   ([(spc, None)] : 'stream_pat )))));
        ([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ));
         `Skeyword ";";
         `Snterm
           (Gram.obj
              (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gram.t ))],
          ("Gram.mk_action\n  (fun (sp : 'stream_pat_comp_err_list)  _  (spc : 'stream_pat_comp) \n     (_loc : FanLoc.t)  -> ((spc, None) :: sp : 'stream_pat ))\n",
            (Gram.mk_action
               (fun (sp : 'stream_pat_comp_err_list)  _ 
                  (spc : 'stream_pat_comp)  (_loc : FanLoc.t)  ->
                  ((spc, None) :: sp : 'stream_pat )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> ([] : 'stream_pat ))\n",
            (Gram.mk_action (fun (_loc : FanLoc.t)  -> ([] : 'stream_pat )))))]));
  Gram.extend_single (stream_pat_comp : 'stream_pat_comp Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Sopt
            (Gram.srules
               [([`Skeyword "when";
                 `Snterm (Gram.obj (stream_exp : 'stream_exp Gram.t ))],
                  ("Gram.mk_action (fun (e : 'stream_exp)  _  (_loc : FanLoc.t)  -> (e : 'e__4 ))\n",
                    (Gram.mk_action
                       (fun (e : 'stream_exp)  _  (_loc : FanLoc.t)  ->
                          (e : 'e__4 )))))])],
           ("Gram.mk_action\n  (fun (eo : 'e__4 option)  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (SpTrm (_loc, p, eo) : 'stream_pat_comp ))\n",
             (Gram.mk_action
                (fun (eo : 'e__4 option)  (p : 'pat)  (_loc : FanLoc.t)  ->
                   (SpTrm (_loc, p, eo) : 'stream_pat_comp )))));
        ([`Snterm (Gram.obj (pat : 'pat Gram.t ));
         `Skeyword "=";
         `Snterm (Gram.obj (stream_exp : 'stream_exp Gram.t ))],
          ("Gram.mk_action\n  (fun (e : 'stream_exp)  _  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (SpNtr (_loc, p, e) : 'stream_pat_comp ))\n",
            (Gram.mk_action
               (fun (e : 'stream_exp)  _  (p : 'pat)  (_loc : FanLoc.t)  ->
                  (SpNtr (_loc, p, e) : 'stream_pat_comp )))));
        ([`Snterm (Gram.obj (stream_quot : 'stream_quot Gram.t ));
         `Snterm (Gram.obj (pat : 'pat Gram.t ))],
          ("Gram.mk_action\n  (fun (p : 'pat)  _  (_loc : FanLoc.t)  ->\n     (SpStr (_loc, p) : 'stream_pat_comp ))\n",
            (Gram.mk_action
               (fun (p : 'pat)  _  (_loc : FanLoc.t)  ->
                  (SpStr (_loc, p) : 'stream_pat_comp )))))]));
  Gram.extend_single (stream_pat_comp_err : 'stream_pat_comp_err Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (stream_pat_comp : 'stream_pat_comp Gram.t ));
          `Sopt
            (Gram.srules
               [([`Skeyword "??";
                 `Snterm (Gram.obj (stream_exp : 'stream_exp Gram.t ))],
                  ("Gram.mk_action (fun (e : 'stream_exp)  _  (_loc : FanLoc.t)  -> (e : 'e__5 ))\n",
                    (Gram.mk_action
                       (fun (e : 'stream_exp)  _  (_loc : FanLoc.t)  ->
                          (e : 'e__5 )))))])],
           ("Gram.mk_action\n  (fun (eo : 'e__5 option)  (spc : 'stream_pat_comp)  (_loc : FanLoc.t)  ->\n     ((spc, eo) : 'stream_pat_comp_err ))\n",
             (Gram.mk_action
                (fun (eo : 'e__5 option)  (spc : 'stream_pat_comp) 
                   (_loc : FanLoc.t)  -> ((spc, eo) : 'stream_pat_comp_err )))))]));
  Gram.extend_single
    (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gram.t )
    (None,
      (None, None,
        [([`Snterm
             (Gram.obj (stream_pat_comp_err : 'stream_pat_comp_err Gram.t ))],
           ("Gram.mk_action\n  (fun (spc : 'stream_pat_comp_err)  (_loc : FanLoc.t)  ->\n     ([spc] : 'stream_pat_comp_err_list ))\n",
             (Gram.mk_action
                (fun (spc : 'stream_pat_comp_err)  (_loc : FanLoc.t)  ->
                   ([spc] : 'stream_pat_comp_err_list )))));
        ([`Snterm
            (Gram.obj (stream_pat_comp_err : 'stream_pat_comp_err Gram.t ));
         `Skeyword ";"],
          ("Gram.mk_action\n  (fun _  (spc : 'stream_pat_comp_err)  (_loc : FanLoc.t)  ->\n     ([spc] : 'stream_pat_comp_err_list ))\n",
            (Gram.mk_action
               (fun _  (spc : 'stream_pat_comp_err)  (_loc : FanLoc.t)  ->
                  ([spc] : 'stream_pat_comp_err_list )))));
        ([`Snterm
            (Gram.obj (stream_pat_comp_err : 'stream_pat_comp_err Gram.t ));
         `Skeyword ";";
         `Sself],
          ("Gram.mk_action\n  (fun (sp : 'stream_pat_comp_err_list)  _  (spc : 'stream_pat_comp_err) \n     (_loc : FanLoc.t)  -> (spc :: sp : 'stream_pat_comp_err_list ))\n",
            (Gram.mk_action
               (fun (sp : 'stream_pat_comp_err_list)  _ 
                  (spc : 'stream_pat_comp_err)  (_loc : FanLoc.t)  -> (spc ::
                  sp : 'stream_pat_comp_err_list )))))]));
  Gram.extend_single (stream_exp_comp_list : 'stream_exp_comp_list Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (stream_exp_comp : 'stream_exp_comp Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (sel : 'stream_exp_comp_list)  _  (se : 'stream_exp_comp) \n     (_loc : FanLoc.t)  -> (se :: sel : 'stream_exp_comp_list ))\n",
             (Gram.mk_action
                (fun (sel : 'stream_exp_comp_list)  _ 
                   (se : 'stream_exp_comp)  (_loc : FanLoc.t)  -> (se ::
                   sel : 'stream_exp_comp_list )))));
        ([`Snterm (Gram.obj (stream_exp_comp : 'stream_exp_comp Gram.t ));
         `Skeyword ";"],
          ("Gram.mk_action\n  (fun _  (se : 'stream_exp_comp)  (_loc : FanLoc.t)  ->\n     ([se] : 'stream_exp_comp_list ))\n",
            (Gram.mk_action
               (fun _  (se : 'stream_exp_comp)  (_loc : FanLoc.t)  ->
                  ([se] : 'stream_exp_comp_list )))));
        ([`Snterm (Gram.obj (stream_exp_comp : 'stream_exp_comp Gram.t ))],
          ("Gram.mk_action\n  (fun (se : 'stream_exp_comp)  (_loc : FanLoc.t)  ->\n     ([se] : 'stream_exp_comp_list ))\n",
            (Gram.mk_action
               (fun (se : 'stream_exp_comp)  (_loc : FanLoc.t)  ->
                  ([se] : 'stream_exp_comp_list )))))]));
  Gram.extend_single (stream_exp_comp : 'stream_exp_comp Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (stream_exp : 'stream_exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'stream_exp)  (_loc : FanLoc.t)  ->\n     (SeTrm (_loc, e) : 'stream_exp_comp ))\n",
             (Gram.mk_action
                (fun (e : 'stream_exp)  (_loc : FanLoc.t)  ->
                   (SeTrm (_loc, e) : 'stream_exp_comp )))));
        ([`Snterm (Gram.obj (stream_quot : 'stream_quot Gram.t ));
         `Snterm (Gram.obj (stream_exp : 'stream_exp Gram.t ))],
          ("Gram.mk_action\n  (fun (e : 'stream_exp)  _  (_loc : FanLoc.t)  ->\n     (SeNtr (_loc, e) : 'stream_exp_comp ))\n",
            (Gram.mk_action
               (fun (e : 'stream_exp)  _  (_loc : FanLoc.t)  ->
                  (SeNtr (_loc, e) : 'stream_exp_comp )))))]))

let _ = AstParsers.register_parser ("stream", apply)