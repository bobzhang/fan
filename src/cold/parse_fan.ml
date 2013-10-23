open FAst
open Ast_gen
open Fan_ops
open! Syntaxf
open FanUtil
open Gramlib
let pos_exps = Fgram.mk "pos_exps"
let apply () =
  (setup_op_parser prefixop
     (fun x  ->
        (not (List.mem x ["!="; "??"])) &&
          (((String.length x) >= 2) &&
             ((List.mem (x.[0]) ['!'; '?'; '~']) && (symbolchar x 1))));
   setup_op_parser infixop2
     (fun x  ->
        (List.mem x ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"]) ||
          ((not (List.mem x ["<-"; "||"; "&&"])) &&
             (((String.length x) >= 2) &&
                ((List.mem (x.[0]) ['='; '<'; '>'; '|'; '&'; '$'; '!']) &&
                   (symbolchar x 1)))));
   setup_op_parser infixop3
     (fun x  ->
        ((String.length x) >= 1) &&
          ((List.mem (x.[0]) ['@'; '^']) && (symbolchar x 1)));
   setup_op_parser infixop4
     (fun x  ->
        (x <> "->") &&
          (((String.length x) >= 1) &&
             ((List.mem (x.[0]) ['+'; '-']) && (symbolchar x 1))));
   setup_op_parser infixop5
     (fun x  ->
        ((String.length x) >= 1) &&
          ((List.mem (x.[0]) ['*'; '/'; '%'; '\\']) &&
             ((((x.[0]) <> '*') ||
                 (((String.length x) < 2) || ((x.[1]) <> '*')))
                && (symbolchar x 1))));
   setup_op_parser infixop6
     (fun x  ->
        ((String.length x) >= 2) &&
          (((x.[0]) == '*') && (((x.[1]) == '*') && (symbolchar x 2))));
   Fgram.setup_parser sem_exp
     (let symb1 = Fgram.parse_origin_tokens exp in
      let symb (__strm : _ Fstream.t) =
        match Fstream.peek __strm with
        | Some (`Ant (("list" as n),s),_loc) ->
            (Fstream.junk __strm; mk_anti ~c:"exp;" _loc n s)
        | _ -> symb1 __strm in
      let rec kont al (__strm : _ Fstream.t) =
        match Fstream.peek __strm with
        | Some (`Key (_,";"),_) ->
            (Fstream.junk __strm;
             (let a =
                try symb __strm
                with | Fstream.NotConsumed  -> raise (Fstream.Error "") in
              let s = __strm in
              let _loc = al <+> a in kont (`Sem (_loc, al, a) : FAst.exp ) s))
        | _ -> al in
      fun (__strm : _ Fstream.t)  -> let a = symb __strm in kont a __strm));
  (Fgram.extend_single (mexp_quot : 'mexp_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'mexp)  (_loc : Locf.t)  -> (x : 'mexp_quot )))))]));
   Fgram.extend (mbind0 : 'mbind0 Fgram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
            `Skeyword ":";
            `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
            `Skeyword ")";
            `Sself],
             ("`Functor (_loc, m, mt, mb)\n",
               (Fgram.mk_action
                  (fun (mb : 'mbind0)  _  (mt : 'mtyp)  _  (m : 'a_uident)  _
                      (_loc : Locf.t)  ->
                     (`Functor (_loc, m, mt, mb) : 'mbind0 )))));
          ([`Skeyword ":";
           `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
           `Skeyword "=";
           `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
            ("`Constraint (_loc, me, mt)\n",
              (Fgram.mk_action
                 (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                    (`Constraint (_loc, me, mt) : 'mbind0 )))));
          ([`Skeyword "="; `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
            ("me\n",
              (Fgram.mk_action
                 (fun (me : 'mexp)  _  (_loc : Locf.t)  -> (me : 'mbind0 )))))])]);
   Fgram.extend (mexp : 'mexp Fgram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
            `Skeyword ":";
            `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("`Functor (_loc, i, t, me)\n",
               (Fgram.mk_action
                  (fun (me : 'mexp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : Locf.t)  ->
                     (`Functor (_loc, i, t, me) : 'mexp )))));
          ([`Skeyword "struct";
           `Snterm (Fgram.obj (strus : 'strus Fgram.t ));
           `Skeyword "end"],
            ("`Struct (_loc, st)\n",
              (Fgram.mk_action
                 (fun _  (st : 'strus)  _  (_loc : Locf.t)  ->
                    (`Struct (_loc, st) : 'mexp )))));
          ([`Skeyword "struct"; `Skeyword "end"],
            ("`StructEnd _loc\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : Locf.t)  -> (`StructEnd _loc : 'mexp )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("`App (_loc, me1, me2)\n",
              (Fgram.mk_action
                 (fun (me2 : 'mexp)  (me1 : 'mexp)  (_loc : Locf.t)  ->
                    (`App (_loc, me1, me2) : 'mexp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti ~c:\"mexp\" _loc n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti ~c:"mexp" _loc n s : 'mexp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("mexp",_) -> true | _ -> false)),
               ("Ant", (`A "mexp")), "`Ant s")],
           ("mk_anti ~c:\"mexp\" _loc n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mexp" as n),s) ->
                       (mk_anti ~c:"mexp" _loc n s : 'mexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mexp\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mexp : 'mexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ))],
           ("(i :>mexp)\n",
             (Fgram.mk_action
                (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                   ((i :>mexp) : 'mexp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword ")"],
           ("`Constraint (_loc, me, mt)\n",
             (Fgram.mk_action
                (fun _  (mt : 'mtyp)  _  (me : 'mexp)  _  (_loc : Locf.t)  ->
                   (`Constraint (_loc, me, mt) : 'mexp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("me\n",
             (Fgram.mk_action
                (fun _  (me : 'mexp)  _  (_loc : Locf.t)  -> (me : 'mexp )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`PackageModule (_loc, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                   (`PackageModule (_loc, e) : 'mexp )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword ")"],
           ("`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p)))))\n",
             (Fgram.mk_action
                (fun _  (p : 'mtyp)  _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                   (`PackageModule
                      (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                   'mexp )))))])]));
  (Fgram.extend_single (mbind_quot : 'mbind_quot Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, b1, b2)\n",
              (Fgram.mk_action
                 (fun (b2 : 'mbind_quot)  _  (b1 : 'mbind_quot) 
                    (_loc : Locf.t)  -> (`And (_loc, b1, b2) : 'mbind_quot )))));
         ([`Stoken
             (((function | `Ant ("mbind",_) -> true | _ -> false)),
               ("Ant", (`A "mbind")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
           ("`Constraint (_loc, m, mt)\n",
             (Fgram.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t)  ->
                   (`Constraint (_loc, m, mt) : 'mbind_quot )))));
         ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
           ("`ModuleBind (_loc, m, mt, me)\n",
             (Fgram.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : Locf.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind_quot )))))]));
   Fgram.extend_single (mbind : 'mbind Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, b1, b2)\n",
              (Fgram.mk_action
                 (fun (b2 : 'mbind)  _  (b1 : 'mbind)  (_loc : Locf.t)  ->
                    (`And (_loc, b1, b2) : 'mbind )))));
         ([`Stoken
             (((function | `Ant ("mbind",_) -> true | _ -> false)),
               ("Ant", (`A "mbind")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mbind\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mbind : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
           ("`ModuleBind (_loc, m, mt, me)\n",
             (Fgram.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : Locf.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind )))))]));
   Fgram.extend_single
     (module_rec_declaration : 'module_rec_declaration Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, m1, m2)\n",
              (Fgram.mk_action
                 (fun (m2 : 'module_rec_declaration)  _ 
                    (m1 : 'module_rec_declaration)  (_loc : Locf.t)  ->
                    (`And (_loc, m1, m2) : 'module_rec_declaration )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("mbind",_) -> true | _ -> false)),
               ("Ant", (`A "mbind")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mbind\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mbind : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
           ("`Constraint (_loc, m, mt)\n",
             (Fgram.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t)  ->
                   (`Constraint (_loc, m, mt) : 'module_rec_declaration )))))])));
  (Fgram.extend_single (constr_quot : 'constr_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (constr : 'constr Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'constr)  (_loc : Locf.t)  -> (x : 'constr_quot )))))]));
   Fgram.extend_single (constr : 'constr Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, wc1, wc2)\n",
              (Fgram.mk_action
                 (fun (wc2 : 'constr)  _  (wc1 : 'constr)  (_loc : Locf.t) 
                    -> (`And (_loc, wc1, wc2) : 'constr )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"constr\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"constr" n s : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("constr",_) -> true | _ -> false)),
               ("Ant", (`A "constr")), "`Ant s")],
           ("mk_anti _loc ~c:\"constr\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("constr" as n),s) ->
                       (mk_anti _loc ~c:"constr" n s : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.constr\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.constr : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "type";
          `Snterm
            (Fgram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("`TypeEq (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : Locf.t)  -> (`TypeEq (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "type";
          `Snterm
            (Fgram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Fgram.t ));
          `Skeyword "=";
          `Skeyword "private";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("`TypeEqPriv (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  _ 
                   (t1 : 'type_longident_and_parameters)  _  (_loc : Locf.t) 
                   -> (`TypeEqPriv (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "type";
          `Snterm
            (Fgram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Fgram.t ));
          `Skeyword ":=";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("`TypeSubst (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : Locf.t)  ->
                   (`TypeSubst (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "module";
          `Snterm (Fgram.obj (module_longident : 'module_longident Fgram.t ));
          `Skeyword "=";
          `Snterm
            (Fgram.obj
               (module_longident_with_app : 'module_longident_with_app
                                              Fgram.t ))],
           ("`ModuleEq (_loc, (i1 : vid  :>ident), i2)\n",
             (Fgram.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`ModuleEq (_loc, (i1 : vid  :>ident), i2) : 'constr )))));
         ([`Skeyword "module";
          `Snterm (Fgram.obj (module_longident : 'module_longident Fgram.t ));
          `Skeyword ":=";
          `Snterm
            (Fgram.obj
               (module_longident_with_app : 'module_longident_with_app
                                              Fgram.t ))],
           ("`ModuleSubst (_loc, (i1 : vid  :>ident), i2)\n",
             (Fgram.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`ModuleSubst (_loc, (i1 : vid  :>ident), i2) : 'constr )))))])));
  (Fgram.extend_single (sigis : 'sigis Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc n ~c:\"sigi\" s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc n ~c:"sigi" s : 'sigis )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               ("Ant", (`A "sigi")), "`Ant s")],
           ("mk_anti _loc n ~c:\"sigi\" s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (mk_anti _loc n ~c:"sigi" s : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Fgram.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               ("Ant", (`A "sigi")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Fgram.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Fgram.mk_action
                (fun (sg : 'sigis)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               ("Ant", (`A "sigi")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Fgram.mk_action
                (fun (sg : 'sigis)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t ));
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, sg, s)\n",
             (Fgram.mk_action
                (fun (s : 'sigis)  _  (sg : 'sigi)  (_loc : Locf.t)  ->
                   (`Sem (_loc, sg, s) : 'sigis )))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t )); `Skeyword ";;"],
           ("sg\n",
             (Fgram.mk_action
                (fun _  (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis )))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t )); `Sself],
           ("`Sem (_loc, sg, s)\n",
             (Fgram.mk_action
                (fun (s : 'sigis)  (sg : 'sigi)  (_loc : Locf.t)  ->
                   (`Sem (_loc, sg, s) : 'sigis )))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t ))],
           ("sg\n",
             (Fgram.mk_action
                (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis )))))]));
   Fgram.extend (mtyp : 'mtyp Fgram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
            `Skeyword ":";
            `Sself;
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("`Functor (_loc, i, t, mt)\n",
               (Fgram.mk_action
                  (fun (mt : 'mtyp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : Locf.t)  ->
                     (`Functor (_loc, i, t, mt) : 'mtyp )))))]);
       ((Some "with"), None,
         [([`Sself;
           `Skeyword "with";
           `Snterm (Fgram.obj (constr : 'constr Fgram.t ))],
            ("`With (_loc, mt, wc)\n",
              (Fgram.mk_action
                 (fun (wc : 'constr)  _  (mt : 'mtyp)  (_loc : Locf.t)  ->
                    (`With (_loc, mt, wc) : 'mtyp )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("match (mt1, mt2) with\n| ((#ident as i1),(#ident as i2)) -> apply i1 i2\n| _ -> raise Fstream.NotConsumed\n",
              (Fgram.mk_action
                 (fun (mt2 : 'mtyp)  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                    (match (mt1, mt2) with
                     | ((#ident as i1),(#ident as i2)) -> apply i1 i2
                     | _ -> raise Fstream.NotConsumed : 'mtyp )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("let acc0 mt1 mt2 =\n  match (mt1, mt2) with\n  | ((#ident as i1),(#ident as i2)) -> dot i1 i2\n  | _ -> raise Fstream.NotConsumed in\nacc0 mt1 mt2\n",
              (Fgram.mk_action
                 (fun (mt2 : 'mtyp)  _  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                    (let acc0 mt1 mt2 =
                       match (mt1, mt2) with
                       | ((#ident as i1),(#ident as i2)) -> dot i1 i2
                       | _ -> raise Fstream.NotConsumed in
                     acc0 mt1 mt2 : 'mtyp )))))]);
       ((Some "sig"), None,
         [([`Skeyword "sig";
           `Snterm (Fgram.obj (sigis : 'sigis Fgram.t ));
           `Skeyword "end"],
            ("`Sig (_loc, sg)\n",
              (Fgram.mk_action
                 (fun _  (sg : 'sigis)  _  (_loc : Locf.t)  ->
                    (`Sig (_loc, sg) : 'mtyp )))));
         ([`Skeyword "sig"; `Skeyword "end"],
           ("`SigEnd _loc\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`SigEnd _loc : 'mtyp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"mtyp\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"mtyp" n s : 'mtyp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("mtyp",_) -> true | _ -> false)),
               ("Ant", (`A "mtyp")), "`Ant s")],
           ("mk_anti _loc ~c:\"mtyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mtyp" as n),s) ->
                       (mk_anti _loc ~c:"mtyp" n s : 'mtyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mtyp\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mtyp : 'mtyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm
             (Fgram.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Fgram.t ))],
           ("(i : ident  :>mtyp)\n",
             (Fgram.mk_action
                (fun (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                   ((i : ident  :>mtyp) : 'mtyp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("mt\n",
             (Fgram.mk_action
                (fun _  (mt : 'mtyp)  _  (_loc : Locf.t)  -> (mt : 'mtyp )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Skeyword "of";
          `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
           ("`ModuleTypeOf (_loc, me)\n",
             (Fgram.mk_action
                (fun (me : 'mexp)  _  _  _  (_loc : Locf.t)  ->
                   (`ModuleTypeOf (_loc, me) : 'mtyp )))))])]);
   Fgram.extend_single (module_declaration : 'module_declaration Fgram.t )
     (None,
       (None, None,
         [([`Skeyword ":"; `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
            ("mt\n",
              (Fgram.mk_action
                 (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                    (mt : 'module_declaration )))));
         ([`Skeyword "(";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword ")";
          `Sself],
           ("`Functor (_loc, i, t, mt)\n",
             (Fgram.mk_action
                (fun (mt : 'module_declaration)  _  (t : 'mtyp)  _ 
                   (i : 'a_uident)  _  (_loc : Locf.t)  ->
                   (`Functor (_loc, i, t, mt) : 'module_declaration )))))]));
   Fgram.extend_single (mtyp_quot : 'mtyp_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'mtyp)  (_loc : Locf.t)  -> (x : 'mtyp_quot )))))])));
  (Fgram.extend_single (sigi_quot : 'sigi_quot Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
            ("`DirectiveSimple (_loc, s)\n",
              (Fgram.mk_action
                 (fun (s : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`DirectiveSimple (_loc, s) : 'sigi_quot )))));
         ([`Skeyword "#";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`Directive (_loc, s, dp)\n",
             (Fgram.mk_action
                (fun (dp : 'exp)  (s : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Directive (_loc, s, dp) : 'sigi_quot )))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, sg1, sg2)\n",
             (Fgram.mk_action
                (fun (sg2 : 'sigi_quot)  _  (sg1 : 'sigi)  (_loc : Locf.t) 
                   -> (`Sem (_loc, sg1, sg2) : 'sigi_quot )))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t ))],
           ("sg\n",
             (Fgram.mk_action
                (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigi_quot )))))]));
   Fgram.extend_single (sigi : 'sigi Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"sigi\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"sigi" n s : 'sigi )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               ("Ant", (`A "sigi")), "`Ant s")],
           ("mk_anti _loc ~c:\"sigi\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (mk_anti _loc ~c:"sigi" n s : 'sigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.sigi\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.sigi : 'sigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "exception";
          `Snterm
            (Fgram.obj
               (constructor_declaration : 'constructor_declaration Fgram.t ))],
           ("(`Exception (_loc, t) : FAst.sigi )\n",
             (Fgram.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)  ->
                   ((`Exception (_loc, t) : FAst.sigi ) : 'sigi )))));
         ([`Skeyword "external";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (string_list : 'string_list Fgram.t ))],
           ("`External (_loc, i, t, sl)\n",
             (Fgram.mk_action
                (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident) 
                   _  (_loc : Locf.t)  ->
                   (`External (_loc, i, t, sl) : 'sigi )))));
         ([`Skeyword "include"; `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
           ("`Include (_loc, mt)\n",
             (Fgram.mk_action
                (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                   (`Include (_loc, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Snterm
            (Fgram.obj (module_declaration : 'module_declaration Fgram.t ))],
           ("`Module (_loc, i, mt)\n",
             (Fgram.mk_action
                (fun (mt : 'module_declaration)  (i : 'a_uident)  _ 
                   (_loc : Locf.t)  -> (`Module (_loc, i, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "rec";
          `Snterm
            (Fgram.obj
               (module_rec_declaration : 'module_rec_declaration Fgram.t ))],
           ("`RecModule (_loc, mb)\n",
             (Fgram.mk_action
                (fun (mb : 'module_rec_declaration)  _  _  (_loc : Locf.t) 
                   -> (`RecModule (_loc, mb) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
           ("`ModuleType (_loc, i, mt)\n",
             (Fgram.mk_action
                (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : Locf.t) 
                   -> (`ModuleType (_loc, i, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ))],
           ("`ModuleTypeEnd (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleTypeEnd (_loc, i) : 'sigi )))));
         ([`Skeyword "open";
          `Snterm (Fgram.obj (module_longident : 'module_longident Fgram.t ))],
           ("`Open (_loc, (`Negative _loc), (i : vid  :>ident))\n",
             (Fgram.mk_action
                (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`Open (_loc, (`Negative _loc), (i : vid  :>ident)) : 
                   'sigi )))));
         ([`Skeyword "open";
          `Skeyword "!";
          `Snterm (Fgram.obj (module_longident : 'module_longident Fgram.t ))],
           ("`Open (_loc, (`Positive _loc), (i : vid  :>ident))\n",
             (Fgram.mk_action
                (fun (i : 'module_longident)  _  _  (_loc : Locf.t)  ->
                   (`Open (_loc, (`Positive _loc), (i : vid  :>ident)) : 
                   'sigi )))));
         ([`Skeyword "type";
          `Snterm (Fgram.obj (type_declaration : 'type_declaration Fgram.t ))],
           ("`Type (_loc, t)\n",
             (Fgram.mk_action
                (fun (t : 'type_declaration)  _  (_loc : Locf.t)  ->
                   (`Type (_loc, t) : 'sigi )))));
         ([`Skeyword "val";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("`Val (_loc, i, t)\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Val (_loc, i, t) : 'sigi )))));
         ([`Skeyword "class";
          `Snterm
            (Fgram.obj (class_description : 'class_description Fgram.t ))],
           ("`Class (_loc, cd)\n",
             (Fgram.mk_action
                (fun (cd : 'class_description)  _  (_loc : Locf.t)  ->
                   (`Class (_loc, cd) : 'sigi )))));
         ([`Skeyword "class";
          `Skeyword "type";
          `Snterm
            (Fgram.obj (cltyp_declaration : 'cltyp_declaration Fgram.t ))],
           ("`ClassType (_loc, ctd)\n",
             (Fgram.mk_action
                (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t)  ->
                   (`ClassType (_loc, ctd) : 'sigi )))))]));
   Fgram.extend_single (interf : 'interf Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t ));
           `Skeyword ";;";
           `Sself],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Fgram.mk_action
                 (fun (rest : 'interf)  _  (si : 'sigi)  (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'interf )))));
         ([`Snterm (Fgram.obj (sigi : 'sigi Fgram.t )); `Sself],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Fgram.mk_action
                (fun (rest : 'interf)  (si : 'sigi)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'interf )))));
         ([`Stoken
             (((function | `EOI _ -> true | _ -> false)), ("EOI", `Empty),
               "`EOI")],
           ("([], None)\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  -> (([], None) : 'interf )))))])));
  (let grammar_entry_create x = Fgram.mk x in
   let name_space: 'name_space Fgram.t = grammar_entry_create "name_space"
   and fun_def_pat: 'fun_def_pat Fgram.t = grammar_entry_create "fun_def_pat" in
   Fgram.extend_single (exp_quot : 'exp_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (exp : 'exp Fgram.t ));
           `Skeyword ",";
           `Snterm (Fgram.obj (comma_exp : 'comma_exp Fgram.t ))],
            ("`Com (_loc, e1, e2)\n",
              (Fgram.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`Com (_loc, e1, e2) : 'exp_quot )))));
         ([`Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ";";
          `Snterm (Fgram.obj (sem_exp : 'sem_exp Fgram.t ))],
           ("`Sem (_loc, e1, e2)\n",
             (Fgram.mk_action
                (fun (e2 : 'sem_exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`Sem (_loc, e1, e2) : 'exp_quot )))));
         ([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("e\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'exp_quot )))))]));
   Fgram.extend_single (cvalue_bind : 'cvalue_bind Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("e\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'cvalue_bind )))));
         ([`Skeyword ":";
          `Skeyword "type";
          `Snterm
            (Fgram.obj (unquoted_typevars : 'unquoted_typevars Fgram.t ));
          `Skeyword ".";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("let u: FAst.ctyp = `TyPol (_loc, t1, t2) in\n(`Constraint (_loc, e, u) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _ 
                   (t1 : 'unquoted_typevars)  _  _  (_loc : Locf.t)  ->
                   (let u: FAst.ctyp = `TyPol (_loc, t1, t2) in
                    (`Constraint (_loc, e, u) : FAst.exp ) : 'cvalue_bind )))));
         ([`Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(`Constraint (_loc, e, t) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, e, t) : FAst.exp ) : 'cvalue_bind )))));
         ([`Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ":>";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("match t with\n| (`TyPol (_loc,_,_) : FAst.ctyp) ->\n    raise (Fstream.Error \"unexpected polytype here\")\n| _ -> (`Coercion (_loc, e, t, t2) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _ 
                   (_loc : Locf.t)  ->
                   (match t with
                    | (`TyPol (_loc,_,_) : FAst.ctyp) ->
                        raise (Fstream.Error "unexpected polytype here")
                    | _ -> (`Coercion (_loc, e, t, t2) : FAst.exp ) : 
                   'cvalue_bind )))));
         ([`Skeyword ":>";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`Subtype (_loc, e, t)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                   (`Subtype (_loc, e, t) : 'cvalue_bind )))))]));
   Fgram.extend (fun_bind : 'fun_bind Fgram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
            `Skeyword ")";
            `Sself],
             ("`LocalTypeFun (_loc, i, e)\n",
               (Fgram.mk_action
                  (fun (e : 'fun_bind)  _  (i : 'a_lident)  _  _ 
                     (_loc : Locf.t)  ->
                     (`LocalTypeFun (_loc, i, e) : 'fun_bind )))));
          ([`Snterm (Fgram.obj (ipat : 'ipat Fgram.t )); `Sself],
            ("`Fun (_loc, (`Case (_loc, p, e)))\n",
              (Fgram.mk_action
                 (fun (e : 'fun_bind)  (p : 'ipat)  (_loc : Locf.t)  ->
                    (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_bind )))));
          ([`Snterm (Fgram.obj (cvalue_bind : 'cvalue_bind Fgram.t ))],
            ("bi\n",
              (Fgram.mk_action
                 (fun (bi : 'cvalue_bind)  (_loc : Locf.t)  ->
                    (bi : 'fun_bind )))))])]);
   Fgram.extend_single (lang : 'lang Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (dot_lstrings : 'dot_lstrings Fgram.t ))],
            ("let old = Ast_quotation.default.contents in\nmatch Ast_quotation.resolve_name ls with\n| Some x -> (Ast_quotation.default := (Some x); old)\n| None  ->\n    Locf.failf _loc \"DDSL `%s' can not be resolved\"\n      (Ftoken.string_of_name ls)\n",
              (Fgram.mk_action
                 (fun (ls : 'dot_lstrings)  (_loc : Locf.t)  ->
                    (let old = Ast_quotation.default.contents in
                     match Ast_quotation.resolve_name ls with
                     | Some x -> (Ast_quotation.default := (Some x); old)
                     | None  ->
                         Locf.failf _loc "DDSL `%s' can not be resolved"
                           (Ftoken.string_of_name ls) : 'lang )))))]));
   Fgram.extend_single (pos_exps : 'pos_exps Fgram.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((`Snterm (Fgram.obj (name_space : 'name_space Fgram.t ))),
                (`Skeyword ";"))],
            ("let old = Ast_quotation.map.contents in\nAst_quotation.map := (Mapf.String.add_list xys old); old\n",
              (Fgram.mk_action
                 (fun (xys : 'name_space list)  (_loc : Locf.t)  ->
                    (let old = Ast_quotation.map.contents in
                     Ast_quotation.map := (Mapf.String.add_list xys old); old : 
                    'pos_exps )))))]));
   Fgram.extend_single (name_space : 'name_space Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
                "`Lid x");
           `Skeyword ":";
           `Snterm (Fgram.obj (dot_lstrings : 'dot_lstrings Fgram.t ))],
            ("(x,\n  (match Ast_quotation.resolve_name y with\n   | None  ->\n       Locf.failf _loc \"DDSL `%s' can not be resolved\"\n         (Ftoken.string_of_name y)\n   | Some x -> x))\n",
              (Fgram.mk_action
                 (fun (y : 'dot_lstrings)  _  (__fan_0 : Ftoken.t) 
                    (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid (_,x) ->
                        ((x,
                           ((match Ast_quotation.resolve_name y with
                             | None  ->
                                 Locf.failf _loc
                                   "DDSL `%s' can not be resolved"
                                   (Ftoken.string_of_name y)
                             | Some x -> x))) : 'name_space )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid x")],
           ("(x,\n  (match Ast_quotation.resolve_name ((`Sub []), x) with\n   | None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" x\n   | Some x -> x))\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,x) ->
                       ((x,
                          ((match Ast_quotation.resolve_name ((`Sub []), x)
                            with
                            | None  ->
                                Locf.failf _loc
                                  "DDSL `%s' can not be resolved" x
                            | Some x -> x))) : 'name_space )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (fun_def_pat : 'fun_def_pat Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "(";
           `Skeyword "type";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
           `Skeyword ")"],
            ("fun e  -> `LocalTypeFun (_loc, i, e)\n",
              (Fgram.mk_action
                 (fun _  (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                    (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat )))));
         ([`Snterm (Fgram.obj (ipat : 'ipat Fgram.t ))],
           ("fun e  -> `Fun (_loc, (`Case (_loc, p, e)))\n",
             (Fgram.mk_action
                (fun (p : 'ipat)  (_loc : Locf.t)  ->
                   (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 'fun_def_pat )))));
         ([`Snterm (Fgram.obj (ipat : 'ipat Fgram.t ));
          `Skeyword "when";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e)))\n",
             (Fgram.mk_action
                (fun (w : 'exp)  _  (p : 'ipat)  (_loc : Locf.t)  ->
                   (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 
                   'fun_def_pat )))))]));
   Fgram.extend (fun_def : 'fun_def Fgram.t )
     (None,
       [(None, (Some `RA),
          [([`Snterm (Fgram.obj (fun_def_pat : 'fun_def_pat Fgram.t ));
            `Skeyword "->";
            `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
             ("f e\n",
               (Fgram.mk_action
                  (fun (e : 'exp)  _  (f : 'fun_def_pat)  (_loc : Locf.t)  ->
                     (f e : 'fun_def )))));
          ([`Snterm (Fgram.obj (fun_def_pat : 'fun_def_pat Fgram.t ));
           `Sself],
            ("f e\n",
              (Fgram.mk_action
                 (fun (e : 'fun_def)  (f : 'fun_def_pat)  (_loc : Locf.t)  ->
                    (f e : 'fun_def )))))])]);
   Fgram.extend (exp : 'exp Fgram.t )
     (None,
       [((Some "top"), (Some `RA),
          [([`Skeyword "let";
            `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
            `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
            `Skeyword "in";
            `Sself],
             ("`LetIn (_loc, r, bi, x)\n",
               (Fgram.mk_action
                  (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                     (_loc : Locf.t)  -> (`LetIn (_loc, r, bi, x) : 'exp )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
           `Snterm (Fgram.obj (mbind0 : 'mbind0 Fgram.t ));
           `Skeyword "in";
           `Sself],
            ("`LetModule (_loc, m, mb, e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : Locf.t)  -> (`LetModule (_loc, m, mb, e) : 
                    'exp )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ));
           `Skeyword "in";
           `Sself],
            ("`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e) : 
                    'exp )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Skeyword "!";
           `Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ));
           `Skeyword "in";
           `Sself],
            ("`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _  _ 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e) : 
                    'exp )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
           `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
           `Skeyword "in";
           `Sself;
           `Skeyword "with";
           `Snterm (Fgram.obj (case : 'case Fgram.t ))],
            ("`LetTryInWith (_loc, r, bi, x, a)\n",
              (Fgram.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                    (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                    (`LetTryInWith (_loc, r, bi, x, a) : 'exp )))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Snterm (Fgram.obj (case : 'case Fgram.t ))],
            ("`Match (_loc, e, a)\n",
              (Fgram.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Match (_loc, e, a) : 'exp )))));
          ([`Skeyword "try";
           `Sself;
           `Skeyword "with";
           `Snterm (Fgram.obj (case : 'case Fgram.t ))],
            ("`Try (_loc, e, a)\n",
              (Fgram.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Try (_loc, e, a) : 'exp )))));
          ([`Skeyword "if";
           `Sself;
           `Skeyword "then";
           `Sself;
           `Skeyword "else";
           `Sself],
            ("`IfThenElse (_loc, e1, e2, e3)\n",
              (Fgram.mk_action
                 (fun (e3 : 'exp)  _  (e2 : 'exp)  _  (e1 : 'exp)  _ 
                    (_loc : Locf.t)  ->
                    (`IfThenElse (_loc, e1, e2, e3) : 'exp )))));
          ([`Skeyword "if"; `Sself; `Skeyword "then"; `Sself],
            ("`IfThen (_loc, e1, e2)\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : Locf.t)  ->
                    (`IfThen (_loc, e1, e2) : 'exp )))));
          ([`Skeyword "do";
           `Snterm (Fgram.obj (sequence : 'sequence Fgram.t ));
           `Skeyword "done"],
            ("`Seq (_loc, seq)\n",
              (Fgram.mk_action
                 (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                    (`Seq (_loc, seq) : 'exp )))));
          ([`Skeyword "with";
           `Snterm (Fgram.obj (lang : 'lang Fgram.t ));
           `Sself],
            ("Ast_quotation.default := old; x\n",
              (Fgram.mk_action
                 (fun (x : 'exp)  (old : 'lang)  _  (_loc : Locf.t)  ->
                    (Ast_quotation.default := old; x : 'exp )))));
          ([`Skeyword "with";
           `Skeyword "{";
           `Snterm (Fgram.obj (pos_exps : 'pos_exps Fgram.t ));
           `Skeyword "}";
           `Sself],
            ("Ast_quotation.map := old; x\n",
              (Fgram.mk_action
                 (fun (x : 'exp)  _  (old : 'pos_exps)  _  _  (_loc : Locf.t)
                     -> (Ast_quotation.map := old; x : 'exp )))));
          ([`Skeyword "for";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
           `Skeyword "=";
           `Sself;
           `Snterm (Fgram.obj (flag : 'flag Fgram.t ));
           `Sself;
           `Skeyword "do";
           `Snterm (Fgram.obj (sequence : 'sequence Fgram.t ));
           `Skeyword "done"],
            ("`For (_loc, i, e1, e2, df, seq)\n",
              (Fgram.mk_action
                 (fun _  (seq : 'sequence)  _  (e2 : 'exp)  (df : 'flag) 
                    (e1 : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`For (_loc, i, e1, e2, df, seq) : 'exp )))));
          ([`Skeyword "while";
           `Sself;
           `Skeyword "do";
           `Snterm (Fgram.obj (sequence : 'sequence Fgram.t ));
           `Skeyword "done"],
            ("`While (_loc, e, seq)\n",
              (Fgram.mk_action
                 (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : Locf.t)
                     -> (`While (_loc, e, seq) : 'exp )))))]);
       ((Some ":="), (Some `NA),
         [([`Sself; `Skeyword ":="; `Sself],
            ("(`Assign (_loc, (`Field (_loc, e1, (`Lid (_loc, \"contents\")))), e2) : \nFAst.exp )\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    ((`Assign
                        (_loc,
                          (`Field (_loc, e1, (`Lid (_loc, "contents")))), e2) : 
                    FAst.exp ) : 'exp )))));
         ([`Sself; `Skeyword "<-"; `Sself],
           ("match Fan_ops.bigarray_set _loc e1 e2 with\n| Some e -> e\n| None  -> `Assign (_loc, e1, e2)\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (match Fan_ops.bigarray_set _loc e1 e2 with
                    | Some e -> e
                    | None  -> `Assign (_loc, e1, e2) : 'exp )))))]);
       ((Some "||"), (Some `RA),
         [([`Sself; `Skeyword "or"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key (_,op) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "||"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]);
       ((Some "&&"), (Some `RA),
         [([`Sself; `Skeyword "&"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key (_,op) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "&&"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]);
       ((Some "<"), (Some `LA),
         [([`Sself;
           `Snterm (Fgram.obj (infixop2 : 'infixop2 Fgram.t ));
           `Sself],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop2)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "^"), (Some `RA),
         [([`Sself;
           `Snterm (Fgram.obj (infixop3 : 'infixop3 Fgram.t ));
           `Sself],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop3)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "::"), (Some `RA),
         [([`Sself; `Skeyword "::"; `Sself],
            ("(`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e1)), e2) : FAst.exp )\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, (`Uid (_loc, "::")), e1)), e2) : 
                    FAst.exp ) : 'exp )))))]);
       ((Some "+"), (Some `LA),
         [([`Sself;
           `Snterm (Fgram.obj (infixop4 : 'infixop4 Fgram.t ));
           `Sself],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop4)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "*"), (Some `LA),
         [([`Sself; `Skeyword "land"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key (_,op) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lor"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lxor"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "mod"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Sself;
          `Snterm (Fgram.obj (infixop5 : 'infixop5 Fgram.t ));
          `Sself],
           ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (op : 'infixop5)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                   'exp )))))]);
       ((Some "**"), (Some `RA),
         [([`Sself; `Skeyword "asr"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key (_,op) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lsl"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lsr"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Ftoken.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key (_,op) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Sself;
          `Snterm (Fgram.obj (infixop6 : 'infixop6 Fgram.t ));
          `Sself],
           ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  (op : 'infixop6)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                   'exp )))))]);
       ((Some "obj"), (Some `RA),
         [([`Skeyword "fun";
           `Skeyword "|";
           `Slist1sep
             ((`Snterm (Fgram.obj (case0 : 'case0 Fgram.t ))),
               (`Skeyword "|"))],
            ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
              (Fgram.mk_action
                 (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                    (let cases = bar_of_list a in `Fun (_loc, cases) : 
                    'exp )))));
         ([`Skeyword "function";
          `Skeyword "|";
          `Slist1sep
            ((`Snterm (Fgram.obj (case0 : 'case0 Fgram.t ))),
              (`Skeyword "|"))],
           ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
             (Fgram.mk_action
                (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                   (let cases = bar_of_list a in `Fun (_loc, cases) : 
                   'exp )))));
         ([`Skeyword "fun";
          `Snterm (Fgram.obj (fun_def : 'fun_def Fgram.t ))],
           ("e\n",
             (Fgram.mk_action
                (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Skeyword "function";
          `Snterm (Fgram.obj (fun_def : 'fun_def Fgram.t ))],
           ("e\n",
             (Fgram.mk_action
                (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ")";
          `Snterm (Fgram.obj (class_structure : 'class_structure Fgram.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, p, cst)\n",
             (Fgram.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, p)\n",
             (Fgram.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPatEnd (_loc, p) : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")";
          `Snterm (Fgram.obj (class_structure : 'class_structure Fgram.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
             (Fgram.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
             (Fgram.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : Locf.t) 
                   -> (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                   'exp )))));
         ([`Skeyword "object";
          `Snterm (Fgram.obj (class_structure : 'class_structure Fgram.t ));
          `Skeyword "end"],
           ("`Obj (_loc, cst)\n",
             (Fgram.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : Locf.t)  ->
                   (`Obj (_loc, cst) : 'exp )))));
         ([`Skeyword "object"; `Skeyword "end"],
           ("`ObjEnd _loc\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'exp )))))]);
       ((Some "unary minus"), (Some `NA),
         [([`Skeyword "-"; `Sself],
            ("Fan_ops.mkumin _loc \"-\" e\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                    (Fan_ops.mkumin _loc "-" e : 'exp )))));
         ([`Skeyword "-."; `Sself],
           ("Fan_ops.mkumin _loc \"-.\" e\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (Fan_ops.mkumin _loc "-." e : 'exp )))))]);
       ((Some "apply"), (Some `LA),
         [([`Sself; `Sself],
            ("`App (_loc, e1, e2)\n",
              (Fgram.mk_action
                 (fun (e2 : 'exp)  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`App (_loc, e1, e2) : 'exp )))));
         ([`Skeyword "assert"; `Sself],
           ("`Assert (_loc, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Assert (_loc, e) : 'exp )))));
         ([`Skeyword "new";
          `Snterm (Fgram.obj (class_longident : 'class_longident Fgram.t ))],
           ("`New (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                   (`New (_loc, i) : 'exp )))));
         ([`Skeyword "lazy"; `Sself],
           ("`Lazy (_loc, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Lazy (_loc, e) : 'exp )))))]);
       ((Some "label"), (Some `NA),
         [([`Skeyword "~";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
           `Skeyword ":";
           `Sself],
            ("`Label (_loc, i, e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`Label (_loc, i, e) : 'exp )))));
         ([`Skeyword "~";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`LabelS (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'exp )))));
         ([`Stoken
             (((function | `Label (_,_) -> true | _ -> false)),
               ("Label", `Any), "`Label i");
          `Sself],
           ("(`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label (_,i) ->
                       ((`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp ) : 
                       'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Optlabel (_,_) -> true | _ -> false)),
               ("Optlabel", `Any), "`Optlabel i");
          `Sself],
           ("`OptLabl (_loc, (`Lid (_loc, i)), e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel (_,i) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Sself],
           ("`OptLabl (_loc, i, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, i, e) : 'exp )))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`OptLablS (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'exp )))))]);
       ((Some "."), (Some `LA),
         [([`Sself; `Skeyword "."; `Skeyword "("; `Sself; `Skeyword ")"],
            ("`ArrayDot (_loc, e1, e2)\n",
              (Fgram.mk_action
                 (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`ArrayDot (_loc, e1, e2) : 'exp )))));
         ([`Sself; `Skeyword "."; `Skeyword "["; `Sself; `Skeyword "]"],
           ("`StringDot (_loc, e1, e2)\n",
             (Fgram.mk_action
                (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`StringDot (_loc, e1, e2) : 'exp )))));
         ([`Sself;
          `Skeyword ".";
          `Skeyword "{";
          `Snterm (Fgram.obj (comma_exp : 'comma_exp Fgram.t ));
          `Skeyword "}"],
           ("Fan_ops.bigarray_get _loc e1 e2\n",
             (Fgram.mk_action
                (fun _  (e2 : 'comma_exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)
                    -> (Fan_ops.bigarray_get _loc e1 e2 : 'exp )))));
         ([`Sself; `Skeyword "."; `Sself],
           ("`Field (_loc, e1, e2)\n",
             (Fgram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`Field (_loc, e1, e2) : 'exp )))));
         ([`Sself;
          `Skeyword "#";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`Send (_loc, e, lab)\n",
             (Fgram.mk_action
                (fun (lab : 'a_lident)  _  (e : 'exp)  (_loc : Locf.t)  ->
                   (`Send (_loc, e, lab) : 'exp )))))]);
       ((Some "~-"), (Some `NA),
         [([`Skeyword "!"; `Sself],
            ("`Field (_loc, e, (`Lid (_loc, \"contents\")))\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Field (_loc, e, (`Lid (_loc, "contents"))) : 'exp )))));
         ([`Snterm (Fgram.obj (prefixop : 'prefixop Fgram.t )); `Sself],
           ("`App (_loc, f, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (f : 'prefixop)  (_loc : Locf.t)  ->
                   (`App (_loc, f, e) : 'exp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
                "`Quot _")],
            ("Ast_quotation.expand x Dyn_tag.exp\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Quot x -> (Ast_quotation.expand x Dyn_tag.exp : 'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("exp",_) -> true | _ -> false)),
               ("Ant", (`A "exp")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("exp" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`bool",_) -> true | _ -> false)),
               ("Ant", (`A "`bool")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`bool" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("par",_) -> true | _ -> false)),
               ("Ant", (`A "par")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("par" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("seq",_) -> true | _ -> false)),
               ("Ant", (`A "seq")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("seq" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int",_) -> true | _ -> false)),
               ("Ant", (`A "int")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int",_) -> true | _ -> false)),
               ("Ant", (`A "`int")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int32",_) -> true | _ -> false)),
               ("Ant", (`A "int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int32" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int32",_) -> true | _ -> false)),
               ("Ant", (`A "`int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int32" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int64",_) -> true | _ -> false)),
               ("Ant", (`A "int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int64" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int64",_) -> true | _ -> false)),
               ("Ant", (`A "`int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int64" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("nativeint",_) -> true | _ -> false)),
               ("Ant", (`A "nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("nativeint" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`nativeint",_) -> true | _ -> false)),
               ("Ant", (`A "`nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`nativeint" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("flo",_) -> true | _ -> false)),
               ("Ant", (`A "flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("flo" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`flo",_) -> true | _ -> false)),
               ("Ant", (`A "`flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`flo" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("chr",_) -> true | _ -> false)),
               ("Ant", (`A "chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("chr" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`chr",_) -> true | _ -> false)),
               ("Ant", (`A "`chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`chr" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("str",_) -> true | _ -> false)),
               ("Ant", (`A "str")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("str" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`str",_) -> true | _ -> false)),
               ("Ant", (`A "`str")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`str" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               ("Ant", (`A "vrn")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int (_,_) -> true | _ -> false)), ("Int", `Any),
               "`Int s")],
           ("`Int (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int (_,s) -> (`Int (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int32 (_,_) -> true | _ -> false)),
               ("Int32", `Any), "`Int32 s")],
           ("`Int32 (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int32 (_,s) -> (`Int32 (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int64 (_,_) -> true | _ -> false)),
               ("Int64", `Any), "`Int64 s")],
           ("`Int64 (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int64 (_,s) -> (`Int64 (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Nativeint (_,_) -> true | _ -> false)),
               ("Nativeint", `Any), "`Nativeint s")],
           ("`Nativeint (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Nativeint (_,s) -> (`Nativeint (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Flo (_,_) -> true | _ -> false)), ("Flo", `Any),
               "`Flo s")],
           ("`Flo (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Flo (_,s) -> (`Flo (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Chr (_,_) -> true | _ -> false)), ("Chr", `Any),
               "`Chr s")],
           ("`Chr (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Chr (_,s) -> (`Chr (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
               "`Str s")],
           ("`Str (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str (_,s) -> (`Str (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stry
             (`Snterm
                (Fgram.obj
                   (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                    Fgram.t )));
          `Sself;
          `Skeyword ")"],
           ("`LetOpen (_loc, (`Negative _loc), i, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  (i : 'module_longident_dot_lparen) 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Negative _loc), i, e) : 'exp )))));
         ([`Snterm (Fgram.obj (vid : 'vid Fgram.t ))],
           ("(i : vid  :>exp)\n",
             (Fgram.mk_action
                (fun (i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>exp) : 'exp )))));
         ([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
           ("`Vrn (_loc, s)\n",
             (Fgram.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   (`Vrn (_loc, s) : 'exp )))));
         ([`Skeyword "["; `Skeyword "]"],
           ("(`Uid (_loc, \"[]\") : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
         ([`Skeyword "[";
          `Snterm (Fgram.obj (sem_exp_for_list : 'sem_exp_for_list Fgram.t ));
          `Skeyword "]"],
           ("mk_list (`Uid (_loc, \"[]\") : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  (mk_list : 'sem_exp_for_list)  _  (_loc : Locf.t)  ->
                   (mk_list (`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'exp )))));
         ([`Skeyword "[|";
          `Snterm (Fgram.obj (sem_exp : 'sem_exp Fgram.t ));
          `Skeyword "|]"],
           ("`Array (_loc, el)\n",
             (Fgram.mk_action
                (fun _  (el : 'sem_exp)  _  (_loc : Locf.t)  ->
                   (`Array (_loc, el) : 'exp )))));
         ([`Skeyword "{";
          `Stoken
            (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
              "`Lid x");
          `Skeyword "with";
          `Snterm (Fgram.obj (label_exp_list : 'label_exp_list Fgram.t ));
          `Skeyword "}"],
           ("(`RecordWith (_loc, el, (`Lid (_loc, x))) : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  (el : 'label_exp_list)  _  (__fan_1 : Ftoken.t)  _ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Lid (_,x) ->
                       ((`RecordWith (_loc, el, (`Lid (_loc, x))) : FAst.exp ) : 
                       'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "{";
          `Snterm (Fgram.obj (label_exp_list : 'label_exp_list Fgram.t ));
          `Skeyword "}"],
           ("`Record (_loc, el)\n",
             (Fgram.mk_action
                (fun _  (el : 'label_exp_list)  _  (_loc : Locf.t)  ->
                   (`Record (_loc, el) : 'exp )))));
         ([`Skeyword "{";
          `Skeyword "(";
          `Sself;
          `Skeyword ")";
          `Skeyword "with";
          `Snterm (Fgram.obj (label_exp_list : 'label_exp_list Fgram.t ));
          `Skeyword "}"],
           ("`RecordWith (_loc, el, e)\n",
             (Fgram.mk_action
                (fun _  (el : 'label_exp_list)  _  _  (e : 'exp)  _  _ 
                   (_loc : Locf.t)  -> (`RecordWith (_loc, el, e) : 'exp )))));
         ([`Skeyword "{<"; `Skeyword ">}"],
           ("`OvrInstEmpty _loc\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`OvrInstEmpty _loc : 'exp )))));
         ([`Skeyword "{<";
          `Snterm (Fgram.obj (field_exp_list : 'field_exp_list Fgram.t ));
          `Skeyword ">}"],
           ("`OvrInst (_loc, fel)\n",
             (Fgram.mk_action
                (fun _  (fel : 'field_exp_list)  _  (_loc : Locf.t)  ->
                   (`OvrInst (_loc, fel) : 'exp )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")"],
           ("`Constraint (_loc, e, t)\n",
             (Fgram.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Constraint (_loc, e, t) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Fgram.obj (comma_exp : 'comma_exp Fgram.t ));
          `Skeyword ")"],
           ("`Par (_loc, (`Com (_loc, e, el)))\n",
             (Fgram.mk_action
                (fun _  (el : 'comma_exp)  _  (e : 'exp)  _  (_loc : Locf.t) 
                   -> (`Par (_loc, (`Com (_loc, e, el))) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ";";
          `Snterm (Fgram.obj (sequence : 'sequence Fgram.t ));
          `Skeyword ")"],
           ("`Seq (_loc, (`Sem (_loc, e, seq)))\n",
             (Fgram.mk_action
                (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : Locf.t) 
                   -> (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp )))));
         ([`Skeyword "("; `Sself; `Skeyword ";"; `Skeyword ")"],
           ("`Seq (_loc, e)\n",
             (Fgram.mk_action
                (fun _  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Seq (_loc, e) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ":>";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")"],
           ("`Coercion (_loc, e, t, t2)\n",
             (Fgram.mk_action
                (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'exp)  _ 
                   (_loc : Locf.t)  -> (`Coercion (_loc, e, t, t2) : 
                   'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":>";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")"],
           ("`Subtype (_loc, e, t)\n",
             (Fgram.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Subtype (_loc, e, t) : 'exp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("e\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Skeyword "begin";
          `Snterm (Fgram.obj (sequence : 'sequence Fgram.t ));
          `Skeyword "end"],
           ("`Seq (_loc, seq)\n",
             (Fgram.mk_action
                (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                   (`Seq (_loc, seq) : 'exp )))));
         ([`Skeyword "begin"; `Skeyword "end"],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ));
          `Skeyword ")"],
           ("`Package_exp (_loc, me)\n",
             (Fgram.mk_action
                (fun _  (me : 'mexp)  _  _  (_loc : Locf.t)  ->
                   (`Package_exp (_loc, me) : 'exp )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword ")"],
           ("`Package_exp (_loc, (`Constraint (_loc, me, pt)))\n",
             (Fgram.mk_action
                (fun _  (pt : 'mtyp)  _  (me : 'mexp)  _  _  (_loc : Locf.t) 
                   ->
                   (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 
                   'exp )))))])]);
   Fgram.extend_single (sem_exp_for_list : 'sem_exp_for_list Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (exp : 'exp Fgram.t )); `Skeyword ";"; `Sself],
            ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), (el acc)) : FAst.exp )\n",
              (Fgram.mk_action
                 (fun (el : 'sem_exp_for_list)  _  (e : 'exp) 
                    (_loc : Locf.t)  ->
                    (fun acc  ->
                       (`App
                          (_loc, (`App (_loc, (`Uid (_loc, "::")), e)),
                            (el acc)) : FAst.exp ) : 'sem_exp_for_list )))));
         ([`Snterm (Fgram.obj (exp : 'exp Fgram.t )); `Skeyword ";"],
           ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      (`App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                      FAst.exp ) : 'sem_exp_for_list )))));
         ([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      (`App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                      FAst.exp ) : 'sem_exp_for_list )))))]));
   Fgram.extend_single (sequence : 'sequence Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "let";
           `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
           `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
           `Skeyword "in";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
           `Snterm (Fgram.obj (sequence' : 'sequence' Fgram.t ))],
            ("k (`LetIn (_loc, rf, bi, e))\n",
              (Fgram.mk_action
                 (fun (k : 'sequence')  (e : 'exp)  _  (bi : 'bind) 
                    (rf : 'opt_rec)  _  (_loc : Locf.t)  ->
                    (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "try";
          `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
          `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
          `Skeyword "in";
          `Sself;
          `Skeyword "with";
          `Snterm (Fgram.obj (case : 'case Fgram.t ));
          `Snterm (Fgram.obj (sequence' : 'sequence' Fgram.t ))],
           ("k (`LetTryInWith (_loc, r, bi, x, a))\n",
             (Fgram.mk_action
                (fun (k : 'sequence')  (a : 'case)  _  (x : 'sequence)  _ 
                   (bi : 'bind)  (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                   (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Snterm (Fgram.obj (mbind0 : 'mbind0 Fgram.t ));
          `Skeyword "in";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Snterm (Fgram.obj (sequence' : 'sequence' Fgram.t ))],
           ("k (`LetModule (_loc, m, mb, e))\n",
             (Fgram.mk_action
                (fun (k : 'sequence')  (e : 'exp)  _  (mb : 'mbind0) 
                   (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "open";
          `Snterm (Fgram.obj (module_longident : 'module_longident Fgram.t ));
          `Skeyword "in";
          `Sself],
           ("`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e)\n",
             (Fgram.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e) : 
                   'sequence )))));
         ([`Skeyword "let";
          `Skeyword "open";
          `Skeyword "!";
          `Snterm (Fgram.obj (module_longident : 'module_longident Fgram.t ));
          `Skeyword "in";
          `Sself],
           ("`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e)\n",
             (Fgram.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _  _ 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e) : 
                   'sequence )))));
         ([`Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Snterm (Fgram.obj (sequence' : 'sequence' Fgram.t ))],
           ("k e\n",
             (Fgram.mk_action
                (fun (k : 'sequence')  (e : 'exp)  (_loc : Locf.t)  ->
                   (k e : 'sequence )))))]));
   Fgram.extend_single (sequence' : 'sequence' Fgram.t )
     (None,
       (None, None,
         [([],
            ("fun e  -> e\n",
              (Fgram.mk_action
                 (fun (_loc : Locf.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Skeyword ";"],
           ("fun e  -> e\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Skeyword ";";
          `Snterm (Fgram.obj (sequence : 'sequence Fgram.t ))],
           ("fun e  -> `Sem (_loc, e, el)\n",
             (Fgram.mk_action
                (fun (el : 'sequence)  _  (_loc : Locf.t)  ->
                   (fun e  -> `Sem (_loc, e, el) : 'sequence' )))))]));
   Fgram.extend_single (comma_exp : 'comma_exp Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("`Com (_loc, e1, e2)\n",
              (Fgram.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'comma_exp) 
                    (_loc : Locf.t)  -> (`Com (_loc, e1, e2) : 'comma_exp )))));
         ([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("e\n",
             (Fgram.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'comma_exp )))))])));
  Fgram.extend_single (with_exp_lang : 'with_exp_lang Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (lang : 'lang Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("Ast_quotation.default := old; x\n",
             (Fgram.mk_action
                (fun (x : 'exp)  _  (old : 'lang)  (_loc : Locf.t)  ->
                   (Ast_quotation.default := old; x : 'with_exp_lang )))))]));
  Fgram.extend_single (with_stru_lang : 'with_stru_lang Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (lang : 'lang Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (stru : 'stru Fgram.t ))],
           ("Ast_quotation.default := old; x\n",
             (Fgram.mk_action
                (fun (x : 'stru)  _  (old : 'lang)  (_loc : Locf.t)  ->
                   (Ast_quotation.default := old; x : 'with_stru_lang )))))]));
  (Fgram.extend_single (bind_quot : 'bind_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (bind : 'bind Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'bind)  (_loc : Locf.t)  -> (x : 'bind_quot )))))]));
   Fgram.extend_single (bind : 'bind Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("bind",_) -> true | _ -> false)),
                ("Ant", (`A "bind")), "`Ant s")],
            ("mk_anti _loc ~c:\"bind\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("bind" as n),s) ->
                        (mk_anti _loc ~c:"bind" n s : 'bind )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"bind\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"bind" n s : 'bind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(`Bind (_loc, (mk_anti _loc ~c:\"pat\" n s), e) : FAst.bind )\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Bind (_loc, (mk_anti _loc ~c:"pat" n s), e) : 
                       FAst.bind ) : 'bind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Sself; `Skeyword "and"; `Sself],
           ("`And (_loc, b1, b2)\n",
             (Fgram.mk_action
                (fun (b2 : 'bind)  _  (b1 : 'bind)  (_loc : Locf.t)  ->
                   (`And (_loc, b1, b2) : 'bind )))));
         ([`Snterm (Fgram.obj (let_bind : 'let_bind Fgram.t ))],
           ("b\n",
             (Fgram.mk_action
                (fun (b : 'let_bind)  (_loc : Locf.t)  -> (b : 'bind )))))]));
   Fgram.extend_single (let_bind : 'let_bind Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
           `Snterm (Fgram.obj (fun_bind : 'fun_bind Fgram.t ))],
            ("`Bind (_loc, p, e)\n",
              (Fgram.mk_action
                 (fun (e : 'fun_bind)  (p : 'pat)  (_loc : Locf.t)  ->
                    (`Bind (_loc, p, e) : 'let_bind )))))])));
  (Fgram.extend_single (case : 'case Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "|";
           `Slist1sep
             ((`Snterm (Fgram.obj (case0 : 'case0 Fgram.t ))),
               (`Skeyword "|"))],
            ("bar_of_list l\n",
              (Fgram.mk_action
                 (fun (l : 'case0 list)  _  (_loc : Locf.t)  ->
                    (bar_of_list l : 'case )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`Case (_loc, p, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                   (`Case (_loc, p, e) : 'case )))))]));
   Fgram.extend_single (case0 : 'case0 Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("case",_) -> true | _ -> false)),
                ("Ant", (`A "case")), "`Ant s")],
            ("mk_anti _loc ~c:\"case\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("case" as n),s) ->
                        (mk_anti _loc ~c:"case" n s : 'case0 )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"case\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"case" n s : 'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword "when";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`CaseWhen (_loc, (mk_anti _loc ~c:\"case\" n s), w, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`CaseWhen (_loc, (mk_anti _loc ~c:"case" n s), w, e) : 
                       'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`Case (_loc, (mk_anti _loc ~c:\"case\" n s), e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Case (_loc, (mk_anti _loc ~c:"case" n s), e) : 
                       'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (pat_as_pat_opt : 'pat_as_pat_opt Fgram.t ));
          `Skeyword "when";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`CaseWhen (_loc, p, w, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (p : 'pat_as_pat_opt) 
                   (_loc : Locf.t)  -> (`CaseWhen (_loc, p, w, e) : 'case0 )))));
         ([`Snterm (Fgram.obj (pat_as_pat_opt : 'pat_as_pat_opt Fgram.t ));
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`Case (_loc, p, e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (p : 'pat_as_pat_opt)  (_loc : Locf.t) 
                   -> (`Case (_loc, p, e) : 'case0 )))))]));
   Fgram.extend_single (case_quot : 'case_quot Fgram.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((`Snterm (Fgram.obj (case0 : 'case0 Fgram.t ))),
                (`Skeyword "|"))],
            ("bar_of_list x\n",
              (Fgram.mk_action
                 (fun (x : 'case0 list)  (_loc : Locf.t)  ->
                    (bar_of_list x : 'case_quot )))))])));
  (Fgram.extend_single (rec_exp_quot : 'rec_exp_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (label_exp_list : 'label_exp_list Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'label_exp_list)  (_loc : Locf.t)  ->
                    (x : 'rec_exp_quot )))))]));
   Fgram.extend_single (label_exp : 'label_exp Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("rec_exp",_) -> true | _ -> false)),
                ("Ant", (`A "rec_exp")), "`Ant s")],
            ("mk_anti _loc ~c:\"rec_exp\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("rec_exp" as n),s) ->
                        (mk_anti _loc ~c:"rec_exp" n s : 'label_exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"rec_exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"rec_exp" n s : 'label_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (label_longident : 'label_longident Fgram.t ));
          `Snterm (Fgram.obj (fun_bind : 'fun_bind Fgram.t ))],
           ("(`RecBind (_loc, i, e) : FAst.rec_exp )\n",
             (Fgram.mk_action
                (fun (e : 'fun_bind)  (i : 'label_longident)  (_loc : Locf.t)
                    ->
                   ((`RecBind (_loc, i, e) : FAst.rec_exp ) : 'label_exp )))));
         ([`Snterm (Fgram.obj (label_longident : 'label_longident Fgram.t ))],
           ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
             (Fgram.mk_action
                (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                   'label_exp )))))]));
   Fgram.extend_single (field_exp : 'field_exp Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"rec_exp\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"rec_exp" n s : 'field_exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("bi",_) -> true | _ -> false)),
               ("Ant", (`A "bi")), "`Ant s")],
           ("mk_anti _loc ~c:\"rec_exp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("bi" as n),s) ->
                       (mk_anti _loc ~c:"rec_exp" n s : 'field_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("`RecBind (_loc, (l :>ident), e)\n",
             (Fgram.mk_action
                (fun (e : 'exp)  _  (l : 'a_lident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, (l :>ident), e) : 'field_exp )))))]));
   Fgram.extend_single (label_exp_list : 'label_exp_list Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (label_exp : 'label_exp Fgram.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, b1, b2)\n",
              (Fgram.mk_action
                 (fun (b2 : 'label_exp_list)  _  (b1 : 'label_exp) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, b1, b2) : 'label_exp_list )))));
         ([`Snterm (Fgram.obj (label_exp : 'label_exp Fgram.t ));
          `Skeyword ";"],
           ("b1\n",
             (Fgram.mk_action
                (fun _  (b1 : 'label_exp)  (_loc : Locf.t)  ->
                   (b1 : 'label_exp_list )))));
         ([`Snterm (Fgram.obj (label_exp : 'label_exp Fgram.t ))],
           ("b1\n",
             (Fgram.mk_action
                (fun (b1 : 'label_exp)  (_loc : Locf.t)  ->
                   (b1 : 'label_exp_list )))))]));
   Fgram.extend_single (field_exp_list : 'field_exp_list Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (field_exp : 'field_exp Fgram.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, b1, b2)\n",
              (Fgram.mk_action
                 (fun (b2 : 'field_exp_list)  _  (b1 : 'field_exp) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, b1, b2) : 'field_exp_list )))));
         ([`Snterm (Fgram.obj (field_exp : 'field_exp Fgram.t ));
          `Skeyword ";"],
           ("b1\n",
             (Fgram.mk_action
                (fun _  (b1 : 'field_exp)  (_loc : Locf.t)  ->
                   (b1 : 'field_exp_list )))));
         ([`Snterm (Fgram.obj (field_exp : 'field_exp Fgram.t ))],
           ("b1\n",
             (Fgram.mk_action
                (fun (b1 : 'field_exp)  (_loc : Locf.t)  ->
                   (b1 : 'field_exp_list )))))])));
  (let grammar_entry_create x = Fgram.mk x in
   let pat_constr: 'pat_constr Fgram.t = grammar_entry_create "pat_constr" in
   Fgram.extend_single (pat_quot : 'pat_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
           `Skeyword ",";
           `Snterm (Fgram.obj (comma_pat : 'comma_pat Fgram.t ))],
            ("`Com (_loc, x, y)\n",
              (Fgram.mk_action
                 (fun (y : 'comma_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                    (`Com (_loc, x, y) : 'pat_quot )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ";";
          `Snterm (Fgram.obj (sem_pat : 'sem_pat Fgram.t ))],
           ("`Sem (_loc, x, y)\n",
             (Fgram.mk_action
                (fun (y : 'sem_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, x, y) : 'pat_quot )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("x\n",
             (Fgram.mk_action
                (fun (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_quot )))))]));
   Fgram.extend_single (pat_as_pat_opt : 'pat_as_pat_opt Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
           `Skeyword "as";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
            ("`Alias (_loc, p1, s)\n",
              (Fgram.mk_action
                 (fun (s : 'a_lident)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Alias (_loc, p1, s) : 'pat_as_pat_opt )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("p\n",
             (Fgram.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_as_pat_opt )))))]));
   Fgram.extend_single (pat_constr : 'pat_constr Fgram.t )
     (None,
       (None, None,
         [([`Snterm
              (Fgram.obj (module_longident : 'module_longident Fgram.t ))],
            ("(i : vid  :>pat)\n",
              (Fgram.mk_action
                 (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                    ((i : vid  :>pat) : 'pat_constr )))));
         ([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
           ("(`Vrn (_loc, s) : pat )\n",
             (Fgram.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : pat ) : 'pat_constr )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               ("Ant", (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               ("Ant", (`A "vrn")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend (pat : 'pat Fgram.t )
     (None,
       [((Some "|"), (Some `LA),
          [([`Sself; `Skeyword "|"; `Sself],
             ("`Bar (_loc, p1, p2)\n",
               (Fgram.mk_action
                  (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                     (`Bar (_loc, p1, p2) : 'pat )))))]);
       ((Some ".."), (Some `NA),
         [([`Sself; `Skeyword ".."; `Sself],
            ("`PaRng (_loc, p1, p2)\n",
              (Fgram.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`PaRng (_loc, p1, p2) : 'pat )))))]);
       ((Some "::"), (Some `RA),
         [([`Sself; `Skeyword "::"; `Sself],
            ("`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p1)), p2)\n",
              (Fgram.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`App (_loc, (`App (_loc, (`Uid (_loc, "::")), p1)), p2) : 
                    'pat )))))]);
       ((Some "apply"), (Some `LA),
         [([`Snterm (Fgram.obj (pat_constr : 'pat_constr Fgram.t )); `Sself],
            ("match p2 with\n| (`Par (_loc,p) : FAst.pat) ->\n    List.fold_left (fun p1  p2  -> (`App (_loc, p1, p2) : FAst.pat )) p1\n      (Ast_basic.list_of_com p [])\n| _ -> (`App (_loc, p1, p2) : FAst.pat )\n",
              (Fgram.mk_action
                 (fun (p2 : 'pat)  (p1 : 'pat_constr)  (_loc : Locf.t)  ->
                    (match p2 with
                     | (`Par (_loc,p) : FAst.pat) ->
                         List.fold_left
                           (fun p1  p2  -> (`App (_loc, p1, p2) : FAst.pat ))
                           p1 (Ast_basic.list_of_com p [])
                     | _ -> (`App (_loc, p1, p2) : FAst.pat ) : 'pat )))));
         ([`Snterm (Fgram.obj (pat_constr : 'pat_constr Fgram.t ))],
           ("p1\n",
             (Fgram.mk_action
                (fun (p1 : 'pat_constr)  (_loc : Locf.t)  -> (p1 : 'pat )))));
         ([`Skeyword "lazy"; `Sself],
           ("`Lazy (_loc, p)\n",
             (Fgram.mk_action
                (fun (p : 'pat)  _  (_loc : Locf.t)  ->
                   (`Lazy (_loc, p) : 'pat )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"pat\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'pat )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               ("Ant", (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("par",_) -> true | _ -> false)),
               ("Ant", (`A "par")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("par" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int",_) -> true | _ -> false)),
               ("Ant", (`A "int")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int",_) -> true | _ -> false)),
               ("Ant", (`A "`int")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int32",_) -> true | _ -> false)),
               ("Ant", (`A "int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int32" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int32",_) -> true | _ -> false)),
               ("Ant", (`A "`int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int32" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int64",_) -> true | _ -> false)),
               ("Ant", (`A "int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int64" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int64",_) -> true | _ -> false)),
               ("Ant", (`A "`int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int64" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               ("Ant", (`A "vrn")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("nativeint",_) -> true | _ -> false)),
               ("Ant", (`A "nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("nativeint" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`nativeint",_) -> true | _ -> false)),
               ("Ant", (`A "`nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`nativeint" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("flo",_) -> true | _ -> false)),
               ("Ant", (`A "flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("flo" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`flo",_) -> true | _ -> false)),
               ("Ant", (`A "`flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`flo" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("chr",_) -> true | _ -> false)),
               ("Ant", (`A "chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("chr" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`chr",_) -> true | _ -> false)),
               ("Ant", (`A "`chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`chr" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("str",_) -> true | _ -> false)),
               ("Ant", (`A "str")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("str" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`str",_) -> true | _ -> false)),
               ("Ant", (`A "`str")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`str" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (vid : 'vid Fgram.t ))],
           ("(i : vid  :>pat)\n",
             (Fgram.mk_action
                (fun (i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>pat) : 'pat )))));
         ([`Stoken
             (((function | `Int (_,_) -> true | _ -> false)), ("Int", `Any),
               "`Int s")],
           ("`Int (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int (_,s) -> (`Int (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int32 (_,_) -> true | _ -> false)),
               ("Int32", `Any), "`Int32 s")],
           ("`Int32 (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int32 (_,s) -> (`Int32 (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int64 (_,_) -> true | _ -> false)),
               ("Int64", `Any), "`Int64 s")],
           ("`Int64 (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int64 (_,s) -> (`Int64 (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Nativeint (_,_) -> true | _ -> false)),
               ("Nativeint", `Any), "`Nativeint s")],
           ("`Nativeint (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Nativeint (_,s) -> (`Nativeint (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Flo (_,_) -> true | _ -> false)), ("Flo", `Any),
               "`Flo s")],
           ("`Flo (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Flo (_,s) -> (`Flo (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Chr (_,_) -> true | _ -> false)), ("Chr", `Any),
               "`Chr s")],
           ("`Chr (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Chr (_,s) -> (`Chr (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
               "`Str s")],
           ("`Str (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str (_,s) -> (`Str (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Int (_,_) -> true | _ -> false)), ("Int", `Any),
              "`Int s")],
           ("`Int (_loc, (Fstring.neg s))\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Int (_,s) -> (`Int (_loc, (Fstring.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Int32 (_,_) -> true | _ -> false)),
              ("Int32", `Any), "`Int32 s")],
           ("`Int32 (_loc, (Fstring.neg s))\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Int32 (_,s) -> (`Int32 (_loc, (Fstring.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Int64 (_,_) -> true | _ -> false)),
              ("Int64", `Any), "`Int64 s")],
           ("`Int64 (_loc, (Fstring.neg s))\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Int64 (_,s) -> (`Int64 (_loc, (Fstring.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Nativeint (_,_) -> true | _ -> false)),
              ("Nativeint", `Any), "`Nativeint s")],
           ("`Nativeint (_loc, (Fstring.neg s))\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Nativeint (_,s) ->
                       (`Nativeint (_loc, (Fstring.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Flo (_,_) -> true | _ -> false)), ("Flo", `Any),
              "`Flo s")],
           ("`Flo (_loc, (Fstring.neg s))\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Flo (_,s) -> (`Flo (_loc, (Fstring.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "["; `Skeyword "]"],
           ("(`Uid (_loc, \"[]\") : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "[]") : FAst.pat ) : 'pat )))));
         ([`Skeyword "[";
          `Snterm (Fgram.obj (sem_pat_for_list : 'sem_pat_for_list Fgram.t ));
          `Skeyword "]"],
           ("mk_list (`Uid (_loc, \"[]\") : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (mk_list : 'sem_pat_for_list)  _  (_loc : Locf.t)  ->
                   (mk_list (`Uid (_loc, "[]") : FAst.pat ) : 'pat )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'pat )))));
         ([`Skeyword "[|";
          `Snterm (Fgram.obj (sem_pat : 'sem_pat Fgram.t ));
          `Skeyword "|]"],
           ("`Array (_loc, pl)\n",
             (Fgram.mk_action
                (fun _  (pl : 'sem_pat)  _  (_loc : Locf.t)  ->
                   (`Array (_loc, pl) : 'pat )))));
         ([`Skeyword "{";
          `Snterm (Fgram.obj (label_pat_list : 'label_pat_list Fgram.t ));
          `Skeyword "}"],
           ("`Record (_loc, pl)\n",
             (Fgram.mk_action
                (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                   (`Record (_loc, pl) : 'pat )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.pat ) : 'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Fgram.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Fgram.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              ("Ant", (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n",
             (Fgram.mk_action
                (fun _  (__fan_4 : Ftoken.t)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_4))))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("p\n",
             (Fgram.mk_action
                (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "as";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   -> ((`Alias (_loc, p, s) : FAst.pat ) : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Fgram.obj (comma_pat : 'comma_pat Fgram.t ));
          `Skeyword ")"],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (pl : 'comma_pat)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'pat )))));
         ([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))));
         ([`Skeyword "#";
          `Snterm (Fgram.obj (type_longident : 'type_longident Fgram.t ))],
           ("(`ClassPath (_loc, i) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (i : 'type_longident)  _  (_loc : Locf.t)  ->
                   ((`ClassPath (_loc, i) : FAst.pat ) : 'pat )))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x -> (Ast_quotation.expand x Dyn_tag.pat : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  -> ((`Any _loc : FAst.pat ) : 'pat )))));
         ([`Stoken
             (((function | `Label (_,_) -> true | _ -> false)),
               ("Label", `Any), "`Label i");
          `Sself],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (p : 'pat)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label (_,i) ->
                       ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "~";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Sself],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (p : 'pat)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'pat )))));
         ([`Skeyword "~";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`LabelS (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'pat )))));
         ([`Stoken
             (((function | `Optlabel (_,_) -> true | _ -> false)),
               ("Optlabel", `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                   (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel (_,i) ->
                       (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Optlabel (_,_) -> true | _ -> false)),
               ("Optlabel", `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Fgram.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel (_,i) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              ("Ant", (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n",
             (Fgram.mk_action
                (fun _  (__fan_6 : Ftoken.t)  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_6))))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, i, p)\n",
             (Fgram.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : Locf.t)  -> (`OptLabl (_loc, i, p) : 'pat )))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`OptLablS (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'pat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Fgram.obj (ipat_tcon : 'ipat_tcon Fgram.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Fgram.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'pat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Fgram.obj (ipat_tcon : 'ipat_tcon Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'pat )))))])]);
   Fgram.extend_single (ipat : 'ipat Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "{";
           `Snterm (Fgram.obj (label_pat_list : 'label_pat_list Fgram.t ));
           `Skeyword "}"],
            ("(`Record (_loc, pl) : FAst.pat )\n",
              (Fgram.mk_action
                 (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                    ((`Record (_loc, pl) : FAst.pat ) : 'ipat )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               ("Ant", (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("par",_) -> true | _ -> false)),
               ("Ant", (`A "par")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("par" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "("; `Skeyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.pat ) : 'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Fgram.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Fgram.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              ("Ant", (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n",
             (Fgram.mk_action
                (fun _  (__fan_4 : Ftoken.t)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_4))))));
         ([`Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ")"],
           ("p\n",
             (Fgram.mk_action
                (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'ipat )))));
         ([`Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword "as";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   -> ((`Alias (_loc, p, s) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ",";
          `Snterm (Fgram.obj (comma_ipat : 'comma_ipat Fgram.t ));
          `Skeyword ")"],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (pl : 'comma_ipat)  _  (p : 'pat)  _  (_loc : Locf.t)
                    ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'ipat )))));
         ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("(s : alident  :>pat)\n",
             (Fgram.mk_action
                (fun (s : 'a_lident)  (_loc : Locf.t)  ->
                   ((s : alident  :>pat) : 'ipat )))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x -> (Ast_quotation.expand x Dyn_tag.pat : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ((`Any _loc : FAst.pat ) : 'ipat )))));
         ([`Stoken
             (((function | `Label (_,_) -> true | _ -> false)),
               ("Label", `Any), "`Label i");
          `Sself],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (p : 'ipat)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label (_,i) ->
                       ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "~";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Sself],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Fgram.mk_action
                (fun (p : 'ipat)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "~";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`LabelS (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'ipat )))));
         ([`Stoken
             (((function | `Optlabel (_,_) -> true | _ -> false)),
               ("Optlabel", `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                   (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel (_,i) ->
                       (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Optlabel (_,_) -> true | _ -> false)),
               ("Optlabel", `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Fgram.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel (_,i) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              ("Ant", (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n",
             (Fgram.mk_action
                (fun _  (__fan_6 : Ftoken.t)  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_6))))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat_tcon : 'pat_tcon Fgram.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, i, p)\n",
             (Fgram.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : Locf.t)  -> (`OptLabl (_loc, i, p) : 'ipat )))));
         ([`Skeyword "?";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`OptLablS (_loc, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'ipat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Fgram.obj (ipat_tcon : 'ipat_tcon Fgram.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Fgram.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'ipat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Fgram.obj (ipat_tcon : 'ipat_tcon Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n",
             (Fgram.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'ipat )))))]));
   Fgram.extend_single (sem_pat : 'sem_pat Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t )); `Skeyword ";"; `Sself],
            ("`Sem (_loc, p1, p2)\n",
              (Fgram.mk_action
                 (fun (p2 : 'sem_pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'sem_pat )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t )); `Skeyword ";"],
           ("p\n",
             (Fgram.mk_action
                (fun _  (p : 'pat)  (_loc : Locf.t)  -> (p : 'sem_pat )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("p\n",
             (Fgram.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'sem_pat )))))]));
   Fgram.extend_single (sem_pat_for_list : 'sem_pat_for_list Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t )); `Skeyword ";"; `Sself],
            ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), (pl acc))\n",
              (Fgram.mk_action
                 (fun (pl : 'sem_pat_for_list)  _  (p : 'pat) 
                    (_loc : Locf.t)  ->
                    (fun acc  ->
                       `App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), p)),
                           (pl acc)) : 'sem_pat_for_list )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t )); `Skeyword ";"],
           ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc)\n",
             (Fgram.mk_action
                (fun _  (p : 'pat)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc)\n",
             (Fgram.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))))]));
   Fgram.extend_single (pat_tcon : 'pat_tcon Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
           `Skeyword ":";
           `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
            ("(`Constraint (_loc, p, t) : FAst.pat )\n",
              (Fgram.mk_action
                 (fun (t : 'ctyp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                    ((`Constraint (_loc, p, t) : FAst.pat ) : 'pat_tcon )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("p\n",
             (Fgram.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_tcon )))))]));
   Fgram.extend_single (ipat_tcon : 'ipat_tcon Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"pat\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'ipat_tcon )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("(i : alident  :>pat)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i : alident  :>pat) : 'ipat_tcon )))));
         ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("(`Constraint (_loc, (i : alident  :>pat), t) : pat )\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, (i : alident  :>pat), t) : pat ) : 
                   'ipat_tcon )))))]));
   Fgram.extend_single (comma_ipat : 'comma_ipat Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("(`Com (_loc, p1, p2) : FAst.pat )\n",
              (Fgram.mk_action
                 (fun (p2 : 'comma_ipat)  _  (p1 : 'comma_ipat) 
                    (_loc : Locf.t)  ->
                    ((`Com (_loc, p1, p2) : FAst.pat ) : 'comma_ipat )))));
         ([`Snterm (Fgram.obj (ipat : 'ipat Fgram.t ))],
           ("p\n",
             (Fgram.mk_action
                (fun (p : 'ipat)  (_loc : Locf.t)  -> (p : 'comma_ipat )))))]));
   Fgram.extend_single (comma_pat : 'comma_pat Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("(`Com (_loc, p1, p2) : FAst.pat )\n",
              (Fgram.mk_action
                 (fun (p2 : 'comma_pat)  _  (p1 : 'comma_pat) 
                    (_loc : Locf.t)  ->
                    ((`Com (_loc, p1, p2) : FAst.pat ) : 'comma_pat )))));
         ([`Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("p\n",
             (Fgram.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'comma_pat )))))]));
   Fgram.extend_single (label_pat_list : 'label_pat_list Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (label_pat : 'label_pat Fgram.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, p1, p2)\n",
              (Fgram.mk_action
                 (fun (p2 : 'label_pat_list)  _  (p1 : 'label_pat) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_pat_list )))));
         ([`Snterm (Fgram.obj (label_pat : 'label_pat Fgram.t ));
          `Skeyword ";";
          `Skeyword "_"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Fgram.mk_action
                (fun _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Snterm (Fgram.obj (label_pat : 'label_pat Fgram.t ));
          `Skeyword ";";
          `Skeyword "_";
          `Skeyword ";"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Fgram.mk_action
                (fun _  _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Snterm (Fgram.obj (label_pat : 'label_pat Fgram.t ));
          `Skeyword ";"],
           ("p1\n",
             (Fgram.mk_action
                (fun _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))));
         ([`Snterm (Fgram.obj (label_pat : 'label_pat Fgram.t ))],
           ("p1\n",
             (Fgram.mk_action
                (fun (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))))]));
   Fgram.extend_single (label_pat : 'label_pat Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"pat\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'label_pat )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               ("Ant", (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'label_pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (label_longident : 'label_longident Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ))],
           ("`RecBind (_loc, i, p)\n",
             (Fgram.mk_action
                (fun (p : 'pat)  _  (i : 'label_longident)  (_loc : Locf.t) 
                   -> (`RecBind (_loc, i, p) : 'label_pat )))));
         ([`Snterm (Fgram.obj (label_longident : 'label_longident Fgram.t ))],
           ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
             (Fgram.mk_action
                (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                   'label_pat )))))])));
  (Fgram.extend_single (luident : 'luident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
                "`Lid i")],
            ("i\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid (_,i) -> (i : 'luident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("i\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) -> (i : 'luident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (aident : 'aident Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
            ("(i :>ident)\n",
              (Fgram.mk_action
                 (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                    ((i :>ident) : 'aident )))));
         ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ))],
           ("(i :>ident)\n",
             (Fgram.mk_action
                (fun (i : 'a_uident)  (_loc : Locf.t)  ->
                   ((i :>ident) : 'aident )))))]));
   Fgram.extend_single (astr : 'astr Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
                "`Lid i")],
            ("`C (_loc, i)\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid (_,i) -> (`C (_loc, i) : 'astr )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("`C (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) -> (`C (_loc, i) : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) -> (mk_anti _loc n s : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               ("Ant", (`A "vrn")), "`Ant s")],
           ("mk_anti _loc n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) -> (mk_anti _loc n s : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend (ident_quot : 'ident_quot Fgram.t )
     (None,
       [((Some "."), None,
          [([`Sself; `Skeyword "."; `Sself],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Fgram.mk_action
                  (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                     (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'ident_quot )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               ("Ant", (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid i")],
           ("(`Lid (_loc, i) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,i) ->
                       ((`Lid (_loc, i) : FAst.ident ) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (j : 'ident_quot)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,s) ->
                       ((`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident ) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "("; `Sself; `Sself; `Skeyword ")"],
           ("`Apply (_loc, i, j)\n",
             (Fgram.mk_action
                (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                   (_loc : Locf.t)  -> (`Apply (_loc, i, j) : 'ident_quot )))))])]);
   Fgram.extend_single (ident : 'ident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'ident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               ("Ant", (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid i")],
           ("`Lid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,i) -> (`Lid (_loc, i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) -> (`Uid (_loc, i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
             (Fgram.mk_action
                (fun (j : 'ident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Uid (_,s) ->
                       (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (vid : 'vid Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'vid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               ("Ant", (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Fgram.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid i")],
           ("`Lid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,i) -> (`Lid (_loc, i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) -> (`Uid (_loc, i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
             (Fgram.mk_action
                (fun (j : 'vid)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,s) -> (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (uident : 'uident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
                "`Uid s")],
            ("`Uid (_loc, s)\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid (_,s) -> (`Uid (_loc, s) : 'uident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"uident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"uident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"uident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("dot (`Uid (_loc, s)) l\n",
             (Fgram.mk_action
                (fun (l : 'uident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Uid (_,s) -> (dot (`Uid (_loc, s)) l : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("dot (mk_anti _loc ~c:\"uident\" n s) i\n",
             (Fgram.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("dot (mk_anti _loc ~c:\"uident\" n s) i\n",
             (Fgram.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("dot (mk_anti _loc ~c:\"uident\" n s) i\n",
             (Fgram.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (dot_lstrings : 'dot_lstrings Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
                "`Lid i")],
            ("((`Sub []), i)\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid (_,i) -> (((`Sub []), i) : 'dot_lstrings )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("match xs with\n| (`Sub xs,v) -> ((`Sub (i :: xs)), v)\n| _ -> raise (Fstream.Error \"impossible dot_lstrings\")\n",
             (Fgram.mk_action
                (fun (xs : 'dot_lstrings)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       ((match xs with
                         | (`Sub xs,v) -> ((`Sub (i :: xs)), v)
                         | _ ->
                             raise (Fstream.Error "impossible dot_lstrings")) : 
                       'dot_lstrings )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword ".";
          `Stoken
            (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
              "`Uid i");
          `Skeyword ".";
          `Sself],
           ("match xs with\n| (`Sub xs,v) -> ((`Absolute (i :: xs)), v)\n| _ -> raise (Fstream.Error \"impossible dot_lstrings\")\n",
             (Fgram.mk_action
                (fun (xs : 'dot_lstrings)  _  (__fan_1 : Ftoken.t)  _ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Uid (_,i) ->
                       ((match xs with
                         | (`Sub xs,v) -> ((`Absolute (i :: xs)), v)
                         | _ ->
                             raise (Fstream.Error "impossible dot_lstrings")) : 
                       'dot_lstrings )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]));
   Fgram.extend_single
     (module_longident_dot_lparen : 'module_longident_dot_lparen Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s");
           `Skeyword ".";
           `Skeyword "("],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun _  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s");
          `Skeyword ".";
          `Skeyword "("],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun _  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Skeyword "("],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun _  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       ((`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident ) : 
                       'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i");
          `Skeyword ".";
          `Skeyword "("],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Fgram.mk_action
                (fun _  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (module_longident : 'module_longident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, i)), l)\n",
             (Fgram.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) -> (`Uid (_loc, i) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n",
             (Fgram.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n",
             (Fgram.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend
     (module_longident_with_app : 'module_longident_with_app Fgram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("`Apply (_loc, i, j)\n",
               (Fgram.mk_action
                  (fun (j : 'module_longident_with_app) 
                     (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                     (`Apply (_loc, i, j) : 'module_longident_with_app )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("(`Dot (_loc, i, j) : FAst.ident )\n",
              (Fgram.mk_action
                 (fun (j : 'module_longident_with_app)  _ 
                    (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                    ((`Dot (_loc, i, j) : FAst.ident ) : 'module_longident_with_app )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       (`Uid (_loc, i) : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("i\n",
             (Fgram.mk_action
                (fun _  (i : 'module_longident_with_app)  _  (_loc : Locf.t) 
                   -> (i : 'module_longident_with_app )))))])]);
   Fgram.extend (type_longident : 'type_longident Fgram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("`Apply (_loc, i, j)\n",
               (Fgram.mk_action
                  (fun (j : 'type_longident)  (i : 'type_longident) 
                     (_loc : Locf.t)  ->
                     (`Apply (_loc, i, j) : 'type_longident )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("(`Dot (_loc, i, j) : FAst.ident )\n",
              (Fgram.mk_action
                 (fun (j : 'type_longident)  _  (i : 'type_longident) 
                    (_loc : Locf.t)  ->
                    ((`Dot (_loc, i, j) : FAst.ident ) : 'type_longident )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'type_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               ("Ant", (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid i")],
           ("(`Lid (_loc, i) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,i) ->
                       ((`Lid (_loc, i) : FAst.ident ) : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i")],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("i\n",
             (Fgram.mk_action
                (fun _  (i : 'type_longident)  _  (_loc : Locf.t)  ->
                   (i : 'type_longident )))))])]);
   Fgram.extend_single (label_longident : 'label_longident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'label_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               ("Ant", (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               ("Ant", (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid i")],
           ("(`Lid (_loc, i) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,i) ->
                       ((`Lid (_loc, i) : FAst.ident ) : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) ->
                       ((`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident ) : 
                       'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Fgram.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (cltyp_longident : 'cltyp_longident Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (type_longident : 'type_longident Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'type_longident)  (_loc : Locf.t)  ->
                    (x : 'cltyp_longident )))))]));
   Fgram.extend_single (val_longident : 'val_longident Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (ident : 'ident Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'ident)  (_loc : Locf.t)  -> (x : 'val_longident )))))]));
   Fgram.extend_single (class_longident : 'class_longident Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (label_longident : 'label_longident Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'label_longident)  (_loc : Locf.t)  ->
                    (x : 'class_longident )))))]));
   Fgram.extend_single (method_opt_override : 'method_opt_override Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "method"; `Skeyword "!"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'method_opt_override )))));
         ([`Skeyword "method";
          `Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'method_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "method";
          `Stoken
            (((function | `Ant ("override",_) -> true | _ -> false)),
              ("Ant", (`A "override")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("override" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'method_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "method"],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (`Negative _loc : 'method_opt_override )))))]));
   Fgram.extend_single (opt_override : 'opt_override Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "!"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'opt_override )))));
         ([`Stoken
             (((function | `Ant ("!",_) -> true | _ -> false)),
               ("Ant", (`A "!")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("!" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("override",_) -> true | _ -> false)),
               ("Ant", (`A "override")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("override" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_override )))))]));
   Fgram.extend_single
     (value_val_opt_override : 'value_val_opt_override Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "val"; `Skeyword "!"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'value_val_opt_override )))));
         ([`Skeyword "val";
          `Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "val";
          `Stoken
            (((function | `Ant ("override",_) -> true | _ -> false)),
              ("Ant", (`A "override")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("override" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "val";
          `Stoken
            (((function | `Ant ("!",_) -> true | _ -> false)),
              ("Ant", (`A "!")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("!" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
         ([`Skeyword "val"],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (`Negative _loc : 'value_val_opt_override )))))]));
   Fgram.extend_single (flag : 'flag Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "to"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'flag )))));
         ([`Skeyword "downto"],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  -> (`Negative _loc : 'flag )))));
         ([`Stoken
             (((function | `Ant ("to",_) -> true | _ -> false)),
               ("Ant", (`A "to")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("to" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'flag )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'flag )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (opt_private : 'opt_private Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "private"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_private )))));
         ([`Stoken
             (((function | `Ant ("private",_) -> true | _ -> false)),
               ("Ant", (`A "private")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("private" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_private )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_private )))))]));
   Fgram.extend_single (opt_mutable : 'opt_mutable Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "mutable"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_mutable )))));
         ([`Stoken
             (((function | `Ant ("mutable",_) -> true | _ -> false)),
               ("Ant", (`A "mutable")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mutable" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_mutable )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_mutable )))))]));
   Fgram.extend_single (opt_virtual : 'opt_virtual Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "virtual"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_virtual )))));
         ([`Stoken
             (((function | `Ant ("virtual",_) -> true | _ -> false)),
               ("Ant", (`A "virtual")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("virtual" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_virtual )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_virtual )))))]));
   Fgram.extend_single (opt_dot_dot : 'opt_dot_dot Fgram.t )
     (None,
       (None, None,
         [([`Skeyword ".."],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_dot_dot )))));
         ([`Stoken
             (((function | `Ant ("..",_) -> true | _ -> false)),
               ("Ant", (`A "..")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant ((".." as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_dot_dot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_dot_dot )))))]));
   Fgram.extend_single (opt_rec : 'opt_rec Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "rec"],
            ("`Positive _loc\n",
              (Fgram.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_rec )))));
         ([`Stoken
             (((function | `Ant ("rec",_) -> true | _ -> false)),
               ("Ant", (`A "rec")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("rec" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_rec )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Fgram.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_rec )))))]));
   Fgram.extend_single (a_lident : 'a_lident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"a_lident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               ("Ant", (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"a_lident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid s")],
           ("`Lid (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,s) -> (`Lid (_loc, s) : 'a_lident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (a_uident : 'a_uident Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"a_uident\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"a_uident" n s : 'a_uident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               ("Ant", (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"a_uident\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"a_uident" n s : 'a_uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid s")],
           ("`Uid (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,s) -> (`Uid (_loc, s) : 'a_uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (string_list : 'string_list Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc \"str_list\" s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant ("",s) ->
                        (mk_anti _loc "str_list" s : 'string_list )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Sself],
           ("`App (_loc, (mk_anti _loc \"\" s), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'string_list)  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant ("",s) ->
                       (`App (_loc, (mk_anti _loc "" s), xs) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
               "`Str x")],
           ("`Str (_loc, x)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str (_,x) -> (`Str (_loc, x) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
               "`Str x");
          `Sself],
           ("`App (_loc, (`Str (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'string_list)  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str (_,x) ->
                       (`App (_loc, (`Str (_loc, x)), xs) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
   Fgram.extend_single (rec_flag_quot : 'rec_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'opt_rec)  (_loc : Locf.t)  ->
                    (x : 'rec_flag_quot )))))]));
   Fgram.extend_single (direction_flag_quot : 'direction_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (flag : 'flag Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'flag)  (_loc : Locf.t)  ->
                    (x : 'direction_flag_quot )))))]));
   Fgram.extend_single (mutable_flag_quot : 'mutable_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (opt_mutable : 'opt_mutable Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'opt_mutable)  (_loc : Locf.t)  ->
                    (x : 'mutable_flag_quot )))))]));
   Fgram.extend_single (private_flag_quot : 'private_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (opt_private : 'opt_private Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'opt_private)  (_loc : Locf.t)  ->
                    (x : 'private_flag_quot )))))]));
   Fgram.extend_single (virtual_flag_quot : 'virtual_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'opt_virtual)  (_loc : Locf.t)  ->
                    (x : 'virtual_flag_quot )))))]));
   Fgram.extend_single (row_var_flag_quot : 'row_var_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (opt_dot_dot : 'opt_dot_dot Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'opt_dot_dot)  (_loc : Locf.t)  ->
                    (x : 'row_var_flag_quot )))))]));
   Fgram.extend_single (override_flag_quot : 'override_flag_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (opt_override : 'opt_override Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'opt_override)  (_loc : Locf.t)  ->
                    (x : 'override_flag_quot )))))]));
   Fgram.extend_single (pat_eoi : 'pat_eoi Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (pat : 'pat Fgram.t ));
           `Stoken
             (((function | `EOI _ -> true | _ -> false)), ("EOI", `Empty),
               "`EOI")],
            ("x\n",
              (Fgram.mk_action
                 (fun _  (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_eoi )))))]));
   Fgram.extend_single (exp_eoi : 'exp_eoi Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (exp : 'exp Fgram.t ));
           `Stoken
             (((function | `EOI _ -> true | _ -> false)), ("EOI", `Empty),
               "`EOI")],
            ("x\n",
              (Fgram.mk_action
                 (fun _  (x : 'exp)  (_loc : Locf.t)  -> (x : 'exp_eoi )))))])));
  (Fgram.extend_single (implem : 'implem Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `DirQuotation _ -> true | _ -> false)),
                ("DirQuotation", `Any), "`DirQuotation _")],
            ("Fdir.handle_quot x; ([], (Some _loc))\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `DirQuotation x ->
                        ((Fdir.handle_quot x; ([], (Some _loc))) : 'implem )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t ));
          `Skeyword ";;";
          `Sself],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Fgram.mk_action
                (fun (rest : 'implem)  _  (si : 'stru)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'implem )))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t )); `Sself],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Fgram.mk_action
                (fun (rest : 'implem)  (si : 'stru)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'implem )))));
         ([`Stoken
             (((function | `EOI _ -> true | _ -> false)), ("EOI", `Empty),
               "`EOI")],
           ("([], None)\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  -> (([], None) : 'implem )))))]));
   Fgram.extend_single (top_phrase : 'top_phrase Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
           `Skeyword ";;"],
            ("Some (`Directive (_loc, n, dp))\n",
              (Fgram.mk_action
                 (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                    (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
         ([`Skeyword "#";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ";;"],
           ("Some (`DirectiveSimple (_loc, n))\n",
             (Fgram.mk_action
                (fun _  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                   (Some (`DirectiveSimple (_loc, n)) : 'top_phrase )))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t )); `Skeyword ";;"],
           ("Some st\n",
             (Fgram.mk_action
                (fun _  (st : 'stru)  (_loc : Locf.t)  ->
                   (Some st : 'top_phrase )))));
         ([`Stoken
             (((function | `EOI _ -> true | _ -> false)), ("EOI", `Empty),
               "`EOI")],
           ("None\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  -> (None : 'top_phrase )))))]));
   Fgram.extend_single (strus : 'strus Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc n ~c:\"stru\" s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc n ~c:"stru" s : 'strus )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               ("Ant", (`A "stri")), "`Ant s")],
           ("mk_anti _loc n ~c:\"stru\" s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (mk_anti _loc n ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";;"],
           ("mk_anti _loc n ~c:\"stru\" s\n",
             (Fgram.mk_action
                (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc n ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               ("Ant", (`A "stri")), "`Ant s");
          `Skeyword ";;"],
           ("mk_anti _loc n ~c:\"stru\" s\n",
             (Fgram.mk_action
                (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (mk_anti _loc n ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Fgram.mk_action
                (fun (st : 'strus)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               ("Ant", (`A "stri")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Fgram.mk_action
                (fun (st : 'strus)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Fgram.mk_action
                (fun (st : 'strus)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               ("Ant", (`A "stri")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Fgram.mk_action
                (fun (st : 'strus)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t ))],
           ("st\n",
             (Fgram.mk_action
                (fun (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus )))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t )); `Skeyword ";;"],
           ("st\n",
             (Fgram.mk_action
                (fun _  (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus )))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t ));
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Fgram.mk_action
                (fun (xs : 'strus)  _  (st : 'stru)  (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'strus )))));
         ([`Snterm (Fgram.obj (stru : 'stru Fgram.t )); `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Fgram.mk_action
                (fun (xs : 'strus)  (st : 'stru)  (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'strus )))))]));
   Fgram.extend_single (stru_quot : 'stru_quot Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("`Directive (_loc, n, dp)\n",
              (Fgram.mk_action
                 (fun (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`Directive (_loc, n, dp) : 'stru_quot )))));
         ([`Skeyword "#";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`DirectiveSimple (_loc, n)\n",
             (Fgram.mk_action
                (fun (n : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`DirectiveSimple (_loc, n) : 'stru_quot )))));
         ([`Snterm (Fgram.obj (strus : 'strus Fgram.t ))],
           ("x\n",
             (Fgram.mk_action
                (fun (x : 'strus)  (_loc : Locf.t)  -> (x : 'stru_quot )))))]));
   Fgram.extend (stru : 'stru Fgram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "exception";
            `Snterm
              (Fgram.obj
                 (constructor_declaration : 'constructor_declaration Fgram.t ))],
             ("`Exception (_loc, t)\n",
               (Fgram.mk_action
                  (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)  ->
                     (`Exception (_loc, t) : 'stru )))));
          ([`Skeyword "external";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
           `Skeyword ":";
           `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
           `Skeyword "=";
           `Snterm (Fgram.obj (string_list : 'string_list Fgram.t ))],
            ("`External (_loc, i, t, sl)\n",
              (Fgram.mk_action
                 (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)
                     _  (_loc : Locf.t)  ->
                    (`External (_loc, i, t, sl) : 'stru )))));
          ([`Skeyword "include"; `Snterm (Fgram.obj (mexp : 'mexp Fgram.t ))],
            ("`Include (_loc, me)\n",
              (Fgram.mk_action
                 (fun (me : 'mexp)  _  (_loc : Locf.t)  ->
                    (`Include (_loc, me) : 'stru )))));
          ([`Skeyword "module";
           `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
           `Snterm (Fgram.obj (mbind0 : 'mbind0 Fgram.t ))],
            ("`Module (_loc, i, mb)\n",
              (Fgram.mk_action
                 (fun (mb : 'mbind0)  (i : 'a_uident)  _  (_loc : Locf.t)  ->
                    (`Module (_loc, i, mb) : 'stru )))));
          ([`Skeyword "module";
           `Skeyword "rec";
           `Snterm (Fgram.obj (mbind : 'mbind Fgram.t ))],
            ("`RecModule (_loc, mb)\n",
              (Fgram.mk_action
                 (fun (mb : 'mbind)  _  _  (_loc : Locf.t)  ->
                    (`RecModule (_loc, mb) : 'stru )))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
           `Skeyword "=";
           `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ))],
            ("`ModuleType (_loc, i, mt)\n",
              (Fgram.mk_action
                 (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : Locf.t)
                     -> (`ModuleType (_loc, i, mt) : 'stru )))));
          ([`Skeyword "open";
           `Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ))],
            ("`Open (_loc, (`Negative _loc), (i : vid  :>ident))\n",
              (Fgram.mk_action
                 (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                    (`Open (_loc, (`Negative _loc), (i : vid  :>ident)) : 
                    'stru )))));
          ([`Skeyword "open";
           `Skeyword "!";
           `Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ))],
            ("`Open (_loc, (`Positive _loc), (i : vid  :>ident))\n",
              (Fgram.mk_action
                 (fun (i : 'module_longident)  _  _  (_loc : Locf.t)  ->
                    (`Open (_loc, (`Positive _loc), (i : vid  :>ident)) : 
                    'stru )))));
          ([`Skeyword "type";
           `Snterm
             (Fgram.obj (type_declaration : 'type_declaration Fgram.t ))],
            ("`Type (_loc, td)\n",
              (Fgram.mk_action
                 (fun (td : 'type_declaration)  _  (_loc : Locf.t)  ->
                    (`Type (_loc, td) : 'stru )))));
          ([`Skeyword "type";
           `Snterm
             (Fgram.obj (type_declaration : 'type_declaration Fgram.t ));
           `Skeyword "with";
           `Skeyword "(";
           `Snterm (Fgram.obj (string_list : 'string_list Fgram.t ));
           `Skeyword ")"],
            ("`TypeWith (_loc, t, ns)\n",
              (Fgram.mk_action
                 (fun _  (ns : 'string_list)  _  _  (t : 'type_declaration) 
                    _  (_loc : Locf.t)  -> (`TypeWith (_loc, t, ns) : 
                    'stru )))));
          ([`Skeyword "let";
           `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
           `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
           `Skeyword "in";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("(`StExp (_loc, (`LetIn (_loc, r, bi, x))) : FAst.stru )\n",
              (Fgram.mk_action
                 (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                    (_loc : Locf.t)  ->
                    ((`StExp (_loc, (`LetIn (_loc, r, bi, x))) : FAst.stru ) : 
                    'stru )))));
          ([`Skeyword "let";
           `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
           `Snterm (Fgram.obj (bind : 'bind Fgram.t ))],
            ("match bi with\n| `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n| _ -> `Value (_loc, r, bi)\n",
              (Fgram.mk_action
                 (fun (bi : 'bind)  (r : 'opt_rec)  _  (_loc : Locf.t)  ->
                    (match bi with
                     | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                     | _ -> `Value (_loc, r, bi) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
           `Snterm (Fgram.obj (mbind0 : 'mbind0 Fgram.t ));
           `Skeyword "in";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("(`StExp (_loc, (`LetModule (_loc, m, mb, e))) : FAst.stru )\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : Locf.t)  ->
                    ((`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 
                    FAst.stru ) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ));
           `Skeyword "in";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("let i = (i : vid  :>ident) in\n(`StExp (_loc, (`LetOpen (_loc, (`Negative _loc), i, e))) : FAst.stru )\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : Locf.t)  ->
                    (let i = (i : vid  :>ident) in
                     (`StExp
                        (_loc, (`LetOpen (_loc, (`Negative _loc), i, e))) : 
                       FAst.stru ) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Skeyword "!";
           `Snterm
             (Fgram.obj (module_longident : 'module_longident Fgram.t ));
           `Skeyword "in";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("let i = (i : vid  :>ident) in\n(`StExp (_loc, (`LetOpen (_loc, (`Positive _loc), i, e))) : FAst.stru )\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _  _ 
                    (_loc : Locf.t)  ->
                    (let i = (i : vid  :>ident) in
                     (`StExp
                        (_loc, (`LetOpen (_loc, (`Positive _loc), i, e))) : 
                       FAst.stru ) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
           `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
           `Skeyword "in";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
           `Skeyword "with";
           `Snterm (Fgram.obj (case : 'case Fgram.t ))],
            ("`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a)))\n",
              (Fgram.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                    (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                    (`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a))) : 
                    'stru )))));
          ([`Skeyword "class";
           `Snterm
             (Fgram.obj (class_declaration : 'class_declaration Fgram.t ))],
            ("`Class (_loc, cd)\n",
              (Fgram.mk_action
                 (fun (cd : 'class_declaration)  _  (_loc : Locf.t)  ->
                    (`Class (_loc, cd) : 'stru )))));
          ([`Skeyword "class";
           `Skeyword "type";
           `Snterm
             (Fgram.obj (cltyp_declaration : 'cltyp_declaration Fgram.t ))],
            ("`ClassType (_loc, ctd)\n",
              (Fgram.mk_action
                 (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t)  ->
                    (`ClassType (_loc, ctd) : 'stru )))));
          ([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"stru\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"stru" n s : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
          ([`Stoken
              (((function | `Ant ("stri",_) -> true | _ -> false)),
                ("Ant", (`A "stri")), "`Ant s")],
            ("mk_anti _loc ~c:\"stru\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("stri" as n),s) ->
                        (mk_anti _loc ~c:"stru" n s : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
          ([`Stoken
              (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
                "`Quot _")],
            ("Ast_quotation.expand x Dyn_tag.stru\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Quot x ->
                        (Ast_quotation.expand x Dyn_tag.stru : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
          ([`Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("`StExp (_loc, e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  (_loc : Locf.t)  ->
                    (`StExp (_loc, e) : 'stru )))))])]));
  (Fgram.extend_single (clsigi_quot : 'clsigi_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (clsigi : 'clsigi Fgram.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, x1, x2)\n",
              (Fgram.mk_action
                 (fun (x2 : 'clsigi_quot)  _  (x1 : 'clsigi)  (_loc : Locf.t)
                     -> (`Sem (_loc, x1, x2) : 'clsigi_quot )))));
         ([`Snterm (Fgram.obj (clsigi : 'clsigi Fgram.t ))],
           ("x\n",
             (Fgram.mk_action
                (fun (x : 'clsigi)  (_loc : Locf.t)  -> (x : 'clsigi_quot )))))]));
   Fgram.extend_single (class_signature : 'class_signature Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clsigi\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               ("Ant", (`A "csg")), "`Ant s")],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Fgram.mk_action
                (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               ("Ant", (`A "csg")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Fgram.mk_action
                (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (csg : 'class_signature)  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               ("Ant", (`A "csg")), "`Ant s");
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (csg : 'class_signature)  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               ("Ant", (`A "csg")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (clsigi : 'clsigi Fgram.t ))],
           ("csg\n",
             (Fgram.mk_action
                (fun (csg : 'clsigi)  (_loc : Locf.t)  ->
                   (csg : 'class_signature )))));
         ([`Snterm (Fgram.obj (clsigi : 'clsigi Fgram.t )); `Skeyword ";"],
           ("csg\n",
             (Fgram.mk_action
                (fun _  (csg : 'clsigi)  (_loc : Locf.t)  ->
                   (csg : 'class_signature )))));
         ([`Snterm (Fgram.obj (clsigi : 'clsigi Fgram.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, csg, xs)\n",
             (Fgram.mk_action
                (fun (xs : 'class_signature)  _  (csg : 'clsigi) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, csg, xs) : 'class_signature )))));
         ([`Snterm (Fgram.obj (clsigi : 'clsigi Fgram.t )); `Sself],
           ("`Sem (_loc, csg, xs)\n",
             (Fgram.mk_action
                (fun (xs : 'class_signature)  (csg : 'clsigi) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, csg, xs) : 'class_signature )))))]));
   Fgram.extend_single (clsigi : 'clsigi Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clsigi\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clsigi" n s : 'clsigi )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               ("Ant", (`A "csg")), "`Ant s")],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'clsigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.clsigi\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.clsigi : 'clsigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "inherit";
          `Snterm (Fgram.obj (cltyp : 'cltyp Fgram.t ))],
           ("`SigInherit (_loc, cs)\n",
             (Fgram.mk_action
                (fun (cs : 'cltyp)  _  (_loc : Locf.t)  ->
                   (`SigInherit (_loc, cs) : 'clsigi )))));
         ([`Skeyword "val";
          `Snterm (Fgram.obj (opt_mutable : 'opt_mutable Fgram.t ));
          `Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("(`CgVal (_loc, l, mf, mv, t) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) 
                   (mf : 'opt_mutable)  _  (_loc : Locf.t)  ->
                   ((`CgVal (_loc, l, mf, mv, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Skeyword "method";
          `Skeyword "virtual";
          `Snterm (Fgram.obj (opt_private : 'opt_private Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("(`VirMeth (_loc, l, pf, t) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   _  (_loc : Locf.t)  ->
                   ((`VirMeth (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Skeyword "method";
          `Snterm (Fgram.obj (opt_private : 'opt_private Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("(`Method (_loc, l, pf, t) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (_loc : Locf.t)  ->
                   ((`Method (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Skeyword "constraint";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("(`Eq (_loc, t1, t2) : FAst.clsigi )\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Eq (_loc, t1, t2) : FAst.clsigi ) : 'clsigi )))))])));
  (Fgram.extend_single (class_structure : 'class_structure Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clfield\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               ("Ant", (`A "cst")), "`Ant s")],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Fgram.mk_action
                (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               ("Ant", (`A "cst")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Fgram.mk_action
                (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), st)\n",
             (Fgram.mk_action
                (fun (st : 'class_structure)  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), st) : 
                       'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               ("Ant", (`A "cst")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), st)\n",
             (Fgram.mk_action
                (fun (st : 'class_structure)  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), st) : 
                       'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), cst) : FAst.clfield )\n",
             (Fgram.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), cst) : 
                       FAst.clfield ) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               ("Ant", (`A "cst")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), cst) : FAst.clfield )\n",
             (Fgram.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), cst) : 
                       FAst.clfield ) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (clfield : 'clfield Fgram.t ))],
           ("st\n",
             (Fgram.mk_action
                (fun (st : 'clfield)  (_loc : Locf.t)  ->
                   (st : 'class_structure )))));
         ([`Snterm (Fgram.obj (clfield : 'clfield Fgram.t )); `Skeyword ";"],
           ("st\n",
             (Fgram.mk_action
                (fun _  (st : 'clfield)  (_loc : Locf.t)  ->
                   (st : 'class_structure )))));
         ([`Snterm (Fgram.obj (clfield : 'clfield Fgram.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Fgram.mk_action
                (fun (xs : 'class_structure)  _  (st : 'clfield) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'class_structure )))));
         ([`Snterm (Fgram.obj (clfield : 'clfield Fgram.t )); `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Fgram.mk_action
                (fun (xs : 'class_structure)  (st : 'clfield) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'class_structure )))))]));
   Fgram.extend_single (clfield : 'clfield Fgram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clfield\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clfield" n s : 'clfield )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               ("Ant", (`A "cst")), "`Ant s")],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'clfield )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.clfield\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.clfield : 'clfield )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Skeyword "inherit";
          `Snterm (Fgram.obj (opt_override : 'opt_override Fgram.t ));
          `Snterm (Fgram.obj (clexp : 'clexp Fgram.t ))],
           ("`Inherit (_loc, o, ce)\n",
             (Fgram.mk_action
                (fun (ce : 'clexp)  (o : 'opt_override)  _  (_loc : Locf.t) 
                   -> (`Inherit (_loc, o, ce) : 'clfield )))));
         ([`Skeyword "inherit";
          `Snterm (Fgram.obj (opt_override : 'opt_override Fgram.t ));
          `Snterm (Fgram.obj (clexp : 'clexp Fgram.t ));
          `Skeyword "as";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`InheritAs (_loc, o, ce, i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (ce : 'clexp)  (o : 'opt_override) 
                   _  (_loc : Locf.t)  ->
                   (`InheritAs (_loc, o, ce, i) : 'clfield )))));
         ([`Snterm
             (Fgram.obj
                (value_val_opt_override : 'value_val_opt_override Fgram.t ));
          `Snterm (Fgram.obj (opt_mutable : 'opt_mutable Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Snterm (Fgram.obj (cvalue_bind : 'cvalue_bind Fgram.t ))],
           ("(`CrVal (_loc, lab, o, mf, e) : FAst.clfield )\n",
             (Fgram.mk_action
                (fun (e : 'cvalue_bind)  (lab : 'a_lident) 
                   (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                   (_loc : Locf.t)  ->
                   ((`CrVal (_loc, lab, o, mf, e) : FAst.clfield ) : 
                   'clfield )))));
         ([`Snterm
             (Fgram.obj
                (value_val_opt_override : 'value_val_opt_override Fgram.t ));
          `Skeyword "virtual";
          `Snterm (Fgram.obj (opt_mutable : 'opt_mutable Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("match o with\n| `Negative _ -> (`VirVal (_loc, l, mf, t) : FAst.clfield )\n| _ -> raise (Fstream.Error \"override (!) is incompatible with virtual\")\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mf : 'opt_mutable)  _ 
                   (o : 'value_val_opt_override)  (_loc : Locf.t)  ->
                   (match o with
                    | `Negative _ ->
                        (`VirVal (_loc, l, mf, t) : FAst.clfield )
                    | _ ->
                        raise
                          (Fstream.Error
                             "override (!) is incompatible with virtual") : 
                   'clfield )))));
         ([`Snterm
             (Fgram.obj (method_opt_override : 'method_opt_override Fgram.t ));
          `Skeyword "virtual";
          `Snterm (Fgram.obj (opt_private : 'opt_private Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("match o with\n| `Negative _ -> `VirMeth (_loc, l, pf, t)\n| _ -> raise (Fstream.Error \"override (!) is incompatible with virtual\")\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (o : 'method_opt_override)  (_loc : Locf.t)  ->
                   (match o with
                    | `Negative _ -> `VirMeth (_loc, l, pf, t)
                    | _ ->
                        raise
                          (Fstream.Error
                             "override (!) is incompatible with virtual") : 
                   'clfield )))));
         ([`Snterm
             (Fgram.obj (method_opt_override : 'method_opt_override Fgram.t ));
          `Snterm (Fgram.obj (opt_private : 'opt_private Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Snterm (Fgram.obj (fun_bind : 'fun_bind Fgram.t ))],
           ("`CrMth (_loc, l, o, pf, e, t)\n",
             (Fgram.mk_action
                (fun (e : 'fun_bind)  (t : 'ctyp)  _  (l : 'a_lident) 
                   (pf : 'opt_private)  (o : 'method_opt_override) 
                   (_loc : Locf.t)  ->
                   (`CrMth (_loc, l, o, pf, e, t) : 'clfield )))));
         ([`Snterm
             (Fgram.obj (method_opt_override : 'method_opt_override Fgram.t ));
          `Snterm (Fgram.obj (opt_private : 'opt_private Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Snterm (Fgram.obj (fun_bind : 'fun_bind Fgram.t ))],
           ("`CrMthS (_loc, l, o, pf, e)\n",
             (Fgram.mk_action
                (fun (e : 'fun_bind)  (l : 'a_lident)  (pf : 'opt_private) 
                   (o : 'method_opt_override)  (_loc : Locf.t)  ->
                   (`CrMthS (_loc, l, o, pf, e) : 'clfield )))));
         ([`Skeyword "constraint";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("(`Eq (_loc, t1, t2) : FAst.clfield )\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Eq (_loc, t1, t2) : FAst.clfield ) : 'clfield )))));
         ([`Skeyword "initializer";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(`Initializer (_loc, se) : FAst.clfield )\n",
             (Fgram.mk_action
                (fun (se : 'exp)  _  (_loc : Locf.t)  ->
                   ((`Initializer (_loc, se) : FAst.clfield ) : 'clfield )))))]));
   Fgram.extend_single (clfield_quot : 'clfield_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (clfield : 'clfield Fgram.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, x1, x2)\n",
              (Fgram.mk_action
                 (fun (x2 : 'clfield_quot)  _  (x1 : 'clfield) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, x1, x2) : 'clfield_quot )))));
         ([`Snterm (Fgram.obj (clfield : 'clfield Fgram.t ))],
           ("x\n",
             (Fgram.mk_action
                (fun (x : 'clfield)  (_loc : Locf.t)  -> (x : 'clfield_quot )))))])));
  (Fgram.extend_single (clexp_quot : 'clexp_quot Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (clexp : 'clexp Fgram.t ))],
            ("x\n",
              (Fgram.mk_action
                 (fun (x : 'clexp)  (_loc : Locf.t)  -> (x : 'clexp_quot )))))]));
   Fgram.extend_single (class_declaration : 'class_declaration Fgram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, c1, c2)\n",
              (Fgram.mk_action
                 (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration)
                     (_loc : Locf.t)  ->
                    (`And (_loc, c1, c2) : 'class_declaration )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"clexp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'class_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cdcl",_) -> true | _ -> false)),
               ("Ant", (`A "cdcl")), "`Ant s")],
           ("mk_anti _loc ~c:\"clexp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cdcl" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'class_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword "[";
          `Snterm
            (Fgram.obj
               (comma_type_parameter : 'comma_type_parameter Fgram.t ));
          `Skeyword "]";
          `Snterm (Fgram.obj (class_fun_bind : 'class_fun_bind Fgram.t ))],
           ("`ClDecl (_loc, mv, (i :>ident), x, ce)\n",
             (Fgram.mk_action
                (fun (ce : 'class_fun_bind)  _  (x : 'comma_type_parameter) 
                   _  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t) 
                   ->
                   (`ClDecl (_loc, mv, (i :>ident), x, ce) : 'class_declaration )))));
         ([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Snterm (Fgram.obj (class_fun_bind : 'class_fun_bind Fgram.t ))],
           ("`ClDeclS (_loc, mv, (i :>ident), ce)\n",
             (Fgram.mk_action
                (fun (ce : 'class_fun_bind)  (i : 'a_lident) 
                   (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                   (`ClDeclS (_loc, mv, (i :>ident), ce) : 'class_declaration )))))]));
   Fgram.extend_single (class_fun_bind : 'class_fun_bind Fgram.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Fgram.obj (clexp : 'clexp Fgram.t ))],
            ("ce\n",
              (Fgram.mk_action
                 (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                    (ce : 'class_fun_bind )))));
         ([`Skeyword ":";
          `Snterm (Fgram.obj (cltyp_plus : 'cltyp_plus Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (clexp : 'clexp Fgram.t ))],
           ("`Constraint (_loc, ce, ct)\n",
             (Fgram.mk_action
                (fun (ce : 'clexp)  _  (ct : 'cltyp_plus)  _  (_loc : Locf.t)
                    -> (`Constraint (_loc, ce, ct) : 'class_fun_bind )))));
         ([`Snterm (Fgram.obj (ipat : 'ipat Fgram.t )); `Sself],
           ("`CeFun (_loc, p, cfb)\n",
             (Fgram.mk_action
                (fun (cfb : 'class_fun_bind)  (p : 'ipat)  (_loc : Locf.t) 
                   -> (`CeFun (_loc, p, cfb) : 'class_fun_bind )))))]));
   Fgram.extend_single (class_fun_def : 'class_fun_def Fgram.t )
     (None,
       (None, None,
         [([`Snterm (Fgram.obj (ipat : 'ipat Fgram.t )); `Sself],
            ("`CeFun (_loc, p, ce)\n",
              (Fgram.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  (_loc : Locf.t)  ->
                    (`CeFun (_loc, p, ce) : 'class_fun_def )))));
         ([`Skeyword "->"; `Snterm (Fgram.obj (clexp : 'clexp Fgram.t ))],
           ("ce\n",
             (Fgram.mk_action
                (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                   (ce : 'class_fun_def )))))]));
   Fgram.extend (clexp : 'clexp Fgram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "fun";
            `Snterm (Fgram.obj (ipat : 'ipat Fgram.t ));
            `Snterm (Fgram.obj (class_fun_def : 'class_fun_def Fgram.t ))],
             ("`CeFun (_loc, p, ce)\n",
               (Fgram.mk_action
                  (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : Locf.t)
                      -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Skeyword "function";
           `Snterm (Fgram.obj (ipat : 'ipat Fgram.t ));
           `Snterm (Fgram.obj (class_fun_def : 'class_fun_def Fgram.t ))],
            ("`CeFun (_loc, p, ce)\n",
              (Fgram.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : Locf.t) 
                    -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Skeyword "let";
           `Snterm (Fgram.obj (opt_rec : 'opt_rec Fgram.t ));
           `Snterm (Fgram.obj (bind : 'bind Fgram.t ));
           `Skeyword "in";
           `Sself],
            ("`LetIn (_loc, rf, bi, ce)\n",
              (Fgram.mk_action
                 (fun (ce : 'clexp)  _  (bi : 'bind)  (rf : 'opt_rec)  _ 
                    (_loc : Locf.t)  -> (`LetIn (_loc, rf, bi, ce) : 
                    'clexp )))))]);
       ((Some "apply"), (Some `NA),
         [([`Sself; `Snterml ((Fgram.obj (exp : 'exp Fgram.t )), "label")],
            ("`CeApp (_loc, ce, e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  (ce : 'clexp)  (_loc : Locf.t)  ->
                    (`CeApp (_loc, ce, e) : 'clexp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                ("Ant", (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clexp\" n s\n",
              (Fgram.mk_action
                 (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clexp" n s : 'clexp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cexp",_) -> true | _ -> false)),
               ("Ant", (`A "cexp")), "`Ant s")],
           ("mk_anti _loc ~c:\"clexp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cexp" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'clexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.clexp\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.clexp : 'clexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
         ([`Snterm (Fgram.obj (vid : 'vid Fgram.t ));
          `Skeyword "[";
          `Snterm (Fgram.obj (comma_ctyp : 'comma_ctyp Fgram.t ));
          `Skeyword "]"],
           ("`ClApply (_loc, ci, t)\n",
             (Fgram.mk_action
                (fun _  (t : 'comma_ctyp)  _  (ci : 'vid)  (_loc : Locf.t) 
                   -> (`ClApply (_loc, ci, t) : 'clexp )))));
         ([`Snterm (Fgram.obj (vid : 'vid Fgram.t ))],
           ("(ci :>clexp)\n",
             (Fgram.mk_action
                (fun (ci : 'vid)  (_loc : Locf.t)  ->
                   ((ci :>clexp) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ")";
          `Snterm (Fgram.obj (class_structure : 'class_structure Fgram.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, p, cst)\n",
             (Fgram.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, p)\n",
             (Fgram.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPatEnd (_loc, p) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")";
          `Snterm (Fgram.obj (class_structure : 'class_structure Fgram.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
             (Fgram.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Fgram.obj (pat : 'pat Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
             (Fgram.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : Locf.t) 
                   ->
                   (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'clexp )))));
         ([`Skeyword "object";
          `Snterm (Fgram.obj (class_structure : 'class_structure Fgram.t ));
          `Skeyword "end"],
           ("`Obj (_loc, cst)\n",
             (Fgram.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : Locf.t)  ->
                   (`Obj (_loc, cst) : 'clexp )))));
         ([`Skeyword "object"; `Skeyword "end"],
           ("`ObjEnd _loc\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'clexp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Fgram.obj (cltyp : 'cltyp Fgram.t ));
          `Skeyword ")"],
           ("`Constraint (_loc, ce, ct)\n",
             (Fgram.mk_action
                (fun _  (ct : 'cltyp)  _  (ce : 'clexp)  _  (_loc : Locf.t) 
                   -> (`Constraint (_loc, ce, ct) : 'clexp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("ce\n",
             (Fgram.mk_action
                (fun _  (ce : 'clexp)  _  (_loc : Locf.t)  -> (ce : 'clexp )))))])]));
  Fgram.extend_single (class_description : 'class_description Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("`And (_loc, cd1, cd2)\n",
             (Fgram.mk_action
                (fun (cd2 : 'class_description)  _ 
                   (cd1 : 'class_description)  (_loc : Locf.t)  ->
                   (`And (_loc, cd1, cd2) : 'class_description )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'class_description )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'class_description )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword "[";
         `Snterm
           (Fgram.obj (comma_type_parameter : 'comma_type_parameter Fgram.t ));
         `Skeyword "]";
         `Skeyword ":";
         `Snterm (Fgram.obj (cltyp_plus : 'cltyp_plus Fgram.t ))],
          ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
            (Fgram.mk_action
               (fun (ct : 'cltyp_plus)  _  _  (x : 'comma_type_parameter)  _ 
                  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'class_description )))));
        ([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword ":";
         `Snterm (Fgram.obj (cltyp_plus : 'cltyp_plus Fgram.t ))],
          ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
            (Fgram.mk_action
               (fun (ct : 'cltyp_plus)  _  (i : 'a_lident) 
                  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDeclS (_loc, mv, (i :>ident), ct) : 'class_description )))))]));
  Fgram.extend_single (cltyp_declaration : 'cltyp_declaration Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("`And (_loc, cd1, cd2)\n",
             (Fgram.mk_action
                (fun (cd2 : 'cltyp_declaration)  _ 
                   (cd1 : 'cltyp_declaration)  (_loc : Locf.t)  ->
                   (`And (_loc, cd1, cd2) : 'cltyp_declaration )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword "[";
         `Snterm
           (Fgram.obj (comma_type_parameter : 'comma_type_parameter Fgram.t ));
         `Skeyword "]";
         `Skeyword "=";
         `Snterm (Fgram.obj (cltyp : 'cltyp Fgram.t ))],
          ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
            (Fgram.mk_action
               (fun (ct : 'cltyp)  _  _  (x : 'comma_type_parameter)  _ 
                  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'cltyp_declaration )))));
        ([`Snterm (Fgram.obj (opt_virtual : 'opt_virtual Fgram.t ));
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword "=";
         `Snterm (Fgram.obj (cltyp : 'cltyp Fgram.t ))],
          ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
            (Fgram.mk_action
               (fun (ct : 'cltyp)  _  (i : 'a_lident)  (mv : 'opt_virtual) 
                  (_loc : Locf.t)  ->
                  (`CtDeclS (_loc, mv, (i :>ident), ct) : 'cltyp_declaration )))))]));
  Fgram.extend_single (cltyp_quot : 'cltyp_quot Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (cltyp : 'cltyp Fgram.t ))],
           ("x\n",
             (Fgram.mk_action
                (fun (x : 'cltyp)  (_loc : Locf.t)  -> (x : 'cltyp_quot )))))]));
  Fgram.extend_single (cltyp_plus : 'cltyp_plus Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "]";
          `Skeyword "->";
          `Sself],
           ("`CtFun (_loc, t, ct)\n",
             (Fgram.mk_action
                (fun (ct : 'cltyp_plus)  _  _  (t : 'ctyp)  _ 
                   (_loc : Locf.t)  -> (`CtFun (_loc, t, ct) : 'cltyp_plus )))));
        ([`Snterm (Fgram.obj (cltyp : 'cltyp Fgram.t ))],
          ("ct\n",
            (Fgram.mk_action
               (fun (ct : 'cltyp)  (_loc : Locf.t)  -> (ct : 'cltyp_plus )))))]));
  Fgram.extend_single (cltyp : 'cltyp Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"cltyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"cltyp" n s : 'cltyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("ctyp",_) -> true | _ -> false)),
              ("Ant", (`A "ctyp")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("ctyp" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.cltyp\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x ->
                      (Ast_quotation.expand x Dyn_tag.cltyp : 'cltyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (vid : 'vid Fgram.t ));
         `Skeyword "[";
         `Snterm (Fgram.obj (comma_ctyp : 'comma_ctyp Fgram.t ));
         `Skeyword "]"],
          ("`ClApply (_loc, i, t)\n",
            (Fgram.mk_action
               (fun _  (t : 'comma_ctyp)  _  (i : 'vid)  (_loc : Locf.t)  ->
                  (`ClApply (_loc, i, t) : 'cltyp )))));
        ([`Snterm (Fgram.obj (vid : 'vid Fgram.t ))],
          ("(i :>cltyp)\n",
            (Fgram.mk_action
               (fun (i : 'vid)  (_loc : Locf.t)  -> ((i :>cltyp) : 'cltyp )))));
        ([`Skeyword "object";
         `Skeyword "(";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
         `Skeyword ")";
         `Snterm (Fgram.obj (class_signature : 'class_signature Fgram.t ));
         `Skeyword "end"],
          ("`ObjTy (_loc, t, csg)\n",
            (Fgram.mk_action
               (fun _  (csg : 'class_signature)  _  (t : 'ctyp)  _  _ 
                  (_loc : Locf.t)  -> (`ObjTy (_loc, t, csg) : 'cltyp )))));
        ([`Skeyword "object";
         `Snterm (Fgram.obj (class_signature : 'class_signature Fgram.t ));
         `Skeyword "end"],
          ("`Obj (_loc, csg)\n",
            (Fgram.mk_action
               (fun _  (csg : 'class_signature)  _  (_loc : Locf.t)  ->
                  (`Obj (_loc, csg) : 'cltyp )))));
        ([`Skeyword "object";
         `Skeyword "(";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
         `Skeyword ")"],
          ("`ObjTyEnd (_loc, t)\n",
            (Fgram.mk_action
               (fun _  (t : 'ctyp)  _  _  (_loc : Locf.t)  ->
                  (`ObjTyEnd (_loc, t) : 'cltyp )))));
        ([`Skeyword "object"; `Skeyword "end"],
          ("`ObjEnd _loc\n",
            (Fgram.mk_action
               (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'cltyp )))))]))
let apply_ctyp () =
  Fgram.extend_single (ctyp_quot : 'ctyp_quot Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "*";
          `Snterm (Fgram.obj (star_ctyp : 'star_ctyp Fgram.t ))],
           ("`Sta (_loc, x, y)\n",
             (Fgram.mk_action
                (fun (y : 'star_ctyp)  _  (x : 'ctyp)  (_loc : Locf.t)  ->
                   (`Sta (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("x\n",
            (Fgram.mk_action
               (fun (x : 'ctyp)  (_loc : Locf.t)  -> (x : 'ctyp_quot )))))]));
  Fgram.extend_single (unquoted_typevars : 'unquoted_typevars Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Sself],
           ("`App (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) 
                   (_loc : Locf.t)  ->
                   (`App (_loc, t1, t2) : 'unquoted_typevars )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x ->
                      (Ast_quotation.expand x Dyn_tag.ctyp : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("(i :>ctyp)\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'unquoted_typevars )))))]));
  Fgram.extend_single (type_parameter : 'type_parameter Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc n s : 'type_parameter )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc n s : 'type_parameter )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "'"; `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("`Quote (_loc, (`Normal _loc), i)\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'type_parameter )))));
        ([`Skeyword "+";
         `Skeyword "'";
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("`Quote (_loc, (`Positive _loc), i)\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Positive _loc), i) : 'type_parameter )))));
        ([`Skeyword "-";
         `Skeyword "'";
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("`Quote (_loc, (`Negative _loc), i)\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Negative _loc), i) : 'type_parameter )))));
        ([`Skeyword "+"; `Skeyword "_"],
          ("`QuoteAny (_loc, (`Positive _loc))\n",
            (Fgram.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (`QuoteAny (_loc, (`Positive _loc)) : 'type_parameter )))));
        ([`Skeyword "-"; `Skeyword "_"],
          ("`QuoteAny (_loc, (`Negative _loc))\n",
            (Fgram.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (`QuoteAny (_loc, (`Negative _loc)) : 'type_parameter )))));
        ([`Skeyword "_"],
          ("`Any _loc\n",
            (Fgram.mk_action
               (fun _  (_loc : Locf.t)  -> (`Any _loc : 'type_parameter )))))]));
  Fgram.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Fgram.obj (type_parameters : 'type_parameters Fgram.t ));
          `Skeyword ")";
          `Snterm (Fgram.obj (type_longident : 'type_longident Fgram.t ))],
           ("tpl (i :>ctyp)\n",
             (Fgram.mk_action
                (fun (i : 'type_longident)  _  (tpl : 'type_parameters)  _ 
                   (_loc : Locf.t)  ->
                   (tpl (i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Snterm (Fgram.obj (type_parameter : 'type_parameter Fgram.t ));
         `Snterm (Fgram.obj (type_longident : 'type_longident Fgram.t ))],
          ("`App (_loc, (i :>ctyp), (tpl :>ctyp))\n",
            (Fgram.mk_action
               (fun (i : 'type_longident)  (tpl : 'type_parameter) 
                  (_loc : Locf.t)  ->
                  (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters )))));
        ([`Snterm (Fgram.obj (type_longident : 'type_longident Fgram.t ))],
          ("(i :>ctyp)\n",
            (Fgram.mk_action
               (fun (i : 'type_longident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc n s ~c:\"ctyp\"\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc n s ~c:"ctyp" : 'type_longident_and_parameters )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (type_parameters : 'type_parameters Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (type_parameter : 'type_parameter Fgram.t ));
          `Sself],
           ("fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp)))\n",
             (Fgram.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : Locf.t)  ->
                   (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 
                   'type_parameters )))));
        ([`Snterm (Fgram.obj (type_parameter : 'type_parameter Fgram.t ))],
          ("fun acc  -> `App (_loc, acc, (t :>ctyp))\n",
            (Fgram.mk_action
               (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                  (fun acc  -> `App (_loc, acc, (t :>ctyp)) : 'type_parameters )))));
        ([],
          ("fun t  -> t\n",
            (Fgram.mk_action
               (fun (_loc : Locf.t)  -> (fun t  -> t : 'type_parameters )))))]));
  Fgram.extend_single (meth_list : 'meth_list Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (meth_decl : 'meth_decl Fgram.t ));
          `Skeyword ";";
          `Sself],
           ("let (ml,v) = rest in ((`Sem (_loc, m, ml)), v)\n",
             (Fgram.mk_action
                (fun (rest : 'meth_list)  _  (m : 'meth_decl) 
                   (_loc : Locf.t)  ->
                   (let (ml,v) = rest in ((`Sem (_loc, m, ml)), v) : 
                   'meth_list )))));
        ([`Snterm (Fgram.obj (meth_decl : 'meth_decl Fgram.t ));
         `Skeyword ";";
         `Snterm (Fgram.obj (opt_dot_dot : 'opt_dot_dot Fgram.t ))],
          ("(m, v)\n",
            (Fgram.mk_action
               (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl)  (_loc : Locf.t) 
                  -> ((m, v) : 'meth_list )))));
        ([`Snterm (Fgram.obj (meth_decl : 'meth_decl Fgram.t ));
         `Snterm (Fgram.obj (opt_dot_dot : 'opt_dot_dot Fgram.t ))],
          ("(m, v)\n",
            (Fgram.mk_action
               (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : Locf.t)  ->
                  ((m, v) : 'meth_list )))))]));
  Fgram.extend_single (meth_decl : 'meth_decl Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'meth_decl )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'meth_decl )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword ":";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyCol (_loc, lab, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, lab, t) : 'meth_decl )))))]));
  Fgram.extend_single (opt_meth_list : 'opt_meth_list Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (meth_list : 'meth_list Fgram.t ))],
           ("let (ml,v) = rest in `TyObj (_loc, ml, v)\n",
             (Fgram.mk_action
                (fun (rest : 'meth_list)  (_loc : Locf.t)  ->
                   (let (ml,v) = rest in `TyObj (_loc, ml, v) : 'opt_meth_list )))));
        ([`Snterm (Fgram.obj (opt_dot_dot : 'opt_dot_dot Fgram.t ))],
          ("`TyObjEnd (_loc, v)\n",
            (Fgram.mk_action
               (fun (v : 'opt_dot_dot)  (_loc : Locf.t)  ->
                  (`TyObjEnd (_loc, v) : 'opt_meth_list )))))]));
  Fgram.extend_single (row_field : 'row_field Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'row_field )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("vrn",_) -> true | _ -> false)),
              ("Ant", (`A "vrn")), "`Ant s")],
          ("`TyVrn (_loc, (mk_anti _loc ~c:\"ctyp\" n s))\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("vrn" as n),s) ->
                      (`TyVrn (_loc, (mk_anti _loc ~c:"ctyp" n s)) : 
                      'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("vrn",_) -> true | _ -> false)),
              ("Ant", (`A "vrn")), "`Ant s");
         `Skeyword "of";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyVrnOf (_loc, (mk_anti _loc ~c:\"ctyp\" n s), t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("vrn" as n),s) ->
                      (`TyVrnOf (_loc, (mk_anti _loc ~c:"ctyp" n s), t) : 
                      'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("`Bar (_loc, t1, t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'row_field)  _  (t1 : 'row_field)  (_loc : Locf.t) 
                  -> (`Bar (_loc, t1, t2) : 'row_field )))));
        ([`Skeyword "`"; `Snterm (Fgram.obj (astr : 'astr Fgram.t ))],
          ("`TyVrn (_loc, i)\n",
            (Fgram.mk_action
               (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrn (_loc, i) : 'row_field )))));
        ([`Skeyword "`";
         `Snterm (Fgram.obj (astr : 'astr Fgram.t ));
         `Skeyword "of";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyVrnOf (_loc, i, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrnOf (_loc, i, t) : 'row_field )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`Ctyp (_loc, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, t) : 'row_field )))))]));
  Fgram.extend_single (name_tags : 'name_tags Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'name_tags )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'name_tags )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Sself],
          ("`App (_loc, t1, t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : Locf.t)  ->
                  (`App (_loc, t1, t2) : 'name_tags )))));
        ([`Skeyword "`"; `Snterm (Fgram.obj (astr : 'astr Fgram.t ))],
          ("`TyVrn (_loc, i)\n",
            (Fgram.mk_action
               (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrn (_loc, i) : 'name_tags )))))]));
  Fgram.extend_single (type_declaration : 'type_declaration Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'type_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'type_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "and"; `Sself],
          ("`And (_loc, t1, t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) 
                  (_loc : Locf.t)  ->
                  (`And (_loc, t1, t2) : 'type_declaration )))));
        ([`Snterm
            (Fgram.obj
               (type_ident_and_parameters : 'type_ident_and_parameters
                                              Fgram.t ));
         `Skeyword "=";
         `Snterm (Fgram.obj (type_info : 'type_info Fgram.t ));
         `Slist0 (`Snterm (Fgram.obj (constrain : 'constrain Fgram.t )))],
          ("let (n,tpl) = rest in\n`TyDcl\n  (_loc, n, tpl, tk,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n",
            (Fgram.mk_action
               (fun (cl : 'constrain list)  (tk : 'type_info)  _ 
                  (rest : 'type_ident_and_parameters)  (_loc : Locf.t)  ->
                  (let (n,tpl) = rest in
                   `TyDcl
                     (_loc, n, tpl, tk,
                       (match cl with
                        | [] -> `None _loc
                        | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))));
        ([`Snterm
            (Fgram.obj
               (type_ident_and_parameters : 'type_ident_and_parameters
                                              Fgram.t ));
         `Slist0 (`Snterm (Fgram.obj (constrain : 'constrain Fgram.t )))],
          ("let (n,tpl) = rest in\n`TyAbstr\n  (_loc, n, tpl,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n",
            (Fgram.mk_action
               (fun (cl : 'constrain list) 
                  (rest : 'type_ident_and_parameters)  (_loc : Locf.t)  ->
                  (let (n,tpl) = rest in
                   `TyAbstr
                     (_loc, n, tpl,
                       (match cl with
                        | [] -> `None _loc
                        | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))))]));
  Fgram.extend_single (type_info : 'type_info Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (type_repr : 'type_repr Fgram.t ))],
           ("`TyRepr (_loc, (`Negative _loc), t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'type_repr)  (_loc : Locf.t)  ->
                   (`TyRepr (_loc, (`Negative _loc), t2) : 'type_info )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
         `Skeyword "=";
         `Snterm (Fgram.obj (type_repr : 'type_repr Fgram.t ))],
          ("`TyMan (_loc, t1, (`Negative _loc), t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'type_repr)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                  (`TyMan (_loc, t1, (`Negative _loc), t2) : 'type_info )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyEq (_loc, (`Negative _loc), t1)\n",
            (Fgram.mk_action
               (fun (t1 : 'ctyp)  (_loc : Locf.t)  ->
                  (`TyEq (_loc, (`Negative _loc), t1) : 'type_info )))));
        ([`Skeyword "private"; `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyEq (_loc, (`Positive _loc), t1)\n",
            (Fgram.mk_action
               (fun (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                  (`TyEq (_loc, (`Positive _loc), t1) : 'type_info )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
         `Skeyword "=";
         `Skeyword "private";
         `Snterm (Fgram.obj (type_repr : 'type_repr Fgram.t ))],
          ("`TyMan (_loc, t1, (`Positive _loc), t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'type_repr)  _  _  (t1 : 'ctyp)  (_loc : Locf.t) 
                  -> (`TyMan (_loc, t1, (`Positive _loc), t2) : 'type_info )))));
        ([`Skeyword "private";
         `Snterm (Fgram.obj (type_repr : 'type_repr Fgram.t ))],
          ("`TyRepr (_loc, (`Positive _loc), t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'type_repr)  _  (_loc : Locf.t)  ->
                  (`TyRepr (_loc, (`Positive _loc), t2) : 'type_info )))))]));
  Fgram.extend_single (type_repr : 'type_repr Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Snterm
            (Fgram.obj
               (constructor_declarations : 'constructor_declarations Fgram.t ))],
           ("`Sum (_loc, t)\n",
             (Fgram.mk_action
                (fun (t : 'constructor_declarations)  _  (_loc : Locf.t)  ->
                   (`Sum (_loc, t) : 'type_repr )))));
        ([`Skeyword "{";
         `Snterm
           (Fgram.obj
              (label_declaration_list : 'label_declaration_list Fgram.t ));
         `Skeyword "}"],
          ("`Record (_loc, t)\n",
            (Fgram.mk_action
               (fun _  (t : 'label_declaration_list)  _  (_loc : Locf.t)  ->
                  (`Record (_loc, t) : 'type_repr )))))]));
  Fgram.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Slist1sep
            ((`Snterm (Fgram.obj (type_parameter : 'type_parameter Fgram.t ))),
              (`Skeyword ","));
          `Skeyword ")";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("(i, (`Some (_loc, (com_of_list (tpl :>decl_params list)))))\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (tpl : 'type_parameter list)  _ 
                   (_loc : Locf.t)  ->
                   ((i,
                      (`Some (_loc, (com_of_list (tpl :>decl_params list))))) : 
                   'type_ident_and_parameters )))));
        ([`Snterm (Fgram.obj (type_parameter : 'type_parameter Fgram.t ));
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("(i, (`Some (_loc, (t :>decl_params))))\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  (t : 'type_parameter)  (_loc : Locf.t) 
                  ->
                  ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters )))));
        ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("(i, (`None _loc))\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i, (`None _loc)) : 'type_ident_and_parameters )))))]));
  Fgram.extend_single (constrain : 'constrain Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "constraint";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
          `Skeyword "=";
          `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
           ("`Eq (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   (`Eq (_loc, t1, t2) : 'constrain )))))]));
  Fgram.extend_single (typevars : 'typevars Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Sself],
           ("`App (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : Locf.t)  ->
                   (`App (_loc, t1, t2) : 'typevars )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x ->
                      (Ast_quotation.expand x Dyn_tag.ctyp : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "'"; `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("`Quote (_loc, (`Normal _loc), i)\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'typevars )))))]));
  Fgram.extend (ctyp : 'ctyp Fgram.t )
    (None,
      [((Some "alias"), (Some `LA),
         [([`Sself;
           `Skeyword "as";
           `Skeyword "'";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
            ("`Alias (_loc, t1, i)\n",
              (Fgram.mk_action
                 (fun (i : 'a_lident)  _  _  (t1 : 'ctyp)  (_loc : Locf.t) 
                    -> (`Alias (_loc, t1, i) : 'ctyp )))))]);
      ((Some "forall"), (Some `LA),
        [([`Skeyword "!";
          `Snterm (Fgram.obj (typevars : 'typevars Fgram.t ));
          `Skeyword ".";
          `Sself],
           ("`TyPol (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _  (_loc : Locf.t) 
                   -> (`TyPol (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "arrow"), (Some `RA),
        [([`Sself; `Skeyword "->"; `Sself],
           ("`Arrow (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`Arrow (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "label"), (Some `NA),
        [([`Skeyword "~";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
          `Skeyword ":";
          `Sself],
           ("`Label (_loc, i, t)\n",
             (Fgram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Label (_loc, i, t) : 'ctyp )))));
        ([`Stoken
            (((function | `Label (_,_) -> true | _ -> false)),
              ("Label", `Any), "`Label s");
         `Skeyword ":";
         `Sself],
          ("`Label (_loc, (`Lid (_loc, s)), t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Label (_,s) ->
                      (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Optlabel (_,_) -> true | _ -> false)),
              ("Optlabel", `Any), "`Optlabel s");
         `Sself],
          ("`OptLabl (_loc, (`Lid (_loc, s)), t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Optlabel (_,s) ->
                      (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "?";
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword ":";
         `Sself],
          ("`OptLabl (_loc, i, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`OptLabl (_loc, i, t) : 'ctyp )))))]);
      ((Some "apply"), (Some `LA),
        [([`Sself; `Sself],
           ("`App (_loc, t2, t1)\n",
             (Fgram.mk_action
                (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`App (_loc, t2, t1) : 'ctyp )))))]);
      ((Some "simple"), None,
        [([`Skeyword "'";
          `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Quote (_loc, (`Normal _loc), i) : 'ctyp )))));
        ([`Skeyword "_"],
          ("`Any _loc\n",
            (Fgram.mk_action
               (fun _  (_loc : Locf.t)  -> (`Any _loc : 'ctyp )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("par",_) -> true | _ -> false)),
              ("Ant", (`A "par")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("par" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("id",_) -> true | _ -> false)),
              ("Ant", (`A "id")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("id" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("id",_) -> true | _ -> false)),
              ("Ant", (`A "id")), "`Ant s");
         `Skeyword ".";
         `Sself],
          ("(try\n   let id = ident_of_ctyp t in\n   fun ()  -> (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), id) : ctyp )\n with | Invalid_argument s -> (fun ()  -> raise (Fstream.Error s))) ()\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("id" as n),s) ->
                      (((try
                           let id = ident_of_ctyp t in
                           fun ()  ->
                             (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), id) : 
                             ctyp )
                         with
                         | Invalid_argument s ->
                             (fun ()  -> raise (Fstream.Error s)))) () : 
                      'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x -> (Ast_quotation.expand x Dyn_tag.ctyp : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
         `Skeyword ".";
         `Sself],
          ("(try let id = ident_of_ctyp t in fun ()  -> `Dot (_loc, (i :>ident), id)\n with | Invalid_argument s -> (fun ()  -> raise (Fstream.Error s))) ()\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_uident)  (_loc : Locf.t)  ->
                  ((try
                      let id = ident_of_ctyp t in
                      fun ()  -> `Dot (_loc, (i :>ident), id)
                    with
                    | Invalid_argument s ->
                        (fun ()  -> raise (Fstream.Error s))) () : 'ctyp )))));
        ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
          ("(i :>ctyp)\n",
            (Fgram.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'ctyp )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword "*";
         `Snterm (Fgram.obj (star_ctyp : 'star_ctyp Fgram.t ));
         `Skeyword ")"],
          ("`Par (_loc, (`Sta (_loc, t, tl)))\n",
            (Fgram.mk_action
               (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _  (_loc : Locf.t) 
                  -> (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("t\n",
            (Fgram.mk_action
               (fun _  (t : 'ctyp)  _  (_loc : Locf.t)  -> (t : 'ctyp )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword ",";
         `Snterm (Fgram.obj (com_ctyp : 'com_ctyp Fgram.t ));
         `Skeyword ")";
         `Snterm (Fgram.obj (type_longident : 'type_longident Fgram.t ))],
          ("appl_of_list ((j :>ctyp) :: t :: (Ast_basic.list_of_com tl []))\n",
            (Fgram.mk_action
               (fun (j : 'type_longident)  _  (tl : 'com_ctyp)  _ 
                  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                  (appl_of_list ((j :>ctyp) :: t ::
                     (Ast_basic.list_of_com tl [])) : 'ctyp )))));
        ([`Skeyword "[";
         `Snterm (Fgram.obj (row_field : 'row_field Fgram.t ));
         `Skeyword "]"],
          ("`PolyEq (_loc, rfl)\n",
            (Fgram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolyEq (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[>";
         `Snterm (Fgram.obj (row_field : 'row_field Fgram.t ));
         `Skeyword "]"],
          ("`PolySup (_loc, rfl)\n",
            (Fgram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolySup (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Fgram.obj (row_field : 'row_field Fgram.t ));
         `Skeyword "]"],
          ("`PolyInf (_loc, rfl)\n",
            (Fgram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolyInf (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Fgram.obj (row_field : 'row_field Fgram.t ));
         `Skeyword ">";
         `Snterm (Fgram.obj (name_tags : 'name_tags Fgram.t ));
         `Skeyword "]"],
          ("`PolyInfSup (_loc, rfl, ntl)\n",
            (Fgram.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                  (_loc : Locf.t)  -> (`PolyInfSup (_loc, rfl, ntl) : 
                  'ctyp )))));
        ([`Skeyword "#";
         `Snterm (Fgram.obj (class_longident : 'class_longident Fgram.t ))],
          ("`ClassPath (_loc, i)\n",
            (Fgram.mk_action
               (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                  (`ClassPath (_loc, i) : 'ctyp )))));
        ([`Skeyword "<";
         `Snterm (Fgram.obj (opt_meth_list : 'opt_meth_list Fgram.t ));
         `Skeyword ">"],
          ("t\n",
            (Fgram.mk_action
               (fun _  (t : 'opt_meth_list)  _  (_loc : Locf.t)  ->
                  (t : 'ctyp )))));
        ([`Skeyword "(";
         `Skeyword "module";
         `Snterm (Fgram.obj (mtyp : 'mtyp Fgram.t ));
         `Skeyword ")"],
          ("`Package (_loc, p)\n",
            (Fgram.mk_action
               (fun _  (p : 'mtyp)  _  _  (_loc : Locf.t)  ->
                  (`Package (_loc, p) : 'ctyp )))))])]);
  Fgram.extend_single (comma_ctyp : 'comma_ctyp Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("`Com (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : Locf.t)  -> (`Com (_loc, t1, t2) : 'comma_ctyp )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp,\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp," n s : 'comma_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`Ctyp (_loc, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, t) : 'comma_ctyp )))))]));
  Fgram.extend_single (com_ctyp : 'com_ctyp Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'com_ctyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'com_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Skeyword ","; `Sself],
          ("`Com (_loc, t1, t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'com_ctyp)  _  (t1 : 'com_ctyp)  (_loc : Locf.t) 
                  -> (`Com (_loc, t1, t2) : 'com_ctyp )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("t\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'com_ctyp )))))]));
  Fgram.extend_single (star_ctyp : 'star_ctyp Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'star_ctyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'star_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "*"; `Sself],
          ("`Sta (_loc, t1, t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp)  (_loc : Locf.t) 
                  -> (`Sta (_loc, t1, t2) : 'star_ctyp )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("t\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'star_ctyp )))))]));
  Fgram.extend_single
    (constructor_declarations : 'constructor_declarations Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'constructor_declarations )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'constructor_declarations )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("`Bar (_loc, t1, t2)\n",
            (Fgram.mk_action
               (fun (t2 : 'constructor_declarations)  _ 
                  (t1 : 'constructor_declarations)  (_loc : Locf.t)  ->
                  (`Bar (_loc, t1, t2) : 'constructor_declarations )))));
        ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
         `Skeyword "of";
         `Snterm
           (Fgram.obj (constructor_arg_list : 'constructor_arg_list Fgram.t ))],
          ("`Of (_loc, s, t)\n",
            (Fgram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : Locf.t)  ->
                  (`Of (_loc, s, t) : 'constructor_declarations )))));
        ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
         `Skeyword ":";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyCol (_loc, s, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, s, t) : 'constructor_declarations )))));
        ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ))],
          ("(s :>or_ctyp)\n",
            (Fgram.mk_action
               (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                  ((s :>or_ctyp) : 'constructor_declarations )))))]));
  Fgram.extend_single
    (constructor_declaration : 'constructor_declaration Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'constructor_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'constructor_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ));
         `Skeyword "of";
         `Snterm
           (Fgram.obj (constructor_arg_list : 'constructor_arg_list Fgram.t ))],
          ("`Of (_loc, (s :>vid), t)\n",
            (Fgram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : Locf.t)  ->
                  (`Of (_loc, (s :>vid), t) : 'constructor_declaration )))));
        ([`Snterm (Fgram.obj (a_uident : 'a_uident Fgram.t ))],
          ("(s :>of_ctyp)\n",
            (Fgram.mk_action
               (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                  ((s :>of_ctyp) : 'constructor_declaration )))))]));
  Fgram.extend_single (constructor_arg_list : 'constructor_arg_list Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "*"; `Sself],
           ("`Sta (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'constructor_arg_list)  _ 
                   (t1 : 'constructor_arg_list)  (_loc : Locf.t)  ->
                   (`Sta (_loc, t1, t2) : 'constructor_arg_list )))));
        ([`Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("t\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (t : 'constructor_arg_list )))))]));
  Fgram.extend_single
    (label_declaration_list : 'label_declaration_list Fgram.t )
    (None,
      (None, None,
        [([`Snterm
             (Fgram.obj (label_declaration : 'label_declaration Fgram.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'label_declaration_list)  _ 
                   (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                   (`Sem (_loc, t1, t2) : 'label_declaration_list )))));
        ([`Snterm
            (Fgram.obj (label_declaration : 'label_declaration Fgram.t ));
         `Skeyword ";"],
          ("t1\n",
            (Fgram.mk_action
               (fun _  (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                  (t1 : 'label_declaration_list )))));
        ([`Snterm
            (Fgram.obj (label_declaration : 'label_declaration Fgram.t ))],
          ("t1\n",
            (Fgram.mk_action
               (fun (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                  (t1 : 'label_declaration_list )))))]));
  Fgram.extend_single (label_declaration : 'label_declaration Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               ("Ant", (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'label_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              ("Ant", (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'label_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword ":";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyCol (_loc, s, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, s, t) : 'label_declaration )))));
        ([`Skeyword "mutable";
         `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ));
         `Skeyword ":";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ))],
          ("`TyColMut (_loc, s, t)\n",
            (Fgram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`TyColMut (_loc, s, t) : 'label_declaration )))))]));
  Fgram.extend_single (comma_type_parameter : 'comma_type_parameter Fgram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("`Com (_loc, t1, t2)\n",
             (Fgram.mk_action
                (fun (t2 : 'comma_type_parameter)  _ 
                   (t1 : 'comma_type_parameter)  (_loc : Locf.t)  ->
                   (`Com (_loc, t1, t2) : 'comma_type_parameter )))));
        ([`Snterm (Fgram.obj (type_parameter : 'type_parameter Fgram.t ))],
          ("`Ctyp (_loc, (t :>ctyp))\n",
            (Fgram.mk_action
               (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter )))))]))
let fill_parsers =
  let applied = ref false in
  fun ()  ->
    if not applied.contents then (applied := true; apply (); apply_ctyp ())
let () = Ast_parsers.register_parser ("revise", fill_parsers)