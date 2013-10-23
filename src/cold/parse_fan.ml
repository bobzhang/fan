open FAst
open Ast_gen
open Fan_ops
open! Syntaxf
open FanUtil
open Gramlib
let pos_exps = Gramf.mk "pos_exps"
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
   Gramf.setup_parser sem_exp
     (let symb1 = Gramf.parse_origin_tokens exp in
      let symb (__strm : _ Streamf.t) =
        match Streamf.peek __strm with
        | Some (`Ant (("list" as n),s),_loc) ->
            (Streamf.junk __strm; mk_anti ~c:"exp;" _loc n s)
        | _ -> symb1 __strm in
      let rec kont al (__strm : _ Streamf.t) =
        match Streamf.peek __strm with
        | Some (`Key ({ txt = ";";_} : Tokenf.txt),_) ->
            (Streamf.junk __strm;
             (let a =
                try symb __strm
                with | Streamf.NotConsumed  -> raise (Streamf.Error "") in
              let s = __strm in
              let _loc = al <+> a in kont (`Sem (_loc, al, a) : FAst.exp ) s))
        | _ -> al in
      fun (__strm : _ Streamf.t)  -> let a = symb __strm in kont a __strm));
  (Gramf.extend_single (mexp_quot : 'mexp_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'mexp)  (_loc : Locf.t)  -> (x : 'mexp_quot )))))]));
   Gramf.extend (mbind0 : 'mbind0 Gramf.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Skeyword ":";
            `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
            `Skeyword ")";
            `Sself],
             ("`Functor (_loc, m, mt, mb)\n",
               (Gramf.mk_action
                  (fun (mb : 'mbind0)  _  (mt : 'mtyp)  _  (m : 'a_uident)  _
                      (_loc : Locf.t)  ->
                     (`Functor (_loc, m, mt, mb) : 'mbind0 )))));
          ([`Skeyword ":";
           `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Skeyword "=";
           `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`Constraint (_loc, me, mt)\n",
              (Gramf.mk_action
                 (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                    (`Constraint (_loc, me, mt) : 'mbind0 )))));
          ([`Skeyword "="; `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("me\n",
              (Gramf.mk_action
                 (fun (me : 'mexp)  _  (_loc : Locf.t)  -> (me : 'mbind0 )))))])]);
   Gramf.extend (mexp : 'mexp Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Skeyword ":";
            `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("`Functor (_loc, i, t, me)\n",
               (Gramf.mk_action
                  (fun (me : 'mexp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : Locf.t)  ->
                     (`Functor (_loc, i, t, me) : 'mexp )))));
          ([`Skeyword "struct";
           `Snterm (Gramf.obj (strus : 'strus Gramf.t ));
           `Skeyword "end"],
            ("`Struct (_loc, st)\n",
              (Gramf.mk_action
                 (fun _  (st : 'strus)  _  (_loc : Locf.t)  ->
                    (`Struct (_loc, st) : 'mexp )))));
          ([`Skeyword "struct"; `Skeyword "end"],
            ("`StructEnd _loc\n",
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  -> (`StructEnd _loc : 'mexp )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("`App (_loc, me1, me2)\n",
              (Gramf.mk_action
                 (fun (me2 : 'mexp)  (me1 : 'mexp)  (_loc : Locf.t)  ->
                    (`App (_loc, me1, me2) : 'mexp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti ~c:\"mexp\" _loc n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti ~c:"mexp" _loc n s : 'mexp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("mexp",_) -> true | _ -> false)),
               (3257031, (`A "mexp")), "`Ant s")],
           ("mk_anti ~c:\"mexp\" _loc n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mexp" as n),s) ->
                       (mk_anti ~c:"mexp" _loc n s : 'mexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mexp\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mexp : 'mexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm
             (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
           ("(i :>mexp)\n",
             (Gramf.mk_action
                (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                   ((i :>mexp) : 'mexp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword ")"],
           ("`Constraint (_loc, me, mt)\n",
             (Gramf.mk_action
                (fun _  (mt : 'mtyp)  _  (me : 'mexp)  _  (_loc : Locf.t)  ->
                   (`Constraint (_loc, me, mt) : 'mexp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("me\n",
             (Gramf.mk_action
                (fun _  (me : 'mexp)  _  (_loc : Locf.t)  -> (me : 'mexp )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`PackageModule (_loc, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                   (`PackageModule (_loc, e) : 'mexp )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword ")"],
           ("`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p)))))\n",
             (Gramf.mk_action
                (fun _  (p : 'mtyp)  _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                   (`PackageModule
                      (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                   'mexp )))))])]));
  (Gramf.extend_single (mbind_quot : 'mbind_quot Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'mbind_quot)  _  (b1 : 'mbind_quot) 
                    (_loc : Locf.t)  -> (`And (_loc, b1, b2) : 'mbind_quot )))));
         ([`Stoken
             (((function | `Ant ("mbind",_) -> true | _ -> false)),
               (3257031, (`A "mbind")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`Constraint (_loc, m, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t)  ->
                   (`Constraint (_loc, m, mt) : 'mbind_quot )))));
         ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
           ("`ModuleBind (_loc, m, mt, me)\n",
             (Gramf.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : Locf.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind_quot )))))]));
   Gramf.extend_single (mbind : 'mbind Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'mbind)  _  (b1 : 'mbind)  (_loc : Locf.t)  ->
                    (`And (_loc, b1, b2) : 'mbind )))));
         ([`Stoken
             (((function | `Ant ("mbind",_) -> true | _ -> false)),
               (3257031, (`A "mbind")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mbind\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mbind : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
           ("`ModuleBind (_loc, m, mt, me)\n",
             (Gramf.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : Locf.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind )))))]));
   Gramf.extend_single
     (module_rec_declaration : 'module_rec_declaration Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, m1, m2)\n",
              (Gramf.mk_action
                 (fun (m2 : 'module_rec_declaration)  _ 
                    (m1 : 'module_rec_declaration)  (_loc : Locf.t)  ->
                    (`And (_loc, m1, m2) : 'module_rec_declaration )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("mbind",_) -> true | _ -> false)),
               (3257031, (`A "mbind")), "`Ant s")],
           ("mk_anti _loc ~c:\"mbind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mbind\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mbind : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`Constraint (_loc, m, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t)  ->
                   (`Constraint (_loc, m, mt) : 'module_rec_declaration )))))])));
  (Gramf.extend_single (constr_quot : 'constr_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (constr : 'constr Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'constr)  (_loc : Locf.t)  -> (x : 'constr_quot )))))]));
   Gramf.extend_single (constr : 'constr Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, wc1, wc2)\n",
              (Gramf.mk_action
                 (fun (wc2 : 'constr)  _  (wc1 : 'constr)  (_loc : Locf.t) 
                    -> (`And (_loc, wc1, wc2) : 'constr )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"constr\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"constr" n s : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("constr",_) -> true | _ -> false)),
               (3257031, (`A "constr")), "`Ant s")],
           ("mk_anti _loc ~c:\"constr\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("constr" as n),s) ->
                       (mk_anti _loc ~c:"constr" n s : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.constr\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.constr : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "type";
          `Snterm
            (Gramf.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TypeEq (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : Locf.t)  -> (`TypeEq (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "type";
          `Snterm
            (Gramf.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gramf.t ));
          `Skeyword "=";
          `Skeyword "private";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TypeEqPriv (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  _ 
                   (t1 : 'type_longident_and_parameters)  _  (_loc : Locf.t) 
                   -> (`TypeEqPriv (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "type";
          `Snterm
            (Gramf.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gramf.t ));
          `Skeyword ":=";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TypeSubst (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : Locf.t)  ->
                   (`TypeSubst (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "module";
          `Snterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Skeyword "=";
          `Snterm
            (Gramf.obj
               (module_longident_with_app : 'module_longident_with_app
                                              Gramf.t ))],
           ("`ModuleEq (_loc, (i1 : vid  :>ident), i2)\n",
             (Gramf.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`ModuleEq (_loc, (i1 : vid  :>ident), i2) : 'constr )))));
         ([`Skeyword "module";
          `Snterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Skeyword ":=";
          `Snterm
            (Gramf.obj
               (module_longident_with_app : 'module_longident_with_app
                                              Gramf.t ))],
           ("`ModuleSubst (_loc, (i1 : vid  :>ident), i2)\n",
             (Gramf.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`ModuleSubst (_loc, (i1 : vid  :>ident), i2) : 'constr )))))])));
  (Gramf.extend_single (sigis : 'sigis Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc n ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc n ~c:"sigi" s : 'sigis )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               (3257031, (`A "sigi")), "`Ant s")],
           ("mk_anti _loc n ~c:\"sigi\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (mk_anti _loc n ~c:"sigi" s : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               (3257031, (`A "sigi")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               (3257031, (`A "sigi")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t ));
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, sg, s)\n",
             (Gramf.mk_action
                (fun (s : 'sigis)  _  (sg : 'sigi)  (_loc : Locf.t)  ->
                   (`Sem (_loc, sg, s) : 'sigis )))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Skeyword ";;"],
           ("sg\n",
             (Gramf.mk_action
                (fun _  (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis )))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Sself],
           ("`Sem (_loc, sg, s)\n",
             (Gramf.mk_action
                (fun (s : 'sigis)  (sg : 'sigi)  (_loc : Locf.t)  ->
                   (`Sem (_loc, sg, s) : 'sigis )))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t ))],
           ("sg\n",
             (Gramf.mk_action
                (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis )))))]));
   Gramf.extend (mtyp : 'mtyp Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Skeyword ":";
            `Sself;
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("`Functor (_loc, i, t, mt)\n",
               (Gramf.mk_action
                  (fun (mt : 'mtyp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : Locf.t)  ->
                     (`Functor (_loc, i, t, mt) : 'mtyp )))))]);
       ((Some "with"), None,
         [([`Sself;
           `Skeyword "with";
           `Snterm (Gramf.obj (constr : 'constr Gramf.t ))],
            ("`With (_loc, mt, wc)\n",
              (Gramf.mk_action
                 (fun (wc : 'constr)  _  (mt : 'mtyp)  (_loc : Locf.t)  ->
                    (`With (_loc, mt, wc) : 'mtyp )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("match (mt1, mt2) with\n| ((#ident as i1),(#ident as i2)) -> apply i1 i2\n| _ -> raise Streamf.NotConsumed\n",
              (Gramf.mk_action
                 (fun (mt2 : 'mtyp)  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                    (match (mt1, mt2) with
                     | ((#ident as i1),(#ident as i2)) -> apply i1 i2
                     | _ -> raise Streamf.NotConsumed : 'mtyp )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("let acc0 mt1 mt2 =\n  match (mt1, mt2) with\n  | ((#ident as i1),(#ident as i2)) -> dot i1 i2\n  | _ -> raise Streamf.NotConsumed in\nacc0 mt1 mt2\n",
              (Gramf.mk_action
                 (fun (mt2 : 'mtyp)  _  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                    (let acc0 mt1 mt2 =
                       match (mt1, mt2) with
                       | ((#ident as i1),(#ident as i2)) -> dot i1 i2
                       | _ -> raise Streamf.NotConsumed in
                     acc0 mt1 mt2 : 'mtyp )))))]);
       ((Some "sig"), None,
         [([`Skeyword "sig";
           `Snterm (Gramf.obj (sigis : 'sigis Gramf.t ));
           `Skeyword "end"],
            ("`Sig (_loc, sg)\n",
              (Gramf.mk_action
                 (fun _  (sg : 'sigis)  _  (_loc : Locf.t)  ->
                    (`Sig (_loc, sg) : 'mtyp )))));
         ([`Skeyword "sig"; `Skeyword "end"],
           ("`SigEnd _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`SigEnd _loc : 'mtyp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"mtyp\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"mtyp" n s : 'mtyp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("mtyp",_) -> true | _ -> false)),
               (3257031, (`A "mtyp")), "`Ant s")],
           ("mk_anti _loc ~c:\"mtyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mtyp" as n),s) ->
                       (mk_anti _loc ~c:"mtyp" n s : 'mtyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.mtyp\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.mtyp : 'mtyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm
             (Gramf.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gramf.t ))],
           ("(i : ident  :>mtyp)\n",
             (Gramf.mk_action
                (fun (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                   ((i : ident  :>mtyp) : 'mtyp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("mt\n",
             (Gramf.mk_action
                (fun _  (mt : 'mtyp)  _  (_loc : Locf.t)  -> (mt : 'mtyp )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Skeyword "of";
          `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
           ("`ModuleTypeOf (_loc, me)\n",
             (Gramf.mk_action
                (fun (me : 'mexp)  _  _  _  (_loc : Locf.t)  ->
                   (`ModuleTypeOf (_loc, me) : 'mtyp )))))])]);
   Gramf.extend_single (module_declaration : 'module_declaration Gramf.t )
     (None,
       (None, None,
         [([`Skeyword ":"; `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("mt\n",
              (Gramf.mk_action
                 (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                    (mt : 'module_declaration )))));
         ([`Skeyword "(";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword ")";
          `Sself],
           ("`Functor (_loc, i, t, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'module_declaration)  _  (t : 'mtyp)  _ 
                   (i : 'a_uident)  _  (_loc : Locf.t)  ->
                   (`Functor (_loc, i, t, mt) : 'module_declaration )))))]));
   Gramf.extend_single (mtyp_quot : 'mtyp_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'mtyp)  (_loc : Locf.t)  -> (x : 'mtyp_quot )))))])));
  (Gramf.extend_single (sigi_quot : 'sigi_quot Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`DirectiveSimple (_loc, s)\n",
              (Gramf.mk_action
                 (fun (s : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`DirectiveSimple (_loc, s) : 'sigi_quot )))));
         ([`Skeyword "#";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Directive (_loc, s, dp)\n",
             (Gramf.mk_action
                (fun (dp : 'exp)  (s : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Directive (_loc, s, dp) : 'sigi_quot )))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, sg1, sg2)\n",
             (Gramf.mk_action
                (fun (sg2 : 'sigi_quot)  _  (sg1 : 'sigi)  (_loc : Locf.t) 
                   -> (`Sem (_loc, sg1, sg2) : 'sigi_quot )))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t ))],
           ("sg\n",
             (Gramf.mk_action
                (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigi_quot )))))]));
   Gramf.extend_single (sigi : 'sigi Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"sigi\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"sigi" n s : 'sigi )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("sigi",_) -> true | _ -> false)),
               (3257031, (`A "sigi")), "`Ant s")],
           ("mk_anti _loc ~c:\"sigi\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("sigi" as n),s) ->
                       (mk_anti _loc ~c:"sigi" n s : 'sigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.sigi\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.sigi : 'sigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "exception";
          `Snterm
            (Gramf.obj
               (constructor_declaration : 'constructor_declaration Gramf.t ))],
           ("(`Exception (_loc, t) : FAst.sigi )\n",
             (Gramf.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)  ->
                   ((`Exception (_loc, t) : FAst.sigi ) : 'sigi )))));
         ([`Skeyword "external";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
           ("`External (_loc, i, t, sl)\n",
             (Gramf.mk_action
                (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident) 
                   _  (_loc : Locf.t)  ->
                   (`External (_loc, i, t, sl) : 'sigi )))));
         ([`Skeyword "include"; `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`Include (_loc, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                   (`Include (_loc, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Snterm
            (Gramf.obj (module_declaration : 'module_declaration Gramf.t ))],
           ("`Module (_loc, i, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'module_declaration)  (i : 'a_uident)  _ 
                   (_loc : Locf.t)  -> (`Module (_loc, i, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "rec";
          `Snterm
            (Gramf.obj
               (module_rec_declaration : 'module_rec_declaration Gramf.t ))],
           ("`RecModule (_loc, mb)\n",
             (Gramf.mk_action
                (fun (mb : 'module_rec_declaration)  _  _  (_loc : Locf.t) 
                   -> (`RecModule (_loc, mb) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`ModuleType (_loc, i, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : Locf.t) 
                   -> (`ModuleType (_loc, i, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
           ("`ModuleTypeEnd (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleTypeEnd (_loc, i) : 'sigi )))));
         ([`Skeyword "open";
          `Snterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
           ("`Open (_loc, (`Negative _loc), (i : vid  :>ident))\n",
             (Gramf.mk_action
                (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`Open (_loc, (`Negative _loc), (i : vid  :>ident)) : 
                   'sigi )))));
         ([`Skeyword "open";
          `Skeyword "!";
          `Snterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
           ("`Open (_loc, (`Positive _loc), (i : vid  :>ident))\n",
             (Gramf.mk_action
                (fun (i : 'module_longident)  _  _  (_loc : Locf.t)  ->
                   (`Open (_loc, (`Positive _loc), (i : vid  :>ident)) : 
                   'sigi )))));
         ([`Skeyword "type";
          `Snterm (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
           ("`Type (_loc, t)\n",
             (Gramf.mk_action
                (fun (t : 'type_declaration)  _  (_loc : Locf.t)  ->
                   (`Type (_loc, t) : 'sigi )))));
         ([`Skeyword "val";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`Val (_loc, i, t)\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Val (_loc, i, t) : 'sigi )))));
         ([`Skeyword "class";
          `Snterm
            (Gramf.obj (class_description : 'class_description Gramf.t ))],
           ("`Class (_loc, cd)\n",
             (Gramf.mk_action
                (fun (cd : 'class_description)  _  (_loc : Locf.t)  ->
                   (`Class (_loc, cd) : 'sigi )))));
         ([`Skeyword "class";
          `Skeyword "type";
          `Snterm
            (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
           ("`ClassType (_loc, ctd)\n",
             (Gramf.mk_action
                (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t)  ->
                   (`ClassType (_loc, ctd) : 'sigi )))))]));
   Gramf.extend_single (interf : 'interf Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t ));
           `Skeyword ";;";
           `Sself],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Gramf.mk_action
                 (fun (rest : 'interf)  _  (si : 'sigi)  (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'interf )))));
         ([`Snterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Sself],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Gramf.mk_action
                (fun (rest : 'interf)  (si : 'sigi)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'interf )))));
         ([`Stoken
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
           ("([], None)\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (([], None) : 'interf )))))])));
  (let grammar_entry_create x = Gramf.mk x in
   let name_space: 'name_space Gramf.t = grammar_entry_create "name_space"
   and fun_def_pat: 'fun_def_pat Gramf.t = grammar_entry_create "fun_def_pat" in
   Gramf.extend_single (exp_quot : 'exp_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Skeyword ",";
           `Snterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ))],
            ("`Com (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`Com (_loc, e1, e2) : 'exp_quot )))));
         ([`Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ";";
          `Snterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ))],
           ("`Sem (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun (e2 : 'sem_exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`Sem (_loc, e1, e2) : 'exp_quot )))));
         ([`Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'exp_quot )))))]));
   Gramf.extend_single (cvalue_bind : 'cvalue_bind Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'cvalue_bind )))));
         ([`Skeyword ":";
          `Skeyword "type";
          `Snterm
            (Gramf.obj (unquoted_typevars : 'unquoted_typevars Gramf.t ));
          `Skeyword ".";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("let u: FAst.ctyp = `TyPol (_loc, t1, t2) in\n(`Constraint (_loc, e, u) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _ 
                   (t1 : 'unquoted_typevars)  _  _  (_loc : Locf.t)  ->
                   (let u: FAst.ctyp = `TyPol (_loc, t1, t2) in
                    (`Constraint (_loc, e, u) : FAst.exp ) : 'cvalue_bind )))));
         ([`Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(`Constraint (_loc, e, t) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, e, t) : FAst.exp ) : 'cvalue_bind )))));
         ([`Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ":>";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("match t with\n| (`TyPol (_loc,_,_) : FAst.ctyp) ->\n    raise (Streamf.Error \"unexpected polytype here\")\n| _ -> (`Coercion (_loc, e, t, t2) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _ 
                   (_loc : Locf.t)  ->
                   (match t with
                    | (`TyPol (_loc,_,_) : FAst.ctyp) ->
                        raise (Streamf.Error "unexpected polytype here")
                    | _ -> (`Coercion (_loc, e, t, t2) : FAst.exp ) : 
                   'cvalue_bind )))));
         ([`Skeyword ":>";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Subtype (_loc, e, t)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                   (`Subtype (_loc, e, t) : 'cvalue_bind )))))]));
   Gramf.extend (fun_bind : 'fun_bind Gramf.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Skeyword ")";
            `Sself],
             ("`LocalTypeFun (_loc, i, e)\n",
               (Gramf.mk_action
                  (fun (e : 'fun_bind)  _  (i : 'a_lident)  _  _ 
                     (_loc : Locf.t)  ->
                     (`LocalTypeFun (_loc, i, e) : 'fun_bind )))));
          ([`Snterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Sself],
            ("`Fun (_loc, (`Case (_loc, p, e)))\n",
              (Gramf.mk_action
                 (fun (e : 'fun_bind)  (p : 'ipat)  (_loc : Locf.t)  ->
                    (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_bind )))));
          ([`Snterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))],
            ("bi\n",
              (Gramf.mk_action
                 (fun (bi : 'cvalue_bind)  (_loc : Locf.t)  ->
                    (bi : 'fun_bind )))))])]);
   Gramf.extend_single (lang : 'lang Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
            ("let old = Ast_quotation.default.contents in\nmatch Ast_quotation.resolve_name ls with\n| Some x -> (Ast_quotation.default := (Some x); old)\n| None  ->\n    Locf.failf _loc \"DDSL `%s' can not be resolved\"\n      (Tokenf.string_of_name ls)\n",
              (Gramf.mk_action
                 (fun (ls : 'dot_lstrings)  (_loc : Locf.t)  ->
                    (let old = Ast_quotation.default.contents in
                     match Ast_quotation.resolve_name ls with
                     | Some x -> (Ast_quotation.default := (Some x); old)
                     | None  ->
                         Locf.failf _loc "DDSL `%s' can not be resolved"
                           (Tokenf.string_of_name ls) : 'lang )))))]));
   Gramf.extend_single (pos_exps : 'pos_exps Gramf.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((`Snterm (Gramf.obj (name_space : 'name_space Gramf.t ))),
                (`Skeyword ";"))],
            ("let old = Ast_quotation.map.contents in\nAst_quotation.map := (Mapf.String.add_list xys old); old\n",
              (Gramf.mk_action
                 (fun (xys : 'name_space list)  (_loc : Locf.t)  ->
                    (let old = Ast_quotation.map.contents in
                     Ast_quotation.map := (Mapf.String.add_list xys old); old : 
                    'pos_exps )))))]));
   Gramf.extend_single (name_space : 'name_space Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid x");
           `Skeyword ":";
           `Snterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
            ("(x,\n  (match Ast_quotation.resolve_name y with\n   | None  ->\n       Locf.failf _loc \"DDSL `%s' can not be resolved\"\n         (Tokenf.string_of_name y)\n   | Some x -> x))\n",
              (Gramf.mk_action
                 (fun (y : 'dot_lstrings)  _  (__fan_0 : Tokenf.t) 
                    (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid ({ txt = x;_} : Tokenf.txt) ->
                        ((x,
                           ((match Ast_quotation.resolve_name y with
                             | None  ->
                                 Locf.failf _loc
                                   "DDSL `%s' can not be resolved"
                                   (Tokenf.string_of_name y)
                             | Some x -> x))) : 'name_space )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid x")],
           ("(x,\n  (match Ast_quotation.resolve_name ((`Sub []), x) with\n   | None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" x\n   | Some x -> x))\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
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
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (fun_def_pat : 'fun_def_pat Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "(";
           `Skeyword "type";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Skeyword ")"],
            ("fun e  -> `LocalTypeFun (_loc, i, e)\n",
              (Gramf.mk_action
                 (fun _  (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                    (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat )))));
         ([`Snterm (Gramf.obj (ipat : 'ipat Gramf.t ))],
           ("fun e  -> `Fun (_loc, (`Case (_loc, p, e)))\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  (_loc : Locf.t)  ->
                   (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 'fun_def_pat )))));
         ([`Snterm (Gramf.obj (ipat : 'ipat Gramf.t ));
          `Skeyword "when";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e)))\n",
             (Gramf.mk_action
                (fun (w : 'exp)  _  (p : 'ipat)  (_loc : Locf.t)  ->
                   (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 
                   'fun_def_pat )))))]));
   Gramf.extend (fun_def : 'fun_def Gramf.t )
     (None,
       [(None, (Some `RA),
          [([`Snterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t ));
            `Skeyword "->";
            `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("f e\n",
               (Gramf.mk_action
                  (fun (e : 'exp)  _  (f : 'fun_def_pat)  (_loc : Locf.t)  ->
                     (f e : 'fun_def )))));
          ([`Snterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t ));
           `Sself],
            ("f e\n",
              (Gramf.mk_action
                 (fun (e : 'fun_def)  (f : 'fun_def_pat)  (_loc : Locf.t)  ->
                    (f e : 'fun_def )))))])]);
   Gramf.extend (exp : 'exp Gramf.t )
     (None,
       [((Some "top"), (Some `RA),
          [([`Skeyword "let";
            `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Skeyword "in";
            `Sself],
             ("`LetIn (_loc, r, bi, x)\n",
               (Gramf.mk_action
                  (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                     (_loc : Locf.t)  -> (`LetIn (_loc, r, bi, x) : 'exp )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Snterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
           `Skeyword "in";
           `Sself],
            ("`LetModule (_loc, m, mb, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : Locf.t)  -> (`LetModule (_loc, m, mb, e) : 
                    'exp )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm
             (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Skeyword "in";
           `Sself],
            ("`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e) : 
                    'exp )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Skeyword "!";
           `Snterm
             (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Skeyword "in";
           `Sself],
            ("`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _  _ 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e) : 
                    'exp )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Skeyword "in";
           `Sself;
           `Skeyword "with";
           `Snterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`LetTryInWith (_loc, r, bi, x, a)\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                    (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                    (`LetTryInWith (_loc, r, bi, x, a) : 'exp )))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Snterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`Match (_loc, e, a)\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Match (_loc, e, a) : 'exp )))));
          ([`Skeyword "try";
           `Sself;
           `Skeyword "with";
           `Snterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`Try (_loc, e, a)\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Try (_loc, e, a) : 'exp )))));
          ([`Skeyword "if";
           `Sself;
           `Skeyword "then";
           `Sself;
           `Skeyword "else";
           `Sself],
            ("`IfThenElse (_loc, e1, e2, e3)\n",
              (Gramf.mk_action
                 (fun (e3 : 'exp)  _  (e2 : 'exp)  _  (e1 : 'exp)  _ 
                    (_loc : Locf.t)  ->
                    (`IfThenElse (_loc, e1, e2, e3) : 'exp )))));
          ([`Skeyword "if"; `Sself; `Skeyword "then"; `Sself],
            ("`IfThen (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : Locf.t)  ->
                    (`IfThen (_loc, e1, e2) : 'exp )))));
          ([`Skeyword "do";
           `Snterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Skeyword "done"],
            ("`Seq (_loc, seq)\n",
              (Gramf.mk_action
                 (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                    (`Seq (_loc, seq) : 'exp )))));
          ([`Skeyword "with";
           `Snterm (Gramf.obj (lang : 'lang Gramf.t ));
           `Sself],
            ("Ast_quotation.default := old; x\n",
              (Gramf.mk_action
                 (fun (x : 'exp)  (old : 'lang)  _  (_loc : Locf.t)  ->
                    (Ast_quotation.default := old; x : 'exp )))));
          ([`Skeyword "with";
           `Skeyword "{";
           `Snterm (Gramf.obj (pos_exps : 'pos_exps Gramf.t ));
           `Skeyword "}";
           `Sself],
            ("Ast_quotation.map := old; x\n",
              (Gramf.mk_action
                 (fun (x : 'exp)  _  (old : 'pos_exps)  _  _  (_loc : Locf.t)
                     -> (Ast_quotation.map := old; x : 'exp )))));
          ([`Skeyword "for";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Skeyword "=";
           `Sself;
           `Snterm (Gramf.obj (flag : 'flag Gramf.t ));
           `Sself;
           `Skeyword "do";
           `Snterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Skeyword "done"],
            ("`For (_loc, i, e1, e2, df, seq)\n",
              (Gramf.mk_action
                 (fun _  (seq : 'sequence)  _  (e2 : 'exp)  (df : 'flag) 
                    (e1 : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`For (_loc, i, e1, e2, df, seq) : 'exp )))));
          ([`Skeyword "while";
           `Sself;
           `Skeyword "do";
           `Snterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Skeyword "done"],
            ("`While (_loc, e, seq)\n",
              (Gramf.mk_action
                 (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : Locf.t)
                     -> (`While (_loc, e, seq) : 'exp )))))]);
       ((Some ":="), (Some `NA),
         [([`Sself; `Skeyword ":="; `Sself],
            ("(`Assign (_loc, (`Field (_loc, e1, (`Lid (_loc, \"contents\")))), e2) : \nFAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    ((`Assign
                        (_loc,
                          (`Field (_loc, e1, (`Lid (_loc, "contents")))), e2) : 
                    FAst.exp ) : 'exp )))));
         ([`Sself; `Skeyword "<-"; `Sself],
           ("match Fan_ops.bigarray_set _loc e1 e2 with\n| Some e -> e\n| None  -> `Assign (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (match Fan_ops.bigarray_set _loc e1 e2 with
                    | Some e -> e
                    | None  -> `Assign (_loc, e1, e2) : 'exp )))))]);
       ((Some "||"), (Some `RA),
         [([`Sself; `Skeyword "or"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key ({ txt = op;_} : Tokenf.txt) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "||"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))))]);
       ((Some "&&"), (Some `RA),
         [([`Sself; `Skeyword "&"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key ({ txt = op;_} : Tokenf.txt) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "&&"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))))]);
       ((Some "<"), (Some `LA),
         [([`Sself;
           `Snterm (Gramf.obj (infixop2 : 'infixop2 Gramf.t ));
           `Sself],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop2)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "^"), (Some `RA),
         [([`Sself;
           `Snterm (Gramf.obj (infixop3 : 'infixop3 Gramf.t ));
           `Sself],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop3)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "::"), (Some `RA),
         [([`Sself; `Skeyword "::"; `Sself],
            ("(`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, (`Uid (_loc, "::")), e1)), e2) : 
                    FAst.exp ) : 'exp )))))]);
       ((Some "+"), (Some `LA),
         [([`Sself;
           `Snterm (Gramf.obj (infixop4 : 'infixop4 Gramf.t ));
           `Sself],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop4)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "*"), (Some `LA),
         [([`Sself; `Skeyword "land"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key ({ txt = op;_} : Tokenf.txt) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lor"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lxor"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "mod"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Sself;
          `Snterm (Gramf.obj (infixop5 : 'infixop5 Gramf.t ));
          `Sself],
           ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (op : 'infixop5)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                   'exp )))))]);
       ((Some "**"), (Some `RA),
         [([`Sself; `Skeyword "asr"; `Sself],
            ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Key ({ txt = op;_} : Tokenf.txt) ->
                        (Ast_gen.appl_of_list
                           [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 
                        'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lsl"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Sself; `Skeyword "lsr"; `Sself],
           ("Ast_gen.appl_of_list [(`Lid (_loc, op) : FAst.exp ); e1; e2]\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (__fan_1 : Tokenf.t)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Key ({ txt = op;_} : Tokenf.txt) ->
                       (Ast_gen.appl_of_list
                          [(`Lid (_loc, op) : FAst.exp ); e1; e2] : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Sself;
          `Snterm (Gramf.obj (infixop6 : 'infixop6 Gramf.t ));
          `Sself],
           ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (op : 'infixop6)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                   'exp )))))]);
       ((Some "obj"), (Some `RA),
         [([`Skeyword "fun";
           `Skeyword "|";
           `Slist1sep
             ((`Snterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
               (`Skeyword "|"))],
            ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
              (Gramf.mk_action
                 (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                    (let cases = bar_of_list a in `Fun (_loc, cases) : 
                    'exp )))));
         ([`Skeyword "function";
          `Skeyword "|";
          `Slist1sep
            ((`Snterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
              (`Skeyword "|"))],
           ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
             (Gramf.mk_action
                (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                   (let cases = bar_of_list a in `Fun (_loc, cases) : 
                   'exp )))));
         ([`Skeyword "fun";
          `Snterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Skeyword "function";
          `Snterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ")";
          `Snterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, p, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, p)\n",
             (Gramf.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPatEnd (_loc, p) : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")";
          `Snterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
             (Gramf.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : Locf.t) 
                   -> (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                   'exp )))));
         ([`Skeyword "object";
          `Snterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Skeyword "end"],
           ("`Obj (_loc, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : Locf.t)  ->
                   (`Obj (_loc, cst) : 'exp )))));
         ([`Skeyword "object"; `Skeyword "end"],
           ("`ObjEnd _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'exp )))))]);
       ((Some "unary minus"), (Some `NA),
         [([`Skeyword "-"; `Sself],
            ("Fan_ops.mkumin _loc \"-\" e\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                    (Fan_ops.mkumin _loc "-" e : 'exp )))));
         ([`Skeyword "-."; `Sself],
           ("Fan_ops.mkumin _loc \"-.\" e\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (Fan_ops.mkumin _loc "-." e : 'exp )))))]);
       ((Some "apply"), (Some `LA),
         [([`Sself; `Sself],
            ("`App (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`App (_loc, e1, e2) : 'exp )))));
         ([`Skeyword "assert"; `Sself],
           ("`Assert (_loc, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Assert (_loc, e) : 'exp )))));
         ([`Skeyword "new";
          `Snterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
           ("`New (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                   (`New (_loc, i) : 'exp )))));
         ([`Skeyword "lazy"; `Sself],
           ("`Lazy (_loc, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Lazy (_loc, e) : 'exp )))))]);
       ((Some "label"), (Some `NA),
         [([`Skeyword "~";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Skeyword ":";
           `Sself],
            ("`Label (_loc, i, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`Label (_loc, i, e) : 'exp )))));
         ([`Skeyword "~";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'exp )))));
         ([`Stoken
             (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
               "`Label i");
          `Sself],
           ("(`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label ({ txt = i;_} : Tokenf.txt) ->
                       ((`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp ) : 
                       'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Sself],
           ("`OptLabl (_loc, (`Lid (_loc, i)), e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Sself],
           ("`OptLabl (_loc, i, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, i, e) : 'exp )))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'exp )))))]);
       ((Some "."), (Some `LA),
         [([`Sself; `Skeyword "."; `Skeyword "("; `Sself; `Skeyword ")"],
            ("`ArrayDot (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`ArrayDot (_loc, e1, e2) : 'exp )))));
         ([`Sself; `Skeyword "."; `Skeyword "["; `Sself; `Skeyword "]"],
           ("`StringDot (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`StringDot (_loc, e1, e2) : 'exp )))));
         ([`Sself;
          `Skeyword ".";
          `Skeyword "{";
          `Snterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
          `Skeyword "}"],
           ("Fan_ops.bigarray_get _loc e1 e2\n",
             (Gramf.mk_action
                (fun _  (e2 : 'comma_exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)
                    -> (Fan_ops.bigarray_get _loc e1 e2 : 'exp )))));
         ([`Sself; `Skeyword "."; `Sself],
           ("`Field (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`Field (_loc, e1, e2) : 'exp )))));
         ([`Sself;
          `Skeyword "#";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Send (_loc, e, lab)\n",
             (Gramf.mk_action
                (fun (lab : 'a_lident)  _  (e : 'exp)  (_loc : Locf.t)  ->
                   (`Send (_loc, e, lab) : 'exp )))))]);
       ((Some "~-"), (Some `NA),
         [([`Skeyword "!"; `Sself],
            ("`Field (_loc, e, (`Lid (_loc, \"contents\")))\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Field (_loc, e, (`Lid (_loc, "contents"))) : 'exp )))));
         ([`Snterm (Gramf.obj (prefixop : 'prefixop Gramf.t )); `Sself],
           ("`App (_loc, f, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (f : 'prefixop)  (_loc : Locf.t)  ->
                   (`App (_loc, f, e) : 'exp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Quot _ -> true | _ -> false)),
                (904098089, `Any), "`Quot _")],
            ("Ast_quotation.expand x Dyn_tag.exp\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Quot x -> (Ast_quotation.expand x Dyn_tag.exp : 'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("exp",_) -> true | _ -> false)),
               (3257031, (`A "exp")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("exp" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`bool",_) -> true | _ -> false)),
               (3257031, (`A "`bool")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`bool" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("par",_) -> true | _ -> false)),
               (3257031, (`A "par")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("par" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("seq",_) -> true | _ -> false)),
               (3257031, (`A "seq")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("seq" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int",_) -> true | _ -> false)),
               (3257031, (`A "int")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int",_) -> true | _ -> false)),
               (3257031, (`A "`int")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int32",_) -> true | _ -> false)),
               (3257031, (`A "int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int32" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int32",_) -> true | _ -> false)),
               (3257031, (`A "`int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int32" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int64",_) -> true | _ -> false)),
               (3257031, (`A "int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int64" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int64",_) -> true | _ -> false)),
               (3257031, (`A "`int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int64" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("nativeint",_) -> true | _ -> false)),
               (3257031, (`A "nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("nativeint" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`nativeint",_) -> true | _ -> false)),
               (3257031, (`A "`nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`nativeint" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("flo",_) -> true | _ -> false)),
               (3257031, (`A "flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("flo" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`flo",_) -> true | _ -> false)),
               (3257031, (`A "`flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`flo" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("chr",_) -> true | _ -> false)),
               (3257031, (`A "chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("chr" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`chr",_) -> true | _ -> false)),
               (3257031, (`A "`chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`chr" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("str",_) -> true | _ -> false)),
               (3257031, (`A "str")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("str" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`str",_) -> true | _ -> false)),
               (3257031, (`A "`str")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`str" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               (3257031, (`A "vrn")), "`Ant s")],
           ("mk_anti _loc ~c:\"exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) ->
                       (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int _ -> true | _ -> false)), (3654863, `Any),
               "`Int s")],
           ("`Int (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int ({ txt = s;_} : Tokenf.txt) ->
                       (`Int (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int32 _ -> true | _ -> false)),
               ((-783416530), `Any), "`Int32 s")],
           ("`Int32 (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int32 ({ txt = s;_} : Tokenf.txt) ->
                       (`Int32 (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int64 _ -> true | _ -> false)),
               ((-783415859), `Any), "`Int64 s")],
           ("`Int64 (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int64 ({ txt = s;_} : Tokenf.txt) ->
                       (`Int64 (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Nativeint _ -> true | _ -> false)),
               ((-113706088), `Any), "`Nativeint s")],
           ("`Nativeint (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Nativeint ({ txt = s;_} : Tokenf.txt) ->
                       (`Nativeint (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Flo _ -> true | _ -> false)), (3505225, `Any),
               "`Flo s")],
           ("`Flo (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Flo ({ txt = s;_} : Tokenf.txt) ->
                       (`Flo (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Chr _ -> true | _ -> false)), (3355149, `Any),
               "`Chr s")],
           ("`Chr (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Chr ({ txt = s;_} : Tokenf.txt) ->
                       (`Chr (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s")],
           ("`Str (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = s;_} : Tokenf.txt) ->
                       (`Str (_loc, s) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stry
             (`Snterm
                (Gramf.obj
                   (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                    Gramf.t )));
          `Sself;
          `Skeyword ")"],
           ("`LetOpen (_loc, (`Negative _loc), i, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  (i : 'module_longident_dot_lparen) 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Negative _loc), i, e) : 'exp )))));
         ([`Snterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(i : vid  :>exp)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>exp) : 'exp )))));
         ([`Skeyword "`"; `Snterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("`Vrn (_loc, s)\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   (`Vrn (_loc, s) : 'exp )))));
         ([`Skeyword "["; `Skeyword "]"],
           ("(`Uid (_loc, \"[]\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
         ([`Skeyword "[";
          `Snterm (Gramf.obj (sem_exp_for_list : 'sem_exp_for_list Gramf.t ));
          `Skeyword "]"],
           ("mk_list (`Uid (_loc, \"[]\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  (mk_list : 'sem_exp_for_list)  _  (_loc : Locf.t)  ->
                   (mk_list (`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'exp )))));
         ([`Skeyword "[|";
          `Snterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ));
          `Skeyword "|]"],
           ("`Array (_loc, el)\n",
             (Gramf.mk_action
                (fun _  (el : 'sem_exp)  _  (_loc : Locf.t)  ->
                   (`Array (_loc, el) : 'exp )))));
         ([`Skeyword "{";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x");
          `Skeyword "with";
          `Snterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
          `Skeyword "}"],
           ("(`RecordWith (_loc, el, (`Lid (_loc, x))) : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  (el : 'label_exp_list)  _  (__fan_1 : Tokenf.t)  _ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       ((`RecordWith (_loc, el, (`Lid (_loc, x))) : FAst.exp ) : 
                       'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "{";
          `Snterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
          `Skeyword "}"],
           ("`Record (_loc, el)\n",
             (Gramf.mk_action
                (fun _  (el : 'label_exp_list)  _  (_loc : Locf.t)  ->
                   (`Record (_loc, el) : 'exp )))));
         ([`Skeyword "{";
          `Skeyword "(";
          `Sself;
          `Skeyword ")";
          `Skeyword "with";
          `Snterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
          `Skeyword "}"],
           ("`RecordWith (_loc, el, e)\n",
             (Gramf.mk_action
                (fun _  (el : 'label_exp_list)  _  _  (e : 'exp)  _  _ 
                   (_loc : Locf.t)  -> (`RecordWith (_loc, el, e) : 'exp )))));
         ([`Skeyword "{<"; `Skeyword ">}"],
           ("`OvrInstEmpty _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`OvrInstEmpty _loc : 'exp )))));
         ([`Skeyword "{<";
          `Snterm (Gramf.obj (field_exp_list : 'field_exp_list Gramf.t ));
          `Skeyword ">}"],
           ("`OvrInst (_loc, fel)\n",
             (Gramf.mk_action
                (fun _  (fel : 'field_exp_list)  _  (_loc : Locf.t)  ->
                   (`OvrInst (_loc, fel) : 'exp )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")"],
           ("`Constraint (_loc, e, t)\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Constraint (_loc, e, t) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
          `Skeyword ")"],
           ("`Par (_loc, (`Com (_loc, e, el)))\n",
             (Gramf.mk_action
                (fun _  (el : 'comma_exp)  _  (e : 'exp)  _  (_loc : Locf.t) 
                   -> (`Par (_loc, (`Com (_loc, e, el))) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ";";
          `Snterm (Gramf.obj (sequence : 'sequence Gramf.t ));
          `Skeyword ")"],
           ("`Seq (_loc, (`Sem (_loc, e, seq)))\n",
             (Gramf.mk_action
                (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : Locf.t) 
                   -> (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp )))));
         ([`Skeyword "("; `Sself; `Skeyword ";"; `Skeyword ")"],
           ("`Seq (_loc, e)\n",
             (Gramf.mk_action
                (fun _  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Seq (_loc, e) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ":>";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")"],
           ("`Coercion (_loc, e, t, t2)\n",
             (Gramf.mk_action
                (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'exp)  _ 
                   (_loc : Locf.t)  -> (`Coercion (_loc, e, t, t2) : 
                   'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":>";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")"],
           ("`Subtype (_loc, e, t)\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Subtype (_loc, e, t) : 'exp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("e\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Skeyword "begin";
          `Snterm (Gramf.obj (sequence : 'sequence Gramf.t ));
          `Skeyword "end"],
           ("`Seq (_loc, seq)\n",
             (Gramf.mk_action
                (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                   (`Seq (_loc, seq) : 'exp )))));
         ([`Skeyword "begin"; `Skeyword "end"],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ));
          `Skeyword ")"],
           ("`Package_exp (_loc, me)\n",
             (Gramf.mk_action
                (fun _  (me : 'mexp)  _  _  (_loc : Locf.t)  ->
                   (`Package_exp (_loc, me) : 'exp )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword ")"],
           ("`Package_exp (_loc, (`Constraint (_loc, me, pt)))\n",
             (Gramf.mk_action
                (fun _  (pt : 'mtyp)  _  (me : 'mexp)  _  _  (_loc : Locf.t) 
                   ->
                   (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 
                   'exp )))))])]);
   Gramf.extend_single (sem_exp_for_list : 'sem_exp_for_list Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (exp : 'exp Gramf.t )); `Skeyword ";"; `Sself],
            ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), (el acc)) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (el : 'sem_exp_for_list)  _  (e : 'exp) 
                    (_loc : Locf.t)  ->
                    (fun acc  ->
                       (`App
                          (_loc, (`App (_loc, (`Uid (_loc, "::")), e)),
                            (el acc)) : FAst.exp ) : 'sem_exp_for_list )))));
         ([`Snterm (Gramf.obj (exp : 'exp Gramf.t )); `Skeyword ";"],
           ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      (`App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                      FAst.exp ) : 'sem_exp_for_list )))));
         ([`Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      (`App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                      FAst.exp ) : 'sem_exp_for_list )))))]));
   Gramf.extend_single (sequence : 'sequence Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "let";
           `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Skeyword "in";
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Snterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k (`LetIn (_loc, rf, bi, e))\n",
              (Gramf.mk_action
                 (fun (k : 'sequence')  (e : 'exp)  _  (bi : 'bind) 
                    (rf : 'opt_rec)  _  (_loc : Locf.t)  ->
                    (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "try";
          `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
          `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
          `Skeyword "in";
          `Sself;
          `Skeyword "with";
          `Snterm (Gramf.obj (case : 'case Gramf.t ));
          `Snterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
           ("k (`LetTryInWith (_loc, r, bi, x, a))\n",
             (Gramf.mk_action
                (fun (k : 'sequence')  (a : 'case)  _  (x : 'sequence)  _ 
                   (bi : 'bind)  (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                   (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Snterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
          `Skeyword "in";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Snterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
           ("k (`LetModule (_loc, m, mb, e))\n",
             (Gramf.mk_action
                (fun (k : 'sequence')  (e : 'exp)  _  (mb : 'mbind0) 
                   (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "open";
          `Snterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Skeyword "in";
          `Sself],
           ("`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e)\n",
             (Gramf.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e) : 
                   'sequence )))));
         ([`Skeyword "let";
          `Skeyword "open";
          `Skeyword "!";
          `Snterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Skeyword "in";
          `Sself],
           ("`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e)\n",
             (Gramf.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _  _ 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e) : 
                   'sequence )))));
         ([`Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Snterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
           ("k e\n",
             (Gramf.mk_action
                (fun (k : 'sequence')  (e : 'exp)  (_loc : Locf.t)  ->
                   (k e : 'sequence )))))]));
   Gramf.extend_single (sequence' : 'sequence' Gramf.t )
     (None,
       (None, None,
         [([],
            ("fun e  -> e\n",
              (Gramf.mk_action
                 (fun (_loc : Locf.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Skeyword ";"],
           ("fun e  -> e\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Skeyword ";";
          `Snterm (Gramf.obj (sequence : 'sequence Gramf.t ))],
           ("fun e  -> `Sem (_loc, e, el)\n",
             (Gramf.mk_action
                (fun (el : 'sequence)  _  (_loc : Locf.t)  ->
                   (fun e  -> `Sem (_loc, e, el) : 'sequence' )))))]));
   Gramf.extend_single (comma_exp : 'comma_exp Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("`Com (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'comma_exp) 
                    (_loc : Locf.t)  -> (`Com (_loc, e1, e2) : 'comma_exp )))));
         ([`Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'comma_exp )))))])));
  Gramf.extend_single (with_exp_lang : 'with_exp_lang Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (lang : 'lang Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("Ast_quotation.default := old; x\n",
             (Gramf.mk_action
                (fun (x : 'exp)  _  (old : 'lang)  (_loc : Locf.t)  ->
                   (Ast_quotation.default := old; x : 'with_exp_lang )))))]));
  Gramf.extend_single (with_stru_lang : 'with_stru_lang Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (lang : 'lang Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (stru : 'stru Gramf.t ))],
           ("Ast_quotation.default := old; x\n",
             (Gramf.mk_action
                (fun (x : 'stru)  _  (old : 'lang)  (_loc : Locf.t)  ->
                   (Ast_quotation.default := old; x : 'with_stru_lang )))))]));
  (Gramf.extend_single (bind_quot : 'bind_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (bind : 'bind Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'bind)  (_loc : Locf.t)  -> (x : 'bind_quot )))))]));
   Gramf.extend_single (bind : 'bind Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("bind",_) -> true | _ -> false)),
                (3257031, (`A "bind")), "`Ant s")],
            ("mk_anti _loc ~c:\"bind\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("bind" as n),s) ->
                        (mk_anti _loc ~c:"bind" n s : 'bind )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"bind\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"bind" n s : 'bind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(`Bind (_loc, (mk_anti _loc ~c:\"pat\" n s), e) : FAst.bind )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Bind (_loc, (mk_anti _loc ~c:"pat" n s), e) : 
                       FAst.bind ) : 'bind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Sself; `Skeyword "and"; `Sself],
           ("`And (_loc, b1, b2)\n",
             (Gramf.mk_action
                (fun (b2 : 'bind)  _  (b1 : 'bind)  (_loc : Locf.t)  ->
                   (`And (_loc, b1, b2) : 'bind )))));
         ([`Snterm (Gramf.obj (let_bind : 'let_bind Gramf.t ))],
           ("b\n",
             (Gramf.mk_action
                (fun (b : 'let_bind)  (_loc : Locf.t)  -> (b : 'bind )))))]));
   Gramf.extend_single (let_bind : 'let_bind Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Snterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
            ("`Bind (_loc, p, e)\n",
              (Gramf.mk_action
                 (fun (e : 'fun_bind)  (p : 'pat)  (_loc : Locf.t)  ->
                    (`Bind (_loc, p, e) : 'let_bind )))))])));
  (Gramf.extend_single (case : 'case Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "|";
           `Slist1sep
             ((`Snterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
               (`Skeyword "|"))],
            ("bar_of_list l\n",
              (Gramf.mk_action
                 (fun (l : 'case0 list)  _  (_loc : Locf.t)  ->
                    (bar_of_list l : 'case )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword "->";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, p, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                   (`Case (_loc, p, e) : 'case )))))]));
   Gramf.extend_single (case0 : 'case0 Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("case",_) -> true | _ -> false)),
                (3257031, (`A "case")), "`Ant s")],
            ("mk_anti _loc ~c:\"case\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("case" as n),s) ->
                        (mk_anti _loc ~c:"case" n s : 'case0 )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"case\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"case" n s : 'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword "when";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword "->";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, (mk_anti _loc ~c:\"case\" n s), w, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`CaseWhen (_loc, (mk_anti _loc ~c:"case" n s), w, e) : 
                       'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword "->";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, (mk_anti _loc ~c:\"case\" n s), e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Case (_loc, (mk_anti _loc ~c:"case" n s), e) : 
                       'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Skeyword "when";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword "->";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, p, w, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (p : 'pat_as_pat_opt) 
                   (_loc : Locf.t)  -> (`CaseWhen (_loc, p, w, e) : 'case0 )))));
         ([`Snterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Skeyword "->";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, p, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (p : 'pat_as_pat_opt)  (_loc : Locf.t) 
                   -> (`Case (_loc, p, e) : 'case0 )))))]));
   Gramf.extend_single (case_quot : 'case_quot Gramf.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((`Snterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                (`Skeyword "|"))],
            ("bar_of_list x\n",
              (Gramf.mk_action
                 (fun (x : 'case0 list)  (_loc : Locf.t)  ->
                    (bar_of_list x : 'case_quot )))))])));
  (Gramf.extend_single (rec_exp_quot : 'rec_exp_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'label_exp_list)  (_loc : Locf.t)  ->
                    (x : 'rec_exp_quot )))))]));
   Gramf.extend_single (label_exp : 'label_exp Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("rec_exp",_) -> true | _ -> false)),
                (3257031, (`A "rec_exp")), "`Ant s")],
            ("mk_anti _loc ~c:\"rec_exp\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("rec_exp" as n),s) ->
                        (mk_anti _loc ~c:"rec_exp" n s : 'label_exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"rec_exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"rec_exp" n s : 'label_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (label_longident : 'label_longident Gramf.t ));
          `Snterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
           ("(`RecBind (_loc, i, e) : FAst.rec_exp )\n",
             (Gramf.mk_action
                (fun (e : 'fun_bind)  (i : 'label_longident)  (_loc : Locf.t)
                    ->
                   ((`RecBind (_loc, i, e) : FAst.rec_exp ) : 'label_exp )))));
         ([`Snterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
           ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
             (Gramf.mk_action
                (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                   'label_exp )))))]));
   Gramf.extend_single (field_exp : 'field_exp Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"rec_exp\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"rec_exp" n s : 'field_exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("bi",_) -> true | _ -> false)),
               (3257031, (`A "bi")), "`Ant s")],
           ("mk_anti _loc ~c:\"rec_exp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("bi" as n),s) ->
                       (mk_anti _loc ~c:"rec_exp" n s : 'field_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`RecBind (_loc, (l :>ident), e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (l : 'a_lident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, (l :>ident), e) : 'field_exp )))))]));
   Gramf.extend_single (label_exp_list : 'label_exp_list Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (label_exp : 'label_exp Gramf.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'label_exp_list)  _  (b1 : 'label_exp) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, b1, b2) : 'label_exp_list )))));
         ([`Snterm (Gramf.obj (label_exp : 'label_exp Gramf.t ));
          `Skeyword ";"],
           ("b1\n",
             (Gramf.mk_action
                (fun _  (b1 : 'label_exp)  (_loc : Locf.t)  ->
                   (b1 : 'label_exp_list )))));
         ([`Snterm (Gramf.obj (label_exp : 'label_exp Gramf.t ))],
           ("b1\n",
             (Gramf.mk_action
                (fun (b1 : 'label_exp)  (_loc : Locf.t)  ->
                   (b1 : 'label_exp_list )))))]));
   Gramf.extend_single (field_exp_list : 'field_exp_list Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (field_exp : 'field_exp Gramf.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'field_exp_list)  _  (b1 : 'field_exp) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, b1, b2) : 'field_exp_list )))));
         ([`Snterm (Gramf.obj (field_exp : 'field_exp Gramf.t ));
          `Skeyword ";"],
           ("b1\n",
             (Gramf.mk_action
                (fun _  (b1 : 'field_exp)  (_loc : Locf.t)  ->
                   (b1 : 'field_exp_list )))));
         ([`Snterm (Gramf.obj (field_exp : 'field_exp Gramf.t ))],
           ("b1\n",
             (Gramf.mk_action
                (fun (b1 : 'field_exp)  (_loc : Locf.t)  ->
                   (b1 : 'field_exp_list )))))])));
  (let grammar_entry_create x = Gramf.mk x in
   let pat_constr: 'pat_constr Gramf.t = grammar_entry_create "pat_constr" in
   Gramf.extend_single (pat_quot : 'pat_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Skeyword ",";
           `Snterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ))],
            ("`Com (_loc, x, y)\n",
              (Gramf.mk_action
                 (fun (y : 'comma_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                    (`Com (_loc, x, y) : 'pat_quot )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ";";
          `Snterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ))],
           ("`Sem (_loc, x, y)\n",
             (Gramf.mk_action
                (fun (y : 'sem_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, x, y) : 'pat_quot )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_quot )))))]));
   Gramf.extend_single (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Skeyword "as";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Alias (_loc, p1, s)\n",
              (Gramf.mk_action
                 (fun (s : 'a_lident)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Alias (_loc, p1, s) : 'pat_as_pat_opt )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_as_pat_opt )))))]));
   Gramf.extend_single (pat_constr : 'pat_constr Gramf.t )
     (None,
       (None, None,
         [([`Snterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("(i : vid  :>pat)\n",
              (Gramf.mk_action
                 (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                    ((i : vid  :>pat) : 'pat_constr )))));
         ([`Skeyword "`"; `Snterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : pat )\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : pat ) : 'pat_constr )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               (3257031, (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               (3257031, (`A "vrn")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend (pat : 'pat Gramf.t )
     (None,
       [((Some "|"), (Some `LA),
          [([`Sself; `Skeyword "|"; `Sself],
             ("`Bar (_loc, p1, p2)\n",
               (Gramf.mk_action
                  (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                     (`Bar (_loc, p1, p2) : 'pat )))))]);
       ((Some ".."), (Some `NA),
         [([`Sself; `Skeyword ".."; `Sself],
            ("`PaRng (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`PaRng (_loc, p1, p2) : 'pat )))))]);
       ((Some "::"), (Some `RA),
         [([`Sself; `Skeyword "::"; `Sself],
            ("`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p1)), p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`App (_loc, (`App (_loc, (`Uid (_loc, "::")), p1)), p2) : 
                    'pat )))))]);
       ((Some "apply"), (Some `LA),
         [([`Snterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t )); `Sself],
            ("match p2 with\n| (`Par (_loc,p) : FAst.pat) ->\n    List.fold_left (fun p1  p2  -> (`App (_loc, p1, p2) : FAst.pat )) p1\n      (Ast_basic.list_of_com p [])\n| _ -> (`App (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (p2 : 'pat)  (p1 : 'pat_constr)  (_loc : Locf.t)  ->
                    (match p2 with
                     | (`Par (_loc,p) : FAst.pat) ->
                         List.fold_left
                           (fun p1  p2  -> (`App (_loc, p1, p2) : FAst.pat ))
                           p1 (Ast_basic.list_of_com p [])
                     | _ -> (`App (_loc, p1, p2) : FAst.pat ) : 'pat )))));
         ([`Snterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t ))],
           ("p1\n",
             (Gramf.mk_action
                (fun (p1 : 'pat_constr)  (_loc : Locf.t)  -> (p1 : 'pat )))));
         ([`Skeyword "lazy"; `Sself],
           ("`Lazy (_loc, p)\n",
             (Gramf.mk_action
                (fun (p : 'pat)  _  (_loc : Locf.t)  ->
                   (`Lazy (_loc, p) : 'pat )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"pat\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'pat )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               (3257031, (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("par",_) -> true | _ -> false)),
               (3257031, (`A "par")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("par" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int",_) -> true | _ -> false)),
               (3257031, (`A "int")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int",_) -> true | _ -> false)),
               (3257031, (`A "`int")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int32",_) -> true | _ -> false)),
               (3257031, (`A "int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int32" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int32",_) -> true | _ -> false)),
               (3257031, (`A "`int32")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int32" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("int64",_) -> true | _ -> false)),
               (3257031, (`A "int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("int64" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`int64",_) -> true | _ -> false)),
               (3257031, (`A "`int64")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`int64" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               (3257031, (`A "vrn")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("nativeint",_) -> true | _ -> false)),
               (3257031, (`A "nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("nativeint" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`nativeint",_) -> true | _ -> false)),
               (3257031, (`A "`nativeint")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`nativeint" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("flo",_) -> true | _ -> false)),
               (3257031, (`A "flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("flo" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`flo",_) -> true | _ -> false)),
               (3257031, (`A "`flo")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`flo" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("chr",_) -> true | _ -> false)),
               (3257031, (`A "chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("chr" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`chr",_) -> true | _ -> false)),
               (3257031, (`A "`chr")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`chr" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("str",_) -> true | _ -> false)),
               (3257031, (`A "str")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("str" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("`str",_) -> true | _ -> false)),
               (3257031, (`A "`str")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("`str" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(i : vid  :>pat)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>pat) : 'pat )))));
         ([`Stoken
             (((function | `Int _ -> true | _ -> false)), (3654863, `Any),
               "`Int s")],
           ("`Int (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int ({ txt = s;_} : Tokenf.txt) ->
                       (`Int (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int32 _ -> true | _ -> false)),
               ((-783416530), `Any), "`Int32 s")],
           ("`Int32 (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int32 ({ txt = s;_} : Tokenf.txt) ->
                       (`Int32 (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Int64 _ -> true | _ -> false)),
               ((-783415859), `Any), "`Int64 s")],
           ("`Int64 (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Int64 ({ txt = s;_} : Tokenf.txt) ->
                       (`Int64 (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Nativeint _ -> true | _ -> false)),
               ((-113706088), `Any), "`Nativeint s")],
           ("`Nativeint (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Nativeint ({ txt = s;_} : Tokenf.txt) ->
                       (`Nativeint (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Flo _ -> true | _ -> false)), (3505225, `Any),
               "`Flo s")],
           ("`Flo (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Flo ({ txt = s;_} : Tokenf.txt) ->
                       (`Flo (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Chr _ -> true | _ -> false)), (3355149, `Any),
               "`Chr s")],
           ("`Chr (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Chr ({ txt = s;_} : Tokenf.txt) ->
                       (`Chr (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s")],
           ("`Str (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = s;_} : Tokenf.txt) ->
                       (`Str (_loc, s) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Int _ -> true | _ -> false)), (3654863, `Any),
              "`Int s")],
           ("`Int (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Int ({ txt = s;_} : Tokenf.txt) ->
                       (`Int (_loc, (Stringf.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Int32 _ -> true | _ -> false)),
              ((-783416530), `Any), "`Int32 s")],
           ("`Int32 (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Int32 ({ txt = s;_} : Tokenf.txt) ->
                       (`Int32 (_loc, (Stringf.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Int64 _ -> true | _ -> false)),
              ((-783415859), `Any), "`Int64 s")],
           ("`Int64 (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Int64 ({ txt = s;_} : Tokenf.txt) ->
                       (`Int64 (_loc, (Stringf.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Nativeint _ -> true | _ -> false)),
              ((-113706088), `Any), "`Nativeint s")],
           ("`Nativeint (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Nativeint ({ txt = s;_} : Tokenf.txt) ->
                       (`Nativeint (_loc, (Stringf.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Flo _ -> true | _ -> false)), (3505225, `Any),
              "`Flo s")],
           ("`Flo (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Flo ({ txt = s;_} : Tokenf.txt) ->
                       (`Flo (_loc, (Stringf.neg s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "["; `Skeyword "]"],
           ("(`Uid (_loc, \"[]\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "[]") : FAst.pat ) : 'pat )))));
         ([`Skeyword "[";
          `Snterm (Gramf.obj (sem_pat_for_list : 'sem_pat_for_list Gramf.t ));
          `Skeyword "]"],
           ("mk_list (`Uid (_loc, \"[]\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (mk_list : 'sem_pat_for_list)  _  (_loc : Locf.t)  ->
                   (mk_list (`Uid (_loc, "[]") : FAst.pat ) : 'pat )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'pat )))));
         ([`Skeyword "[|";
          `Snterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ));
          `Skeyword "|]"],
           ("`Array (_loc, pl)\n",
             (Gramf.mk_action
                (fun _  (pl : 'sem_pat)  _  (_loc : Locf.t)  ->
                   (`Array (_loc, pl) : 'pat )))));
         ([`Skeyword "{";
          `Snterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
          `Skeyword "}"],
           ("`Record (_loc, pl)\n",
             (Gramf.mk_action
                (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                   (`Record (_loc, pl) : 'pat )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.pat ) : 'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Gramf.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (3257031, (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n",
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.t)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_4))))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("p\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "as";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   -> ((`Alias (_loc, p, s) : FAst.pat ) : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ));
          `Skeyword ")"],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (pl : 'comma_pat)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'pat )))));
         ([`Skeyword "`"; `Snterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))));
         ([`Skeyword "#";
          `Snterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("(`ClassPath (_loc, i) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (i : 'type_longident)  _  (_loc : Locf.t)  ->
                   ((`ClassPath (_loc, i) : FAst.pat ) : 'pat )))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x -> (Ast_quotation.expand x Dyn_tag.pat : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> ((`Any _loc : FAst.pat ) : 'pat )))));
         ([`Stoken
             (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
               "`Label i");
          `Sself],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label ({ txt = i;_} : Tokenf.txt) ->
                       ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "~";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Sself],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'pat)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'pat )))));
         ([`Skeyword "~";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'pat )))));
         ([`Stoken
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (3257031, (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n",
             (Gramf.mk_action
                (fun _  (__fan_6 : Tokenf.t)  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_6))))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, i, p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : Locf.t)  -> (`OptLabl (_loc, i, p) : 'pat )))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'pat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'pat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'pat )))))])]);
   Gramf.extend_single (ipat : 'ipat Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "{";
           `Snterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
           `Skeyword "}"],
            ("(`Record (_loc, pl) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                    ((`Record (_loc, pl) : FAst.pat ) : 'ipat )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               (3257031, (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("par",_) -> true | _ -> false)),
               (3257031, (`A "par")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("par" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "("; `Skeyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.pat ) : 'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Gramf.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (3257031, (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n",
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.t)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_4))))));
         ([`Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ")"],
           ("p\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'ipat )))));
         ([`Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword "as";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   -> ((`Alias (_loc, p, s) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ",";
          `Snterm (Gramf.obj (comma_ipat : 'comma_ipat Gramf.t ));
          `Skeyword ")"],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (pl : 'comma_ipat)  _  (p : 'pat)  _  (_loc : Locf.t)
                    ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'ipat )))));
         ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(s : alident  :>pat)\n",
             (Gramf.mk_action
                (fun (s : 'a_lident)  (_loc : Locf.t)  ->
                   ((s : alident  :>pat) : 'ipat )))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x -> (Ast_quotation.expand x Dyn_tag.pat : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "`"; `Snterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ((`Any _loc : FAst.pat ) : 'ipat )))));
         ([`Stoken
             (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
               "`Label i");
          `Sself],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label ({ txt = i;_} : Tokenf.txt) ->
                       ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "~";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Sself],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'ipat )))));
         ([`Skeyword "~";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'ipat )))));
         ([`Stoken
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (3257031, (`A "opt")), "`Ant s");
          `Skeyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n",
             (Gramf.mk_action
                (fun _  (__fan_6 : Tokenf.t)  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_6))))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, i, p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : Locf.t)  -> (`OptLabl (_loc, i, p) : 'ipat )))));
         ([`Skeyword "?";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'ipat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Skeyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'ipat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Skeyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'ipat )))))]));
   Gramf.extend_single (sem_pat : 'sem_pat Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t )); `Skeyword ";"; `Sself],
            ("`Sem (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'sem_pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'sem_pat )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t )); `Skeyword ";"],
           ("p\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  (_loc : Locf.t)  -> (p : 'sem_pat )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'sem_pat )))))]));
   Gramf.extend_single (sem_pat_for_list : 'sem_pat_for_list Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t )); `Skeyword ";"; `Sself],
            ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), (pl acc))\n",
              (Gramf.mk_action
                 (fun (pl : 'sem_pat_for_list)  _  (p : 'pat) 
                    (_loc : Locf.t)  ->
                    (fun acc  ->
                       `App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), p)),
                           (pl acc)) : 'sem_pat_for_list )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t )); `Skeyword ";"],
           ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc)\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))))]));
   Gramf.extend_single (pat_tcon : 'pat_tcon Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Skeyword ":";
           `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`Constraint (_loc, p, t) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (t : 'ctyp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                    ((`Constraint (_loc, p, t) : FAst.pat ) : 'pat_tcon )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_tcon )))))]));
   Gramf.extend_single (ipat_tcon : 'ipat_tcon Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"pat\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'ipat_tcon )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i : alident  :>pat)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i : alident  :>pat) : 'ipat_tcon )))));
         ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Constraint (_loc, (i : alident  :>pat), t) : pat )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, (i : alident  :>pat), t) : pat ) : 
                   'ipat_tcon )))))]));
   Gramf.extend_single (comma_ipat : 'comma_ipat Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("(`Com (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (p2 : 'comma_ipat)  _  (p1 : 'comma_ipat) 
                    (_loc : Locf.t)  ->
                    ((`Com (_loc, p1, p2) : FAst.pat ) : 'comma_ipat )))));
         ([`Snterm (Gramf.obj (ipat : 'ipat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  (_loc : Locf.t)  -> (p : 'comma_ipat )))))]));
   Gramf.extend_single (comma_pat : 'comma_pat Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("(`Com (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (p2 : 'comma_pat)  _  (p1 : 'comma_pat) 
                    (_loc : Locf.t)  ->
                    ((`Com (_loc, p1, p2) : FAst.pat ) : 'comma_pat )))));
         ([`Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'comma_pat )))))]));
   Gramf.extend_single (label_pat_list : 'label_pat_list Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'label_pat_list)  _  (p1 : 'label_pat) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_pat_list )))));
         ([`Snterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Skeyword ";";
          `Skeyword "_"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Snterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Skeyword ";";
          `Skeyword "_";
          `Skeyword ";"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun _  _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Snterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Skeyword ";"],
           ("p1\n",
             (Gramf.mk_action
                (fun _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))));
         ([`Snterm (Gramf.obj (label_pat : 'label_pat Gramf.t ))],
           ("p1\n",
             (Gramf.mk_action
                (fun (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))))]));
   Gramf.extend_single (label_pat : 'label_pat Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"pat\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'label_pat )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("pat",_) -> true | _ -> false)),
               (3257031, (`A "pat")), "`Ant s")],
           ("mk_anti _loc ~c:\"pat\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("pat" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'label_pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (label_longident : 'label_longident Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("`RecBind (_loc, i, p)\n",
             (Gramf.mk_action
                (fun (p : 'pat)  _  (i : 'label_longident)  (_loc : Locf.t) 
                   -> (`RecBind (_loc, i, p) : 'label_pat )))));
         ([`Snterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
           ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
             (Gramf.mk_action
                (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                   'label_pat )))))])));
  (Gramf.extend_single (luident : 'luident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid i")],
            ("i\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid ({ txt = i;_} : Tokenf.txt) -> (i : 'luident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("i\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) -> (i : 'luident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (aident : 'aident Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("(i :>ident)\n",
              (Gramf.mk_action
                 (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                    ((i :>ident) : 'aident )))));
         ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
           ("(i :>ident)\n",
             (Gramf.mk_action
                (fun (i : 'a_uident)  (_loc : Locf.t)  ->
                   ((i :>ident) : 'aident )))))]));
   Gramf.extend_single (astr : 'astr Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid i")],
            ("`C (_loc, i)\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid ({ txt = i;_} : Tokenf.txt) ->
                        (`C (_loc, i) : 'astr )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("`C (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`C (_loc, i) : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) -> (mk_anti _loc n s : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("vrn",_) -> true | _ -> false)),
               (3257031, (`A "vrn")), "`Ant s")],
           ("mk_anti _loc n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("vrn" as n),s) -> (mk_anti _loc n s : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend (ident_quot : 'ident_quot Gramf.t )
     (None,
       [((Some "."), None,
          [([`Sself; `Skeyword "."; `Sself],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Gramf.mk_action
                  (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                     (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'ident_quot )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (3257031, (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid i")],
           ("(`Lid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Lid (_loc, i) : FAst.ident ) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (j : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       ((`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident ) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "("; `Sself; `Sself; `Skeyword ")"],
           ("`Apply (_loc, i, j)\n",
             (Gramf.mk_action
                (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                   (_loc : Locf.t)  -> (`Apply (_loc, i, j) : 'ident_quot )))))])]);
   Gramf.extend_single (ident : 'ident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'ident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (3257031, (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid i")],
           ("`Lid (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       (`Lid (_loc, i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`Uid (_loc, i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
             (Gramf.mk_action
                (fun (j : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (vid : 'vid Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'vid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (3257031, (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid i")],
           ("`Lid (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       (`Lid (_loc, i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`Uid (_loc, i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
             (Gramf.mk_action
                (fun (j : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (uident : 'uident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "`Uid s")],
            ("`Uid (_loc, s)\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = s;_} : Tokenf.txt) ->
                        (`Uid (_loc, s) : 'uident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"uident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"uident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"uident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Skeyword ".";
          `Sself],
           ("dot (`Uid (_loc, s)) l\n",
             (Gramf.mk_action
                (fun (l : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (dot (`Uid (_loc, s)) l : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("dot (mk_anti _loc ~c:\"uident\" n s) i\n",
             (Gramf.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("dot (mk_anti _loc ~c:\"uident\" n s) i\n",
             (Gramf.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("dot (mk_anti _loc ~c:\"uident\" n s) i\n",
             (Gramf.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (dot_lstrings : 'dot_lstrings Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid i")],
            ("((`Sub []), i)\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid ({ txt = i;_} : Tokenf.txt) ->
                        (((`Sub []), i) : 'dot_lstrings )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("match xs with\n| (`Sub xs,v) -> ((`Sub (i :: xs)), v)\n| _ -> raise (Streamf.Error \"impossible dot_lstrings\")\n",
             (Gramf.mk_action
                (fun (xs : 'dot_lstrings)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((match xs with
                         | (`Sub xs,v) -> ((`Sub (i :: xs)), v)
                         | _ ->
                             raise (Streamf.Error "impossible dot_lstrings")) : 
                       'dot_lstrings )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword ".";
          `Stoken
            (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
              "`Uid i");
          `Skeyword ".";
          `Sself],
           ("match xs with\n| (`Sub xs,v) -> ((`Absolute (i :: xs)), v)\n| _ -> raise (Streamf.Error \"impossible dot_lstrings\")\n",
             (Gramf.mk_action
                (fun (xs : 'dot_lstrings)  _  (__fan_1 : Tokenf.t)  _ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((match xs with
                         | (`Sub xs,v) -> ((`Absolute (i :: xs)), v)
                         | _ ->
                             raise (Streamf.Error "impossible dot_lstrings")) : 
                       'dot_lstrings )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))))]));
   Gramf.extend_single
     (module_longident_dot_lparen : 'module_longident_dot_lparen Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s");
           `Skeyword ".";
           `Skeyword "("],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s");
          `Skeyword ".";
          `Skeyword "("],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Skeyword "("],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident ) : 
                       'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Skeyword ".";
          `Skeyword "("],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (module_longident : 'module_longident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, i)), l)\n",
             (Gramf.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`Uid (_loc, i) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n",
             (Gramf.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n",
             (Gramf.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend
     (module_longident_with_app : 'module_longident_with_app Gramf.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("`Apply (_loc, i, j)\n",
               (Gramf.mk_action
                  (fun (j : 'module_longident_with_app) 
                     (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                     (`Apply (_loc, i, j) : 'module_longident_with_app )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("(`Dot (_loc, i, j) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun (j : 'module_longident_with_app)  _ 
                    (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                    ((`Dot (_loc, i, j) : FAst.ident ) : 'module_longident_with_app )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("`Uid (_loc, i)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`Uid (_loc, i) : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("i\n",
             (Gramf.mk_action
                (fun _  (i : 'module_longident_with_app)  _  (_loc : Locf.t) 
                   -> (i : 'module_longident_with_app )))))])]);
   Gramf.extend (type_longident : 'type_longident Gramf.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("`Apply (_loc, i, j)\n",
               (Gramf.mk_action
                  (fun (j : 'type_longident)  (i : 'type_longident) 
                     (_loc : Locf.t)  ->
                     (`Apply (_loc, i, j) : 'type_longident )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("(`Dot (_loc, i, j) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun (j : 'type_longident)  _  (i : 'type_longident) 
                    (_loc : Locf.t)  ->
                    ((`Dot (_loc, i, j) : FAst.ident ) : 'type_longident )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'type_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (3257031, (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid i")],
           ("(`Lid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Lid (_loc, i) : FAst.ident ) : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("i\n",
             (Gramf.mk_action
                (fun _  (i : 'type_longident)  _  (_loc : Locf.t)  ->
                   (i : 'type_longident )))))])]);
   Gramf.extend_single (label_longident : 'label_longident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"ident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'label_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("id",_) -> true | _ -> false)),
               (3257031, (`A "id")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("id" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (3257031, (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"ident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid i")],
           ("(`Lid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Lid (_loc, i) : FAst.ident ) : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident ) : 
                       'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s");
          `Skeyword ".";
          `Sself],
           ("(`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       ((`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       FAst.ident ) : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (cltyp_longident : 'cltyp_longident Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'type_longident)  (_loc : Locf.t)  ->
                    (x : 'cltyp_longident )))))]));
   Gramf.extend_single (val_longident : 'val_longident Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (ident : 'ident Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'ident)  (_loc : Locf.t)  -> (x : 'val_longident )))))]));
   Gramf.extend_single (class_longident : 'class_longident Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'label_longident)  (_loc : Locf.t)  ->
                    (x : 'class_longident )))))]));
   Gramf.extend_single (method_opt_override : 'method_opt_override Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "method"; `Skeyword "!"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'method_opt_override )))));
         ([`Skeyword "method";
          `Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'method_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "method";
          `Stoken
            (((function | `Ant ("override",_) -> true | _ -> false)),
              (3257031, (`A "override")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("override" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'method_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "method"],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (`Negative _loc : 'method_opt_override )))))]));
   Gramf.extend_single (opt_override : 'opt_override Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "!"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'opt_override )))));
         ([`Stoken
             (((function | `Ant ("!",_) -> true | _ -> false)),
               (3257031, (`A "!")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("!" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("override",_) -> true | _ -> false)),
               (3257031, (`A "override")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("override" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_override )))))]));
   Gramf.extend_single
     (value_val_opt_override : 'value_val_opt_override Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "val"; `Skeyword "!"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'value_val_opt_override )))));
         ([`Skeyword "val";
          `Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "val";
          `Stoken
            (((function | `Ant ("override",_) -> true | _ -> false)),
              (3257031, (`A "override")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("override" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "val";
          `Stoken
            (((function | `Ant ("!",_) -> true | _ -> false)),
              (3257031, (`A "!")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (("!" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
         ([`Skeyword "val"],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (`Negative _loc : 'value_val_opt_override )))))]));
   Gramf.extend_single (flag : 'flag Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "to"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'flag )))));
         ([`Skeyword "downto"],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (`Negative _loc : 'flag )))));
         ([`Stoken
             (((function | `Ant ("to",_) -> true | _ -> false)),
               (3257031, (`A "to")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("to" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'flag )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'flag )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (opt_private : 'opt_private Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "private"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_private )))));
         ([`Stoken
             (((function | `Ant ("private",_) -> true | _ -> false)),
               (3257031, (`A "private")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("private" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_private )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_private )))))]));
   Gramf.extend_single (opt_mutable : 'opt_mutable Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "mutable"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_mutable )))));
         ([`Stoken
             (((function | `Ant ("mutable",_) -> true | _ -> false)),
               (3257031, (`A "mutable")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("mutable" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_mutable )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_mutable )))))]));
   Gramf.extend_single (opt_virtual : 'opt_virtual Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "virtual"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_virtual )))));
         ([`Stoken
             (((function | `Ant ("virtual",_) -> true | _ -> false)),
               (3257031, (`A "virtual")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("virtual" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_virtual )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_virtual )))))]));
   Gramf.extend_single (opt_dot_dot : 'opt_dot_dot Gramf.t )
     (None,
       (None, None,
         [([`Skeyword ".."],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_dot_dot )))));
         ([`Stoken
             (((function | `Ant ("..",_) -> true | _ -> false)),
               (3257031, (`A "..")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant ((".." as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_dot_dot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_dot_dot )))))]));
   Gramf.extend_single (opt_rec : 'opt_rec Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "rec"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_rec )))));
         ([`Stoken
             (((function | `Ant ("rec",_) -> true | _ -> false)),
               (3257031, (`A "rec")), "`Ant s")],
           ("mk_anti _loc ~c:\"flag\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("rec" as n),s) ->
                       (mk_anti _loc ~c:"flag" n s : 'opt_rec )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_rec )))))]));
   Gramf.extend_single (a_lident : 'a_lident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"a_lident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (3257031, (`A "lid")), "`Ant s")],
           ("mk_anti _loc ~c:\"a_lident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid s")],
           ("`Lid (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = s;_} : Tokenf.txt) ->
                       (`Lid (_loc, s) : 'a_lident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (a_uident : 'a_uident Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"a_uident\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"a_uident" n s : 'a_uident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("uid",_) -> true | _ -> false)),
               (3257031, (`A "uid")), "`Ant s")],
           ("mk_anti _loc ~c:\"a_uident\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("uid" as n),s) ->
                       (mk_anti _loc ~c:"a_uident" n s : 'a_uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s")],
           ("`Uid (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (`Uid (_loc, s) : 'a_uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (string_list : 'string_list Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc \"str_list\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant ("",s) ->
                        (mk_anti _loc "str_list" s : 'string_list )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Sself],
           ("`App (_loc, (mk_anti _loc \"\" s), xs)\n",
             (Gramf.mk_action
                (fun (xs : 'string_list)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant ("",s) ->
                       (`App (_loc, (mk_anti _loc "" s), xs) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str x")],
           ("`Str (_loc, x)\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = x;_} : Tokenf.txt) ->
                       (`Str (_loc, x) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str x");
          `Sself],
           ("`App (_loc, (`Str (_loc, x)), xs)\n",
             (Gramf.mk_action
                (fun (xs : 'string_list)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = x;_} : Tokenf.txt) ->
                       (`App (_loc, (`Str (_loc, x)), xs) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))))]));
   Gramf.extend_single (rec_flag_quot : 'rec_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_rec)  (_loc : Locf.t)  ->
                    (x : 'rec_flag_quot )))))]));
   Gramf.extend_single (direction_flag_quot : 'direction_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (flag : 'flag Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'flag)  (_loc : Locf.t)  ->
                    (x : 'direction_flag_quot )))))]));
   Gramf.extend_single (mutable_flag_quot : 'mutable_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_mutable)  (_loc : Locf.t)  ->
                    (x : 'mutable_flag_quot )))))]));
   Gramf.extend_single (private_flag_quot : 'private_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (opt_private : 'opt_private Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_private)  (_loc : Locf.t)  ->
                    (x : 'private_flag_quot )))))]));
   Gramf.extend_single (virtual_flag_quot : 'virtual_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_virtual)  (_loc : Locf.t)  ->
                    (x : 'virtual_flag_quot )))))]));
   Gramf.extend_single (row_var_flag_quot : 'row_var_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_dot_dot)  (_loc : Locf.t)  ->
                    (x : 'row_var_flag_quot )))))]));
   Gramf.extend_single (override_flag_quot : 'override_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (opt_override : 'opt_override Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_override)  (_loc : Locf.t)  ->
                    (x : 'override_flag_quot )))))]));
   Gramf.extend_single (pat_eoi : 'pat_eoi Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Stoken
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
            ("x\n",
              (Gramf.mk_action
                 (fun _  (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_eoi )))))]));
   Gramf.extend_single (exp_eoi : 'exp_eoi Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Stoken
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
            ("x\n",
              (Gramf.mk_action
                 (fun _  (x : 'exp)  (_loc : Locf.t)  -> (x : 'exp_eoi )))))])));
  (Gramf.extend_single (implem : 'implem Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `DirQuotation _ -> true | _ -> false)),
                ((-440645089), `Any), "`DirQuotation _")],
            ("Fdir.handle_quot x; ([], (Some _loc))\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `DirQuotation x ->
                        ((Fdir.handle_quot x; ([], (Some _loc))) : 'implem )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t ));
          `Skeyword ";;";
          `Sself],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Gramf.mk_action
                (fun (rest : 'implem)  _  (si : 'stru)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'implem )))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t )); `Sself],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Gramf.mk_action
                (fun (rest : 'implem)  (si : 'stru)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'implem )))));
         ([`Stoken
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
           ("([], None)\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (([], None) : 'implem )))))]));
   Gramf.extend_single (top_phrase : 'top_phrase Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Skeyword ";;"],
            ("Some (`Directive (_loc, n, dp))\n",
              (Gramf.mk_action
                 (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                    (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
         ([`Skeyword "#";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ";;"],
           ("Some (`DirectiveSimple (_loc, n))\n",
             (Gramf.mk_action
                (fun _  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                   (Some (`DirectiveSimple (_loc, n)) : 'top_phrase )))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t )); `Skeyword ";;"],
           ("Some st\n",
             (Gramf.mk_action
                (fun _  (st : 'stru)  (_loc : Locf.t)  ->
                   (Some st : 'top_phrase )))));
         ([`Stoken
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
           ("None\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (None : 'top_phrase )))))]));
   Gramf.extend_single (strus : 'strus Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc n ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc n ~c:"stru" s : 'strus )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               (3257031, (`A "stri")), "`Ant s")],
           ("mk_anti _loc n ~c:\"stru\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (mk_anti _loc n ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";;"],
           ("mk_anti _loc n ~c:\"stru\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc n ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               (3257031, (`A "stri")), "`Ant s");
          `Skeyword ";;"],
           ("mk_anti _loc n ~c:\"stru\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (mk_anti _loc n ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               (3257031, (`A "stri")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("stri",_) -> true | _ -> false)),
               (3257031, (`A "stri")), "`Ant s");
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (("stri" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t ))],
           ("st\n",
             (Gramf.mk_action
                (fun (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus )))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t )); `Skeyword ";;"],
           ("st\n",
             (Gramf.mk_action
                (fun _  (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus )))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t ));
          `Skeyword ";;";
          `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'strus)  _  (st : 'stru)  (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'strus )))));
         ([`Snterm (Gramf.obj (stru : 'stru Gramf.t )); `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'strus)  (st : 'stru)  (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'strus )))))]));
   Gramf.extend_single (stru_quot : 'stru_quot Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Directive (_loc, n, dp)\n",
              (Gramf.mk_action
                 (fun (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`Directive (_loc, n, dp) : 'stru_quot )))));
         ([`Skeyword "#";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`DirectiveSimple (_loc, n)\n",
             (Gramf.mk_action
                (fun (n : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`DirectiveSimple (_loc, n) : 'stru_quot )))));
         ([`Snterm (Gramf.obj (strus : 'strus Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'strus)  (_loc : Locf.t)  -> (x : 'stru_quot )))))]));
   Gramf.extend (stru : 'stru Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "exception";
            `Snterm
              (Gramf.obj
                 (constructor_declaration : 'constructor_declaration Gramf.t ))],
             ("`Exception (_loc, t)\n",
               (Gramf.mk_action
                  (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)  ->
                     (`Exception (_loc, t) : 'stru )))));
          ([`Skeyword "external";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Skeyword ":";
           `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Skeyword "=";
           `Snterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
            ("`External (_loc, i, t, sl)\n",
              (Gramf.mk_action
                 (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)
                     _  (_loc : Locf.t)  ->
                    (`External (_loc, i, t, sl) : 'stru )))));
          ([`Skeyword "include"; `Snterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`Include (_loc, me)\n",
              (Gramf.mk_action
                 (fun (me : 'mexp)  _  (_loc : Locf.t)  ->
                    (`Include (_loc, me) : 'stru )))));
          ([`Skeyword "module";
           `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Snterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ))],
            ("`Module (_loc, i, mb)\n",
              (Gramf.mk_action
                 (fun (mb : 'mbind0)  (i : 'a_uident)  _  (_loc : Locf.t)  ->
                    (`Module (_loc, i, mb) : 'stru )))));
          ([`Skeyword "module";
           `Skeyword "rec";
           `Snterm (Gramf.obj (mbind : 'mbind Gramf.t ))],
            ("`RecModule (_loc, mb)\n",
              (Gramf.mk_action
                 (fun (mb : 'mbind)  _  _  (_loc : Locf.t)  ->
                    (`RecModule (_loc, mb) : 'stru )))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Skeyword "=";
           `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`ModuleType (_loc, i, mt)\n",
              (Gramf.mk_action
                 (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : Locf.t)
                     -> (`ModuleType (_loc, i, mt) : 'stru )))));
          ([`Skeyword "open";
           `Snterm
             (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("`Open (_loc, (`Negative _loc), (i : vid  :>ident))\n",
              (Gramf.mk_action
                 (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                    (`Open (_loc, (`Negative _loc), (i : vid  :>ident)) : 
                    'stru )))));
          ([`Skeyword "open";
           `Skeyword "!";
           `Snterm
             (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("`Open (_loc, (`Positive _loc), (i : vid  :>ident))\n",
              (Gramf.mk_action
                 (fun (i : 'module_longident)  _  _  (_loc : Locf.t)  ->
                    (`Open (_loc, (`Positive _loc), (i : vid  :>ident)) : 
                    'stru )))));
          ([`Skeyword "type";
           `Snterm
             (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
            ("`Type (_loc, td)\n",
              (Gramf.mk_action
                 (fun (td : 'type_declaration)  _  (_loc : Locf.t)  ->
                    (`Type (_loc, td) : 'stru )))));
          ([`Skeyword "type";
           `Snterm
             (Gramf.obj (type_declaration : 'type_declaration Gramf.t ));
           `Skeyword "with";
           `Skeyword "(";
           `Snterm (Gramf.obj (string_list : 'string_list Gramf.t ));
           `Skeyword ")"],
            ("`TypeWith (_loc, t, ns)\n",
              (Gramf.mk_action
                 (fun _  (ns : 'string_list)  _  _  (t : 'type_declaration) 
                    _  (_loc : Locf.t)  -> (`TypeWith (_loc, t, ns) : 
                    'stru )))));
          ([`Skeyword "let";
           `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Skeyword "in";
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`StExp (_loc, (`LetIn (_loc, r, bi, x))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                    (_loc : Locf.t)  ->
                    ((`StExp (_loc, (`LetIn (_loc, r, bi, x))) : FAst.stru ) : 
                    'stru )))));
          ([`Skeyword "let";
           `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Snterm (Gramf.obj (bind : 'bind Gramf.t ))],
            ("match bi with\n| `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n| _ -> `Value (_loc, r, bi)\n",
              (Gramf.mk_action
                 (fun (bi : 'bind)  (r : 'opt_rec)  _  (_loc : Locf.t)  ->
                    (match bi with
                     | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                     | _ -> `Value (_loc, r, bi) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Snterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
           `Skeyword "in";
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`StExp (_loc, (`LetModule (_loc, m, mb, e))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : Locf.t)  ->
                    ((`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 
                    FAst.stru ) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm
             (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Skeyword "in";
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("let i = (i : vid  :>ident) in\n(`StExp (_loc, (`LetOpen (_loc, (`Negative _loc), i, e))) : FAst.stru )\n",
              (Gramf.mk_action
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
             (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Skeyword "in";
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("let i = (i : vid  :>ident) in\n(`StExp (_loc, (`LetOpen (_loc, (`Positive _loc), i, e))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _  _ 
                    (_loc : Locf.t)  ->
                    (let i = (i : vid  :>ident) in
                     (`StExp
                        (_loc, (`LetOpen (_loc, (`Positive _loc), i, e))) : 
                       FAst.stru ) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Skeyword "in";
           `Snterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Skeyword "with";
           `Snterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a)))\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                    (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                    (`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a))) : 
                    'stru )))));
          ([`Skeyword "class";
           `Snterm
             (Gramf.obj (class_declaration : 'class_declaration Gramf.t ))],
            ("`Class (_loc, cd)\n",
              (Gramf.mk_action
                 (fun (cd : 'class_declaration)  _  (_loc : Locf.t)  ->
                    (`Class (_loc, cd) : 'stru )))));
          ([`Skeyword "class";
           `Skeyword "type";
           `Snterm
             (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
            ("`ClassType (_loc, ctd)\n",
              (Gramf.mk_action
                 (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t)  ->
                    (`ClassType (_loc, ctd) : 'stru )))));
          ([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"stru\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"stru" n s : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
          ([`Stoken
              (((function | `Ant ("stri",_) -> true | _ -> false)),
                (3257031, (`A "stri")), "`Ant s")],
            ("mk_anti _loc ~c:\"stru\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("stri" as n),s) ->
                        (mk_anti _loc ~c:"stru" n s : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
          ([`Stoken
              (((function | `Quot _ -> true | _ -> false)),
                (904098089, `Any), "`Quot _")],
            ("Ast_quotation.expand x Dyn_tag.stru\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Quot x ->
                        (Ast_quotation.expand x Dyn_tag.stru : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
          ([`Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`StExp (_loc, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  (_loc : Locf.t)  ->
                    (`StExp (_loc, e) : 'stru )))))])]));
  (Gramf.extend_single (clsigi_quot : 'clsigi_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, x1, x2)\n",
              (Gramf.mk_action
                 (fun (x2 : 'clsigi_quot)  _  (x1 : 'clsigi)  (_loc : Locf.t)
                     -> (`Sem (_loc, x1, x2) : 'clsigi_quot )))));
         ([`Snterm (Gramf.obj (clsigi : 'clsigi Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'clsigi)  (_loc : Locf.t)  -> (x : 'clsigi_quot )))))]));
   Gramf.extend_single (class_signature : 'class_signature Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clsigi\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               (3257031, (`A "csg")), "`Ant s")],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               (3257031, (`A "csg")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               (3257031, (`A "csg")), "`Ant s");
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               (3257031, (`A "csg")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       FAst.clsigi ) : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (clsigi : 'clsigi Gramf.t ))],
           ("csg\n",
             (Gramf.mk_action
                (fun (csg : 'clsigi)  (_loc : Locf.t)  ->
                   (csg : 'class_signature )))));
         ([`Snterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); `Skeyword ";"],
           ("csg\n",
             (Gramf.mk_action
                (fun _  (csg : 'clsigi)  (_loc : Locf.t)  ->
                   (csg : 'class_signature )))));
         ([`Snterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, csg, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_signature)  _  (csg : 'clsigi) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, csg, xs) : 'class_signature )))));
         ([`Snterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); `Sself],
           ("`Sem (_loc, csg, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_signature)  (csg : 'clsigi) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, csg, xs) : 'class_signature )))))]));
   Gramf.extend_single (clsigi : 'clsigi Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clsigi\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clsigi" n s : 'clsigi )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("csg",_) -> true | _ -> false)),
               (3257031, (`A "csg")), "`Ant s")],
           ("mk_anti _loc ~c:\"clsigi\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("csg" as n),s) ->
                       (mk_anti _loc ~c:"clsigi" n s : 'clsigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.clsigi\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.clsigi : 'clsigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "inherit";
          `Snterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("`SigInherit (_loc, cs)\n",
             (Gramf.mk_action
                (fun (cs : 'cltyp)  _  (_loc : Locf.t)  ->
                   (`SigInherit (_loc, cs) : 'clsigi )))));
         ([`Skeyword "val";
          `Snterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
          `Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`CgVal (_loc, l, mf, mv, t) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) 
                   (mf : 'opt_mutable)  _  (_loc : Locf.t)  ->
                   ((`CgVal (_loc, l, mf, mv, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Skeyword "method";
          `Skeyword "virtual";
          `Snterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`VirMeth (_loc, l, pf, t) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   _  (_loc : Locf.t)  ->
                   ((`VirMeth (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Skeyword "method";
          `Snterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Method (_loc, l, pf, t) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (_loc : Locf.t)  ->
                   ((`Method (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Skeyword "constraint";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Eq (_loc, t1, t2) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Eq (_loc, t1, t2) : FAst.clsigi ) : 'clsigi )))))])));
  (Gramf.extend_single (class_structure : 'class_structure Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clfield\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               (3257031, (`A "cst")), "`Ant s")],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               (3257031, (`A "cst")), "`Ant s");
          `Skeyword ";"],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), st)\n",
             (Gramf.mk_action
                (fun (st : 'class_structure)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), st) : 
                       'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               (3257031, (`A "cst")), "`Ant s");
          `Sself],
           ("`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), st)\n",
             (Gramf.mk_action
                (fun (st : 'class_structure)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), st) : 
                       'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), cst) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), cst) : 
                       FAst.clfield ) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               (3257031, (`A "cst")), "`Ant s");
          `Skeyword ";";
          `Sself],
           ("(`Sem (_loc, (mk_anti _loc ~c:\"clfield\" n s), cst) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       ((`Sem (_loc, (mk_anti _loc ~c:"clfield" n s), cst) : 
                       FAst.clfield ) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (clfield : 'clfield Gramf.t ))],
           ("st\n",
             (Gramf.mk_action
                (fun (st : 'clfield)  (_loc : Locf.t)  ->
                   (st : 'class_structure )))));
         ([`Snterm (Gramf.obj (clfield : 'clfield Gramf.t )); `Skeyword ";"],
           ("st\n",
             (Gramf.mk_action
                (fun _  (st : 'clfield)  (_loc : Locf.t)  ->
                   (st : 'class_structure )))));
         ([`Snterm (Gramf.obj (clfield : 'clfield Gramf.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_structure)  _  (st : 'clfield) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'class_structure )))));
         ([`Snterm (Gramf.obj (clfield : 'clfield Gramf.t )); `Sself],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_structure)  (st : 'clfield) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'class_structure )))))]));
   Gramf.extend_single (clfield : 'clfield Gramf.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clfield\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clfield" n s : 'clfield )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cst",_) -> true | _ -> false)),
               (3257031, (`A "cst")), "`Ant s")],
           ("mk_anti _loc ~c:\"clfield\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cst" as n),s) ->
                       (mk_anti _loc ~c:"clfield" n s : 'clfield )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.clfield\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.clfield : 'clfield )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Skeyword "inherit";
          `Snterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
          `Snterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
           ("`Inherit (_loc, o, ce)\n",
             (Gramf.mk_action
                (fun (ce : 'clexp)  (o : 'opt_override)  _  (_loc : Locf.t) 
                   -> (`Inherit (_loc, o, ce) : 'clfield )))));
         ([`Skeyword "inherit";
          `Snterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
          `Snterm (Gramf.obj (clexp : 'clexp Gramf.t ));
          `Skeyword "as";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`InheritAs (_loc, o, ce, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (ce : 'clexp)  (o : 'opt_override) 
                   _  (_loc : Locf.t)  ->
                   (`InheritAs (_loc, o, ce, i) : 'clfield )))));
         ([`Snterm
             (Gramf.obj
                (value_val_opt_override : 'value_val_opt_override Gramf.t ));
          `Snterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Snterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))],
           ("(`CrVal (_loc, lab, o, mf, e) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (e : 'cvalue_bind)  (lab : 'a_lident) 
                   (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                   (_loc : Locf.t)  ->
                   ((`CrVal (_loc, lab, o, mf, e) : FAst.clfield ) : 
                   'clfield )))));
         ([`Snterm
             (Gramf.obj
                (value_val_opt_override : 'value_val_opt_override Gramf.t ));
          `Skeyword "virtual";
          `Snterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("match o with\n| `Negative _ -> (`VirVal (_loc, l, mf, t) : FAst.clfield )\n| _ -> raise (Streamf.Error \"override (!) is incompatible with virtual\")\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mf : 'opt_mutable)  _ 
                   (o : 'value_val_opt_override)  (_loc : Locf.t)  ->
                   (match o with
                    | `Negative _ ->
                        (`VirVal (_loc, l, mf, t) : FAst.clfield )
                    | _ ->
                        raise
                          (Streamf.Error
                             "override (!) is incompatible with virtual") : 
                   'clfield )))));
         ([`Snterm
             (Gramf.obj (method_opt_override : 'method_opt_override Gramf.t ));
          `Skeyword "virtual";
          `Snterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("match o with\n| `Negative _ -> `VirMeth (_loc, l, pf, t)\n| _ -> raise (Streamf.Error \"override (!) is incompatible with virtual\")\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (o : 'method_opt_override)  (_loc : Locf.t)  ->
                   (match o with
                    | `Negative _ -> `VirMeth (_loc, l, pf, t)
                    | _ ->
                        raise
                          (Streamf.Error
                             "override (!) is incompatible with virtual") : 
                   'clfield )))));
         ([`Snterm
             (Gramf.obj (method_opt_override : 'method_opt_override Gramf.t ));
          `Snterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Snterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
           ("`CrMth (_loc, l, o, pf, e, t)\n",
             (Gramf.mk_action
                (fun (e : 'fun_bind)  (t : 'ctyp)  _  (l : 'a_lident) 
                   (pf : 'opt_private)  (o : 'method_opt_override) 
                   (_loc : Locf.t)  ->
                   (`CrMth (_loc, l, o, pf, e, t) : 'clfield )))));
         ([`Snterm
             (Gramf.obj (method_opt_override : 'method_opt_override Gramf.t ));
          `Snterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Snterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
           ("`CrMthS (_loc, l, o, pf, e)\n",
             (Gramf.mk_action
                (fun (e : 'fun_bind)  (l : 'a_lident)  (pf : 'opt_private) 
                   (o : 'method_opt_override)  (_loc : Locf.t)  ->
                   (`CrMthS (_loc, l, o, pf, e) : 'clfield )))));
         ([`Skeyword "constraint";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Eq (_loc, t1, t2) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Eq (_loc, t1, t2) : FAst.clfield ) : 'clfield )))));
         ([`Skeyword "initializer";
          `Snterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(`Initializer (_loc, se) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (se : 'exp)  _  (_loc : Locf.t)  ->
                   ((`Initializer (_loc, se) : FAst.clfield ) : 'clfield )))))]));
   Gramf.extend_single (clfield_quot : 'clfield_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (clfield : 'clfield Gramf.t ));
           `Skeyword ";";
           `Sself],
            ("`Sem (_loc, x1, x2)\n",
              (Gramf.mk_action
                 (fun (x2 : 'clfield_quot)  _  (x1 : 'clfield) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, x1, x2) : 'clfield_quot )))));
         ([`Snterm (Gramf.obj (clfield : 'clfield Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'clfield)  (_loc : Locf.t)  -> (x : 'clfield_quot )))))])));
  (Gramf.extend_single (clexp_quot : 'clexp_quot Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'clexp)  (_loc : Locf.t)  -> (x : 'clexp_quot )))))]));
   Gramf.extend_single (class_declaration : 'class_declaration Gramf.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("`And (_loc, c1, c2)\n",
              (Gramf.mk_action
                 (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration)
                     (_loc : Locf.t)  ->
                    (`And (_loc, c1, c2) : 'class_declaration )))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"clexp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'class_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cdcl",_) -> true | _ -> false)),
               (3257031, (`A "cdcl")), "`Ant s")],
           ("mk_anti _loc ~c:\"clexp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cdcl" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'class_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword "[";
          `Snterm
            (Gramf.obj
               (comma_type_parameter : 'comma_type_parameter Gramf.t ));
          `Skeyword "]";
          `Snterm (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))],
           ("`ClDecl (_loc, mv, (i :>ident), x, ce)\n",
             (Gramf.mk_action
                (fun (ce : 'class_fun_bind)  _  (x : 'comma_type_parameter) 
                   _  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t) 
                   ->
                   (`ClDecl (_loc, mv, (i :>ident), x, ce) : 'class_declaration )))));
         ([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Snterm (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))],
           ("`ClDeclS (_loc, mv, (i :>ident), ce)\n",
             (Gramf.mk_action
                (fun (ce : 'class_fun_bind)  (i : 'a_lident) 
                   (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                   (`ClDeclS (_loc, mv, (i :>ident), ce) : 'class_declaration )))))]));
   Gramf.extend_single (class_fun_bind : 'class_fun_bind Gramf.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("ce\n",
              (Gramf.mk_action
                 (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                    (ce : 'class_fun_bind )))));
         ([`Skeyword ":";
          `Snterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
           ("`Constraint (_loc, ce, ct)\n",
             (Gramf.mk_action
                (fun (ce : 'clexp)  _  (ct : 'cltyp_plus)  _  (_loc : Locf.t)
                    -> (`Constraint (_loc, ce, ct) : 'class_fun_bind )))));
         ([`Snterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Sself],
           ("`CeFun (_loc, p, cfb)\n",
             (Gramf.mk_action
                (fun (cfb : 'class_fun_bind)  (p : 'ipat)  (_loc : Locf.t) 
                   -> (`CeFun (_loc, p, cfb) : 'class_fun_bind )))))]));
   Gramf.extend_single (class_fun_def : 'class_fun_def Gramf.t )
     (None,
       (None, None,
         [([`Snterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Sself],
            ("`CeFun (_loc, p, ce)\n",
              (Gramf.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  (_loc : Locf.t)  ->
                    (`CeFun (_loc, p, ce) : 'class_fun_def )))));
         ([`Skeyword "->"; `Snterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
           ("ce\n",
             (Gramf.mk_action
                (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                   (ce : 'class_fun_def )))))]));
   Gramf.extend (clexp : 'clexp Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "fun";
            `Snterm (Gramf.obj (ipat : 'ipat Gramf.t ));
            `Snterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
             ("`CeFun (_loc, p, ce)\n",
               (Gramf.mk_action
                  (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : Locf.t)
                      -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Skeyword "function";
           `Snterm (Gramf.obj (ipat : 'ipat Gramf.t ));
           `Snterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
            ("`CeFun (_loc, p, ce)\n",
              (Gramf.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : Locf.t) 
                    -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Skeyword "let";
           `Snterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Snterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Skeyword "in";
           `Sself],
            ("`LetIn (_loc, rf, bi, ce)\n",
              (Gramf.mk_action
                 (fun (ce : 'clexp)  _  (bi : 'bind)  (rf : 'opt_rec)  _ 
                    (_loc : Locf.t)  -> (`LetIn (_loc, rf, bi, ce) : 
                    'clexp )))))]);
       ((Some "apply"), (Some `NA),
         [([`Sself; `Snterml ((Gramf.obj (exp : 'exp Gramf.t )), "label")],
            ("`CeApp (_loc, ce, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  (ce : 'clexp)  (_loc : Locf.t)  ->
                    (`CeApp (_loc, ce, e) : 'clexp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (3257031, (`A "")), "`Ant s")],
            ("mk_anti _loc ~c:\"clexp\" n s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (("" as n),s) ->
                        (mk_anti _loc ~c:"clexp" n s : 'clexp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s"
                             (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Ant ("cexp",_) -> true | _ -> false)),
               (3257031, (`A "cexp")), "`Ant s")],
           ("mk_anti _loc ~c:\"clexp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("cexp" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'clexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Stoken
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.clexp\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (Ast_quotation.expand x Dyn_tag.clexp : 'clexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
         ([`Snterm (Gramf.obj (vid : 'vid Gramf.t ));
          `Skeyword "[";
          `Snterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
          `Skeyword "]"],
           ("`ClApply (_loc, ci, t)\n",
             (Gramf.mk_action
                (fun _  (t : 'comma_ctyp)  _  (ci : 'vid)  (_loc : Locf.t) 
                   -> (`ClApply (_loc, ci, t) : 'clexp )))));
         ([`Snterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(ci :>clexp)\n",
             (Gramf.mk_action
                (fun (ci : 'vid)  (_loc : Locf.t)  ->
                   ((ci :>clexp) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ")";
          `Snterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, p, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, p)\n",
             (Gramf.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPatEnd (_loc, p) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")";
          `Snterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Skeyword "end"],
           ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Skeyword ":";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
             (Gramf.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : Locf.t) 
                   ->
                   (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'clexp )))));
         ([`Skeyword "object";
          `Snterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Skeyword "end"],
           ("`Obj (_loc, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : Locf.t)  ->
                   (`Obj (_loc, cst) : 'clexp )))));
         ([`Skeyword "object"; `Skeyword "end"],
           ("`ObjEnd _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'clexp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gramf.obj (cltyp : 'cltyp Gramf.t ));
          `Skeyword ")"],
           ("`Constraint (_loc, ce, ct)\n",
             (Gramf.mk_action
                (fun _  (ct : 'cltyp)  _  (ce : 'clexp)  _  (_loc : Locf.t) 
                   -> (`Constraint (_loc, ce, ct) : 'clexp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("ce\n",
             (Gramf.mk_action
                (fun _  (ce : 'clexp)  _  (_loc : Locf.t)  -> (ce : 'clexp )))))])]));
  Gramf.extend_single (class_description : 'class_description Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("`And (_loc, cd1, cd2)\n",
             (Gramf.mk_action
                (fun (cd2 : 'class_description)  _ 
                   (cd1 : 'class_description)  (_loc : Locf.t)  ->
                   (`And (_loc, cd1, cd2) : 'class_description )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'class_description )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'class_description )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword "[";
         `Snterm
           (Gramf.obj (comma_type_parameter : 'comma_type_parameter Gramf.t ));
         `Skeyword "]";
         `Skeyword ":";
         `Snterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))],
          ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp_plus)  _  _  (x : 'comma_type_parameter)  _ 
                  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'class_description )))));
        ([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword ":";
         `Snterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))],
          ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp_plus)  _  (i : 'a_lident) 
                  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDeclS (_loc, mv, (i :>ident), ct) : 'class_description )))))]));
  Gramf.extend_single (cltyp_declaration : 'cltyp_declaration Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("`And (_loc, cd1, cd2)\n",
             (Gramf.mk_action
                (fun (cd2 : 'cltyp_declaration)  _ 
                   (cd1 : 'cltyp_declaration)  (_loc : Locf.t)  ->
                   (`And (_loc, cd1, cd2) : 'cltyp_declaration )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword "[";
         `Snterm
           (Gramf.obj (comma_type_parameter : 'comma_type_parameter Gramf.t ));
         `Skeyword "]";
         `Skeyword "=";
         `Snterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
          ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp)  _  _  (x : 'comma_type_parameter)  _ 
                  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'cltyp_declaration )))));
        ([`Snterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword "=";
         `Snterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
          ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp)  _  (i : 'a_lident)  (mv : 'opt_virtual) 
                  (_loc : Locf.t)  ->
                  (`CtDeclS (_loc, mv, (i :>ident), ct) : 'cltyp_declaration )))))]));
  Gramf.extend_single (cltyp_quot : 'cltyp_quot Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'cltyp)  (_loc : Locf.t)  -> (x : 'cltyp_quot )))))]));
  Gramf.extend_single (cltyp_plus : 'cltyp_plus Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "]";
          `Skeyword "->";
          `Sself],
           ("`CtFun (_loc, t, ct)\n",
             (Gramf.mk_action
                (fun (ct : 'cltyp_plus)  _  _  (t : 'ctyp)  _ 
                   (_loc : Locf.t)  -> (`CtFun (_loc, t, ct) : 'cltyp_plus )))));
        ([`Snterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
          ("ct\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp)  (_loc : Locf.t)  -> (ct : 'cltyp_plus )))))]));
  Gramf.extend_single (cltyp : 'cltyp Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"cltyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"cltyp" n s : 'cltyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("ctyp",_) -> true | _ -> false)),
              (3257031, (`A "ctyp")), "`Ant s")],
          ("mk_anti _loc ~c:\"cltyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("ctyp" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.cltyp\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x ->
                      (Ast_quotation.expand x Dyn_tag.cltyp : 'cltyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (vid : 'vid Gramf.t ));
         `Skeyword "[";
         `Snterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
         `Skeyword "]"],
          ("`ClApply (_loc, i, t)\n",
            (Gramf.mk_action
               (fun _  (t : 'comma_ctyp)  _  (i : 'vid)  (_loc : Locf.t)  ->
                  (`ClApply (_loc, i, t) : 'cltyp )))));
        ([`Snterm (Gramf.obj (vid : 'vid Gramf.t ))],
          ("(i :>cltyp)\n",
            (Gramf.mk_action
               (fun (i : 'vid)  (_loc : Locf.t)  -> ((i :>cltyp) : 'cltyp )))));
        ([`Skeyword "object";
         `Skeyword "(";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Skeyword ")";
         `Snterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
         `Skeyword "end"],
          ("`ObjTy (_loc, t, csg)\n",
            (Gramf.mk_action
               (fun _  (csg : 'class_signature)  _  (t : 'ctyp)  _  _ 
                  (_loc : Locf.t)  -> (`ObjTy (_loc, t, csg) : 'cltyp )))));
        ([`Skeyword "object";
         `Snterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
         `Skeyword "end"],
          ("`Obj (_loc, csg)\n",
            (Gramf.mk_action
               (fun _  (csg : 'class_signature)  _  (_loc : Locf.t)  ->
                  (`Obj (_loc, csg) : 'cltyp )))));
        ([`Skeyword "object";
         `Skeyword "(";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Skeyword ")"],
          ("`ObjTyEnd (_loc, t)\n",
            (Gramf.mk_action
               (fun _  (t : 'ctyp)  _  _  (_loc : Locf.t)  ->
                  (`ObjTyEnd (_loc, t) : 'cltyp )))));
        ([`Skeyword "object"; `Skeyword "end"],
          ("`ObjEnd _loc\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'cltyp )))))]))
let apply_ctyp () =
  Gramf.extend_single (ctyp_quot : 'ctyp_quot Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "*";
          `Snterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ))],
           ("`Sta (_loc, x, y)\n",
             (Gramf.mk_action
                (fun (y : 'star_ctyp)  _  (x : 'ctyp)  (_loc : Locf.t)  ->
                   (`Sta (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("x\n",
            (Gramf.mk_action
               (fun (x : 'ctyp)  (_loc : Locf.t)  -> (x : 'ctyp_quot )))))]));
  Gramf.extend_single (unquoted_typevars : 'unquoted_typevars Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Sself],
           ("`App (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) 
                   (_loc : Locf.t)  ->
                   (`App (_loc, t1, t2) : 'unquoted_typevars )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x ->
                      (Ast_quotation.expand x Dyn_tag.ctyp : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i :>ctyp)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'unquoted_typevars )))))]));
  Gramf.extend_single (type_parameter : 'type_parameter Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc n s : 'type_parameter )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc n s : 'type_parameter )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Skeyword "'"; `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Normal _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'type_parameter )))));
        ([`Skeyword "+";
         `Skeyword "'";
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Positive _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Positive _loc), i) : 'type_parameter )))));
        ([`Skeyword "-";
         `Skeyword "'";
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Negative _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Negative _loc), i) : 'type_parameter )))));
        ([`Skeyword "+"; `Skeyword "_"],
          ("`QuoteAny (_loc, (`Positive _loc))\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (`QuoteAny (_loc, (`Positive _loc)) : 'type_parameter )))));
        ([`Skeyword "-"; `Skeyword "_"],
          ("`QuoteAny (_loc, (`Negative _loc))\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (`QuoteAny (_loc, (`Negative _loc)) : 'type_parameter )))));
        ([`Skeyword "_"],
          ("`Any _loc\n",
            (Gramf.mk_action
               (fun _  (_loc : Locf.t)  -> (`Any _loc : 'type_parameter )))))]));
  Gramf.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Gramf.obj (type_parameters : 'type_parameters Gramf.t ));
          `Skeyword ")";
          `Snterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("tpl (i :>ctyp)\n",
             (Gramf.mk_action
                (fun (i : 'type_longident)  _  (tpl : 'type_parameters)  _ 
                   (_loc : Locf.t)  ->
                   (tpl (i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Snterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
         `Snterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
          ("`App (_loc, (i :>ctyp), (tpl :>ctyp))\n",
            (Gramf.mk_action
               (fun (i : 'type_longident)  (tpl : 'type_parameter) 
                  (_loc : Locf.t)  ->
                  (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters )))));
        ([`Snterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
          ("(i :>ctyp)\n",
            (Gramf.mk_action
               (fun (i : 'type_longident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc n s ~c:\"ctyp\"\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc n s ~c:"ctyp" : 'type_longident_and_parameters )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))))]));
  Gramf.extend_single (type_parameters : 'type_parameters Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
          `Sself],
           ("fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp)))\n",
             (Gramf.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : Locf.t)  ->
                   (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 
                   'type_parameters )))));
        ([`Snterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))],
          ("fun acc  -> `App (_loc, acc, (t :>ctyp))\n",
            (Gramf.mk_action
               (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                  (fun acc  -> `App (_loc, acc, (t :>ctyp)) : 'type_parameters )))));
        ([],
          ("fun t  -> t\n",
            (Gramf.mk_action
               (fun (_loc : Locf.t)  -> (fun t  -> t : 'type_parameters )))))]));
  Gramf.extend_single (meth_list : 'meth_list Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
          `Skeyword ";";
          `Sself],
           ("let (ml,v) = rest in ((`Sem (_loc, m, ml)), v)\n",
             (Gramf.mk_action
                (fun (rest : 'meth_list)  _  (m : 'meth_decl) 
                   (_loc : Locf.t)  ->
                   (let (ml,v) = rest in ((`Sem (_loc, m, ml)), v) : 
                   'meth_list )))));
        ([`Snterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
         `Skeyword ";";
         `Snterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
          ("(m, v)\n",
            (Gramf.mk_action
               (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl)  (_loc : Locf.t) 
                  -> ((m, v) : 'meth_list )))));
        ([`Snterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
         `Snterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
          ("(m, v)\n",
            (Gramf.mk_action
               (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : Locf.t)  ->
                  ((m, v) : 'meth_list )))))]));
  Gramf.extend_single (meth_decl : 'meth_decl Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'meth_decl )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'meth_decl )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword ":";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyCol (_loc, lab, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, lab, t) : 'meth_decl )))))]));
  Gramf.extend_single (opt_meth_list : 'opt_meth_list Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (meth_list : 'meth_list Gramf.t ))],
           ("let (ml,v) = rest in `TyObj (_loc, ml, v)\n",
             (Gramf.mk_action
                (fun (rest : 'meth_list)  (_loc : Locf.t)  ->
                   (let (ml,v) = rest in `TyObj (_loc, ml, v) : 'opt_meth_list )))));
        ([`Snterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
          ("`TyObjEnd (_loc, v)\n",
            (Gramf.mk_action
               (fun (v : 'opt_dot_dot)  (_loc : Locf.t)  ->
                  (`TyObjEnd (_loc, v) : 'opt_meth_list )))))]));
  Gramf.extend_single (row_field : 'row_field Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'row_field )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("vrn",_) -> true | _ -> false)),
              (3257031, (`A "vrn")), "`Ant s")],
          ("`TyVrn (_loc, (mk_anti _loc ~c:\"ctyp\" n s))\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("vrn" as n),s) ->
                      (`TyVrn (_loc, (mk_anti _loc ~c:"ctyp" n s)) : 
                      'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("vrn",_) -> true | _ -> false)),
              (3257031, (`A "vrn")), "`Ant s");
         `Skeyword "of";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyVrnOf (_loc, (mk_anti _loc ~c:\"ctyp\" n s), t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("vrn" as n),s) ->
                      (`TyVrnOf (_loc, (mk_anti _loc ~c:"ctyp" n s), t) : 
                      'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("`Bar (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'row_field)  _  (t1 : 'row_field)  (_loc : Locf.t) 
                  -> (`Bar (_loc, t1, t2) : 'row_field )))));
        ([`Skeyword "`"; `Snterm (Gramf.obj (astr : 'astr Gramf.t ))],
          ("`TyVrn (_loc, i)\n",
            (Gramf.mk_action
               (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrn (_loc, i) : 'row_field )))));
        ([`Skeyword "`";
         `Snterm (Gramf.obj (astr : 'astr Gramf.t ));
         `Skeyword "of";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyVrnOf (_loc, i, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrnOf (_loc, i, t) : 'row_field )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`Ctyp (_loc, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, t) : 'row_field )))))]));
  Gramf.extend_single (name_tags : 'name_tags Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'name_tags )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'name_tags )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Sself; `Sself],
          ("`App (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : Locf.t)  ->
                  (`App (_loc, t1, t2) : 'name_tags )))));
        ([`Skeyword "`"; `Snterm (Gramf.obj (astr : 'astr Gramf.t ))],
          ("`TyVrn (_loc, i)\n",
            (Gramf.mk_action
               (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrn (_loc, i) : 'name_tags )))))]));
  Gramf.extend_single (type_declaration : 'type_declaration Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'type_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'type_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "and"; `Sself],
          ("`And (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) 
                  (_loc : Locf.t)  ->
                  (`And (_loc, t1, t2) : 'type_declaration )))));
        ([`Snterm
            (Gramf.obj
               (type_ident_and_parameters : 'type_ident_and_parameters
                                              Gramf.t ));
         `Skeyword "=";
         `Snterm (Gramf.obj (type_info : 'type_info Gramf.t ));
         `Slist0 (`Snterm (Gramf.obj (constrain : 'constrain Gramf.t )))],
          ("let (n,tpl) = rest in\n`TyDcl\n  (_loc, n, tpl, tk,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n",
            (Gramf.mk_action
               (fun (cl : 'constrain list)  (tk : 'type_info)  _ 
                  (rest : 'type_ident_and_parameters)  (_loc : Locf.t)  ->
                  (let (n,tpl) = rest in
                   `TyDcl
                     (_loc, n, tpl, tk,
                       (match cl with
                        | [] -> `None _loc
                        | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))));
        ([`Snterm
            (Gramf.obj
               (type_ident_and_parameters : 'type_ident_and_parameters
                                              Gramf.t ));
         `Slist0 (`Snterm (Gramf.obj (constrain : 'constrain Gramf.t )))],
          ("let (n,tpl) = rest in\n`TyAbstr\n  (_loc, n, tpl,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n",
            (Gramf.mk_action
               (fun (cl : 'constrain list) 
                  (rest : 'type_ident_and_parameters)  (_loc : Locf.t)  ->
                  (let (n,tpl) = rest in
                   `TyAbstr
                     (_loc, n, tpl,
                       (match cl with
                        | [] -> `None _loc
                        | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))))]));
  Gramf.extend_single (type_info : 'type_info Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyRepr (_loc, (`Negative _loc), t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'type_repr)  (_loc : Locf.t)  ->
                   (`TyRepr (_loc, (`Negative _loc), t2) : 'type_info )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Skeyword "=";
         `Snterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
          ("`TyMan (_loc, t1, (`Negative _loc), t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_repr)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                  (`TyMan (_loc, t1, (`Negative _loc), t2) : 'type_info )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyEq (_loc, (`Negative _loc), t1)\n",
            (Gramf.mk_action
               (fun (t1 : 'ctyp)  (_loc : Locf.t)  ->
                  (`TyEq (_loc, (`Negative _loc), t1) : 'type_info )))));
        ([`Skeyword "private"; `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyEq (_loc, (`Positive _loc), t1)\n",
            (Gramf.mk_action
               (fun (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                  (`TyEq (_loc, (`Positive _loc), t1) : 'type_info )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Skeyword "=";
         `Skeyword "private";
         `Snterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
          ("`TyMan (_loc, t1, (`Positive _loc), t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_repr)  _  _  (t1 : 'ctyp)  (_loc : Locf.t) 
                  -> (`TyMan (_loc, t1, (`Positive _loc), t2) : 'type_info )))));
        ([`Skeyword "private";
         `Snterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
          ("`TyRepr (_loc, (`Positive _loc), t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_repr)  _  (_loc : Locf.t)  ->
                  (`TyRepr (_loc, (`Positive _loc), t2) : 'type_info )))))]));
  Gramf.extend_single (type_repr : 'type_repr Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Snterm
            (Gramf.obj
               (constructor_declarations : 'constructor_declarations Gramf.t ))],
           ("`Sum (_loc, t)\n",
             (Gramf.mk_action
                (fun (t : 'constructor_declarations)  _  (_loc : Locf.t)  ->
                   (`Sum (_loc, t) : 'type_repr )))));
        ([`Skeyword "{";
         `Snterm
           (Gramf.obj
              (label_declaration_list : 'label_declaration_list Gramf.t ));
         `Skeyword "}"],
          ("`Record (_loc, t)\n",
            (Gramf.mk_action
               (fun _  (t : 'label_declaration_list)  _  (_loc : Locf.t)  ->
                  (`Record (_loc, t) : 'type_repr )))))]));
  Gramf.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Slist1sep
            ((`Snterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))),
              (`Skeyword ","));
          `Skeyword ")";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i, (`Some (_loc, (com_of_list (tpl :>decl_params list)))))\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (tpl : 'type_parameter list)  _ 
                   (_loc : Locf.t)  ->
                   ((i,
                      (`Some (_loc, (com_of_list (tpl :>decl_params list))))) : 
                   'type_ident_and_parameters )))));
        ([`Snterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i, (`Some (_loc, (t :>decl_params))))\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (t : 'type_parameter)  (_loc : Locf.t) 
                  ->
                  ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters )))));
        ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i, (`None _loc))\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i, (`None _loc)) : 'type_ident_and_parameters )))))]));
  Gramf.extend_single (constrain : 'constrain Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "constraint";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Skeyword "=";
          `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`Eq (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   (`Eq (_loc, t1, t2) : 'constrain )))))]));
  Gramf.extend_single (typevars : 'typevars Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Sself],
           ("`App (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : Locf.t)  ->
                   (`App (_loc, t1, t2) : 'typevars )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x ->
                      (Ast_quotation.expand x Dyn_tag.ctyp : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Skeyword "'"; `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Normal _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'typevars )))))]));
  Gramf.extend (ctyp : 'ctyp Gramf.t )
    (None,
      [((Some "alias"), (Some `LA),
         [([`Sself;
           `Skeyword "as";
           `Skeyword "'";
           `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Alias (_loc, t1, i)\n",
              (Gramf.mk_action
                 (fun (i : 'a_lident)  _  _  (t1 : 'ctyp)  (_loc : Locf.t) 
                    -> (`Alias (_loc, t1, i) : 'ctyp )))))]);
      ((Some "forall"), (Some `LA),
        [([`Skeyword "!";
          `Snterm (Gramf.obj (typevars : 'typevars Gramf.t ));
          `Skeyword ".";
          `Sself],
           ("`TyPol (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _  (_loc : Locf.t) 
                   -> (`TyPol (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "arrow"), (Some `RA),
        [([`Sself; `Skeyword "->"; `Sself],
           ("`Arrow (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`Arrow (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "label"), (Some `NA),
        [([`Skeyword "~";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Skeyword ":";
          `Sself],
           ("`Label (_loc, i, t)\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Label (_loc, i, t) : 'ctyp )))));
        ([`Stoken
            (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
              "`Label s");
         `Skeyword ":";
         `Sself],
          ("`Label (_loc, (`Lid (_loc, s)), t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Label ({ txt = s;_} : Tokenf.txt) ->
                      (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Optlabel _ -> true | _ -> false)),
              (688526593, `Any), "`Optlabel s");
         `Sself],
          ("`OptLabl (_loc, (`Lid (_loc, s)), t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Optlabel ({ txt = s;_} : Tokenf.txt) ->
                      (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Skeyword "?";
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword ":";
         `Sself],
          ("`OptLabl (_loc, i, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`OptLabl (_loc, i, t) : 'ctyp )))))]);
      ((Some "apply"), (Some `LA),
        [([`Sself; `Sself],
           ("`App (_loc, t2, t1)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`App (_loc, t2, t1) : 'ctyp )))))]);
      ((Some "simple"), None,
        [([`Skeyword "'";
          `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Quote (_loc, (`Normal _loc), i) : 'ctyp )))));
        ([`Skeyword "_"],
          ("`Any _loc\n",
            (Gramf.mk_action
               (fun _  (_loc : Locf.t)  -> (`Any _loc : 'ctyp )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("par",_) -> true | _ -> false)),
              (3257031, (`A "par")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("par" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("id",_) -> true | _ -> false)),
              (3257031, (`A "id")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("id" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("id",_) -> true | _ -> false)),
              (3257031, (`A "id")), "`Ant s");
         `Skeyword ".";
         `Sself],
          ("(try\n   let id = ident_of_ctyp t in\n   fun ()  -> (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), id) : ctyp )\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("id" as n),s) ->
                      (((try
                           let id = ident_of_ctyp t in
                           fun ()  ->
                             (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), id) : 
                             ctyp )
                         with
                         | Invalid_argument s ->
                             (fun ()  -> raise (Streamf.Error s)))) () : 
                      'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x -> (Ast_quotation.expand x Dyn_tag.ctyp : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Skeyword ".";
         `Sself],
          ("(try let id = ident_of_ctyp t in fun ()  -> `Dot (_loc, (i :>ident), id)\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_uident)  (_loc : Locf.t)  ->
                  ((try
                      let id = ident_of_ctyp t in
                      fun ()  -> `Dot (_loc, (i :>ident), id)
                    with
                    | Invalid_argument s ->
                        (fun ()  -> raise (Streamf.Error s))) () : 'ctyp )))));
        ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i :>ctyp)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'ctyp )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword "*";
         `Snterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ));
         `Skeyword ")"],
          ("`Par (_loc, (`Sta (_loc, t, tl)))\n",
            (Gramf.mk_action
               (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _  (_loc : Locf.t) 
                  -> (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("t\n",
            (Gramf.mk_action
               (fun _  (t : 'ctyp)  _  (_loc : Locf.t)  -> (t : 'ctyp )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword ",";
         `Snterm (Gramf.obj (com_ctyp : 'com_ctyp Gramf.t ));
         `Skeyword ")";
         `Snterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
          ("appl_of_list ((j :>ctyp) :: t :: (Ast_basic.list_of_com tl []))\n",
            (Gramf.mk_action
               (fun (j : 'type_longident)  _  (tl : 'com_ctyp)  _ 
                  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                  (appl_of_list ((j :>ctyp) :: t ::
                     (Ast_basic.list_of_com tl [])) : 'ctyp )))));
        ([`Skeyword "[";
         `Snterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Skeyword "]"],
          ("`PolyEq (_loc, rfl)\n",
            (Gramf.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolyEq (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[>";
         `Snterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Skeyword "]"],
          ("`PolySup (_loc, rfl)\n",
            (Gramf.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolySup (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Skeyword "]"],
          ("`PolyInf (_loc, rfl)\n",
            (Gramf.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolyInf (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Skeyword ">";
         `Snterm (Gramf.obj (name_tags : 'name_tags Gramf.t ));
         `Skeyword "]"],
          ("`PolyInfSup (_loc, rfl, ntl)\n",
            (Gramf.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                  (_loc : Locf.t)  -> (`PolyInfSup (_loc, rfl, ntl) : 
                  'ctyp )))));
        ([`Skeyword "#";
         `Snterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
          ("`ClassPath (_loc, i)\n",
            (Gramf.mk_action
               (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                  (`ClassPath (_loc, i) : 'ctyp )))));
        ([`Skeyword "<";
         `Snterm (Gramf.obj (opt_meth_list : 'opt_meth_list Gramf.t ));
         `Skeyword ">"],
          ("t\n",
            (Gramf.mk_action
               (fun _  (t : 'opt_meth_list)  _  (_loc : Locf.t)  ->
                  (t : 'ctyp )))));
        ([`Skeyword "(";
         `Skeyword "module";
         `Snterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
         `Skeyword ")"],
          ("`Package (_loc, p)\n",
            (Gramf.mk_action
               (fun _  (p : 'mtyp)  _  _  (_loc : Locf.t)  ->
                  (`Package (_loc, p) : 'ctyp )))))])]);
  Gramf.extend_single (comma_ctyp : 'comma_ctyp Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("`Com (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : Locf.t)  -> (`Com (_loc, t1, t2) : 'comma_ctyp )))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (3257031, (`A "")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp,\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("" as n),s) ->
                      (mk_anti _loc ~c:"ctyp," n s : 'comma_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`Ctyp (_loc, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, t) : 'comma_ctyp )))))]));
  Gramf.extend_single (com_ctyp : 'com_ctyp Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'com_ctyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'com_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Sself; `Skeyword ","; `Sself],
          ("`Com (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'com_ctyp)  _  (t1 : 'com_ctyp)  (_loc : Locf.t) 
                  -> (`Com (_loc, t1, t2) : 'com_ctyp )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("t\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'com_ctyp )))))]));
  Gramf.extend_single (star_ctyp : 'star_ctyp Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'star_ctyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'star_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "*"; `Sself],
          ("`Sta (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp)  (_loc : Locf.t) 
                  -> (`Sta (_loc, t1, t2) : 'star_ctyp )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("t\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'star_ctyp )))))]));
  Gramf.extend_single
    (constructor_declarations : 'constructor_declarations Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'constructor_declarations )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'constructor_declarations )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("`Bar (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'constructor_declarations)  _ 
                  (t1 : 'constructor_declarations)  (_loc : Locf.t)  ->
                  (`Bar (_loc, t1, t2) : 'constructor_declarations )))));
        ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Skeyword "of";
         `Snterm
           (Gramf.obj (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
          ("`Of (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : Locf.t)  ->
                  (`Of (_loc, s, t) : 'constructor_declarations )))));
        ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Skeyword ":";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyCol (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, s, t) : 'constructor_declarations )))));
        ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
          ("(s :>or_ctyp)\n",
            (Gramf.mk_action
               (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                  ((s :>or_ctyp) : 'constructor_declarations )))))]));
  Gramf.extend_single
    (constructor_declaration : 'constructor_declaration Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'constructor_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'constructor_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Skeyword "of";
         `Snterm
           (Gramf.obj (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
          ("`Of (_loc, (s :>vid), t)\n",
            (Gramf.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : Locf.t)  ->
                  (`Of (_loc, (s :>vid), t) : 'constructor_declaration )))));
        ([`Snterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
          ("(s :>of_ctyp)\n",
            (Gramf.mk_action
               (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                  ((s :>of_ctyp) : 'constructor_declaration )))))]));
  Gramf.extend_single (constructor_arg_list : 'constructor_arg_list Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "*"; `Sself],
           ("`Sta (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'constructor_arg_list)  _ 
                   (t1 : 'constructor_arg_list)  (_loc : Locf.t)  ->
                   (`Sta (_loc, t1, t2) : 'constructor_arg_list )))));
        ([`Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("t\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (t : 'constructor_arg_list )))))]));
  Gramf.extend_single
    (label_declaration_list : 'label_declaration_list Gramf.t )
    (None,
      (None, None,
        [([`Snterm
             (Gramf.obj (label_declaration : 'label_declaration Gramf.t ));
          `Skeyword ";";
          `Sself],
           ("`Sem (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'label_declaration_list)  _ 
                   (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                   (`Sem (_loc, t1, t2) : 'label_declaration_list )))));
        ([`Snterm
            (Gramf.obj (label_declaration : 'label_declaration Gramf.t ));
         `Skeyword ";"],
          ("t1\n",
            (Gramf.mk_action
               (fun _  (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                  (t1 : 'label_declaration_list )))));
        ([`Snterm
            (Gramf.obj (label_declaration : 'label_declaration Gramf.t ))],
          ("t1\n",
            (Gramf.mk_action
               (fun (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                  (t1 : 'label_declaration_list )))))]));
  Gramf.extend_single (label_declaration : 'label_declaration Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (3257031, (`A "")), "`Ant s")],
           ("mk_anti _loc ~c:\"ctyp\" n s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (("" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'label_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("typ",_) -> true | _ -> false)),
              (3257031, (`A "typ")), "`Ant s")],
          ("mk_anti _loc ~c:\"ctyp\" n s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (("typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'label_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))));
        ([`Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword ":";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyCol (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, s, t) : 'label_declaration )))));
        ([`Skeyword "mutable";
         `Snterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Skeyword ":";
         `Snterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyColMut (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`TyColMut (_loc, s, t) : 'label_declaration )))))]));
  Gramf.extend_single (comma_type_parameter : 'comma_type_parameter Gramf.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("`Com (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'comma_type_parameter)  _ 
                   (t1 : 'comma_type_parameter)  (_loc : Locf.t)  ->
                   (`Com (_loc, t1, t2) : 'comma_type_parameter )))));
        ([`Snterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))],
          ("`Ctyp (_loc, (t :>ctyp))\n",
            (Gramf.mk_action
               (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter )))))]))
let fill_parsers =
  let applied = ref false in
  fun ()  ->
    if not applied.contents then (applied := true; apply (); apply_ctyp ())
let () = Ast_parsers.register_parser ("revise", fill_parsers)