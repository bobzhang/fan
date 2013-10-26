let mk_ant = Tokenf.mk_ant
open FAst
open Ast_gen
open Fan_ops
open! Syntaxf
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
        | Some (`Ant (({ kind = "list";_} as n) : Tokenf.ant)) ->
            (Streamf.junk __strm; mk_ant ~c:"exp;" n)
        | _ -> symb1 __strm in
      let rec kont al (__strm : _ Streamf.t) =
        match Streamf.peek __strm with
        | Some (`Key ({ txt = ";";_} : Tokenf.txt)) ->
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
         [([`Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'mexp)  (_loc : Locf.t)  -> (x : 'mexp_quot )))))]));
   Gramf.extend (mbind0 : 'mbind0 Gramf.t )
     (None,
       [(None, (Some `RA),
          [([`Keyword "(";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Keyword ":";
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
            `Keyword ")";
            `Self],
             ("`Functor (_loc, m, mt, mb)\n",
               (Gramf.mk_action
                  (fun (mb : 'mbind0)  _  (mt : 'mtyp)  _  (m : 'a_uident)  _
                      (_loc : Locf.t)  ->
                     (`Functor (_loc, m, mt, mb) : 'mbind0 )))));
          ([`Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`Constraint (_loc, me, mt)\n",
              (Gramf.mk_action
                 (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                    (`Constraint (_loc, me, mt) : 'mbind0 )))));
          ([`Keyword "="; `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("me\n",
              (Gramf.mk_action
                 (fun (me : 'mexp)  _  (_loc : Locf.t)  -> (me : 'mbind0 )))))])]);
   Gramf.extend (mexp : 'mexp Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Keyword "functor";
            `Keyword "(";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Keyword ":";
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
            `Keyword ")";
            `Keyword "->";
            `Self],
             ("`Functor (_loc, i, t, me)\n",
               (Gramf.mk_action
                  (fun (me : 'mexp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : Locf.t)  ->
                     (`Functor (_loc, i, t, me) : 'mexp )))));
          ([`Keyword "struct";
           `Nterm (Gramf.obj (strus : 'strus Gramf.t ));
           `Keyword "end"],
            ("`Struct (_loc, st)\n",
              (Gramf.mk_action
                 (fun _  (st : 'strus)  _  (_loc : Locf.t)  ->
                    (`Struct (_loc, st) : 'mexp )))));
          ([`Keyword "struct"; `Keyword "end"],
            ("`StructEnd _loc\n",
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  -> (`StructEnd _loc : 'mexp )))))]);
       ((Some "apply"), None,
         [([`Self; `Self],
            ("`App (_loc, me1, me2)\n",
              (Gramf.mk_action
                 (fun (me2 : 'mexp)  (me1 : 'mexp)  (_loc : Locf.t)  ->
                    (`App (_loc, me1, me2) : 'mexp )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"mexp\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"mexp" s : 'mexp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "mexp";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "mexp")), "`Ant s")],
           ("mk_ant ~c:\"mexp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "mexp";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mexp" s : 'mexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
           ("(i :>mexp)\n",
             (Gramf.mk_action
                (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                   ((i :>mexp) : 'mexp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`Constraint (_loc, me, mt)\n",
             (Gramf.mk_action
                (fun _  (mt : 'mtyp)  _  (me : 'mexp)  _  (_loc : Locf.t)  ->
                   (`Constraint (_loc, me, mt) : 'mexp )))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("me\n",
             (Gramf.mk_action
                (fun _  (me : 'mexp)  _  (_loc : Locf.t)  -> (me : 'mexp )))));
         ([`Keyword "(";
          `Keyword "val";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("`PackageModule (_loc, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                   (`PackageModule (_loc, e) : 'mexp )))));
         ([`Keyword "(";
          `Keyword "val";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p)))))\n",
             (Gramf.mk_action
                (fun _  (p : 'mtyp)  _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                   (`PackageModule
                      (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                   'mexp )))))])]));
  (Gramf.extend_single (mbind_quot : 'mbind_quot Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'mbind_quot)  _  (b1 : 'mbind_quot) 
                    (_loc : Locf.t)  -> (`And (_loc, b1, b2) : 'mbind_quot )))));
         ([`Token
             (((function
                | `Ant ({ kind = "mbind";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "mbind")), "`Ant s")],
           ("mk_ant ~c:\"mbind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "mbind";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mbind" s : 'mbind_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"mbind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mbind" s : 'mbind_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`Constraint (_loc, m, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t)  ->
                   (`Constraint (_loc, m, mt) : 'mbind_quot )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
           ("`ModuleBind (_loc, m, mt, me)\n",
             (Gramf.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : Locf.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind_quot )))))]));
   Gramf.extend_single (mbind : 'mbind Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'mbind)  _  (b1 : 'mbind)  (_loc : Locf.t)  ->
                    (`And (_loc, b1, b2) : 'mbind )))));
         ([`Token
             (((function
                | `Ant ({ kind = "mbind";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "mbind")), "`Ant s")],
           ("mk_ant ~c:\"mbind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "mbind";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mbind" s : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"mbind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mbind" s : 'mbind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
           ("`ModuleBind (_loc, m, mt, me)\n",
             (Gramf.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : Locf.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind )))))]));
   Gramf.extend_single
     (module_rec_declaration : 'module_rec_declaration Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, m1, m2)\n",
              (Gramf.mk_action
                 (fun (m2 : 'module_rec_declaration)  _ 
                    (m1 : 'module_rec_declaration)  (_loc : Locf.t)  ->
                    (`And (_loc, m1, m2) : 'module_rec_declaration )))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"mbind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mbind" s : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "mbind";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "mbind")), "`Ant s")],
           ("mk_ant ~c:\"mbind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "mbind";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mbind" s : 'module_rec_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`Constraint (_loc, m, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t)  ->
                   (`Constraint (_loc, m, mt) : 'module_rec_declaration )))))])));
  (Gramf.extend_single (constr_quot : 'constr_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (constr : 'constr Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'constr)  (_loc : Locf.t)  -> (x : 'constr_quot )))))]));
   Gramf.extend_single (constr : 'constr Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, wc1, wc2)\n",
              (Gramf.mk_action
                 (fun (wc2 : 'constr)  _  (wc1 : 'constr)  (_loc : Locf.t) 
                    -> (`And (_loc, wc1, wc2) : 'constr )))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"constr\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"constr" s : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "constr";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "constr")), "`Ant s")],
           ("mk_ant ~c:\"constr\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "constr";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"constr" s : 'constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "type";
          `Nterm
            (Gramf.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TypeEq (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : Locf.t)  -> (`TypeEq (_loc, t1, t2) : 'constr )))));
         ([`Keyword "type";
          `Nterm
            (Gramf.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gramf.t ));
          `Keyword "=";
          `Keyword "private";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TypeEqPriv (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  _ 
                   (t1 : 'type_longident_and_parameters)  _  (_loc : Locf.t) 
                   -> (`TypeEqPriv (_loc, t1, t2) : 'constr )))));
         ([`Keyword "type";
          `Nterm
            (Gramf.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gramf.t ));
          `Keyword ":=";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TypeSubst (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : Locf.t)  ->
                   (`TypeSubst (_loc, t1, t2) : 'constr )))));
         ([`Keyword "module";
          `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Keyword "=";
          `Nterm
            (Gramf.obj
               (module_longident_with_app : 'module_longident_with_app
                                              Gramf.t ))],
           ("`ModuleEq (_loc, (i1 : vid  :>ident), i2)\n",
             (Gramf.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`ModuleEq (_loc, (i1 : vid  :>ident), i2) : 'constr )))));
         ([`Keyword "module";
          `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Keyword ":=";
          `Nterm
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
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"sigi" s : 'sigis )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "sigi")), "`Ant s")],
           ("mk_ant ~c:\"sigi\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "sigi";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"sigi" s : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";;";
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "sigi")), "`Ant s");
          `Keyword ";;";
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "sigi";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "sigi")), "`Ant s");
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
             (Gramf.mk_action
                (fun (sg : 'sigis)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "sigi";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";;"; `Self],
           ("`Sem (_loc, sg, s)\n",
             (Gramf.mk_action
                (fun (s : 'sigis)  _  (sg : 'sigi)  (_loc : Locf.t)  ->
                   (`Sem (_loc, sg, s) : 'sigis )))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";;"],
           ("sg\n",
             (Gramf.mk_action
                (fun _  (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis )))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Self],
           ("`Sem (_loc, sg, s)\n",
             (Gramf.mk_action
                (fun (s : 'sigis)  (sg : 'sigi)  (_loc : Locf.t)  ->
                   (`Sem (_loc, sg, s) : 'sigis )))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ))],
           ("sg\n",
             (Gramf.mk_action
                (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis )))))]));
   Gramf.extend (mtyp : 'mtyp Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Keyword "functor";
            `Keyword "(";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Keyword ":";
            `Self;
            `Keyword ")";
            `Keyword "->";
            `Self],
             ("`Functor (_loc, i, t, mt)\n",
               (Gramf.mk_action
                  (fun (mt : 'mtyp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : Locf.t)  ->
                     (`Functor (_loc, i, t, mt) : 'mtyp )))))]);
       ((Some "with"), None,
         [([`Self;
           `Keyword "with";
           `Nterm (Gramf.obj (constr : 'constr Gramf.t ))],
            ("`With (_loc, mt, wc)\n",
              (Gramf.mk_action
                 (fun (wc : 'constr)  _  (mt : 'mtyp)  (_loc : Locf.t)  ->
                    (`With (_loc, mt, wc) : 'mtyp )))))]);
       ((Some "apply"), None,
         [([`Self; `Self],
            ("match (mt1, mt2) with\n| ((#ident as i1),(#ident as i2)) -> apply i1 i2\n| _ -> raise Streamf.NotConsumed\n",
              (Gramf.mk_action
                 (fun (mt2 : 'mtyp)  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                    (match (mt1, mt2) with
                     | ((#ident as i1),(#ident as i2)) -> apply i1 i2
                     | _ -> raise Streamf.NotConsumed : 'mtyp )))))]);
       ((Some "."), None,
         [([`Self; `Keyword "."; `Self],
            ("let acc0 mt1 mt2 =\n  match (mt1, mt2) with\n  | ((#ident as i1),(#ident as i2)) -> dot i1 i2\n  | _ -> raise Streamf.NotConsumed in\nacc0 mt1 mt2\n",
              (Gramf.mk_action
                 (fun (mt2 : 'mtyp)  _  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                    (let acc0 mt1 mt2 =
                       match (mt1, mt2) with
                       | ((#ident as i1),(#ident as i2)) -> dot i1 i2
                       | _ -> raise Streamf.NotConsumed in
                     acc0 mt1 mt2 : 'mtyp )))))]);
       ((Some "sig"), None,
         [([`Keyword "sig";
           `Nterm (Gramf.obj (sigis : 'sigis Gramf.t ));
           `Keyword "end"],
            ("`Sig (_loc, sg)\n",
              (Gramf.mk_action
                 (fun _  (sg : 'sigis)  _  (_loc : Locf.t)  ->
                    (`Sig (_loc, sg) : 'mtyp )))));
         ([`Keyword "sig"; `Keyword "end"],
           ("`SigEnd _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`SigEnd _loc : 'mtyp )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"mtyp\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"mtyp" s : 'mtyp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "mtyp";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "mtyp")), "`Ant s")],
           ("mk_ant ~c:\"mtyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "mtyp";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"mtyp" s : 'mtyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm
             (Gramf.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gramf.t ))],
           ("(i : ident  :>mtyp)\n",
             (Gramf.mk_action
                (fun (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                   ((i : ident  :>mtyp) : 'mtyp )))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("mt\n",
             (Gramf.mk_action
                (fun _  (mt : 'mtyp)  _  (_loc : Locf.t)  -> (mt : 'mtyp )))));
         ([`Keyword "module";
          `Keyword "type";
          `Keyword "of";
          `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
           ("`ModuleTypeOf (_loc, me)\n",
             (Gramf.mk_action
                (fun (me : 'mexp)  _  _  _  (_loc : Locf.t)  ->
                   (`ModuleTypeOf (_loc, me) : 'mtyp )))))])]);
   Gramf.extend_single (module_declaration : 'module_declaration Gramf.t )
     (None,
       (None, None,
         [([`Keyword ":"; `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("mt\n",
              (Gramf.mk_action
                 (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                    (mt : 'module_declaration )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")";
          `Self],
           ("`Functor (_loc, i, t, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'module_declaration)  _  (t : 'mtyp)  _ 
                   (i : 'a_uident)  _  (_loc : Locf.t)  ->
                   (`Functor (_loc, i, t, mt) : 'module_declaration )))))]));
   Gramf.extend_single (mtyp_quot : 'mtyp_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'mtyp)  (_loc : Locf.t)  -> (x : 'mtyp_quot )))))])));
  (Gramf.extend_single (sigi_quot : 'sigi_quot Gramf.t )
     (None,
       (None, None,
         [([`Keyword "#"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`DirectiveSimple (_loc, s)\n",
              (Gramf.mk_action
                 (fun (s : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`DirectiveSimple (_loc, s) : 'sigi_quot )))));
         ([`Keyword "#";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Directive (_loc, s, dp)\n",
             (Gramf.mk_action
                (fun (dp : 'exp)  (s : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Directive (_loc, s, dp) : 'sigi_quot )))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";"; `Self],
           ("`Sem (_loc, sg1, sg2)\n",
             (Gramf.mk_action
                (fun (sg2 : 'sigi_quot)  _  (sg1 : 'sigi)  (_loc : Locf.t) 
                   -> (`Sem (_loc, sg1, sg2) : 'sigi_quot )))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ))],
           ("sg\n",
             (Gramf.mk_action
                (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigi_quot )))))]));
   Gramf.extend_single (sigi : 'sigi Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"sigi" s : 'sigi )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "sigi")), "`Ant s")],
           ("mk_ant ~c:\"sigi\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "sigi";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"sigi" s : 'sigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "exception";
          `Nterm
            (Gramf.obj
               (constructor_declaration : 'constructor_declaration Gramf.t ))],
           ("(`Exception (_loc, t) : FAst.sigi )\n",
             (Gramf.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)  ->
                   ((`Exception (_loc, t) : FAst.sigi ) : 'sigi )))));
         ([`Keyword "external";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
           ("`External (_loc, i, t, sl)\n",
             (Gramf.mk_action
                (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident) 
                   _  (_loc : Locf.t)  ->
                   (`External (_loc, i, t, sl) : 'sigi )))));
         ([`Keyword "include"; `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`Include (_loc, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                   (`Include (_loc, mt) : 'sigi )))));
         ([`Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Nterm
            (Gramf.obj (module_declaration : 'module_declaration Gramf.t ))],
           ("`Module (_loc, i, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'module_declaration)  (i : 'a_uident)  _ 
                   (_loc : Locf.t)  -> (`Module (_loc, i, mt) : 'sigi )))));
         ([`Keyword "module";
          `Keyword "rec";
          `Nterm
            (Gramf.obj
               (module_rec_declaration : 'module_rec_declaration Gramf.t ))],
           ("`RecModule (_loc, mb)\n",
             (Gramf.mk_action
                (fun (mb : 'module_rec_declaration)  _  _  (_loc : Locf.t) 
                   -> (`RecModule (_loc, mb) : 'sigi )))));
         ([`Keyword "module";
          `Keyword "type";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
           ("`ModuleType (_loc, i, mt)\n",
             (Gramf.mk_action
                (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : Locf.t) 
                   -> (`ModuleType (_loc, i, mt) : 'sigi )))));
         ([`Keyword "module";
          `Keyword "type";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
           ("`ModuleTypeEnd (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleTypeEnd (_loc, i) : 'sigi )))));
         ([`Keyword "open";
          `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
           ("`Open (_loc, (`Negative _loc), (i : vid  :>ident))\n",
             (Gramf.mk_action
                (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                   (`Open (_loc, (`Negative _loc), (i : vid  :>ident)) : 
                   'sigi )))));
         ([`Keyword "open";
          `Keyword "!";
          `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
           ("`Open (_loc, (`Positive _loc), (i : vid  :>ident))\n",
             (Gramf.mk_action
                (fun (i : 'module_longident)  _  _  (_loc : Locf.t)  ->
                   (`Open (_loc, (`Positive _loc), (i : vid  :>ident)) : 
                   'sigi )))));
         ([`Keyword "type";
          `Nterm (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
           ("`Type (_loc, t)\n",
             (Gramf.mk_action
                (fun (t : 'type_declaration)  _  (_loc : Locf.t)  ->
                   (`Type (_loc, t) : 'sigi )))));
         ([`Keyword "val";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`Val (_loc, i, t)\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Val (_loc, i, t) : 'sigi )))));
         ([`Keyword "class";
          `Nterm
            (Gramf.obj (class_description : 'class_description Gramf.t ))],
           ("`Class (_loc, cd)\n",
             (Gramf.mk_action
                (fun (cd : 'class_description)  _  (_loc : Locf.t)  ->
                   (`Class (_loc, cd) : 'sigi )))));
         ([`Keyword "class";
          `Keyword "type";
          `Nterm
            (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
           ("`ClassType (_loc, ctd)\n",
             (Gramf.mk_action
                (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t)  ->
                   (`ClassType (_loc, ctd) : 'sigi )))))]));
   Gramf.extend_single (interf : 'interf Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";;"; `Self],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Gramf.mk_action
                 (fun (rest : 'interf)  _  (si : 'sigi)  (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'interf )))));
         ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Self],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Gramf.mk_action
                (fun (rest : 'interf)  (si : 'sigi)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'interf )))));
         ([`Token
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
         [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Keyword ",";
           `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ))],
            ("`Com (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`Com (_loc, e1, e2) : 'exp_quot )))));
         ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ";";
          `Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ))],
           ("`Sem (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun (e2 : 'sem_exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`Sem (_loc, e1, e2) : 'exp_quot )))));
         ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'exp_quot )))))]));
   Gramf.extend_single (cvalue_bind : 'cvalue_bind Gramf.t )
     (None,
       (None, None,
         [([`Keyword "="; `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'cvalue_bind )))));
         ([`Keyword ":";
          `Keyword "type";
          `Nterm
            (Gramf.obj (unquoted_typevars : 'unquoted_typevars Gramf.t ));
          `Keyword ".";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("let u: FAst.ctyp = `TyPol (_loc, t1, t2) in\n(`Constraint (_loc, e, u) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _ 
                   (t1 : 'unquoted_typevars)  _  _  (_loc : Locf.t)  ->
                   (let u: FAst.ctyp = `TyPol (_loc, t1, t2) in
                    (`Constraint (_loc, e, u) : FAst.exp ) : 'cvalue_bind )))));
         ([`Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(`Constraint (_loc, e, t) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, e, t) : FAst.exp ) : 'cvalue_bind )))));
         ([`Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ":>";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("match t with\n| (`TyPol (_loc,_,_) : FAst.ctyp) ->\n    raise (Streamf.Error \"unexpected polytype here\")\n| _ -> (`Coercion (_loc, e, t, t2) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _ 
                   (_loc : Locf.t)  ->
                   (match t with
                    | (`TyPol (_loc,_,_) : FAst.ctyp) ->
                        raise (Streamf.Error "unexpected polytype here")
                    | _ -> (`Coercion (_loc, e, t, t2) : FAst.exp ) : 
                   'cvalue_bind )))));
         ([`Keyword ":>";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Subtype (_loc, e, t)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                   (`Subtype (_loc, e, t) : 'cvalue_bind )))))]));
   Gramf.extend (fun_bind : 'fun_bind Gramf.t )
     (None,
       [(None, (Some `RA),
          [([`Keyword "(";
            `Keyword "type";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Keyword ")";
            `Self],
             ("`LocalTypeFun (_loc, i, e)\n",
               (Gramf.mk_action
                  (fun (e : 'fun_bind)  _  (i : 'a_lident)  _  _ 
                     (_loc : Locf.t)  ->
                     (`LocalTypeFun (_loc, i, e) : 'fun_bind )))));
          ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Self],
            ("`Fun (_loc, (`Case (_loc, p, e)))\n",
              (Gramf.mk_action
                 (fun (e : 'fun_bind)  (p : 'ipat)  (_loc : Locf.t)  ->
                    (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_bind )))));
          ([`Nterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))],
            ("bi\n",
              (Gramf.mk_action
                 (fun (bi : 'cvalue_bind)  (_loc : Locf.t)  ->
                    (bi : 'fun_bind )))))])]);
   Gramf.extend_single (lang : 'lang Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
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
              ((`Nterm (Gramf.obj (name_space : 'name_space Gramf.t ))),
                (`Keyword ";"))],
            ("let old = Ast_quotation.map.contents in\nAst_quotation.map := (Mapf.String.add_list xys old); old\n",
              (Gramf.mk_action
                 (fun (xys : 'name_space list)  (_loc : Locf.t)  ->
                    (let old = Ast_quotation.map.contents in
                     Ast_quotation.map := (Mapf.String.add_list xys old); old : 
                    'pos_exps )))))]));
   Gramf.extend_single (name_space : 'name_space Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid x");
           `Keyword ":";
           `Nterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (fun_def_pat : 'fun_def_pat Gramf.t )
     (None,
       (None, None,
         [([`Keyword "(";
           `Keyword "type";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ")"],
            ("fun e  -> `LocalTypeFun (_loc, i, e)\n",
              (Gramf.mk_action
                 (fun _  (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                    (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat )))));
         ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t ))],
           ("fun e  -> `Fun (_loc, (`Case (_loc, p, e)))\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  (_loc : Locf.t)  ->
                   (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 'fun_def_pat )))));
         ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
          `Keyword "when";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e)))\n",
             (Gramf.mk_action
                (fun (w : 'exp)  _  (p : 'ipat)  (_loc : Locf.t)  ->
                   (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 
                   'fun_def_pat )))))]));
   Gramf.extend (fun_def : 'fun_def Gramf.t )
     (None,
       [(None, (Some `RA),
          [([`Nterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t ));
            `Keyword "->";
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("f e\n",
               (Gramf.mk_action
                  (fun (e : 'exp)  _  (f : 'fun_def_pat)  (_loc : Locf.t)  ->
                     (f e : 'fun_def )))));
          ([`Nterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t )); `Self],
            ("f e\n",
              (Gramf.mk_action
                 (fun (e : 'fun_def)  (f : 'fun_def_pat)  (_loc : Locf.t)  ->
                    (f e : 'fun_def )))))])]);
   Gramf.extend (exp : 'exp Gramf.t )
     (None,
       [((Some "top"), (Some `RA),
          [([`Keyword "let";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Keyword "in";
            `Self],
             ("`LetIn (_loc, r, bi, x)\n",
               (Gramf.mk_action
                  (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                     (_loc : Locf.t)  -> (`LetIn (_loc, r, bi, x) : 'exp )))));
          ([`Keyword "let";
           `Keyword "module";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
           `Keyword "in";
           `Self],
            ("`LetModule (_loc, m, mb, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : Locf.t)  -> (`LetModule (_loc, m, mb, e) : 
                    'exp )))));
          ([`Keyword "let";
           `Keyword "open";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "in";
           `Self],
            ("`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e) : 
                    'exp )))));
          ([`Keyword "let";
           `Keyword "open";
           `Keyword "!";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "in";
           `Self],
            ("`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _  _ 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e) : 
                    'exp )))));
          ([`Keyword "let";
           `Keyword "try";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Keyword "in";
           `Self;
           `Keyword "with";
           `Nterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`LetTryInWith (_loc, r, bi, x, a)\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                    (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                    (`LetTryInWith (_loc, r, bi, x, a) : 'exp )))));
          ([`Keyword "match";
           `Self;
           `Keyword "with";
           `Nterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`Match (_loc, e, a)\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Match (_loc, e, a) : 'exp )))));
          ([`Keyword "try";
           `Self;
           `Keyword "with";
           `Nterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`Try (_loc, e, a)\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Try (_loc, e, a) : 'exp )))));
          ([`Keyword "if";
           `Self;
           `Keyword "then";
           `Self;
           `Keyword "else";
           `Self],
            ("`IfThenElse (_loc, e1, e2, e3)\n",
              (Gramf.mk_action
                 (fun (e3 : 'exp)  _  (e2 : 'exp)  _  (e1 : 'exp)  _ 
                    (_loc : Locf.t)  ->
                    (`IfThenElse (_loc, e1, e2, e3) : 'exp )))));
          ([`Keyword "if"; `Self; `Keyword "then"; `Self],
            ("`IfThen (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : Locf.t)  ->
                    (`IfThen (_loc, e1, e2) : 'exp )))));
          ([`Keyword "do";
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Keyword "done"],
            ("`Seq (_loc, seq)\n",
              (Gramf.mk_action
                 (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                    (`Seq (_loc, seq) : 'exp )))));
          ([`Keyword "with";
           `Nterm (Gramf.obj (lang : 'lang Gramf.t ));
           `Self],
            ("Ast_quotation.default := old; x\n",
              (Gramf.mk_action
                 (fun (x : 'exp)  (old : 'lang)  _  (_loc : Locf.t)  ->
                    (Ast_quotation.default := old; x : 'exp )))));
          ([`Keyword "with";
           `Keyword "{";
           `Nterm (Gramf.obj (pos_exps : 'pos_exps Gramf.t ));
           `Keyword "}";
           `Self],
            ("Ast_quotation.map := old; x\n",
              (Gramf.mk_action
                 (fun (x : 'exp)  _  (old : 'pos_exps)  _  _  (_loc : Locf.t)
                     -> (Ast_quotation.map := old; x : 'exp )))));
          ([`Keyword "for";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword "=";
           `Self;
           `Nterm (Gramf.obj (flag : 'flag Gramf.t ));
           `Self;
           `Keyword "do";
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Keyword "done"],
            ("`For (_loc, i, e1, e2, df, seq)\n",
              (Gramf.mk_action
                 (fun _  (seq : 'sequence)  _  (e2 : 'exp)  (df : 'flag) 
                    (e1 : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`For (_loc, i, e1, e2, df, seq) : 'exp )))));
          ([`Keyword "while";
           `Self;
           `Keyword "do";
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Keyword "done"],
            ("`While (_loc, e, seq)\n",
              (Gramf.mk_action
                 (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : Locf.t)
                     -> (`While (_loc, e, seq) : 'exp )))))]);
       ((Some ":="), (Some `NA),
         [([`Self; `Keyword ":="; `Self],
            ("(`Assign (_loc, (`Field (_loc, e1, (`Lid (_loc, \"contents\")))), e2) : \nFAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    ((`Assign
                        (_loc,
                          (`Field (_loc, e1, (`Lid (_loc, "contents")))), e2) : 
                    FAst.exp ) : 'exp )))));
         ([`Self; `Keyword "<-"; `Self],
           ("match Fan_ops.bigarray_set _loc e1 e2 with\n| Some e -> e\n| None  -> `Assign (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (match Fan_ops.bigarray_set _loc e1 e2 with
                    | Some e -> e
                    | None  -> `Assign (_loc, e1, e2) : 'exp )))))]);
       ((Some "||"), (Some `RA),
         [([`Self; `Keyword "or"; `Self],
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "||"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]);
       ((Some "&&"), (Some `RA),
         [([`Self; `Keyword "&"; `Self],
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "&&"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]);
       ((Some "<"), (Some `LA),
         [([`Self; `Nterm (Gramf.obj (infixop2 : 'infixop2 Gramf.t )); `Self],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop2)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "^"), (Some `RA),
         [([`Self; `Nterm (Gramf.obj (infixop3 : 'infixop3 Gramf.t )); `Self],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop3)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "::"), (Some `RA),
         [([`Self; `Keyword "::"; `Self],
            ("(`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, (`Uid (_loc, "::")), e1)), e2) : 
                    FAst.exp ) : 'exp )))))]);
       ((Some "+"), (Some `LA),
         [([`Self; `Nterm (Gramf.obj (infixop4 : 'infixop4 Gramf.t )); `Self],
            ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop4)  (e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                    'exp )))))]);
       ((Some "*"), (Some `LA),
         [([`Self; `Keyword "land"; `Self],
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "lor"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "lxor"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "mod"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Nterm (Gramf.obj (infixop5 : 'infixop5 Gramf.t )); `Self],
           ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (op : 'infixop5)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                   'exp )))))]);
       ((Some "**"), (Some `RA),
         [([`Self; `Keyword "asr"; `Self],
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "lsl"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Keyword "lsr"; `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Self; `Nterm (Gramf.obj (infixop6 : 'infixop6 Gramf.t )); `Self],
           ("(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  (op : 'infixop6)  (e1 : 'exp) 
                   (_loc : Locf.t)  ->
                   ((`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                   'exp )))))]);
       ((Some "obj"), (Some `RA),
         [([`Keyword "fun";
           `Keyword "|";
           `Slist1sep
             ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))), (`Keyword "|"))],
            ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
              (Gramf.mk_action
                 (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                    (let cases = bar_of_list a in `Fun (_loc, cases) : 
                    'exp )))));
         ([`Keyword "function";
          `Keyword "|";
          `Slist1sep
            ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))), (`Keyword "|"))],
           ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
             (Gramf.mk_action
                (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                   (let cases = bar_of_list a in `Fun (_loc, cases) : 
                   'exp )))));
         ([`Keyword "fun"; `Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Keyword "function";
          `Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Keyword "end"],
           ("`ObjPat (_loc, p, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'exp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ")";
          `Keyword "end"],
           ("`ObjPatEnd (_loc, p)\n",
             (Gramf.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPatEnd (_loc, p) : 'exp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Keyword "end"],
           ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'exp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")";
          `Keyword "end"],
           ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
             (Gramf.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : Locf.t) 
                   -> (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                   'exp )))));
         ([`Keyword "object";
          `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Keyword "end"],
           ("`Obj (_loc, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : Locf.t)  ->
                   (`Obj (_loc, cst) : 'exp )))));
         ([`Keyword "object"; `Keyword "end"],
           ("`ObjEnd _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'exp )))))]);
       ((Some "unary minus"), (Some `NA),
         [([`Keyword "-"; `Self],
            ("Fan_ops.mkumin _loc \"-\" e\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                    (Fan_ops.mkumin _loc "-" e : 'exp )))));
         ([`Keyword "-."; `Self],
           ("Fan_ops.mkumin _loc \"-.\" e\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (Fan_ops.mkumin _loc "-." e : 'exp )))))]);
       ((Some "apply"), (Some `LA),
         [([`Self; `Self],
            ("`App (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'exp)  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`App (_loc, e1, e2) : 'exp )))));
         ([`Keyword "assert"; `Self],
           ("`Assert (_loc, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Assert (_loc, e) : 'exp )))));
         ([`Keyword "new";
          `Nterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
           ("`New (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                   (`New (_loc, i) : 'exp )))));
         ([`Keyword "lazy"; `Self],
           ("`Lazy (_loc, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Lazy (_loc, e) : 'exp )))))]);
       ((Some "label"), (Some `NA),
         [([`Keyword "~";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Self],
            ("`Label (_loc, i, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`Label (_loc, i, e) : 'exp )))));
         ([`Keyword "~"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'exp )))));
         ([`Token
             (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
               "`Label i");
          `Self],
           ("(`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label ({ txt = i;_} : Tokenf.txt) ->
                       ((`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp ) : 
                       'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Self],
           ("`OptLabl (_loc, (`Lid (_loc, i)), e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Self],
           ("`OptLabl (_loc, i, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, i, e) : 'exp )))));
         ([`Keyword "?"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'exp )))))]);
       ((Some "."), (Some `LA),
         [([`Self; `Keyword "."; `Keyword "("; `Self; `Keyword ")"],
            ("`ArrayDot (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                    (`ArrayDot (_loc, e1, e2) : 'exp )))));
         ([`Self; `Keyword "."; `Keyword "["; `Self; `Keyword "]"],
           ("`StringDot (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`StringDot (_loc, e1, e2) : 'exp )))));
         ([`Self;
          `Keyword ".";
          `Keyword "{";
          `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
          `Keyword "}"],
           ("Fan_ops.bigarray_get _loc e1 e2\n",
             (Gramf.mk_action
                (fun _  (e2 : 'comma_exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)
                    -> (Fan_ops.bigarray_get _loc e1 e2 : 'exp )))));
         ([`Self; `Keyword "."; `Self],
           ("`Field (_loc, e1, e2)\n",
             (Gramf.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : Locf.t)  ->
                   (`Field (_loc, e1, e2) : 'exp )))));
         ([`Self;
          `Keyword "#";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Send (_loc, e, lab)\n",
             (Gramf.mk_action
                (fun (lab : 'a_lident)  _  (e : 'exp)  (_loc : Locf.t)  ->
                   (`Send (_loc, e, lab) : 'exp )))))]);
       ((Some "~-"), (Some `NA),
         [([`Keyword "!"; `Self],
            ("`Field (_loc, e, (`Lid (_loc, \"contents\")))\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                    (`Field (_loc, e, (`Lid (_loc, "contents"))) : 'exp )))));
         ([`Nterm (Gramf.obj (prefixop : 'prefixop Gramf.t )); `Self],
           ("`App (_loc, f, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (f : 'prefixop)  (_loc : Locf.t)  ->
                   (`App (_loc, f, e) : 'exp )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function | `Quot _ -> true | _ -> false)),
                (904098089, `Any), "`Quot _")],
            ("Ast_quotation.expand x Dyn_tag.exp\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Quot x -> (Ast_quotation.expand x Dyn_tag.exp : 'exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "exp";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "exp")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "exp";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "par")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "par";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "seq";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "seq")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "seq";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "chr";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "chr")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "chr";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int32";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int32")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int32";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "str";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "str")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "str";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int64";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int64")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int64";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "flo";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "flo")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "flo";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "nativeint";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "nativeint")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "nativeint";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "vrn")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "vrn";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "chr'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "chr'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "chr'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int64'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int64'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int64'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "nativeint'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "nativeint'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "nativeint'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "bool'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "bool'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "bool'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int32'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int32'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int32'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "flo'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "flo'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "flo'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "str'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "str'")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "str'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`chr";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`chr")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`chr";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`int64";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`int64")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`int64";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`nativeint";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`nativeint")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`nativeint";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`bool";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`bool")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`bool";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`int";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`int")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`int";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`int32";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`int32")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`int32";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`flo";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`flo")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`flo";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`str";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`str")), "`Ant s")],
           ("mk_ant ~c:\"exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`str";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"exp" s : 'exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Try
             (`Nterm
                (Gramf.obj
                   (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                    Gramf.t )));
          `Self;
          `Keyword ")"],
           ("`LetOpen (_loc, (`Negative _loc), i, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  (i : 'module_longident_dot_lparen) 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Negative _loc), i, e) : 'exp )))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(i : vid  :>exp)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>exp) : 'exp )))));
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("`Vrn (_loc, s)\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   (`Vrn (_loc, s) : 'exp )))));
         ([`Keyword "["; `Keyword "]"],
           ("(`Uid (_loc, \"[]\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
         ([`Keyword "[";
          `Nterm (Gramf.obj (sem_exp_for_list : 'sem_exp_for_list Gramf.t ));
          `Keyword "]"],
           ("mk_list (`Uid (_loc, \"[]\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  (mk_list : 'sem_exp_for_list)  _  (_loc : Locf.t)  ->
                   (mk_list (`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
         ([`Keyword "[|"; `Keyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'exp )))));
         ([`Keyword "[|";
          `Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ));
          `Keyword "|]"],
           ("`Array (_loc, el)\n",
             (Gramf.mk_action
                (fun _  (el : 'sem_exp)  _  (_loc : Locf.t)  ->
                   (`Array (_loc, el) : 'exp )))));
         ([`Keyword "{";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x");
          `Keyword "with";
          `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
          `Keyword "}"],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "{";
          `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
          `Keyword "}"],
           ("`Record (_loc, el)\n",
             (Gramf.mk_action
                (fun _  (el : 'label_exp_list)  _  (_loc : Locf.t)  ->
                   (`Record (_loc, el) : 'exp )))));
         ([`Keyword "{";
          `Keyword "(";
          `Self;
          `Keyword ")";
          `Keyword "with";
          `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
          `Keyword "}"],
           ("`RecordWith (_loc, el, e)\n",
             (Gramf.mk_action
                (fun _  (el : 'label_exp_list)  _  _  (e : 'exp)  _  _ 
                   (_loc : Locf.t)  -> (`RecordWith (_loc, el, e) : 'exp )))));
         ([`Keyword "{<"; `Keyword ">}"],
           ("`OvrInstEmpty _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`OvrInstEmpty _loc : 'exp )))));
         ([`Keyword "{<";
          `Nterm (Gramf.obj (field_exp_list : 'field_exp_list Gramf.t ));
          `Keyword ">}"],
           ("`OvrInst (_loc, fel)\n",
             (Gramf.mk_action
                (fun _  (fel : 'field_exp_list)  _  (_loc : Locf.t)  ->
                   (`OvrInst (_loc, fel) : 'exp )))));
         ([`Keyword "("; `Keyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("`Constraint (_loc, e, t)\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Constraint (_loc, e, t) : 'exp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ",";
          `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
          `Keyword ")"],
           ("`Par (_loc, (`Com (_loc, e, el)))\n",
             (Gramf.mk_action
                (fun _  (el : 'comma_exp)  _  (e : 'exp)  _  (_loc : Locf.t) 
                   -> (`Par (_loc, (`Com (_loc, e, el))) : 'exp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ";";
          `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
          `Keyword ")"],
           ("`Seq (_loc, (`Sem (_loc, e, seq)))\n",
             (Gramf.mk_action
                (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : Locf.t) 
                   -> (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp )))));
         ([`Keyword "("; `Self; `Keyword ";"; `Keyword ")"],
           ("`Seq (_loc, e)\n",
             (Gramf.mk_action
                (fun _  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Seq (_loc, e) : 'exp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ":>";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("`Coercion (_loc, e, t, t2)\n",
             (Gramf.mk_action
                (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'exp)  _ 
                   (_loc : Locf.t)  -> (`Coercion (_loc, e, t, t2) : 
                   'exp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":>";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("`Subtype (_loc, e, t)\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                   (`Subtype (_loc, e, t) : 'exp )))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("e\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'exp )))));
         ([`Keyword "begin";
          `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
          `Keyword "end"],
           ("`Seq (_loc, seq)\n",
             (Gramf.mk_action
                (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                   (`Seq (_loc, seq) : 'exp )))));
         ([`Keyword "begin"; `Keyword "end"],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
          `Keyword ")"],
           ("`Package_exp (_loc, me)\n",
             (Gramf.mk_action
                (fun _  (me : 'mexp)  _  _  (_loc : Locf.t)  ->
                   (`Package_exp (_loc, me) : 'exp )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`Package_exp (_loc, (`Constraint (_loc, me, pt)))\n",
             (Gramf.mk_action
                (fun _  (pt : 'mtyp)  _  (me : 'mexp)  _  _  (_loc : Locf.t) 
                   ->
                   (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 
                   'exp )))))])]);
   Gramf.extend_single (sem_exp_for_list : 'sem_exp_for_list Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (exp : 'exp Gramf.t )); `Keyword ";"; `Self],
            ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), (el acc)) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun (el : 'sem_exp_for_list)  _  (e : 'exp) 
                    (_loc : Locf.t)  ->
                    (fun acc  ->
                       (`App
                          (_loc, (`App (_loc, (`Uid (_loc, "::")), e)),
                            (el acc)) : FAst.exp ) : 'sem_exp_for_list )))));
         ([`Nterm (Gramf.obj (exp : 'exp Gramf.t )); `Keyword ";"],
           ("fun acc  ->\n  (`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : FAst.exp )\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      (`App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                      FAst.exp ) : 'sem_exp_for_list )))));
         ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
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
         [([`Keyword "let";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k (`LetIn (_loc, rf, bi, e))\n",
              (Gramf.mk_action
                 (fun (k : 'sequence')  (e : 'exp)  _  (bi : 'bind) 
                    (rf : 'opt_rec)  _  (_loc : Locf.t)  ->
                    (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
         ([`Keyword "let";
          `Keyword "try";
          `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
          `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
          `Keyword "in";
          `Self;
          `Keyword "with";
          `Nterm (Gramf.obj (case : 'case Gramf.t ));
          `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
           ("k (`LetTryInWith (_loc, r, bi, x, a))\n",
             (Gramf.mk_action
                (fun (k : 'sequence')  (a : 'case)  _  (x : 'sequence)  _ 
                   (bi : 'bind)  (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                   (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence )))));
         ([`Keyword "let";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
          `Keyword "in";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
           ("k (`LetModule (_loc, m, mb, e))\n",
             (Gramf.mk_action
                (fun (k : 'sequence')  (e : 'exp)  _  (mb : 'mbind0) 
                   (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
         ([`Keyword "let";
          `Keyword "open";
          `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Keyword "in";
          `Self],
           ("`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e)\n",
             (Gramf.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Negative _loc), (i : vid  :>ident), e) : 
                   'sequence )))));
         ([`Keyword "let";
          `Keyword "open";
          `Keyword "!";
          `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
          `Keyword "in";
          `Self],
           ("`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e)\n",
             (Gramf.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _  _ 
                   (_loc : Locf.t)  ->
                   (`LetOpen (_loc, (`Positive _loc), (i : vid  :>ident), e) : 
                   'sequence )))));
         ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
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
         ([`Keyword ";"],
           ("fun e  -> e\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Keyword ";"; `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ))],
           ("fun e  -> `Sem (_loc, e, el)\n",
             (Gramf.mk_action
                (fun (el : 'sequence)  _  (_loc : Locf.t)  ->
                   (fun e  -> `Sem (_loc, e, el) : 'sequence' )))))]));
   Gramf.extend_single (comma_exp : 'comma_exp Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword ","; `Self],
            ("`Com (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'comma_exp) 
                    (_loc : Locf.t)  -> (`Com (_loc, e1, e2) : 'comma_exp )))));
         ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("e\n",
             (Gramf.mk_action
                (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'comma_exp )))))])));
  Gramf.extend_single (with_exp_lang : 'with_exp_lang Gramf.t )
    (None,
      (None, None,
        [([`Nterm (Gramf.obj (lang : 'lang Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("Ast_quotation.default := old; x\n",
             (Gramf.mk_action
                (fun (x : 'exp)  _  (old : 'lang)  (_loc : Locf.t)  ->
                   (Ast_quotation.default := old; x : 'with_exp_lang )))))]));
  Gramf.extend_single (with_stru_lang : 'with_stru_lang Gramf.t )
    (None,
      (None, None,
        [([`Nterm (Gramf.obj (lang : 'lang Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (stru : 'stru Gramf.t ))],
           ("Ast_quotation.default := old; x\n",
             (Gramf.mk_action
                (fun (x : 'stru)  _  (old : 'lang)  (_loc : Locf.t)  ->
                   (Ast_quotation.default := old; x : 'with_stru_lang )))))]));
  (Gramf.extend_single (bind_quot : 'bind_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (bind : 'bind Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'bind)  (_loc : Locf.t)  -> (x : 'bind_quot )))))]));
   Gramf.extend_single (bind : 'bind Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "bind";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "bind")), "`Ant s")],
            ("mk_ant ~c:\"bind\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "bind";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"bind" s : 'bind )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"bind\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"bind" s : 'bind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(`Bind (_loc, (mk_ant ~c:\"pat\" s), e) : FAst.bind )\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       ((`Bind (_loc, (mk_ant ~c:"pat" s), e) : FAst.bind ) : 
                       'bind )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Self; `Keyword "and"; `Self],
           ("`And (_loc, b1, b2)\n",
             (Gramf.mk_action
                (fun (b2 : 'bind)  _  (b1 : 'bind)  (_loc : Locf.t)  ->
                   (`And (_loc, b1, b2) : 'bind )))));
         ([`Nterm (Gramf.obj (let_bind : 'let_bind Gramf.t ))],
           ("b\n",
             (Gramf.mk_action
                (fun (b : 'let_bind)  (_loc : Locf.t)  -> (b : 'bind )))))]));
   Gramf.extend_single (let_bind : 'let_bind Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
            ("`Bind (_loc, p, e)\n",
              (Gramf.mk_action
                 (fun (e : 'fun_bind)  (p : 'pat)  (_loc : Locf.t)  ->
                    (`Bind (_loc, p, e) : 'let_bind )))))])));
  (Gramf.extend_single (case : 'case Gramf.t )
     (None,
       (None, None,
         [([`Keyword "|";
           `Slist1sep
             ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))), (`Keyword "|"))],
            ("bar_of_list l\n",
              (Gramf.mk_action
                 (fun (l : 'case0 list)  _  (_loc : Locf.t)  ->
                    (bar_of_list l : 'case )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, p, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                   (`Case (_loc, p, e) : 'case )))))]));
   Gramf.extend_single (case0 : 'case0 Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "case";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "case")), "`Ant s")],
            ("mk_ant ~c:\"case\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "case";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"case" s : 'case0 )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"case\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"case" s : 'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword "when";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, (mk_ant ~c:\"case\" s), w, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`CaseWhen (_loc, (mk_ant ~c:"case" s), w, e) : 
                       'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, (mk_ant ~c:\"case\" s), e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Case (_loc, (mk_ant ~c:"case" s), e) : 'case0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Keyword "when";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, p, w, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (p : 'pat_as_pat_opt) 
                   (_loc : Locf.t)  -> (`CaseWhen (_loc, p, w, e) : 'case0 )))));
         ([`Nterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, p, e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (p : 'pat_as_pat_opt)  (_loc : Locf.t) 
                   -> (`Case (_loc, p, e) : 'case0 )))))]));
   Gramf.extend_single (case_quot : 'case_quot Gramf.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                (`Keyword "|"))],
            ("bar_of_list x\n",
              (Gramf.mk_action
                 (fun (x : 'case0 list)  (_loc : Locf.t)  ->
                    (bar_of_list x : 'case_quot )))))])));
  (Gramf.extend_single (rec_exp_quot : 'rec_exp_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'label_exp_list)  (_loc : Locf.t)  ->
                    (x : 'rec_exp_quot )))))]));
   Gramf.extend_single (label_exp : 'label_exp Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "rec_exp";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "rec_exp")), "`Ant s")],
            ("mk_ant ~c:\"rec_exp\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "rec_exp";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"rec_exp" s : 'label_exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"rec_exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"rec_exp" s : 'label_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ));
          `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
           ("(`RecBind (_loc, i, e) : FAst.rec_exp )\n",
             (Gramf.mk_action
                (fun (e : 'fun_bind)  (i : 'label_longident)  (_loc : Locf.t)
                    ->
                   ((`RecBind (_loc, i, e) : FAst.rec_exp ) : 'label_exp )))));
         ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
           ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
             (Gramf.mk_action
                (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                   'label_exp )))))]));
   Gramf.extend_single (field_exp : 'field_exp Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"rec_exp\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"rec_exp" s : 'field_exp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "bi";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "bi")), "`Ant s")],
           ("mk_ant ~c:\"rec_exp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "bi";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"rec_exp" s : 'field_exp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`RecBind (_loc, (l :>ident), e)\n",
             (Gramf.mk_action
                (fun (e : 'exp)  _  (l : 'a_lident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, (l :>ident), e) : 'field_exp )))))]));
   Gramf.extend_single (label_exp_list : 'label_exp_list Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (label_exp : 'label_exp Gramf.t ));
           `Keyword ";";
           `Self],
            ("`Sem (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'label_exp_list)  _  (b1 : 'label_exp) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, b1, b2) : 'label_exp_list )))));
         ([`Nterm (Gramf.obj (label_exp : 'label_exp Gramf.t ));
          `Keyword ";"],
           ("b1\n",
             (Gramf.mk_action
                (fun _  (b1 : 'label_exp)  (_loc : Locf.t)  ->
                   (b1 : 'label_exp_list )))));
         ([`Nterm (Gramf.obj (label_exp : 'label_exp Gramf.t ))],
           ("b1\n",
             (Gramf.mk_action
                (fun (b1 : 'label_exp)  (_loc : Locf.t)  ->
                   (b1 : 'label_exp_list )))))]));
   Gramf.extend_single (field_exp_list : 'field_exp_list Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (field_exp : 'field_exp Gramf.t ));
           `Keyword ";";
           `Self],
            ("`Sem (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun (b2 : 'field_exp_list)  _  (b1 : 'field_exp) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, b1, b2) : 'field_exp_list )))));
         ([`Nterm (Gramf.obj (field_exp : 'field_exp Gramf.t ));
          `Keyword ";"],
           ("b1\n",
             (Gramf.mk_action
                (fun _  (b1 : 'field_exp)  (_loc : Locf.t)  ->
                   (b1 : 'field_exp_list )))));
         ([`Nterm (Gramf.obj (field_exp : 'field_exp Gramf.t ))],
           ("b1\n",
             (Gramf.mk_action
                (fun (b1 : 'field_exp)  (_loc : Locf.t)  ->
                   (b1 : 'field_exp_list )))))])));
  (let grammar_entry_create x = Gramf.mk x in
   let pat_constr: 'pat_constr Gramf.t = grammar_entry_create "pat_constr" in
   Gramf.extend_single (pat_quot : 'pat_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ",";
           `Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ))],
            ("`Com (_loc, x, y)\n",
              (Gramf.mk_action
                 (fun (y : 'comma_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                    (`Com (_loc, x, y) : 'pat_quot )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ";";
          `Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ))],
           ("`Sem (_loc, x, y)\n",
             (Gramf.mk_action
                (fun (y : 'sem_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, x, y) : 'pat_quot )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_quot )))))]));
   Gramf.extend_single (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword "as";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Alias (_loc, p1, s)\n",
              (Gramf.mk_action
                 (fun (s : 'a_lident)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Alias (_loc, p1, s) : 'pat_as_pat_opt )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_as_pat_opt )))))]));
   Gramf.extend_single (pat_constr : 'pat_constr Gramf.t )
     (None,
       (None, None,
         [([`Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("(i : vid  :>pat)\n",
              (Gramf.mk_action
                 (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                    ((i : vid  :>pat) : 'pat_constr )))));
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : pat )\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : pat ) : 'pat_constr )))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "pat")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "pat";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "vrn")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "vrn";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat_constr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend (pat : 'pat Gramf.t )
     (None,
       [((Some "|"), (Some `LA),
          [([`Self; `Keyword "|"; `Self],
             ("`Bar (_loc, p1, p2)\n",
               (Gramf.mk_action
                  (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                     (`Bar (_loc, p1, p2) : 'pat )))))]);
       ((Some ".."), (Some `NA),
         [([`Self; `Keyword ".."; `Self],
            ("`PaRng (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`PaRng (_loc, p1, p2) : 'pat )))))]);
       ((Some "::"), (Some `RA),
         [([`Self; `Keyword "::"; `Self],
            ("`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p1)), p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`App (_loc, (`App (_loc, (`Uid (_loc, "::")), p1)), p2) : 
                    'pat )))))]);
       ((Some "apply"), (Some `LA),
         [([`Nterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t )); `Self],
            ("match p2 with\n| (`Par (_loc,p) : FAst.pat) ->\n    List.fold_left (fun p1  p2  -> (`App (_loc, p1, p2) : FAst.pat )) p1\n      (Ast_basic.list_of_com p [])\n| _ -> (`App (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (p2 : 'pat)  (p1 : 'pat_constr)  (_loc : Locf.t)  ->
                    (match p2 with
                     | (`Par (_loc,p) : FAst.pat) ->
                         List.fold_left
                           (fun p1  p2  -> (`App (_loc, p1, p2) : FAst.pat ))
                           p1 (Ast_basic.list_of_com p [])
                     | _ -> (`App (_loc, p1, p2) : FAst.pat ) : 'pat )))));
         ([`Nterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t ))],
           ("p1\n",
             (Gramf.mk_action
                (fun (p1 : 'pat_constr)  (_loc : Locf.t)  -> (p1 : 'pat )))));
         ([`Keyword "lazy"; `Self],
           ("`Lazy (_loc, p)\n",
             (Gramf.mk_action
                (fun (p : 'pat)  _  (_loc : Locf.t)  ->
                   (`Lazy (_loc, p) : 'pat )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"pat" s : 'pat )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "pat")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "pat";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "par")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "par";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int32";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int32")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int32";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int64";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int64")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int64";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "vrn")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "vrn";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "flo";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "flo")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "flo";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "chr";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "chr")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "chr";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "nativeint";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "nativeint")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "nativeint";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "str";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "str")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "str";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int32'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int32'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int32'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "int64'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "int64'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "int64'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "nativeint'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "nativeint'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "nativeint'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "flo'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "flo'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "flo'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "chr'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "chr'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "chr'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "str'";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "str'")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "str'";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`int";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`int")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`int";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`int32";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`int32")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`int32";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`int64";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`int64")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`int64";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`nativeint";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`nativeint")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`nativeint";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`flo";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`flo")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`flo";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`chr";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`chr")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`chr";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "`str";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "`str")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "`str";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(i : vid  :>pat)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>pat) : 'pat )))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "-";
          `Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "-";
          `Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "-";
          `Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "-";
          `Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "-";
          `Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "["; `Keyword "]"],
           ("(`Uid (_loc, \"[]\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "[]") : FAst.pat ) : 'pat )))));
         ([`Keyword "[";
          `Nterm (Gramf.obj (sem_pat_for_list : 'sem_pat_for_list Gramf.t ));
          `Keyword "]"],
           ("mk_list (`Uid (_loc, \"[]\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (mk_list : 'sem_pat_for_list)  _  (_loc : Locf.t)  ->
                   (mk_list (`Uid (_loc, "[]") : FAst.pat ) : 'pat )))));
         ([`Keyword "[|"; `Keyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'pat )))));
         ([`Keyword "[|";
          `Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ));
          `Keyword "|]"],
           ("`Array (_loc, pl)\n",
             (Gramf.mk_action
                (fun _  (pl : 'sem_pat)  _  (_loc : Locf.t)  ->
                   (`Array (_loc, pl) : 'pat )))));
         ([`Keyword "{";
          `Nterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
          `Keyword "}"],
           ("`Record (_loc, pl)\n",
             (Gramf.mk_action
                (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                   (`Record (_loc, pl) : 'pat )))));
         ([`Keyword "("; `Keyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.pat ) : 'pat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Gramf.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'pat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'pat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Token
            (((function
               | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "opt")), "`Ant s");
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_ant s))\n",
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.t)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   match __fan_4 with
                   | `Ant (({ kind = "opt";_} as s) : Tokenf.ant) ->
                       (`ModuleConstraint (_loc, m, (mk_ant s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_4))))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("p\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'pat )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'pat )))));
         ([`Keyword "(";
          `Self;
          `Keyword "as";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   -> ((`Alias (_loc, p, s) : FAst.pat ) : 'pat )))));
         ([`Keyword "(";
          `Self;
          `Keyword ",";
          `Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ));
          `Keyword ")"],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (pl : 'comma_pat)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'pat )))));
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))));
         ([`Keyword "#";
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("(`ClassPath (_loc, i) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (i : 'type_longident)  _  (_loc : Locf.t)  ->
                   ((`ClassPath (_loc, i) : FAst.pat ) : 'pat )))));
         ([`Token
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x -> (Ast_quotation.expand x Dyn_tag.pat : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> ((`Any _loc : FAst.pat ) : 'pat )))));
         ([`Token
             (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
               "`Label i");
          `Self],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label ({ txt = i;_} : Tokenf.txt) ->
                       ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                       'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "~";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Self],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'pat)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'pat )))));
         ([`Keyword "~"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'pat )))));
         ([`Token
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("`OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Token
            (((function
               | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "opt")), "`Ant s");
          `Keyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_ant s))\n",
             (Gramf.mk_action
                (fun _  (__fan_6 : Tokenf.t)  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   match __fan_6 with
                   | `Ant (({ kind = "opt";_} as s) : Tokenf.ant) ->
                       (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_6))))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
           ("`OptLabl (_loc, i, p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : Locf.t)  -> (`OptLabl (_loc, i, p) : 'pat )))));
         ([`Keyword "?"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'pat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'pat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'pat )))))])]);
   Gramf.extend_single (ipat : 'ipat Gramf.t )
     (None,
       (None, None,
         [([`Keyword "{";
           `Nterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
           `Keyword "}"],
            ("(`Record (_loc, pl) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                    ((`Record (_loc, pl) : FAst.pat ) : 'ipat )))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "pat")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "pat";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "par")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "par";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "("; `Keyword ")"],
           ("(`Uid (_loc, \"()\") : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : FAst.pat ) : 'ipat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Gramf.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'ipat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'ipat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Token
            (((function
               | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "opt")), "`Ant s");
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_ant s))\n",
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.t)  _  (m : 'a_uident)  _  _ 
                   (_loc : Locf.t)  ->
                   match __fan_4 with
                   | `Ant (({ kind = "opt";_} as s) : Tokenf.ant) ->
                       (`ModuleConstraint (_loc, m, (mk_ant s)) : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_4))))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ")"],
           ("p\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword "as";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : Locf.t) 
                   -> ((`Alias (_loc, p, s) : FAst.pat ) : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ",";
          `Nterm (Gramf.obj (comma_ipat : 'comma_ipat Gramf.t ));
          `Keyword ")"],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (pl : 'comma_ipat)  _  (p : 'pat)  _  (_loc : Locf.t)
                    ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'ipat )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(s : alident  :>pat)\n",
             (Gramf.mk_action
                (fun (s : 'a_lident)  (_loc : Locf.t)  ->
                   ((s : alident  :>pat) : 'ipat )))));
         ([`Token
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x -> (Ast_quotation.expand x Dyn_tag.pat : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'ipat )))));
         ([`Keyword "_"],
           ("(`Any _loc : FAst.pat )\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ((`Any _loc : FAst.pat ) : 'ipat )))));
         ([`Token
             (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
               "`Label i");
          `Self],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Label ({ txt = i;_} : Tokenf.txt) ->
                       ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                       'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "~";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Self],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'ipat )))));
         ([`Keyword "~"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`LabelS (_loc, i) : 'ipat )))));
         ([`Token
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Optlabel _ -> true | _ -> false)),
               (688526593, `Any), "`Optlabel i");
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Optlabel ({ txt = i;_} : Tokenf.txt) ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("`OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Token
            (((function
               | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "opt")), "`Ant s");
          `Keyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_ant s))\n",
             (Gramf.mk_action
                (fun _  (__fan_6 : Tokenf.t)  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   match __fan_6 with
                   | `Ant (({ kind = "opt";_} as s) : Tokenf.ant) ->
                       (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'ipat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_6))))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
           ("`OptLabl (_loc, i, p)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : Locf.t)  -> (`OptLabl (_loc, i, p) : 'ipat )))));
         ([`Keyword "?"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`OptLablS (_loc, i) : 'ipat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword ")"],
           ("`OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'ipat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n",
             (Gramf.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : Locf.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'ipat )))))]));
   Gramf.extend_single (sem_pat : 'sem_pat Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t )); `Keyword ";"; `Self],
            ("`Sem (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'sem_pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'sem_pat )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t )); `Keyword ";"],
           ("p\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  (_loc : Locf.t)  -> (p : 'sem_pat )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'sem_pat )))))]));
   Gramf.extend_single (sem_pat_for_list : 'sem_pat_for_list Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t )); `Keyword ";"; `Self],
            ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), (pl acc))\n",
              (Gramf.mk_action
                 (fun (pl : 'sem_pat_for_list)  _  (p : 'pat) 
                    (_loc : Locf.t)  ->
                    (fun acc  ->
                       `App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), p)),
                           (pl acc)) : 'sem_pat_for_list )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t )); `Keyword ";"],
           ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc)\n",
             (Gramf.mk_action
                (fun _  (p : 'pat)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc)\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))))]));
   Gramf.extend_single (pat_tcon : 'pat_tcon Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`Constraint (_loc, p, t) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (t : 'ctyp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                    ((`Constraint (_loc, p, t) : FAst.pat ) : 'pat_tcon )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_tcon )))))]));
   Gramf.extend_single (ipat_tcon : 'ipat_tcon Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"pat" s : 'ipat_tcon )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i : alident  :>pat)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i : alident  :>pat) : 'ipat_tcon )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Constraint (_loc, (i : alident  :>pat), t) : pat )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, (i : alident  :>pat), t) : pat ) : 
                   'ipat_tcon )))))]));
   Gramf.extend_single (comma_ipat : 'comma_ipat Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword ","; `Self],
            ("(`Com (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (p2 : 'comma_ipat)  _  (p1 : 'comma_ipat) 
                    (_loc : Locf.t)  ->
                    ((`Com (_loc, p1, p2) : FAst.pat ) : 'comma_ipat )))));
         ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'ipat)  (_loc : Locf.t)  -> (p : 'comma_ipat )))))]));
   Gramf.extend_single (comma_pat : 'comma_pat Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword ","; `Self],
            ("(`Com (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun (p2 : 'comma_pat)  _  (p1 : 'comma_pat) 
                    (_loc : Locf.t)  ->
                    ((`Com (_loc, p1, p2) : FAst.pat ) : 'comma_pat )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'comma_pat )))))]));
   Gramf.extend_single (label_pat_list : 'label_pat_list Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
           `Keyword ";";
           `Self],
            ("`Sem (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun (p2 : 'label_pat_list)  _  (p1 : 'label_pat) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Keyword ";";
          `Keyword "_"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Keyword ";";
          `Keyword "_";
          `Keyword ";"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun _  _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Keyword ";"],
           ("p1\n",
             (Gramf.mk_action
                (fun _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ))],
           ("p1\n",
             (Gramf.mk_action
                (fun (p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))))]));
   Gramf.extend_single (label_pat : 'label_pat Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"pat" s : 'label_pat )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "pat")), "`Ant s")],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "pat";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"pat" s : 'label_pat )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("`RecBind (_loc, i, p)\n",
             (Gramf.mk_action
                (fun (p : 'pat)  _  (i : 'label_longident)  (_loc : Locf.t) 
                   -> (`RecBind (_loc, i, p) : 'label_pat )))));
         ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
           ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
             (Gramf.mk_action
                (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                   'label_pat )))))])));
  (Gramf.extend_single (luident : 'luident Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "`Lid i")],
            ("i\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid ({ txt = i;_} : Tokenf.txt) -> (i : 'luident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i")],
           ("i\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) -> (i : 'luident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (aident : 'aident Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("(i :>ident)\n",
              (Gramf.mk_action
                 (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                    ((i :>ident) : 'aident )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
           ("(i :>ident)\n",
             (Gramf.mk_action
                (fun (i : 'a_uident)  (_loc : Locf.t)  ->
                   ((i :>ident) : 'aident )))))]));
   Gramf.extend_single (astr : 'astr Gramf.t )
     (None,
       (None, None,
         [([`Token
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant s : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "vrn")), "`Ant s")],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "vrn";_} as s) : Tokenf.ant) ->
                       (mk_ant s : 'astr )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend (ident_quot : 'ident_quot Gramf.t )
     (None,
       [((Some "."), None,
          [([`Self; `Keyword "."; `Self],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Gramf.mk_action
                  (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                     (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'ident_quot )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'ident_quot )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Keyword ".";
          `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "("; `Self; `Self; `Keyword ")"],
           ("`Apply (_loc, i, j)\n",
             (Gramf.mk_action
                (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                   (_loc : Locf.t)  -> (`Apply (_loc, i, j) : 'ident_quot )))))])]);
   Gramf.extend_single (ident : 'ident Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'ident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
             (Gramf.mk_action
                (fun (j : 'ident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (vid : 'vid Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'vid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
             (Gramf.mk_action
                (fun (i : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
             (Gramf.mk_action
                (fun (j : 'vid)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (uident : 'uident Gramf.t )
     (None,
       (None, None,
         [([`Token
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"uident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"uident" s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"uident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"uident" s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"uident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"uident" s : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid s");
          `Keyword ".";
          `Self],
           ("dot (`Uid (_loc, s)) l\n",
             (Gramf.mk_action
                (fun (l : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Uid ({ txt = s;_} : Tokenf.txt) ->
                       (dot (`Uid (_loc, s)) l : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("dot (mk_ant ~c:\"uident\" s) i\n",
             (Gramf.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (dot (mk_ant ~c:"uident" s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s");
          `Keyword ".";
          `Self],
           ("dot (mk_ant ~c:\"uident\" s) i\n",
             (Gramf.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (dot (mk_ant ~c:"uident" s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("dot (mk_ant ~c:\"uident\" s) i\n",
             (Gramf.mk_action
                (fun (i : 'uident)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (dot (mk_ant ~c:"uident" s) i : 'uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (dot_lstrings : 'dot_lstrings Gramf.t )
     (None,
       (None, None,
         [([`Token
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Keyword ".";
          `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword ".";
          `Token
            (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
              "`Uid i");
          `Keyword ".";
          `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]));
   Gramf.extend_single
     (module_longident_dot_lparen : 'module_longident_dot_lparen Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s");
           `Keyword ".";
           `Keyword "("],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s");
          `Keyword ".";
          `Keyword "("],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Keyword "("],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Keyword ".";
          `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Keyword ".";
          `Keyword "("],
           ("(`Uid (_loc, i) : FAst.ident )\n",
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       ((`Uid (_loc, i) : FAst.ident ) : 'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                       'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                       'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (module_longident : 'module_longident Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'module_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (`Uid (_loc, i)), l)\n",
             (Gramf.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = i;_} : Tokenf.txt) ->
                       (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
             (Gramf.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
             (Gramf.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'module_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend
     (module_longident_with_app : 'module_longident_with_app Gramf.t )
     (None,
       [((Some "apply"), None,
          [([`Self; `Self],
             ("`Apply (_loc, i, j)\n",
               (Gramf.mk_action
                  (fun (j : 'module_longident_with_app) 
                     (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                     (`Apply (_loc, i, j) : 'module_longident_with_app )))))]);
       ((Some "."), None,
         [([`Self; `Keyword "."; `Self],
            ("(`Dot (_loc, i, j) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun (j : 'module_longident_with_app)  _ 
                    (i : 'module_longident_with_app)  (_loc : Locf.t)  ->
                    ((`Dot (_loc, i, j) : FAst.ident ) : 'module_longident_with_app )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'module_longident_with_app )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'module_longident_with_app )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("i\n",
             (Gramf.mk_action
                (fun _  (i : 'module_longident_with_app)  _  (_loc : Locf.t) 
                   -> (i : 'module_longident_with_app )))))])]);
   Gramf.extend (type_longident : 'type_longident Gramf.t )
     (None,
       [((Some "apply"), None,
          [([`Self; `Self],
             ("`Apply (_loc, i, j)\n",
               (Gramf.mk_action
                  (fun (j : 'type_longident)  (i : 'type_longident) 
                     (_loc : Locf.t)  ->
                     (`Apply (_loc, i, j) : 'type_longident )))))]);
       ((Some "."), None,
         [([`Self; `Keyword "."; `Self],
            ("(`Dot (_loc, i, j) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun (j : 'type_longident)  _  (i : 'type_longident) 
                    (_loc : Locf.t)  ->
                    ((`Dot (_loc, i, j) : FAst.ident ) : 'type_longident )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'type_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'type_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("i\n",
             (Gramf.mk_action
                (fun _  (i : 'type_longident)  _  (_loc : Locf.t)  ->
                   (i : 'type_longident )))))])]);
   Gramf.extend_single (label_longident : 'label_longident Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"ident" s : 'label_longident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "id")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("mk_ant ~c:\"ident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ident" s : 'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid i");
          `Keyword ".";
          `Self],
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ".";
          `Self],
           ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                       'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s");
          `Keyword ".";
          `Self],
           ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
             (Gramf.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                       'label_longident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (cltyp_longident : 'cltyp_longident Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'type_longident)  (_loc : Locf.t)  ->
                    (x : 'cltyp_longident )))))]));
   Gramf.extend_single (val_longident : 'val_longident Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (ident : 'ident Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'ident)  (_loc : Locf.t)  -> (x : 'val_longident )))))]));
   Gramf.extend_single (class_longident : 'class_longident Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'label_longident)  (_loc : Locf.t)  ->
                    (x : 'class_longident )))))]));
   Gramf.extend_single (method_opt_override : 'method_opt_override Gramf.t )
     (None,
       (None, None,
         [([`Keyword "method"; `Keyword "!"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'method_opt_override )))));
         ([`Keyword "method";
          `Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'method_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "method";
          `Token
            (((function
               | `Ant ({ kind = "override";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "override")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (({ kind = "override";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'method_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "method"],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (`Negative _loc : 'method_opt_override )))))]));
   Gramf.extend_single (opt_override : 'opt_override Gramf.t )
     (None,
       (None, None,
         [([`Keyword "!"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'opt_override )))));
         ([`Token
             (((function
                | `Ant ({ kind = "!";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "!")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "!";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "override";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "override")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "override";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_override )))))]));
   Gramf.extend_single
     (value_val_opt_override : 'value_val_opt_override Gramf.t )
     (None,
       (None, None,
         [([`Keyword "val"; `Keyword "!"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  ->
                    (`Positive _loc : 'value_val_opt_override )))));
         ([`Keyword "val";
          `Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "val";
          `Token
            (((function
               | `Ant ({ kind = "override";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "override")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (({ kind = "override";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "val";
          `Token
            (((function
               | `Ant ({ kind = "!";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "!")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Ant (({ kind = "!";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'value_val_opt_override )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "val"],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (`Negative _loc : 'value_val_opt_override )))))]));
   Gramf.extend_single (flag : 'flag Gramf.t )
     (None,
       (None, None,
         [([`Keyword "to"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'flag )))));
         ([`Keyword "downto"],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (`Negative _loc : 'flag )))));
         ([`Token
             (((function
                | `Ant ({ kind = "to";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "to")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "to";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'flag )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'flag )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (opt_private : 'opt_private Gramf.t )
     (None,
       (None, None,
         [([`Keyword "private"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_private )))));
         ([`Token
             (((function
                | `Ant ({ kind = "private";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "private")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "private";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_private )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_private )))))]));
   Gramf.extend_single (opt_mutable : 'opt_mutable Gramf.t )
     (None,
       (None, None,
         [([`Keyword "mutable"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_mutable )))));
         ([`Token
             (((function
                | `Ant ({ kind = "mutable";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "mutable")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "mutable";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_mutable )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_mutable )))))]));
   Gramf.extend_single (opt_virtual : 'opt_virtual Gramf.t )
     (None,
       (None, None,
         [([`Keyword "virtual"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_virtual )))));
         ([`Token
             (((function
                | `Ant ({ kind = "virtual";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "virtual")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "virtual";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_virtual )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_virtual )))))]));
   Gramf.extend_single (opt_dot_dot : 'opt_dot_dot Gramf.t )
     (None,
       (None, None,
         [([`Keyword ".."],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_dot_dot )))));
         ([`Token
             (((function
                | `Ant ({ kind = "..";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "..")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "..";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_dot_dot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_dot_dot )))))]));
   Gramf.extend_single (opt_rec : 'opt_rec Gramf.t )
     (None,
       (None, None,
         [([`Keyword "rec"],
            ("`Positive _loc\n",
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'opt_rec )))));
         ([`Token
             (((function
                | `Ant ({ kind = "rec";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "rec")), "`Ant s")],
           ("mk_ant ~c:\"flag\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "rec";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"flag" s : 'opt_rec )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([],
           ("`Negative _loc\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_rec )))))]));
   Gramf.extend_single (a_lident : 'a_lident Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"a_lident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"a_lident" s : 'a_lident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "lid")), "`Ant s")],
           ("mk_ant ~c:\"a_lident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "lid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"a_lident" s : 'a_lident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (a_uident : 'a_uident Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"a_uident\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"a_uident" s : 'a_uident )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "uid")), "`Ant s")],
           ("mk_ant ~c:\"a_uident\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "uid";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"a_uident" s : 'a_uident )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (string_list : 'string_list Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"str_list\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"str_list" s : 'string_list )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Self],
           ("`App (_loc, (mk_ant ~c:\"\" s), xs)\n",
             (Gramf.mk_action
                (fun (xs : 'string_list)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`App (_loc, (mk_ant ~c:"" s), xs) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str x");
          `Self],
           ("`App (_loc, (`Str (_loc, x)), xs)\n",
             (Gramf.mk_action
                (fun (xs : 'string_list)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = x;_} : Tokenf.txt) ->
                       (`App (_loc, (`Str (_loc, x)), xs) : 'string_list )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
   Gramf.extend_single (rec_flag_quot : 'rec_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_rec)  (_loc : Locf.t)  ->
                    (x : 'rec_flag_quot )))))]));
   Gramf.extend_single (direction_flag_quot : 'direction_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (flag : 'flag Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'flag)  (_loc : Locf.t)  ->
                    (x : 'direction_flag_quot )))))]));
   Gramf.extend_single (mutable_flag_quot : 'mutable_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_mutable)  (_loc : Locf.t)  ->
                    (x : 'mutable_flag_quot )))))]));
   Gramf.extend_single (private_flag_quot : 'private_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_private)  (_loc : Locf.t)  ->
                    (x : 'private_flag_quot )))))]));
   Gramf.extend_single (virtual_flag_quot : 'virtual_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_virtual)  (_loc : Locf.t)  ->
                    (x : 'virtual_flag_quot )))))]));
   Gramf.extend_single (row_var_flag_quot : 'row_var_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_dot_dot)  (_loc : Locf.t)  ->
                    (x : 'row_var_flag_quot )))))]));
   Gramf.extend_single (override_flag_quot : 'override_flag_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'opt_override)  (_loc : Locf.t)  ->
                    (x : 'override_flag_quot )))))]));
   Gramf.extend_single (pat_eoi : 'pat_eoi Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
            ("x\n",
              (Gramf.mk_action
                 (fun _  (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_eoi )))))]));
   Gramf.extend_single (exp_eoi : 'exp_eoi Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Token
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
            ("x\n",
              (Gramf.mk_action
                 (fun _  (x : 'exp)  (_loc : Locf.t)  -> (x : 'exp_eoi )))))])));
  (Gramf.extend_single (implem : 'implem Gramf.t )
     (None,
       (None, None,
         [([`Token
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"; `Self],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Gramf.mk_action
                (fun (rest : 'implem)  _  (si : 'stru)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'implem )))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Self],
           ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
             (Gramf.mk_action
                (fun (rest : 'implem)  (si : 'stru)  (_loc : Locf.t)  ->
                   (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                   'implem )))));
         ([`Token
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
           ("([], None)\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (([], None) : 'implem )))))]));
   Gramf.extend_single (top_phrase : 'top_phrase Gramf.t )
     (None,
       (None, None,
         [([`Keyword "#";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Keyword ";;"],
            ("Some (`Directive (_loc, n, dp))\n",
              (Gramf.mk_action
                 (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                    (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
         ([`Keyword "#";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ";;"],
           ("Some (`DirectiveSimple (_loc, n))\n",
             (Gramf.mk_action
                (fun _  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                   (Some (`DirectiveSimple (_loc, n)) : 'top_phrase )))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"],
           ("Some st\n",
             (Gramf.mk_action
                (fun _  (st : 'stru)  (_loc : Locf.t)  ->
                   (Some st : 'top_phrase )))));
         ([`Token
             (((function | `EOI _ -> true | _ -> false)), (3448991, `Empty),
               "`EOI")],
           ("None\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (None : 'top_phrase )))))]));
   Gramf.extend_single (strus : 'strus Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"stru" s : 'strus )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "stri")), "`Ant s")],
           ("mk_ant ~c:\"stru\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "stri";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";;"],
           ("mk_ant ~c:\"stru\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "stri")), "`Ant s");
          `Keyword ";;"],
           ("mk_ant ~c:\"stru\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "stri";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"stru" s : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "stri")), "`Ant s");
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "stri";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";;";
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "stri")), "`Ant s");
          `Keyword ";;";
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'strus)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t) 
                   ->
                   match __fan_0 with
                   | `Ant (({ kind = "stri";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t ))],
           ("st\n",
             (Gramf.mk_action
                (fun (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus )))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"],
           ("st\n",
             (Gramf.mk_action
                (fun _  (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus )))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"; `Self],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'strus)  _  (st : 'stru)  (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'strus )))));
         ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Self],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'strus)  (st : 'stru)  (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'strus )))))]));
   Gramf.extend_single (stru_quot : 'stru_quot Gramf.t )
     (None,
       (None, None,
         [([`Keyword "#";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Directive (_loc, n, dp)\n",
              (Gramf.mk_action
                 (fun (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                    (`Directive (_loc, n, dp) : 'stru_quot )))));
         ([`Keyword "#"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`DirectiveSimple (_loc, n)\n",
             (Gramf.mk_action
                (fun (n : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`DirectiveSimple (_loc, n) : 'stru_quot )))));
         ([`Nterm (Gramf.obj (strus : 'strus Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'strus)  (_loc : Locf.t)  -> (x : 'stru_quot )))))]));
   Gramf.extend (stru : 'stru Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Keyword "exception";
            `Nterm
              (Gramf.obj
                 (constructor_declaration : 'constructor_declaration Gramf.t ))],
             ("`Exception (_loc, t)\n",
               (Gramf.mk_action
                  (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)  ->
                     (`Exception (_loc, t) : 'stru )))));
          ([`Keyword "external";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
            ("`External (_loc, i, t, sl)\n",
              (Gramf.mk_action
                 (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)
                     _  (_loc : Locf.t)  ->
                    (`External (_loc, i, t, sl) : 'stru )))));
          ([`Keyword "include"; `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`Include (_loc, me)\n",
              (Gramf.mk_action
                 (fun (me : 'mexp)  _  (_loc : Locf.t)  ->
                    (`Include (_loc, me) : 'stru )))));
          ([`Keyword "module";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ))],
            ("`Module (_loc, i, mb)\n",
              (Gramf.mk_action
                 (fun (mb : 'mbind0)  (i : 'a_uident)  _  (_loc : Locf.t)  ->
                    (`Module (_loc, i, mb) : 'stru )))));
          ([`Keyword "module";
           `Keyword "rec";
           `Nterm (Gramf.obj (mbind : 'mbind Gramf.t ))],
            ("`RecModule (_loc, mb)\n",
              (Gramf.mk_action
                 (fun (mb : 'mbind)  _  _  (_loc : Locf.t)  ->
                    (`RecModule (_loc, mb) : 'stru )))));
          ([`Keyword "module";
           `Keyword "type";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`ModuleType (_loc, i, mt)\n",
              (Gramf.mk_action
                 (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : Locf.t)
                     -> (`ModuleType (_loc, i, mt) : 'stru )))));
          ([`Keyword "open";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("`Open (_loc, (`Negative _loc), (i : vid  :>ident))\n",
              (Gramf.mk_action
                 (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                    (`Open (_loc, (`Negative _loc), (i : vid  :>ident)) : 
                    'stru )))));
          ([`Keyword "open";
           `Keyword "!";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("`Open (_loc, (`Positive _loc), (i : vid  :>ident))\n",
              (Gramf.mk_action
                 (fun (i : 'module_longident)  _  _  (_loc : Locf.t)  ->
                    (`Open (_loc, (`Positive _loc), (i : vid  :>ident)) : 
                    'stru )))));
          ([`Keyword "type";
           `Nterm (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
            ("`Type (_loc, td)\n",
              (Gramf.mk_action
                 (fun (td : 'type_declaration)  _  (_loc : Locf.t)  ->
                    (`Type (_loc, td) : 'stru )))));
          ([`Keyword "type";
           `Nterm (Gramf.obj (type_declaration : 'type_declaration Gramf.t ));
           `Keyword "with";
           `Keyword "(";
           `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ));
           `Keyword ")"],
            ("`TypeWith (_loc, t, ns)\n",
              (Gramf.mk_action
                 (fun _  (ns : 'string_list)  _  _  (t : 'type_declaration) 
                    _  (_loc : Locf.t)  -> (`TypeWith (_loc, t, ns) : 
                    'stru )))));
          ([`Keyword "let";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`StExp (_loc, (`LetIn (_loc, r, bi, x))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                    (_loc : Locf.t)  ->
                    ((`StExp (_loc, (`LetIn (_loc, r, bi, x))) : FAst.stru ) : 
                    'stru )))));
          ([`Keyword "let";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ))],
            ("match bi with\n| `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n| _ -> `Value (_loc, r, bi)\n",
              (Gramf.mk_action
                 (fun (bi : 'bind)  (r : 'opt_rec)  _  (_loc : Locf.t)  ->
                    (match bi with
                     | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                     | _ -> `Value (_loc, r, bi) : 'stru )))));
          ([`Keyword "let";
           `Keyword "module";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`StExp (_loc, (`LetModule (_loc, m, mb, e))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : Locf.t)  ->
                    ((`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 
                    FAst.stru ) : 'stru )))));
          ([`Keyword "let";
           `Keyword "open";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("let i = (i : vid  :>ident) in\n(`StExp (_loc, (`LetOpen (_loc, (`Negative _loc), i, e))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : Locf.t)  ->
                    (let i = (i : vid  :>ident) in
                     (`StExp
                        (_loc, (`LetOpen (_loc, (`Negative _loc), i, e))) : 
                       FAst.stru ) : 'stru )))));
          ([`Keyword "let";
           `Keyword "open";
           `Keyword "!";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("let i = (i : vid  :>ident) in\n(`StExp (_loc, (`LetOpen (_loc, (`Positive _loc), i, e))) : FAst.stru )\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _  _ 
                    (_loc : Locf.t)  ->
                    (let i = (i : vid  :>ident) in
                     (`StExp
                        (_loc, (`LetOpen (_loc, (`Positive _loc), i, e))) : 
                       FAst.stru ) : 'stru )))));
          ([`Keyword "let";
           `Keyword "try";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Keyword "with";
           `Nterm (Gramf.obj (case : 'case Gramf.t ))],
            ("`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a)))\n",
              (Gramf.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                    (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                    (`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a))) : 
                    'stru )))));
          ([`Keyword "class";
           `Nterm
             (Gramf.obj (class_declaration : 'class_declaration Gramf.t ))],
            ("`Class (_loc, cd)\n",
              (Gramf.mk_action
                 (fun (cd : 'class_declaration)  _  (_loc : Locf.t)  ->
                    (`Class (_loc, cd) : 'stru )))));
          ([`Keyword "class";
           `Keyword "type";
           `Nterm
             (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
            ("`ClassType (_loc, ctd)\n",
              (Gramf.mk_action
                 (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t)  ->
                    (`ClassType (_loc, ctd) : 'stru )))));
          ([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"stru" s : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
          ([`Token
              (((function
                 | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "stri")), "`Ant s")],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "stri";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"stru" s : 'stru )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
          ([`Token
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
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`StExp (_loc, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  (_loc : Locf.t)  ->
                    (`StExp (_loc, e) : 'stru )))))])]));
  (Gramf.extend_single (clsigi_quot : 'clsigi_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
           `Keyword ";";
           `Self],
            ("`Sem (_loc, x1, x2)\n",
              (Gramf.mk_action
                 (fun (x2 : 'clsigi_quot)  _  (x1 : 'clsigi)  (_loc : Locf.t)
                     -> (`Sem (_loc, x1, x2) : 'clsigi_quot )))));
         ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'clsigi)  (_loc : Locf.t)  -> (x : 'clsigi_quot )))))]));
   Gramf.extend_single (class_signature : 'class_signature Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"clsigi" s : 'class_signature )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "csg")), "`Ant s")],
           ("mk_ant ~c:\"clsigi\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "csg";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clsigi" s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";"],
           ("mk_ant ~c:\"clsigi\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clsigi" s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "csg")), "`Ant s");
          `Keyword ";"],
           ("mk_ant ~c:\"clsigi\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "csg";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clsigi" s : 'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Self],
           ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                       'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "csg")), "`Ant s");
          `Self],
           ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "csg";_} as s) : Tokenf.ant) ->
                       ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                       'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";";
          `Self],
           ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                       'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "csg")), "`Ant s");
          `Keyword ";";
          `Self],
           ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "csg";_} as s) : Tokenf.ant) ->
                       ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                       'class_signature )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ))],
           ("csg\n",
             (Gramf.mk_action
                (fun (csg : 'clsigi)  (_loc : Locf.t)  ->
                   (csg : 'class_signature )))));
         ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); `Keyword ";"],
           ("csg\n",
             (Gramf.mk_action
                (fun _  (csg : 'clsigi)  (_loc : Locf.t)  ->
                   (csg : 'class_signature )))));
         ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
          `Keyword ";";
          `Self],
           ("`Sem (_loc, csg, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_signature)  _  (csg : 'clsigi) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, csg, xs) : 'class_signature )))));
         ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); `Self],
           ("`Sem (_loc, csg, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_signature)  (csg : 'clsigi) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, csg, xs) : 'class_signature )))))]));
   Gramf.extend_single (clsigi : 'clsigi Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"clsigi" s : 'clsigi )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "csg")), "`Ant s")],
           ("mk_ant ~c:\"clsigi\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "csg";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clsigi" s : 'clsigi )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "inherit"; `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("`SigInherit (_loc, cs)\n",
             (Gramf.mk_action
                (fun (cs : 'cltyp)  _  (_loc : Locf.t)  ->
                   (`SigInherit (_loc, cs) : 'clsigi )))));
         ([`Keyword "val";
          `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
          `Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`CgVal (_loc, l, mf, mv, t) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) 
                   (mf : 'opt_mutable)  _  (_loc : Locf.t)  ->
                   ((`CgVal (_loc, l, mf, mv, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Keyword "method";
          `Keyword "virtual";
          `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`VirMeth (_loc, l, pf, t) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   _  (_loc : Locf.t)  ->
                   ((`VirMeth (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Keyword "method";
          `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Method (_loc, l, pf, t) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (_loc : Locf.t)  ->
                   ((`Method (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
         ([`Keyword "constraint";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Eq (_loc, t1, t2) : FAst.clsigi )\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Eq (_loc, t1, t2) : FAst.clsigi ) : 'clsigi )))))])));
  (Gramf.extend_single (class_structure : 'class_structure Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"clfield" s : 'class_structure )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cst")), "`Ant s")],
           ("mk_ant ~c:\"clfield\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cst";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clfield" s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";"],
           ("mk_ant ~c:\"clfield\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clfield" s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cst")), "`Ant s");
          `Keyword ";"],
           ("mk_ant ~c:\"clfield\" s\n",
             (Gramf.mk_action
                (fun _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cst";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clfield" s : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'class_structure)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cst")), "`Ant s");
          `Self],
           ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
             (Gramf.mk_action
                (fun (st : 'class_structure)  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cst";_} as s) : Tokenf.ant) ->
                       (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s");
          `Keyword ";";
          `Self],
           ("(`Sem (_loc, (mk_ant ~c:\"clfield\" s), cst) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       ((`Sem (_loc, (mk_ant ~c:"clfield" s), cst) : 
                       FAst.clfield ) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cst")), "`Ant s");
          `Keyword ";";
          `Self],
           ("(`Sem (_loc, (mk_ant ~c:\"clfield\" s), cst) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cst";_} as s) : Tokenf.ant) ->
                       ((`Sem (_loc, (mk_ant ~c:"clfield" s), cst) : 
                       FAst.clfield ) : 'class_structure )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ))],
           ("st\n",
             (Gramf.mk_action
                (fun (st : 'clfield)  (_loc : Locf.t)  ->
                   (st : 'class_structure )))));
         ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t )); `Keyword ";"],
           ("st\n",
             (Gramf.mk_action
                (fun _  (st : 'clfield)  (_loc : Locf.t)  ->
                   (st : 'class_structure )))));
         ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ));
          `Keyword ";";
          `Self],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_structure)  _  (st : 'clfield) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'class_structure )))));
         ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t )); `Self],
           ("`Sem (_loc, st, xs)\n",
             (Gramf.mk_action
                (fun (xs : 'class_structure)  (st : 'clfield) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, st, xs) : 'class_structure )))))]));
   Gramf.extend_single (clfield : 'clfield Gramf.t )
     (None,
       (None, None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"clfield" s : 'clfield )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cst")), "`Ant s")],
           ("mk_ant ~c:\"clfield\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cst";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clfield" s : 'clfield )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "inherit";
          `Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
          `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
           ("`Inherit (_loc, o, ce)\n",
             (Gramf.mk_action
                (fun (ce : 'clexp)  (o : 'opt_override)  _  (_loc : Locf.t) 
                   -> (`Inherit (_loc, o, ce) : 'clfield )))));
         ([`Keyword "inherit";
          `Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
          `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ));
          `Keyword "as";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`InheritAs (_loc, o, ce, i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (ce : 'clexp)  (o : 'opt_override) 
                   _  (_loc : Locf.t)  ->
                   (`InheritAs (_loc, o, ce, i) : 'clfield )))));
         ([`Nterm
             (Gramf.obj
                (value_val_opt_override : 'value_val_opt_override Gramf.t ));
          `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Nterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))],
           ("(`CrVal (_loc, lab, o, mf, e) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (e : 'cvalue_bind)  (lab : 'a_lident) 
                   (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                   (_loc : Locf.t)  ->
                   ((`CrVal (_loc, lab, o, mf, e) : FAst.clfield ) : 
                   'clfield )))));
         ([`Nterm
             (Gramf.obj
                (value_val_opt_override : 'value_val_opt_override Gramf.t ));
          `Keyword "virtual";
          `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
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
         ([`Nterm
             (Gramf.obj (method_opt_override : 'method_opt_override Gramf.t ));
          `Keyword "virtual";
          `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
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
         ([`Nterm
             (Gramf.obj (method_opt_override : 'method_opt_override Gramf.t ));
          `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
           ("`CrMth (_loc, l, o, pf, e, t)\n",
             (Gramf.mk_action
                (fun (e : 'fun_bind)  (t : 'ctyp)  _  (l : 'a_lident) 
                   (pf : 'opt_private)  (o : 'method_opt_override) 
                   (_loc : Locf.t)  ->
                   (`CrMth (_loc, l, o, pf, e, t) : 'clfield )))));
         ([`Nterm
             (Gramf.obj (method_opt_override : 'method_opt_override Gramf.t ));
          `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
           ("`CrMthS (_loc, l, o, pf, e)\n",
             (Gramf.mk_action
                (fun (e : 'fun_bind)  (l : 'a_lident)  (pf : 'opt_private) 
                   (o : 'method_opt_override)  (_loc : Locf.t)  ->
                   (`CrMthS (_loc, l, o, pf, e) : 'clfield )))));
         ([`Keyword "constraint";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Eq (_loc, t1, t2) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   ((`Eq (_loc, t1, t2) : FAst.clfield ) : 'clfield )))));
         ([`Keyword "initializer"; `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("(`Initializer (_loc, se) : FAst.clfield )\n",
             (Gramf.mk_action
                (fun (se : 'exp)  _  (_loc : Locf.t)  ->
                   ((`Initializer (_loc, se) : FAst.clfield ) : 'clfield )))))]));
   Gramf.extend_single (clfield_quot : 'clfield_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ));
           `Keyword ";";
           `Self],
            ("`Sem (_loc, x1, x2)\n",
              (Gramf.mk_action
                 (fun (x2 : 'clfield_quot)  _  (x1 : 'clfield) 
                    (_loc : Locf.t)  ->
                    (`Sem (_loc, x1, x2) : 'clfield_quot )))));
         ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'clfield)  (_loc : Locf.t)  -> (x : 'clfield_quot )))))])));
  (Gramf.extend_single (clexp_quot : 'clexp_quot Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun (x : 'clexp)  (_loc : Locf.t)  -> (x : 'clexp_quot )))))]));
   Gramf.extend_single (class_declaration : 'class_declaration Gramf.t )
     (None,
       (None, None,
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, c1, c2)\n",
              (Gramf.mk_action
                 (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration)
                     (_loc : Locf.t)  ->
                    (`And (_loc, c1, c2) : 'class_declaration )))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"clexp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clexp" s : 'class_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cdcl";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cdcl")), "`Ant s")],
           ("mk_ant ~c:\"clexp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cdcl";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clexp" s : 'class_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword "[";
          `Nterm
            (Gramf.obj
               (comma_type_parameter : 'comma_type_parameter Gramf.t ));
          `Keyword "]";
          `Nterm (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))],
           ("`ClDecl (_loc, mv, (i :>ident), x, ce)\n",
             (Gramf.mk_action
                (fun (ce : 'class_fun_bind)  _  (x : 'comma_type_parameter) 
                   _  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t) 
                   ->
                   (`ClDecl (_loc, mv, (i :>ident), x, ce) : 'class_declaration )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Nterm (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))],
           ("`ClDeclS (_loc, mv, (i :>ident), ce)\n",
             (Gramf.mk_action
                (fun (ce : 'class_fun_bind)  (i : 'a_lident) 
                   (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                   (`ClDeclS (_loc, mv, (i :>ident), ce) : 'class_declaration )))))]));
   Gramf.extend_single (class_fun_bind : 'class_fun_bind Gramf.t )
     (None,
       (None, None,
         [([`Keyword "="; `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("ce\n",
              (Gramf.mk_action
                 (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                    (ce : 'class_fun_bind )))));
         ([`Keyword ":";
          `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
           ("`Constraint (_loc, ce, ct)\n",
             (Gramf.mk_action
                (fun (ce : 'clexp)  _  (ct : 'cltyp_plus)  _  (_loc : Locf.t)
                    -> (`Constraint (_loc, ce, ct) : 'class_fun_bind )))));
         ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Self],
           ("`CeFun (_loc, p, cfb)\n",
             (Gramf.mk_action
                (fun (cfb : 'class_fun_bind)  (p : 'ipat)  (_loc : Locf.t) 
                   -> (`CeFun (_loc, p, cfb) : 'class_fun_bind )))))]));
   Gramf.extend_single (class_fun_def : 'class_fun_def Gramf.t )
     (None,
       (None, None,
         [([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Self],
            ("`CeFun (_loc, p, ce)\n",
              (Gramf.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  (_loc : Locf.t)  ->
                    (`CeFun (_loc, p, ce) : 'class_fun_def )))));
         ([`Keyword "->"; `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
           ("ce\n",
             (Gramf.mk_action
                (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                   (ce : 'class_fun_def )))))]));
   Gramf.extend (clexp : 'clexp Gramf.t )
     (None,
       [((Some "top"), None,
          [([`Keyword "fun";
            `Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
            `Nterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
             ("`CeFun (_loc, p, ce)\n",
               (Gramf.mk_action
                  (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : Locf.t)
                      -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Keyword "function";
           `Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
           `Nterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
            ("`CeFun (_loc, p, ce)\n",
              (Gramf.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : Locf.t) 
                    -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Keyword "let";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Keyword "in";
           `Self],
            ("`LetIn (_loc, rf, bi, ce)\n",
              (Gramf.mk_action
                 (fun (ce : 'clexp)  _  (bi : 'bind)  (rf : 'opt_rec)  _ 
                    (_loc : Locf.t)  -> (`LetIn (_loc, rf, bi, ce) : 
                    'clexp )))))]);
       ((Some "apply"), (Some `NA),
         [([`Self; `Snterml ((Gramf.obj (exp : 'exp Gramf.t )), "label")],
            ("`CeApp (_loc, ce, e)\n",
              (Gramf.mk_action
                 (fun (e : 'exp)  (ce : 'clexp)  (_loc : Locf.t)  ->
                    (`CeApp (_loc, ce, e) : 'clexp )))))]);
       ((Some "simple"), None,
         [([`Token
              (((function
                 | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                 | _ -> false)), (3257031, (`A "")), "`Ant s")],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                        (mk_ant ~c:"clexp" s : 'clexp )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "cexp";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "cexp")), "`Ant s")],
           ("mk_ant ~c:\"clexp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "cexp";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"clexp" s : 'clexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
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
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ));
          `Keyword "[";
          `Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
          `Keyword "]"],
           ("`ClApply (_loc, ci, t)\n",
             (Gramf.mk_action
                (fun _  (t : 'comma_ctyp)  _  (ci : 'vid)  (_loc : Locf.t) 
                   -> (`ClApply (_loc, ci, t) : 'clexp )))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(ci :>clexp)\n",
             (Gramf.mk_action
                (fun (ci : 'vid)  (_loc : Locf.t)  ->
                   ((ci :>clexp) : 'clexp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Keyword "end"],
           ("`ObjPat (_loc, p, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'clexp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ")";
          `Keyword "end"],
           ("`ObjPatEnd (_loc, p)\n",
             (Gramf.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPatEnd (_loc, p) : 'clexp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Keyword "end"],
           ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : Locf.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'clexp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")";
          `Keyword "end"],
           ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
             (Gramf.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : Locf.t) 
                   ->
                   (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'clexp )))));
         ([`Keyword "object";
          `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
          `Keyword "end"],
           ("`Obj (_loc, cst)\n",
             (Gramf.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : Locf.t)  ->
                   (`Obj (_loc, cst) : 'clexp )))));
         ([`Keyword "object"; `Keyword "end"],
           ("`ObjEnd _loc\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'clexp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":";
          `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ));
          `Keyword ")"],
           ("`Constraint (_loc, ce, ct)\n",
             (Gramf.mk_action
                (fun _  (ct : 'cltyp)  _  (ce : 'clexp)  _  (_loc : Locf.t) 
                   -> (`Constraint (_loc, ce, ct) : 'clexp )))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("ce\n",
             (Gramf.mk_action
                (fun _  (ce : 'clexp)  _  (_loc : Locf.t)  -> (ce : 'clexp )))))])]));
  Gramf.extend_single (class_description : 'class_description Gramf.t )
    (None,
      (None, None,
        [([`Self; `Keyword "and"; `Self],
           ("`And (_loc, cd1, cd2)\n",
             (Gramf.mk_action
                (fun (cd2 : 'class_description)  _ 
                   (cd1 : 'class_description)  (_loc : Locf.t)  ->
                   (`And (_loc, cd1, cd2) : 'class_description )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant ~c:\"cltyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"cltyp" s : 'class_description )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"cltyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"cltyp" s : 'class_description )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword "[";
         `Nterm
           (Gramf.obj (comma_type_parameter : 'comma_type_parameter Gramf.t ));
         `Keyword "]";
         `Keyword ":";
         `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))],
          ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp_plus)  _  _  (x : 'comma_type_parameter)  _ 
                  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'class_description )))));
        ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword ":";
         `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))],
          ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp_plus)  _  (i : 'a_lident) 
                  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDeclS (_loc, mv, (i :>ident), ct) : 'class_description )))))]));
  Gramf.extend_single (cltyp_declaration : 'cltyp_declaration Gramf.t )
    (None,
      (None, None,
        [([`Self; `Keyword "and"; `Self],
           ("`And (_loc, cd1, cd2)\n",
             (Gramf.mk_action
                (fun (cd2 : 'cltyp_declaration)  _ 
                   (cd1 : 'cltyp_declaration)  (_loc : Locf.t)  ->
                   (`And (_loc, cd1, cd2) : 'cltyp_declaration )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant ~c:\"cltyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"cltyp" s : 'cltyp_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"cltyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"cltyp" s : 'cltyp_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword "[";
         `Nterm
           (Gramf.obj (comma_type_parameter : 'comma_type_parameter Gramf.t ));
         `Keyword "]";
         `Keyword "=";
         `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
          ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp)  _  _  (x : 'comma_type_parameter)  _ 
                  (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                  (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'cltyp_declaration )))));
        ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword "=";
         `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
          ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp)  _  (i : 'a_lident)  (mv : 'opt_virtual) 
                  (_loc : Locf.t)  ->
                  (`CtDeclS (_loc, mv, (i :>ident), ct) : 'cltyp_declaration )))))]));
  Gramf.extend_single (cltyp_quot : 'cltyp_quot Gramf.t )
    (None,
      (None, None,
        [([`Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun (x : 'cltyp)  (_loc : Locf.t)  -> (x : 'cltyp_quot )))))]));
  Gramf.extend_single (cltyp_plus : 'cltyp_plus Gramf.t )
    (None,
      (None, None,
        [([`Keyword "[";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "]";
          `Keyword "->";
          `Self],
           ("`CtFun (_loc, t, ct)\n",
             (Gramf.mk_action
                (fun (ct : 'cltyp_plus)  _  _  (t : 'ctyp)  _ 
                   (_loc : Locf.t)  -> (`CtFun (_loc, t, ct) : 'cltyp_plus )))));
        ([`Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
          ("ct\n",
            (Gramf.mk_action
               (fun (ct : 'cltyp)  (_loc : Locf.t)  -> (ct : 'cltyp_plus )))))]));
  Gramf.extend_single (cltyp : 'cltyp Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"cltyp" s : 'cltyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "ctyp";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "ctyp")), "`Ant s")],
          ("mk_ant ~c:\"cltyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "ctyp";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"cltyp" s : 'cltyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
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
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ));
         `Keyword "[";
         `Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
         `Keyword "]"],
          ("`ClApply (_loc, i, t)\n",
            (Gramf.mk_action
               (fun _  (t : 'comma_ctyp)  _  (i : 'vid)  (_loc : Locf.t)  ->
                  (`ClApply (_loc, i, t) : 'cltyp )))));
        ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
          ("(i :>cltyp)\n",
            (Gramf.mk_action
               (fun (i : 'vid)  (_loc : Locf.t)  -> ((i :>cltyp) : 'cltyp )))));
        ([`Keyword "object";
         `Keyword "(";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Keyword ")";
         `Nterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
         `Keyword "end"],
          ("`ObjTy (_loc, t, csg)\n",
            (Gramf.mk_action
               (fun _  (csg : 'class_signature)  _  (t : 'ctyp)  _  _ 
                  (_loc : Locf.t)  -> (`ObjTy (_loc, t, csg) : 'cltyp )))));
        ([`Keyword "object";
         `Nterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
         `Keyword "end"],
          ("`Obj (_loc, csg)\n",
            (Gramf.mk_action
               (fun _  (csg : 'class_signature)  _  (_loc : Locf.t)  ->
                  (`Obj (_loc, csg) : 'cltyp )))));
        ([`Keyword "object";
         `Keyword "(";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Keyword ")"],
          ("`ObjTyEnd (_loc, t)\n",
            (Gramf.mk_action
               (fun _  (t : 'ctyp)  _  _  (_loc : Locf.t)  ->
                  (`ObjTyEnd (_loc, t) : 'cltyp )))));
        ([`Keyword "object"; `Keyword "end"],
          ("`ObjEnd _loc\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'cltyp )))))]))
let apply_ctyp () =
  Gramf.extend_single (ctyp_quot : 'ctyp_quot Gramf.t )
    (None,
      (None, None,
        [([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "*";
          `Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ))],
           ("`Sta (_loc, x, y)\n",
             (Gramf.mk_action
                (fun (y : 'star_ctyp)  _  (x : 'ctyp)  (_loc : Locf.t)  ->
                   (`Sta (_loc, x, y) : 'ctyp_quot )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("x\n",
            (Gramf.mk_action
               (fun (x : 'ctyp)  (_loc : Locf.t)  -> (x : 'ctyp_quot )))))]));
  Gramf.extend_single (unquoted_typevars : 'unquoted_typevars Gramf.t )
    (None,
      (None, None,
        [([`Self; `Self],
           ("`App (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) 
                   (_loc : Locf.t)  ->
                   (`App (_loc, t1, t2) : 'unquoted_typevars )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'unquoted_typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
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
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i :>ctyp)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'unquoted_typevars )))))]));
  Gramf.extend_single (type_parameter : 'type_parameter Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant s : 'type_parameter )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant s : 'type_parameter )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Keyword "'"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Normal _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'type_parameter )))));
        ([`Keyword "+";
         `Keyword "'";
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Positive _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Positive _loc), i) : 'type_parameter )))));
        ([`Keyword "-";
         `Keyword "'";
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Negative _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Negative _loc), i) : 'type_parameter )))));
        ([`Keyword "+"; `Keyword "_"],
          ("`QuoteAny (_loc, (`Positive _loc))\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (`QuoteAny (_loc, (`Positive _loc)) : 'type_parameter )))));
        ([`Keyword "-"; `Keyword "_"],
          ("`QuoteAny (_loc, (`Negative _loc))\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (`QuoteAny (_loc, (`Negative _loc)) : 'type_parameter )))));
        ([`Keyword "_"],
          ("`Any _loc\n",
            (Gramf.mk_action
               (fun _  (_loc : Locf.t)  -> (`Any _loc : 'type_parameter )))))]));
  Gramf.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Gramf.t )
    (None,
      (None, None,
        [([`Keyword "(";
          `Nterm (Gramf.obj (type_parameters : 'type_parameters Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("tpl (i :>ctyp)\n",
             (Gramf.mk_action
                (fun (i : 'type_longident)  _  (tpl : 'type_parameters)  _ 
                   (_loc : Locf.t)  ->
                   (tpl (i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
         `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
          ("`App (_loc, (i :>ctyp), (tpl :>ctyp))\n",
            (Gramf.mk_action
               (fun (i : 'type_longident)  (tpl : 'type_parameter) 
                  (_loc : Locf.t)  ->
                  (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters )))));
        ([`Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
          ("(i :>ctyp)\n",
            (Gramf.mk_action
               (fun (i : 'type_longident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant s ~c:\"ctyp\"\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant s ~c:"ctyp" : 'type_longident_and_parameters )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]));
  Gramf.extend_single (type_parameters : 'type_parameters Gramf.t )
    (None,
      (None, None,
        [([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
          `Self],
           ("fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp)))\n",
             (Gramf.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : Locf.t)  ->
                   (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 
                   'type_parameters )))));
        ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))],
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
        [([`Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
          `Keyword ";";
          `Self],
           ("let (ml,v) = rest in ((`Sem (_loc, m, ml)), v)\n",
             (Gramf.mk_action
                (fun (rest : 'meth_list)  _  (m : 'meth_decl) 
                   (_loc : Locf.t)  ->
                   (let (ml,v) = rest in ((`Sem (_loc, m, ml)), v) : 
                   'meth_list )))));
        ([`Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
         `Keyword ";";
         `Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
          ("(m, v)\n",
            (Gramf.mk_action
               (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl)  (_loc : Locf.t) 
                  -> ((m, v) : 'meth_list )))));
        ([`Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
         `Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
          ("(m, v)\n",
            (Gramf.mk_action
               (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : Locf.t)  ->
                  ((m, v) : 'meth_list )))))]));
  Gramf.extend_single (meth_decl : 'meth_decl Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'meth_decl )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'meth_decl )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword ":";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyCol (_loc, lab, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, lab, t) : 'meth_decl )))))]));
  Gramf.extend_single (opt_meth_list : 'opt_meth_list Gramf.t )
    (None,
      (None, None,
        [([`Nterm (Gramf.obj (meth_list : 'meth_list Gramf.t ))],
           ("let (ml,v) = rest in `TyObj (_loc, ml, v)\n",
             (Gramf.mk_action
                (fun (rest : 'meth_list)  (_loc : Locf.t)  ->
                   (let (ml,v) = rest in `TyObj (_loc, ml, v) : 'opt_meth_list )))));
        ([`Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
          ("`TyObjEnd (_loc, v)\n",
            (Gramf.mk_action
               (fun (v : 'opt_dot_dot)  (_loc : Locf.t)  ->
                  (`TyObjEnd (_loc, v) : 'opt_meth_list )))))]));
  Gramf.extend_single (row_field : 'row_field Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'row_field )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "vrn")), "`Ant s")],
          ("`TyVrn (_loc, (mk_ant ~c:\"ctyp\" s))\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "vrn";_} as s) : Tokenf.ant) ->
                      (`TyVrn (_loc, (mk_ant ~c:"ctyp" s)) : 'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "vrn")), "`Ant s");
         `Keyword "of";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyVrnOf (_loc, (mk_ant ~c:\"ctyp\" s), t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "vrn";_} as s) : Tokenf.ant) ->
                      (`TyVrnOf (_loc, (mk_ant ~c:"ctyp" s), t) : 'row_field )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Self; `Keyword "|"; `Self],
          ("`Bar (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'row_field)  _  (t1 : 'row_field)  (_loc : Locf.t) 
                  -> (`Bar (_loc, t1, t2) : 'row_field )))));
        ([`Keyword "`"; `Nterm (Gramf.obj (astr : 'astr Gramf.t ))],
          ("`TyVrn (_loc, i)\n",
            (Gramf.mk_action
               (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrn (_loc, i) : 'row_field )))));
        ([`Keyword "`";
         `Nterm (Gramf.obj (astr : 'astr Gramf.t ));
         `Keyword "of";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyVrnOf (_loc, i, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrnOf (_loc, i, t) : 'row_field )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`Ctyp (_loc, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, t) : 'row_field )))))]));
  Gramf.extend_single (name_tags : 'name_tags Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'name_tags )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'name_tags )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Self; `Self],
          ("`App (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : Locf.t)  ->
                  (`App (_loc, t1, t2) : 'name_tags )))));
        ([`Keyword "`"; `Nterm (Gramf.obj (astr : 'astr Gramf.t ))],
          ("`TyVrn (_loc, i)\n",
            (Gramf.mk_action
               (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                  (`TyVrn (_loc, i) : 'name_tags )))))]));
  Gramf.extend_single (type_declaration : 'type_declaration Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'type_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'type_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Self; `Keyword "and"; `Self],
          ("`And (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) 
                  (_loc : Locf.t)  ->
                  (`And (_loc, t1, t2) : 'type_declaration )))));
        ([`Nterm
            (Gramf.obj
               (type_ident_and_parameters : 'type_ident_and_parameters
                                              Gramf.t ));
         `Keyword "=";
         `Nterm (Gramf.obj (type_info : 'type_info Gramf.t ));
         `Slist0 (`Nterm (Gramf.obj (constrain : 'constrain Gramf.t )))],
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
        ([`Nterm
            (Gramf.obj
               (type_ident_and_parameters : 'type_ident_and_parameters
                                              Gramf.t ));
         `Slist0 (`Nterm (Gramf.obj (constrain : 'constrain Gramf.t )))],
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
        [([`Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyRepr (_loc, (`Negative _loc), t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'type_repr)  (_loc : Locf.t)  ->
                   (`TyRepr (_loc, (`Negative _loc), t2) : 'type_info )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Keyword "=";
         `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
          ("`TyMan (_loc, t1, (`Negative _loc), t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_repr)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                  (`TyMan (_loc, t1, (`Negative _loc), t2) : 'type_info )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyEq (_loc, (`Negative _loc), t1)\n",
            (Gramf.mk_action
               (fun (t1 : 'ctyp)  (_loc : Locf.t)  ->
                  (`TyEq (_loc, (`Negative _loc), t1) : 'type_info )))));
        ([`Keyword "private"; `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyEq (_loc, (`Positive _loc), t1)\n",
            (Gramf.mk_action
               (fun (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                  (`TyEq (_loc, (`Positive _loc), t1) : 'type_info )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
         `Keyword "=";
         `Keyword "private";
         `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
          ("`TyMan (_loc, t1, (`Positive _loc), t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_repr)  _  _  (t1 : 'ctyp)  (_loc : Locf.t) 
                  -> (`TyMan (_loc, t1, (`Positive _loc), t2) : 'type_info )))));
        ([`Keyword "private";
         `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
          ("`TyRepr (_loc, (`Positive _loc), t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'type_repr)  _  (_loc : Locf.t)  ->
                  (`TyRepr (_loc, (`Positive _loc), t2) : 'type_info )))))]));
  Gramf.extend_single (type_repr : 'type_repr Gramf.t )
    (None,
      (None, None,
        [([`Keyword "|";
          `Nterm
            (Gramf.obj
               (constructor_declarations : 'constructor_declarations Gramf.t ))],
           ("`Sum (_loc, t)\n",
             (Gramf.mk_action
                (fun (t : 'constructor_declarations)  _  (_loc : Locf.t)  ->
                   (`Sum (_loc, t) : 'type_repr )))));
        ([`Keyword "{";
         `Nterm
           (Gramf.obj
              (label_declaration_list : 'label_declaration_list Gramf.t ));
         `Keyword "}"],
          ("`Record (_loc, t)\n",
            (Gramf.mk_action
               (fun _  (t : 'label_declaration_list)  _  (_loc : Locf.t)  ->
                  (`Record (_loc, t) : 'type_repr )))))]));
  Gramf.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Gramf.t )
    (None,
      (None, None,
        [([`Keyword "(";
          `Slist1sep
            ((`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))),
              (`Keyword ","));
          `Keyword ")";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i, (`Some (_loc, (com_of_list (tpl :>decl_params list)))))\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (tpl : 'type_parameter list)  _ 
                   (_loc : Locf.t)  ->
                   ((i,
                      (`Some (_loc, (com_of_list (tpl :>decl_params list))))) : 
                   'type_ident_and_parameters )))));
        ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i, (`Some (_loc, (t :>decl_params))))\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (t : 'type_parameter)  (_loc : Locf.t) 
                  ->
                  ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters )))));
        ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i, (`None _loc))\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i, (`None _loc)) : 'type_ident_and_parameters )))))]));
  Gramf.extend_single (constrain : 'constrain Gramf.t )
    (None,
      (None, None,
        [([`Keyword "constraint";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`Eq (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                   (`Eq (_loc, t1, t2) : 'constrain )))))]));
  Gramf.extend_single (typevars : 'typevars Gramf.t )
    (None,
      (None, None,
        [([`Self; `Self],
           ("`App (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : Locf.t)  ->
                   (`App (_loc, t1, t2) : 'typevars )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'typevars )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
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
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Keyword "'"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("`Quote (_loc, (`Normal _loc), i)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'typevars )))))]));
  Gramf.extend (ctyp : 'ctyp Gramf.t )
    (None,
      [((Some "alias"), (Some `LA),
         [([`Self;
           `Keyword "as";
           `Keyword "'";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Alias (_loc, t1, i)\n",
              (Gramf.mk_action
                 (fun (i : 'a_lident)  _  _  (t1 : 'ctyp)  (_loc : Locf.t) 
                    -> (`Alias (_loc, t1, i) : 'ctyp )))))]);
      ((Some "forall"), (Some `LA),
        [([`Keyword "!";
          `Nterm (Gramf.obj (typevars : 'typevars Gramf.t ));
          `Keyword ".";
          `Self],
           ("`TyPol (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _  (_loc : Locf.t) 
                   -> (`TyPol (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "arrow"), (Some `RA),
        [([`Self; `Keyword "->"; `Self],
           ("`Arrow (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`Arrow (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "label"), (Some `NA),
        [([`Keyword "~";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Self],
           ("`Label (_loc, i, t)\n",
             (Gramf.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Label (_loc, i, t) : 'ctyp )))));
        ([`Token
            (((function | `Label _ -> true | _ -> false)), (48004564, `Any),
              "`Label s");
         `Keyword ":";
         `Self],
          ("`Label (_loc, (`Lid (_loc, s)), t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Label ({ txt = s;_} : Tokenf.txt) ->
                      (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function | `Optlabel _ -> true | _ -> false)),
              (688526593, `Any), "`Optlabel s");
         `Self],
          ("`OptLabl (_loc, (`Lid (_loc, s)), t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Optlabel ({ txt = s;_} : Tokenf.txt) ->
                      (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Keyword "?";
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword ":";
         `Self],
          ("`OptLabl (_loc, i, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`OptLabl (_loc, i, t) : 'ctyp )))))]);
      ((Some "apply"), (Some `LA),
        [([`Self; `Self],
           ("`App (_loc, t2, t1)\n",
             (Gramf.mk_action
                (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`App (_loc, t2, t1) : 'ctyp )))))]);
      ((Some "simple"), None,
        [([`Keyword "'"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Gramf.mk_action
                (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                   (`Quote (_loc, (`Normal _loc), i) : 'ctyp )))));
        ([`Keyword "_"],
          ("`Any _loc\n",
            (Gramf.mk_action
               (fun _  (_loc : Locf.t)  -> (`Any _loc : 'ctyp )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "par")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "par";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "id")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "id")), "`Ant s");
         `Keyword ".";
         `Self],
          ("(try\n   let id = ident_of_ctyp t in\n   fun ()  -> (`Dot (_loc, (mk_ant ~c:\"ident\" s), id) : ctyp )\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "id";_} as s) : Tokenf.ant) ->
                      (((try
                           let id = ident_of_ctyp t in
                           fun ()  ->
                             (`Dot (_loc, (mk_ant ~c:"ident" s), id) : 
                             ctyp )
                         with
                         | Invalid_argument s ->
                             (fun ()  -> raise (Streamf.Error s)))) () : 
                      'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
              "`Quot _")],
          ("Ast_quotation.expand x Dyn_tag.ctyp\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Quot x -> (Ast_quotation.expand x Dyn_tag.ctyp : 'ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Keyword ".";
         `Self],
          ("(try let id = ident_of_ctyp t in fun ()  -> `Dot (_loc, (i :>ident), id)\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_uident)  (_loc : Locf.t)  ->
                  ((try
                      let id = ident_of_ctyp t in
                      fun ()  -> `Dot (_loc, (i :>ident), id)
                    with
                    | Invalid_argument s ->
                        (fun ()  -> raise (Streamf.Error s))) () : 'ctyp )))));
        ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
          ("(i :>ctyp)\n",
            (Gramf.mk_action
               (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                  ((i :>ctyp) : 'ctyp )))));
        ([`Keyword "(";
         `Self;
         `Keyword "*";
         `Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ));
         `Keyword ")"],
          ("`Par (_loc, (`Sta (_loc, t, tl)))\n",
            (Gramf.mk_action
               (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _  (_loc : Locf.t) 
                  -> (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
        ([`Keyword "("; `Self; `Keyword ")"],
          ("t\n",
            (Gramf.mk_action
               (fun _  (t : 'ctyp)  _  (_loc : Locf.t)  -> (t : 'ctyp )))));
        ([`Keyword "(";
         `Self;
         `Keyword ",";
         `Nterm (Gramf.obj (com_ctyp : 'com_ctyp Gramf.t ));
         `Keyword ")";
         `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
          ("appl_of_list ((j :>ctyp) :: t :: (Ast_basic.list_of_com tl []))\n",
            (Gramf.mk_action
               (fun (j : 'type_longident)  _  (tl : 'com_ctyp)  _ 
                  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                  (appl_of_list ((j :>ctyp) :: t ::
                     (Ast_basic.list_of_com tl [])) : 'ctyp )))));
        ([`Keyword "[";
         `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Keyword "]"],
          ("`PolyEq (_loc, rfl)\n",
            (Gramf.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolyEq (_loc, rfl) : 'ctyp )))));
        ([`Keyword "[>";
         `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Keyword "]"],
          ("`PolySup (_loc, rfl)\n",
            (Gramf.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolySup (_loc, rfl) : 'ctyp )))));
        ([`Keyword "[<";
         `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Keyword "]"],
          ("`PolyInf (_loc, rfl)\n",
            (Gramf.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                  (`PolyInf (_loc, rfl) : 'ctyp )))));
        ([`Keyword "[<";
         `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
         `Keyword ">";
         `Nterm (Gramf.obj (name_tags : 'name_tags Gramf.t ));
         `Keyword "]"],
          ("`PolyInfSup (_loc, rfl, ntl)\n",
            (Gramf.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                  (_loc : Locf.t)  -> (`PolyInfSup (_loc, rfl, ntl) : 
                  'ctyp )))));
        ([`Keyword "#";
         `Nterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
          ("`ClassPath (_loc, i)\n",
            (Gramf.mk_action
               (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                  (`ClassPath (_loc, i) : 'ctyp )))));
        ([`Keyword "<";
         `Nterm (Gramf.obj (opt_meth_list : 'opt_meth_list Gramf.t ));
         `Keyword ">"],
          ("t\n",
            (Gramf.mk_action
               (fun _  (t : 'opt_meth_list)  _  (_loc : Locf.t)  ->
                  (t : 'ctyp )))));
        ([`Keyword "(";
         `Keyword "module";
         `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
         `Keyword ")"],
          ("`Package (_loc, p)\n",
            (Gramf.mk_action
               (fun _  (p : 'mtyp)  _  _  (_loc : Locf.t)  ->
                  (`Package (_loc, p) : 'ctyp )))))])]);
  Gramf.extend_single (comma_ctyp : 'comma_ctyp Gramf.t )
    (None,
      (None, None,
        [([`Self; `Keyword ","; `Self],
           ("`Com (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : Locf.t)  -> (`Com (_loc, t1, t2) : 'comma_ctyp )))));
        ([`Token
            (((function
               | `Ant ({ kind = "";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "")), "`Ant s")],
          ("mk_ant ~c:\"ctyp,\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp," s : 'comma_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`Ctyp (_loc, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, t) : 'comma_ctyp )))))]));
  Gramf.extend_single (com_ctyp : 'com_ctyp Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'com_ctyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'com_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Self; `Keyword ","; `Self],
          ("`Com (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'com_ctyp)  _  (t1 : 'com_ctyp)  (_loc : Locf.t) 
                  -> (`Com (_loc, t1, t2) : 'com_ctyp )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("t\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'com_ctyp )))))]));
  Gramf.extend_single (star_ctyp : 'star_ctyp Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'star_ctyp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'star_ctyp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Self; `Keyword "*"; `Self],
          ("`Sta (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp)  (_loc : Locf.t) 
                  -> (`Sta (_loc, t1, t2) : 'star_ctyp )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("t\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'star_ctyp )))))]));
  Gramf.extend_single
    (constructor_declarations : 'constructor_declarations Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'constructor_declarations )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'constructor_declarations )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Self; `Keyword "|"; `Self],
          ("`Bar (_loc, t1, t2)\n",
            (Gramf.mk_action
               (fun (t2 : 'constructor_declarations)  _ 
                  (t1 : 'constructor_declarations)  (_loc : Locf.t)  ->
                  (`Bar (_loc, t1, t2) : 'constructor_declarations )))));
        ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Keyword "of";
         `Nterm
           (Gramf.obj (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
          ("`Of (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : Locf.t)  ->
                  (`Of (_loc, s, t) : 'constructor_declarations )))));
        ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Keyword ":";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyCol (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, s, t) : 'constructor_declarations )))));
        ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
          ("(s :>or_ctyp)\n",
            (Gramf.mk_action
               (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                  ((s :>or_ctyp) : 'constructor_declarations )))))]));
  Gramf.extend_single
    (constructor_declaration : 'constructor_declaration Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'constructor_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'constructor_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
         `Keyword "of";
         `Nterm
           (Gramf.obj (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
          ("`Of (_loc, (s :>vid), t)\n",
            (Gramf.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : Locf.t)  ->
                  (`Of (_loc, (s :>vid), t) : 'constructor_declaration )))));
        ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
          ("(s :>of_ctyp)\n",
            (Gramf.mk_action
               (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                  ((s :>of_ctyp) : 'constructor_declaration )))))]));
  Gramf.extend_single (constructor_arg_list : 'constructor_arg_list Gramf.t )
    (None,
      (None, None,
        [([`Self; `Keyword "*"; `Self],
           ("`Sta (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'constructor_arg_list)  _ 
                   (t1 : 'constructor_arg_list)  (_loc : Locf.t)  ->
                   (`Sta (_loc, t1, t2) : 'constructor_arg_list )))));
        ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("t\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                  (t : 'constructor_arg_list )))))]));
  Gramf.extend_single
    (label_declaration_list : 'label_declaration_list Gramf.t )
    (None,
      (None, None,
        [([`Nterm
             (Gramf.obj (label_declaration : 'label_declaration Gramf.t ));
          `Keyword ";";
          `Self],
           ("`Sem (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'label_declaration_list)  _ 
                   (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                   (`Sem (_loc, t1, t2) : 'label_declaration_list )))));
        ([`Nterm
            (Gramf.obj (label_declaration : 'label_declaration Gramf.t ));
         `Keyword ";"],
          ("t1\n",
            (Gramf.mk_action
               (fun _  (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                  (t1 : 'label_declaration_list )))));
        ([`Nterm
            (Gramf.obj (label_declaration : 'label_declaration Gramf.t ))],
          ("t1\n",
            (Gramf.mk_action
               (fun (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                  (t1 : 'label_declaration_list )))))]));
  Gramf.extend_single (label_declaration : 'label_declaration Gramf.t )
    (None,
      (None, None,
        [([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (mk_ant ~c:"ctyp" s : 'label_declaration )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Token
            (((function
               | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
               | _ -> false)), (3257031, (`A "typ")), "`Ant s")],
          ("mk_ant ~c:\"ctyp\" s\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant (({ kind = "typ";_} as s) : Tokenf.ant) ->
                      (mk_ant ~c:"ctyp" s : 'label_declaration )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
        ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword ":";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyCol (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : Locf.t)  ->
                  (`TyCol (_loc, s, t) : 'label_declaration )))));
        ([`Keyword "mutable";
         `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
         `Keyword ":";
         `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
          ("`TyColMut (_loc, s, t)\n",
            (Gramf.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  _  (_loc : Locf.t)  ->
                  (`TyColMut (_loc, s, t) : 'label_declaration )))))]));
  Gramf.extend_single (comma_type_parameter : 'comma_type_parameter Gramf.t )
    (None,
      (None, None,
        [([`Self; `Keyword ","; `Self],
           ("`Com (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun (t2 : 'comma_type_parameter)  _ 
                   (t1 : 'comma_type_parameter)  (_loc : Locf.t)  ->
                   (`Com (_loc, t1, t2) : 'comma_type_parameter )))));
        ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))],
          ("`Ctyp (_loc, (t :>ctyp))\n",
            (Gramf.mk_action
               (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                  (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter )))))]))
let fill_parsers =
  let applied = ref false in
  fun ()  ->
    if not applied.contents then (applied := true; apply (); apply_ctyp ())
let () = Ast_parsers.register_parser ("revise", fill_parsers)