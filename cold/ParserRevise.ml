open Ast

open AstLoc

open FanOps

open Syntax

open LibUtil

open FanUtil

open GramLib

let pos_exps = Gram.mk "pos_exps"

let apply () =
  (let list = ['!'; '?'; '~'] in
   let excl = ["!="; "??"] in
   setup_op_parser prefixop
     (fun x  ->
        (not (List.mem x excl)) &&
          (((String.length x) >= 2) &&
             ((List.mem (x.[0]) list) && (symbolchar x 1)))));
  (let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
   let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
   let excl = ["<-"; "||"; "&&"] in
   setup_op_parser infixop2
     (fun x  ->
        (List.mem x list_ok) ||
          ((not (List.mem x excl)) &&
             (((String.length x) >= 2) &&
                ((List.mem (x.[0]) list_first_char_ok) && (symbolchar x 1))))));
  (let list = ['@'; '^'] in
   setup_op_parser infixop3
     (fun x  ->
        ((String.length x) >= 1) &&
          ((List.mem (x.[0]) list) && (symbolchar x 1))));
  (let list = ['+'; '-'] in
   setup_op_parser infixop4
     (fun x  ->
        (x <> "->") &&
          (((String.length x) >= 1) &&
             ((List.mem (x.[0]) list) && (symbolchar x 1)))));
  (let list = ['*'; '/'; '%'; '\\'] in
   setup_op_parser infixop5
     (fun x  ->
        ((String.length x) >= 1) &&
          ((List.mem (x.[0]) list) &&
             ((((x.[0]) <> '*') ||
                 (((String.length x) < 2) || ((x.[1]) <> '*')))
                && (symbolchar x 1)))));
  setup_op_parser infixop6
    (fun x  ->
       ((String.length x) >= 2) &&
         (((x.[0]) == '*') && (((x.[1]) == '*') && (symbolchar x 2))));
  FanTokenFilter.define_filter (Gram.get_filter ())
    (fun f  strm  -> infix_kwds_filter (f strm));
  Gram.setup_parser sem_exp
    (let symb1 = Gram.parse_origin_tokens exp in
     let symb (__strm : _ XStream.t) =
       match XStream.peek __strm with
       | Some (`Ant (("list" as n),s),_loc) ->
           (XStream.junk __strm; mk_anti ~c:"exp;" _loc n s)
       | _ -> symb1 __strm in
     let rec kont al (__strm : _ XStream.t) =
       match XStream.peek __strm with
       | Some (`KEYWORD ";",_) ->
           (XStream.junk __strm;
            (let a =
               try symb __strm
               with | XStream.Failure  -> raise (XStream.Error "") in
             let s = __strm in
             let _loc = al <+> a in kont (`Sem (_loc, al, a)) s))
       | _ -> al in
     fun (__strm : _ XStream.t)  -> let a = symb __strm in kont a __strm);
  (Gram.extend_single (mexp_quot : 'mexp_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
            ("Gram.mk_action (fun (x : 'mexp)  (_loc : FanLoc.t)  -> (x : 'mexp_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'mexp)  (_loc : FanLoc.t)  -> (x : 'mexp_quot )))))]));
   Gram.extend (mbind0 : 'mbind0 Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
            `Skeyword ")";
            `Sself],
             ("Gram.mk_action\n  (fun (mb : 'mbind0)  _  (mt : 'mtyp)  _  (m : 'a_uident)  _ \n     (_loc : FanLoc.t)  -> (`Functor (_loc, m, mt, mb) : 'mbind0 ))\n",
               (Gram.mk_action
                  (fun (mb : 'mbind0)  _  (mt : 'mtyp)  _  (m : 'a_uident)  _
                      (_loc : FanLoc.t)  ->
                     (`Functor (_loc, m, mt, mb) : 'mbind0 )))));
          ([`Skeyword ":";
           `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
            ("Gram.mk_action\n  (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, me, mt) : 'mbind0 ))\n",
              (Gram.mk_action
                 (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (_loc : FanLoc.t)  ->
                    (`Constraint (_loc, me, mt) : 'mbind0 )))));
          ([`Skeyword "="; `Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
            ("Gram.mk_action (fun (me : 'mexp)  _  (_loc : FanLoc.t)  -> (me : 'mbind0 ))\n",
              (Gram.mk_action
                 (fun (me : 'mexp)  _  (_loc : FanLoc.t)  -> (me : 'mbind0 )))))])]);
   Gram.extend (mexp : 'mexp Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("Gram.mk_action\n  (fun (me : 'mexp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  -> (`Functor (_loc, i, t, me) : 'mexp ))\n",
               (Gram.mk_action
                  (fun (me : 'mexp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : FanLoc.t)  ->
                     (`Functor (_loc, i, t, me) : 'mexp )))));
          ([`Skeyword "struct";
           `Snterm (Gram.obj (strus : 'strus Gram.t ));
           `Skeyword "end"],
            ("Gram.mk_action\n  (fun _  (st : 'strus)  _  (_loc : FanLoc.t)  ->\n     (`Struct (_loc, st) : 'mexp ))\n",
              (Gram.mk_action
                 (fun _  (st : 'strus)  _  (_loc : FanLoc.t)  ->
                    (`Struct (_loc, st) : 'mexp )))));
          ([`Skeyword "struct"; `Skeyword "end"],
            ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`StructEnd _loc : 'mexp ))\n",
              (Gram.mk_action
                 (fun _  _  (_loc : FanLoc.t)  -> (`StructEnd _loc : 'mexp )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("Gram.mk_action\n  (fun (me2 : 'mexp)  (me1 : 'mexp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, me1, me2) : 'mexp ))\n",
              (Gram.mk_action
                 (fun (me2 : 'mexp)  (me1 : 'mexp)  (_loc : FanLoc.t)  ->
                    (`App (_loc, me1, me2) : 'mexp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"mexp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"mexp\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti ~c:\"mexp\" _loc n s : 'mexp )\n     | _ -> failwith \"mk_anti ~c:\"mexp\" _loc n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"mexp"|"anti"|"list" as n),s) ->
                        (mk_anti ~c:"mexp" _loc n s : 'mexp )
                    | _ -> failwith "mk_anti ~c:\"mexp\" _loc n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.mexp_tag : 'mexp )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.mexp_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.mexp_tag : 
                       'mexp )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.mexp_tag\n"))));
         ([`Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'module_longident)  (_loc : FanLoc.t)  -> ((i :>mexp) : 'mexp ))\n",
             (Gram.mk_action
                (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                   ((i :>mexp) : 'mexp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (mt : 'mtyp)  _  (me : 'mexp)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, me, mt) : 'mexp ))\n",
             (Gram.mk_action
                (fun _  (mt : 'mtyp)  _  (me : 'mexp)  _  (_loc : FanLoc.t) 
                   -> (`Constraint (_loc, me, mt) : 'mexp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (me : 'mexp)  _  (_loc : FanLoc.t)  -> (me : 'mexp ))\n",
             (Gram.mk_action
                (fun _  (me : 'mexp)  _  (_loc : FanLoc.t)  -> (me : 'mexp )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  _  (_loc : FanLoc.t)  ->\n     (`PackageModule (_loc, e) : 'mexp ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  _  (_loc : FanLoc.t)  ->
                   (`PackageModule (_loc, e) : 'mexp )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'mtyp)  _  (e : 'exp)  _  _  (_loc : FanLoc.t)  ->\n     (`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : \n     'mexp ))\n",
             (Gram.mk_action
                (fun _  (p : 'mtyp)  _  (e : 'exp)  _  _  (_loc : FanLoc.t) 
                   ->
                   (`PackageModule
                      (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                   'mexp )))))])]));
  (Gram.extend_single (mbind_quot : 'mbind_quot Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'mbind_quot)  _  (b1 : 'mbind_quot)  (_loc : FanLoc.t)  ->\n     (`And (_loc, b1, b2) : 'mbind_quot ))\n",
              (Gram.mk_action
                 (fun (b2 : 'mbind_quot)  _  (b1 : 'mbind_quot) 
                    (_loc : FanLoc.t)  ->
                    (`And (_loc, b1, b2) : 'mbind_quot )))));
         ([`Stoken
             (((function | `Ant (("mbind"|"anti"|""),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"mbind\"|\"anti\"|\"\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"mbind\"|\"anti\"|\"\" as n),s) ->\n         (mk_anti _loc ~c:\"mbind\" n s : 'mbind_quot )\n     | _ -> failwith \"mk_anti _loc ~c:\"mbind\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind"|"anti"|"" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind_quot )
                   | _ -> failwith "mk_anti _loc ~c:\"mbind\" n s\n"))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, m, mt) : 'mbind_quot ))\n",
             (Gram.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, m, mt) : 'mbind_quot )))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
           ("Gram.mk_action\n  (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : FanLoc.t) \n     -> (`ModuleBind (_loc, m, mt, me) : 'mbind_quot ))\n",
             (Gram.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : FanLoc.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind_quot )))))]));
   Gram.extend_single (mbind : 'mbind Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'mbind)  _  (b1 : 'mbind)  (_loc : FanLoc.t)  ->\n     (`And (_loc, b1, b2) : 'mbind ))\n",
              (Gram.mk_action
                 (fun (b2 : 'mbind)  _  (b1 : 'mbind)  (_loc : FanLoc.t)  ->
                    (`And (_loc, b1, b2) : 'mbind )))));
         ([`Stoken
             (((function
                | `Ant (("mbind"|"anti"|"list"|""),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"mbind\"|\"anti\"|\"list\"|\"\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"mbind\"|\"anti\"|\"list\"|\"\" as n),s) ->\n         (mk_anti _loc ~c:\"mbind\" n s : 'mbind )\n     | _ -> failwith \"mk_anti _loc ~c:\"mbind\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("mbind"|"anti"|"list"|"" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'mbind )
                   | _ -> failwith "mk_anti _loc ~c:\"mbind\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.mbind_tag : 'mbind )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.mbind_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.mbind_tag : 
                       'mbind )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.mbind_tag\n"))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
           ("Gram.mk_action\n  (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : FanLoc.t) \n     -> (`ModuleBind (_loc, m, mt, me) : 'mbind ))\n",
             (Gram.mk_action
                (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                   (_loc : FanLoc.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'mbind )))))]));
   Gram.extend_single
     (module_rec_declaration : 'module_rec_declaration Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (m2 : 'module_rec_declaration)  _  (m1 : 'module_rec_declaration) \n     (_loc : FanLoc.t)  -> (`And (_loc, m1, m2) : 'module_rec_declaration ))\n",
              (Gram.mk_action
                 (fun (m2 : 'module_rec_declaration)  _ 
                    (m1 : 'module_rec_declaration)  (_loc : FanLoc.t)  ->
                    (`And (_loc, m1, m2) : 'module_rec_declaration )))));
         ([`Stoken
             (((function
                | `Ant ((""|"mbind"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"mbind\"|\"anti\"|\"list\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"mbind\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"mbind\" n s : 'module_rec_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"mbind\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"mbind"|"anti"|"list" as n),s) ->
                       (mk_anti _loc ~c:"mbind" n s : 'module_rec_declaration )
                   | _ -> failwith "mk_anti _loc ~c:\"mbind\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.mbind_tag : 'module_rec_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.mbind_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.mbind_tag : 
                       'module_rec_declaration )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.mbind_tag\n"))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, m, mt) : 'module_rec_declaration ))\n",
             (Gram.mk_action
                (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, m, mt) : 'module_rec_declaration )))))])));
  (Gram.extend_single (constr_quot : 'constr_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (constr : 'constr Gram.t ))],
            ("Gram.mk_action (fun (x : 'constr)  (_loc : FanLoc.t)  -> (x : 'constr_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'constr)  (_loc : FanLoc.t)  ->
                    (x : 'constr_quot )))))]));
   Gram.extend_single (constr : 'constr Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (wc2 : 'constr)  _  (wc1 : 'constr)  (_loc : FanLoc.t)  ->\n     (`And (_loc, wc1, wc2) : 'constr ))\n",
              (Gram.mk_action
                 (fun (wc2 : 'constr)  _  (wc1 : 'constr)  (_loc : FanLoc.t) 
                    -> (`And (_loc, wc1, wc2) : 'constr )))));
         ([`Stoken
             (((function
                | `Ant ((""|"constr"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"constr\"|\"anti\"|\"list\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"constr\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"constr\" n s : 'constr )\n     | _ -> failwith \"mk_anti _loc ~c:\"constr\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"constr"|"anti"|"list" as n),s) ->
                       (mk_anti _loc ~c:"constr" n s : 'constr )
                   | _ -> failwith "mk_anti _loc ~c:\"constr\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.constr_tag : 'constr )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.constr_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.constr_tag : 
                       'constr )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.constr_tag\n"))));
         ([`Skeyword "type";
          `Snterm
            (Gram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters)  _ \n     (_loc : FanLoc.t)  -> (`TypeEq (_loc, t1, t2) : 'constr ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : FanLoc.t)  ->
                   (`TypeEq (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "type";
          `Snterm
            (Gram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gram.t ));
          `Skeyword "=";
          `Skeyword "private";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  _  (t1 : 'type_longident_and_parameters)  _ \n     (_loc : FanLoc.t)  -> (`TypeEqPriv (_loc, t1, t2) : 'constr ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  _ 
                   (t1 : 'type_longident_and_parameters)  _ 
                   (_loc : FanLoc.t)  ->
                   (`TypeEqPriv (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "type";
          `Snterm
            (Gram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gram.t ));
          `Skeyword ":=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters)  _ \n     (_loc : FanLoc.t)  -> (`TypeSubst (_loc, t1, t2) : 'constr ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : FanLoc.t)  ->
                   (`TypeSubst (_loc, t1, t2) : 'constr )))));
         ([`Skeyword "module";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
          `Skeyword "=";
          `Snterm
            (Gram.obj
               (module_longident_with_app : 'module_longident_with_app Gram.t ))],
           ("Gram.mk_action\n  (fun (i2 : 'module_longident_with_app)  _  (i1 : 'module_longident)  _ \n     (_loc : FanLoc.t)  ->\n     (`ModuleEq (_loc, (i1 : vid  :>ident), i2) : 'constr ))\n",
             (Gram.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (`ModuleEq (_loc, (i1 : vid  :>ident), i2) : 'constr )))));
         ([`Skeyword "module";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
          `Skeyword ":=";
          `Snterm
            (Gram.obj
               (module_longident_with_app : 'module_longident_with_app Gram.t ))],
           ("Gram.mk_action\n  (fun (i2 : 'module_longident_with_app)  _  (i1 : 'module_longident)  _ \n     (_loc : FanLoc.t)  ->\n     (`ModuleSubst (_loc, (i1 : vid  :>ident), i2) : 'constr ))\n",
             (Gram.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (`ModuleSubst (_loc, (i1 : vid  :>ident), i2) : 'constr )))))])));
  (Gram.extend (mtyp : 'mtyp Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
            `Skeyword ":";
            `Sself;
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  -> (`Functor (_loc, i, t, mt) : 'mtyp ))\n",
               (Gram.mk_action
                  (fun (mt : 'mtyp)  _  _  (t : 'mtyp)  _  (i : 'a_uident)  _
                      _  (_loc : FanLoc.t)  ->
                     (`Functor (_loc, i, t, mt) : 'mtyp )))))]);
       ((Some "with"), None,
         [([`Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (constr : 'constr Gram.t ))],
            ("Gram.mk_action\n  (fun (wc : 'constr)  _  (mt : 'mtyp)  (_loc : FanLoc.t)  ->\n     (`With (_loc, mt, wc) : 'mtyp ))\n",
              (Gram.mk_action
                 (fun (wc : 'constr)  _  (mt : 'mtyp)  (_loc : FanLoc.t)  ->
                    (`With (_loc, mt, wc) : 'mtyp )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("Gram.mk_action\n  (fun (mt2 : 'mtyp)  (mt1 : 'mtyp)  (_loc : FanLoc.t)  ->\n     (match (mt1, mt2) with\n      | ((#ident as i1),(#ident as i2)) -> apply i1 i2\n      | _ -> raise XStream.Failure : 'mtyp ))\n",
              (Gram.mk_action
                 (fun (mt2 : 'mtyp)  (mt1 : 'mtyp)  (_loc : FanLoc.t)  ->
                    (match (mt1, mt2) with
                     | ((#ident as i1),(#ident as i2)) -> apply i1 i2
                     | _ -> raise XStream.Failure : 'mtyp )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("Gram.mk_action\n  (fun (mt2 : 'mtyp)  _  (mt1 : 'mtyp)  (_loc : FanLoc.t)  ->\n     (let acc0 mt1 mt2 =\n        match (mt1, mt2) with\n        | ((#ident as i1),(#ident as i2)) -> dot i1 i2\n        | _ -> raise XStream.Failure in\n      acc0 mt1 mt2 : 'mtyp ))\n",
              (Gram.mk_action
                 (fun (mt2 : 'mtyp)  _  (mt1 : 'mtyp)  (_loc : FanLoc.t)  ->
                    (let acc0 mt1 mt2 =
                       match (mt1, mt2) with
                       | ((#ident as i1),(#ident as i2)) -> dot i1 i2
                       | _ -> raise XStream.Failure in
                     acc0 mt1 mt2 : 'mtyp )))))]);
       ((Some "sig"), None,
         [([`Skeyword "sig";
           `Snterm (Gram.obj (sigis : 'sigis Gram.t ));
           `Skeyword "end"],
            ("Gram.mk_action\n  (fun _  (sg : 'sigis)  _  (_loc : FanLoc.t)  -> (`Sig (_loc, sg) : 'mtyp ))\n",
              (Gram.mk_action
                 (fun _  (sg : 'sigis)  _  (_loc : FanLoc.t)  ->
                    (`Sig (_loc, sg) : 'mtyp )))));
         ([`Skeyword "sig"; `Skeyword "end"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`SigEnd _loc : 'mtyp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`SigEnd _loc : 'mtyp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"mtyp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"mtyp\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"mtyp\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"mtyp\" n s : 'mtyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"mtyp\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"mtyp"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"mtyp" n s : 'mtyp )
                    | _ -> failwith "mk_anti _loc ~c:\"mtyp\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.mtyp_tag : 'mtyp )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.mtyp_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.mtyp_tag : 
                       'mtyp )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.mtyp_tag\n"))));
         ([`Snterm
             (Gram.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->\n     ((i : ident  :>mtyp) : 'mtyp ))\n",
             (Gram.mk_action
                (fun (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                   ((i : ident  :>mtyp) : 'mtyp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (mt : 'mtyp)  _  (_loc : FanLoc.t)  -> (mt : 'mtyp ))\n",
             (Gram.mk_action
                (fun _  (mt : 'mtyp)  _  (_loc : FanLoc.t)  -> (mt : 'mtyp )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Skeyword "of";
          `Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
           ("Gram.mk_action\n  (fun (me : 'mexp)  _  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleTypeOf (_loc, me) : 'mtyp ))\n",
             (Gram.mk_action
                (fun (me : 'mexp)  _  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleTypeOf (_loc, me) : 'mtyp )))))])]);
   Gram.extend_single (module_declaration : 'module_declaration Gram.t )
     (None,
       (None, None,
         [([`Skeyword ":"; `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
            ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  (_loc : FanLoc.t)  -> (mt : 'module_declaration ))\n",
              (Gram.mk_action
                 (fun (mt : 'mtyp)  _  (_loc : FanLoc.t)  ->
                    (mt : 'module_declaration )))));
         ([`Skeyword "(";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword ")";
          `Sself],
           ("Gram.mk_action\n  (fun (mt : 'module_declaration)  _  (t : 'mtyp)  _  (i : 'a_uident)  _ \n     (_loc : FanLoc.t)  -> (`Functor (_loc, i, t, mt) : 'module_declaration ))\n",
             (Gram.mk_action
                (fun (mt : 'module_declaration)  _  (t : 'mtyp)  _ 
                   (i : 'a_uident)  _  (_loc : FanLoc.t)  ->
                   (`Functor (_loc, i, t, mt) : 'module_declaration )))))]));
   Gram.extend_single (mtyp_quot : 'mtyp_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
            ("Gram.mk_action (fun (x : 'mtyp)  (_loc : FanLoc.t)  -> (x : 'mtyp_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'mtyp)  (_loc : FanLoc.t)  -> (x : 'mtyp_quot )))))])));
  (Gram.extend_single (sigi_quot : 'sigi_quot Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (s : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`DirectiveSimple (_loc, s) : 'sigi_quot ))\n",
              (Gram.mk_action
                 (fun (s : 'a_lident)  _  (_loc : FanLoc.t)  ->
                    (`DirectiveSimple (_loc, s) : 'sigi_quot )))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (dp : 'exp)  (s : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Directive (_loc, s, dp) : 'sigi_quot ))\n",
             (Gram.mk_action
                (fun (dp : 'exp)  (s : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Directive (_loc, s, dp) : 'sigi_quot )))));
         ([`Snterm (Gram.obj (sigi : 'sigi Gram.t )); `Skeyword ";"; `Sself],
           ("Gram.mk_action\n  (fun (sg2 : 'sigi_quot)  _  (sg1 : 'sigi)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, sg1, sg2) : 'sigi_quot ))\n",
             (Gram.mk_action
                (fun (sg2 : 'sigi_quot)  _  (sg1 : 'sigi)  (_loc : FanLoc.t) 
                   -> (`Sem (_loc, sg1, sg2) : 'sigi_quot )))));
         ([`Snterm (Gram.obj (sigi : 'sigi Gram.t ))],
           ("Gram.mk_action (fun (sg : 'sigi)  (_loc : FanLoc.t)  -> (sg : 'sigi_quot ))\n",
             (Gram.mk_action
                (fun (sg : 'sigi)  (_loc : FanLoc.t)  -> (sg : 'sigi_quot )))))]));
   Gram.extend_single (sigi : 'sigi Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"sigi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"sigi\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"sigi\" n s : 'sigi )\n     | _ -> failwith \"mk_anti _loc ~c:\"sigi\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"sigi"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"sigi" n s : 'sigi )
                    | _ -> failwith "mk_anti _loc ~c:\"sigi\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.sigi_tag : 'sigi )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.sigi_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.sigi_tag : 
                       'sigi )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.sigi_tag\n"))));
         ([`Skeyword "exception";
          `Snterm
            (Gram.obj
               (constructor_declaration : 'constructor_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Exception (_loc, t) : 'sigi ))\n",
             (Gram.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->
                   (`Exception (_loc, t) : 'sigi )))));
         ([`Skeyword "external";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (string_list : 'string_list Gram.t ))],
           ("Gram.mk_action\n  (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`External (_loc, i, t, sl) : 'sigi ))\n",
             (Gram.mk_action
                (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident) 
                   _  (_loc : FanLoc.t)  ->
                   (`External (_loc, i, t, sl) : 'sigi )))));
         ([`Skeyword "include"; `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  (_loc : FanLoc.t)  -> (`Include (_loc, mt) : 'sigi ))\n",
             (Gram.mk_action
                (fun (mt : 'mtyp)  _  (_loc : FanLoc.t)  ->
                   (`Include (_loc, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Snterm
            (Gram.obj (module_declaration : 'module_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'module_declaration)  (i : 'a_uident)  _  (_loc : FanLoc.t)  ->\n     (`Module (_loc, i, mt) : 'sigi ))\n",
             (Gram.mk_action
                (fun (mt : 'module_declaration)  (i : 'a_uident)  _ 
                   (_loc : FanLoc.t)  -> (`Module (_loc, i, mt) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "rec";
          `Snterm
            (Gram.obj
               (module_rec_declaration : 'module_rec_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (mb : 'module_rec_declaration)  _  _  (_loc : FanLoc.t)  ->\n     (`RecModule (_loc, mb) : 'sigi ))\n",
             (Gram.mk_action
                (fun (mb : 'module_rec_declaration)  _  _  (_loc : FanLoc.t) 
                   -> (`RecModule (_loc, mb) : 'sigi )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleType (_loc, i, mt) : 'sigi ))\n",
             (Gram.mk_action
                (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  -> (`ModuleType (_loc, i, mt) : 
                   'sigi )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleTypeEnd (_loc, i) : 'sigi ))\n",
             (Gram.mk_action
                (fun (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleTypeEnd (_loc, i) : 'sigi )))));
         ([`Skeyword "open";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->\n     (`Open (_loc, (i : vid  :>ident)) : 'sigi ))\n",
             (Gram.mk_action
                (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (`Open (_loc, (i : vid  :>ident)) : 'sigi )))));
         ([`Skeyword "type";
          `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Type (_loc, t) : 'sigi ))\n",
             (Gram.mk_action
                (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                   (`Type (_loc, t) : 'sigi )))));
         ([`Skeyword "val";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Val (_loc, i, t) : 'sigi ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`Val (_loc, i, t) : 'sigi )))));
         ([`Skeyword "class";
          `Snterm (Gram.obj (class_description : 'class_description Gram.t ))],
           ("Gram.mk_action\n  (fun (cd : 'class_description)  _  (_loc : FanLoc.t)  ->\n     (`Class (_loc, cd) : 'sigi ))\n",
             (Gram.mk_action
                (fun (cd : 'class_description)  _  (_loc : FanLoc.t)  ->
                   (`Class (_loc, cd) : 'sigi )))));
         ([`Skeyword "class";
          `Skeyword "type";
          `Snterm (Gram.obj (cltyp_declaration : 'cltyp_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (ctd : 'cltyp_declaration)  _  _  (_loc : FanLoc.t)  ->\n     (`ClassType (_loc, ctd) : 'sigi ))\n",
             (Gram.mk_action
                (fun (ctd : 'cltyp_declaration)  _  _  (_loc : FanLoc.t)  ->
                   (`ClassType (_loc, ctd) : 'sigi )))))]));
   Gram.extend_single (interf : 'interf Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ";;"],
            ("Gram.mk_action\n  (fun _  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (([`DirectiveSimple (_loc, n)], (Some _loc)) : 'interf ))\n",
              (Gram.mk_action
                 (fun _  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->
                    (([`DirectiveSimple (_loc, n)], (Some _loc)) : 'interf )))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ";;"],
           ("Gram.mk_action\n  (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (([`Directive (_loc, n, dp)], (Some _loc)) : 'interf ))\n",
             (Gram.mk_action
                (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (([`Directive (_loc, n, dp)], (Some _loc)) : 'interf )))));
         ([`Snterm (Gram.obj (sigi : 'sigi Gram.t )); `Skeyword ";"; `Sself],
           ("Gram.mk_action\n  (fun ((sil,stopped) : 'interf)  _  (si : 'sigi)  (_loc : FanLoc.t)  ->\n     (((si :: sil), stopped) : 'interf ))\n",
             (Gram.mk_action
                (fun ((sil,stopped) : 'interf)  _  (si : 'sigi) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'interf )))));
         ([`Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `EOI -> (([], None) : 'interf )\n     | _ -> failwith \"([], None)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `EOI -> (([], None) : 'interf )
                   | _ -> failwith "([], None)\n"))))]));
   Gram.extend_single (sigis : 'sigis Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"sigi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"sigi\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc n ~c:\"sigi\" s : 'sigis )\n     | _ -> failwith \"mk_anti _loc n ~c:\"sigi\" s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"sigi"|"anti"|"list" as n),s) ->
                        (mk_anti _loc n ~c:"sigi" s : 'sigis )
                    | _ -> failwith "mk_anti _loc n ~c:\"sigi\" s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"sigi"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (sg : 'sigis)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"sigi\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg) : 'sigis )\n     | _ -> failwith \"`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n\")\n",
             (Gram.mk_action
                (fun (sg : 'sigis)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"sigi"|"anti"|"list" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"sigi" s), sg) : 
                       'sigis )
                   | _ ->
                       failwith
                         "`Sem (_loc, (mk_anti _loc n ~c:\"sigi\" s), sg)\n"))));
         ([`Slist1
             (Gram.srules
                [([`Snterm (Gram.obj (sigi : 'sigi Gram.t )); `Skeyword ";"],
                   ("Gram.mk_action (fun _  (sg : 'sigi)  (_loc : FanLoc.t)  -> (sg : 'e__1 ))\n",
                     (Gram.mk_action
                        (fun _  (sg : 'sigi)  (_loc : FanLoc.t)  ->
                           (sg : 'e__1 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__1 list)  (_loc : FanLoc.t)  -> (sem_of_list l : 'sigis ))\n",
             (Gram.mk_action
                (fun (l : 'e__1 list)  (_loc : FanLoc.t)  ->
                   (sem_of_list l : 'sigis )))))])));
  (let grammar_entry_create = Gram.mk in
   let fun_def_pat: 'fun_def_pat Gram.t = grammar_entry_create "fun_def_pat" in
   Gram.extend_single (exp_quot : 'exp_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Skeyword ",";
           `Snterm (Gram.obj (comma_exp : 'comma_exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e2 : 'comma_exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, e1, e2) : 'exp_quot ))\n",
              (Gram.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'exp)  (_loc : FanLoc.t) 
                    -> (`Com (_loc, e1, e2) : 'exp_quot )))));
         ([`Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ";";
          `Snterm (Gram.obj (sem_exp : 'sem_exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e2 : 'sem_exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, e1, e2) : 'exp_quot ))\n",
             (Gram.mk_action
                (fun (e2 : 'sem_exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, e1, e2) : 'exp_quot )))));
         ([`Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action (fun (e : 'exp)  (_loc : FanLoc.t)  -> (e : 'exp_quot ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  (_loc : FanLoc.t)  -> (e : 'exp_quot )))))]));
   Gram.extend_single (cvalue_binding : 'cvalue_binding Gram.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  -> (e : 'cvalue_binding ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                    (e : 'cvalue_binding )))));
         ([`Skeyword ":";
          `Skeyword "type";
          `Snterm (Gram.obj (unquoted_typevars : 'unquoted_typevars Gram.t ));
          `Skeyword ".";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t1 : 'unquoted_typevars)  _  _ \n     (_loc : FanLoc.t)  ->\n     (let u = `TyPol (_loc, t1, t2) in `Constraint (_loc, e, u) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _ 
                   (t1 : 'unquoted_typevars)  _  _  (_loc : FanLoc.t)  ->
                   (let u = `TyPol (_loc, t1, t2) in `Constraint (_loc, e, u) : 
                   'cvalue_binding )))));
         ([`Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, e, t) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, e, t) : 'cvalue_binding )))));
         ([`Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (match t with\n      | `TyPol (_loc,_,_) -> raise (XStream.Error \"unexpected polytype here\")\n      | _ -> `Coercion (_loc, e, t, t2) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _ 
                   (_loc : FanLoc.t)  ->
                   (match t with
                    | `TyPol (_loc,_,_) ->
                        raise (XStream.Error "unexpected polytype here")
                    | _ -> `Coercion (_loc, e, t, t2) : 'cvalue_binding )))));
         ([`Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Subtype (_loc, e, t) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Subtype (_loc, e, t) : 'cvalue_binding )))))]));
   Gram.extend (fun_binding : 'fun_binding Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
            `Skeyword ")";
            `Sself],
             ("Gram.mk_action\n  (fun (e : 'fun_binding)  _  (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (`LocalTypeFun (_loc, i, e) : 'fun_binding ))\n",
               (Gram.mk_action
                  (fun (e : 'fun_binding)  _  (i : 'a_lident)  _  _ 
                     (_loc : FanLoc.t)  ->
                     (`LocalTypeFun (_loc, i, e) : 'fun_binding )))));
          ([`Snterm (Gram.obj (ipat : 'ipat Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (e : 'fun_binding)  (p : 'ipat)  (_loc : FanLoc.t)  ->\n     (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_binding ))\n",
              (Gram.mk_action
                 (fun (e : 'fun_binding)  (p : 'ipat)  (_loc : FanLoc.t)  ->
                    (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_binding )))));
          ([`Snterm (Gram.obj (cvalue_binding : 'cvalue_binding Gram.t ))],
            ("Gram.mk_action\n  (fun (bi : 'cvalue_binding)  (_loc : FanLoc.t)  -> (bi : 'fun_binding ))\n",
              (Gram.mk_action
                 (fun (bi : 'cvalue_binding)  (_loc : FanLoc.t)  ->
                    (bi : 'fun_binding )))))])]);
   Gram.extend_single (lang : 'lang Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (dot_lstrings : 'dot_lstrings Gram.t ))],
            ("Gram.mk_action\n  (fun (ls : 'dot_lstrings)  (_loc : FanLoc.t)  ->\n     (let old = AstQuotation.default.contents in\n      AstQuotation.default := (FanToken.resolve_name ls); old : 'lang ))\n",
              (Gram.mk_action
                 (fun (ls : 'dot_lstrings)  (_loc : FanLoc.t)  ->
                    (let old = AstQuotation.default.contents in
                     AstQuotation.default := (FanToken.resolve_name ls); old : 
                    'lang )))))]));
   Gram.extend_single (pos_exps : 'pos_exps Gram.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((Gram.srules
                  [([`Stoken
                       (((function | `Lid _ -> true | _ -> false)),
                         (`Normal, "`Lid _"));
                    `Skeyword ":";
                    `Snterm (Gram.obj (dot_lstrings : 'dot_lstrings Gram.t ))],
                     ("Gram.mk_action\n  (fun (y : 'dot_lstrings)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Lid x -> (((x : string ), (FanToken.resolve_name y)) : 'e__2 )\n     | _ -> failwith \"((x : string ), (FanToken.resolve_name y))\n\")\n",
                       (Gram.mk_action
                          (fun (y : 'dot_lstrings)  _ 
                             (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) 
                             ->
                             match __fan_0 with
                             | `Lid x ->
                                 (((x : string ), (FanToken.resolve_name y)) : 
                                 'e__2 )
                             | _ ->
                                 failwith
                                   "((x : string ), (FanToken.resolve_name y))\n"))));
                  ([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         (((x : string ), (FanToken.resolve_name ((`Sub []), x))) : 'e__2 )\n     | _ ->\n         failwith \"((x : string ), (FanToken.resolve_name ((`Sub []), x)))\n\")\n",
                      (Gram.mk_action
                         (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) 
                            ->
                            match __fan_0 with
                            | `Lid x ->
                                (((x : string ),
                                   (FanToken.resolve_name ((`Sub []), x))) : 
                                'e__2 )
                            | _ ->
                                failwith
                                  "((x : string ), (FanToken.resolve_name ((`Sub []), x)))\n"))))]),
                (`Skeyword ";"))],
            ("Gram.mk_action\n  (fun (xys : 'e__2 list)  (_loc : FanLoc.t)  ->\n     (let old = AstQuotation.map.contents in\n      AstQuotation.map := (SMap.add_list xys old); old : 'pos_exps ))\n",
              (Gram.mk_action
                 (fun (xys : 'e__2 list)  (_loc : FanLoc.t)  ->
                    (let old = AstQuotation.map.contents in
                     AstQuotation.map := (SMap.add_list xys old); old : 
                    'pos_exps )))))]));
   Gram.extend_single (fun_def_pat : 'fun_def_pat Gram.t )
     (None,
       (None, None,
         [([`Skeyword "(";
           `Skeyword "type";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ")"],
            ("Gram.mk_action\n  (fun _  (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat ))\n",
              (Gram.mk_action
                 (fun _  (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->
                    (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat )))));
         ([`Snterm (Gram.obj (ipat : 'ipat Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'ipat)  (_loc : FanLoc.t)  ->\n     (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 'fun_def_pat ))\n",
             (Gram.mk_action
                (fun (p : 'ipat)  (_loc : FanLoc.t)  ->
                   (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 'fun_def_pat )))));
         ([`Snterm (Gram.obj (ipat : 'ipat Gram.t ));
          `Skeyword "when";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (w : 'exp)  _  (p : 'ipat)  (_loc : FanLoc.t)  ->\n     (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 'fun_def_pat ))\n",
             (Gram.mk_action
                (fun (w : 'exp)  _  (p : 'ipat)  (_loc : FanLoc.t)  ->
                   (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 
                   'fun_def_pat )))))]));
   Gram.extend (fun_def : 'fun_def Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Snterm (Gram.obj (fun_def_pat : 'fun_def_pat Gram.t ));
            `Skeyword "->";
            `Snterm (Gram.obj (exp : 'exp Gram.t ))],
             ("Gram.mk_action\n  (fun (e : 'exp)  _  (f : 'fun_def_pat)  (_loc : FanLoc.t)  ->\n     (f e : 'fun_def ))\n",
               (Gram.mk_action
                  (fun (e : 'exp)  _  (f : 'fun_def_pat)  (_loc : FanLoc.t) 
                     -> (f e : 'fun_def )))));
          ([`Snterm (Gram.obj (fun_def_pat : 'fun_def_pat Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (e : 'fun_def)  (f : 'fun_def_pat)  (_loc : FanLoc.t)  ->\n     (f e : 'fun_def ))\n",
              (Gram.mk_action
                 (fun (e : 'fun_def)  (f : 'fun_def_pat)  (_loc : FanLoc.t) 
                    -> (f e : 'fun_def )))))])]);
   Gram.extend (exp : 'exp Gram.t )
     (None,
       [((Some "top"), (Some `RA),
          [([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Sself],
             ("Gram.mk_action\n  (fun (x : 'exp)  _  (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) \n     -> (`LetIn (_loc, r, bi, x) : 'exp ))\n",
               (Gram.mk_action
                  (fun (x : 'exp)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                     (_loc : FanLoc.t)  -> (`LetIn (_loc, r, bi, x) : 
                     'exp )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Snterm (Gram.obj (mbind0 : 'mbind0 Gram.t ));
           `Skeyword "in";
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  -> (`LetModule (_loc, m, mb, e) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`LetModule (_loc, m, mb, e) : 'exp )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (i : 'module_longident)  _  _  (_loc : FanLoc.t)  ->\n     (`LetOpen (_loc, (i : vid  :>ident), e) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`LetOpen (_loc, (i : vid  :>ident), e) : 'exp )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (case : 'case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'binding)  (r : 'opt_rec)  _  _ \n     (_loc : FanLoc.t)  -> (`LetTryInWith (_loc, r, bi, x, a) : 'exp ))\n",
              (Gram.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'binding) 
                    (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->
                    (`LetTryInWith (_loc, r, bi, x, a) : 'exp )))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (case : 'case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'case)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Match (_loc, e, a) : 'exp ))\n",
              (Gram.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                    (`Match (_loc, e, a) : 'exp )))));
          ([`Skeyword "try";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (case : 'case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'case)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Try (_loc, e, a) : 'exp ))\n",
              (Gram.mk_action
                 (fun (a : 'case)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                    (`Try (_loc, e, a) : 'exp )))));
          ([`Skeyword "if";
           `Sself;
           `Skeyword "then";
           `Sself;
           `Skeyword "else";
           `Sself],
            ("Gram.mk_action\n  (fun (e3 : 'exp)  _  (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`IfThenElse (_loc, e1, e2, e3) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e3 : 'exp)  _  (e2 : 'exp)  _  (e1 : 'exp)  _ 
                    (_loc : FanLoc.t)  ->
                    (`IfThenElse (_loc, e1, e2, e3) : 'exp )))));
          ([`Skeyword "if"; `Sself; `Skeyword "then"; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`IfThen (_loc, e1, e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : FanLoc.t)  ->
                    (`IfThen (_loc, e1, e2) : 'exp )))));
          ([`Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->\n     (`Seq (_loc, seq) : 'exp ))\n",
              (Gram.mk_action
                 (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                    (`Seq (_loc, seq) : 'exp )))));
          ([`Skeyword "with";
           `Snterm (Gram.obj (lang : 'lang Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (x : 'exp)  (old : 'lang)  _  (_loc : FanLoc.t)  ->\n     (AstQuotation.default := old; x : 'exp ))\n",
              (Gram.mk_action
                 (fun (x : 'exp)  (old : 'lang)  _  (_loc : FanLoc.t)  ->
                    (AstQuotation.default := old; x : 'exp )))));
          ([`Skeyword "with";
           `Skeyword "{";
           `Snterm (Gram.obj (pos_exps : 'pos_exps Gram.t ));
           `Skeyword "}";
           `Sself],
            ("Gram.mk_action\n  (fun (x : 'exp)  _  (old : 'pos_exps)  _  _  (_loc : FanLoc.t)  ->\n     (AstQuotation.map := old; x : 'exp ))\n",
              (Gram.mk_action
                 (fun (x : 'exp)  _  (old : 'pos_exps)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (AstQuotation.map := old; x : 'exp )))));
          ([`Skeyword "for";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword "=";
           `Sself;
           `Snterm (Gram.obj (direction_flag : 'direction_flag Gram.t ));
           `Sself;
           `Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (e2 : 'exp)  (df : 'direction_flag) \n     (e1 : 'exp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`For (_loc, i, e1, e2, df, seq) : 'exp ))\n",
              (Gram.mk_action
                 (fun _  (seq : 'sequence)  _  (e2 : 'exp) 
                    (df : 'direction_flag)  (e1 : 'exp)  _  (i : 'a_lident) 
                    _  (_loc : FanLoc.t)  ->
                    (`For (_loc, i, e1, e2, df, seq) : 'exp )))));
          ([`Skeyword "while";
           `Sself;
           `Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`While (_loc, e, seq) : 'exp ))\n",
              (Gram.mk_action
                 (fun _  (seq : 'sequence)  _  (e : 'exp)  _ 
                    (_loc : FanLoc.t)  -> (`While (_loc, e, seq) : 'exp )))))]);
       ((Some ":="), (Some `NA),
         [([`Sself; `Skeyword ":="; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     ((`Assign (_loc, (`Field (_loc, e1, (`Lid (_loc, \"contents\")))), e2) : \n     exp ) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                    ((`Assign
                        (_loc,
                          (`Field (_loc, e1, (`Lid (_loc, "contents")))), e2) : 
                    exp ) : 'exp )))));
         ([`Sself; `Skeyword "<-"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (match FanOps.bigarray_set _loc e1 e2 with\n      | Some e -> e\n      | None  -> `Assign (_loc, e1, e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (match FanOps.bigarray_set _loc e1 e2 with
                    | Some e -> e
                    | None  -> `Assign (_loc, e1, e2) : 'exp )))))]);
       ((Some "||"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop0 : 'infixop0 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop0)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop0)  (e1 : 'exp) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "&&"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop1 : 'infixop1 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop1)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop1)  (e1 : 'exp) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "<"), (Some `LA),
         [([`Sself;
           `Snterm (Gram.obj (infixop2 : 'infixop2 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop2)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop2)  (e1 : 'exp) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "^"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop3 : 'infixop3 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop3)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop3)  (e1 : 'exp) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "+"), (Some `LA),
         [([`Sself;
           `Snterm (Gram.obj (infixop4 : 'infixop4 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop4)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  (op : 'infixop4)  (e1 : 'exp) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "*"), (Some `LA),
         [([`Sself; `Skeyword "land"; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"land\")), e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                    (`App
                       (_loc, (`App (_loc, (`Lid (_loc, "land")), e1)), e2) : 
                    'exp )))));
         ([`Sself; `Skeyword "lor"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"lor\")), e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, (`Lid (_loc, "lor")), e1)), e2) : 
                   'exp )))));
         ([`Sself; `Skeyword "lxor"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"lxor\")), e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, (`Lid (_loc, "lxor")), e1)), e2) : 
                   'exp )))));
         ([`Sself; `Skeyword "mod"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"mod\")), e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, (`Lid (_loc, "mod")), e1)), e2) : 
                   'exp )))));
         ([`Sself; `Snterm (Gram.obj (infixop5 : 'infixop5 Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop5)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  (op : 'infixop5)  (e1 : 'exp) 
                   (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "**"), (Some `RA),
         [([`Sself; `Skeyword "asr"; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"asr\")), e1)), e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, (`Lid (_loc, "asr")), e1)), e2) : 
                    'exp )))));
         ([`Sself; `Skeyword "lsl"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"lsl\")), e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, (`Lid (_loc, "lsl")), e1)), e2) : 
                   'exp )))));
         ([`Sself; `Skeyword "lsr"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Lid (_loc, \"lsr\")), e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, (`Lid (_loc, "lsr")), e1)), e2) : 
                   'exp )))));
         ([`Sself; `Snterm (Gram.obj (infixop6 : 'infixop6 Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  (op : 'infixop6)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  (op : 'infixop6)  (e1 : 'exp) 
                   (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, op, e1)), e2) : 'exp )))))]);
       ((Some "obj"), (Some `RA),
         [([`Skeyword "fun";
           `Skeyword "[";
           `Slist1sep
             ((`Snterm (Gram.obj (case0 : 'case0 Gram.t ))), (`Skeyword "|"));
           `Skeyword "]"],
            ("Gram.mk_action\n  (fun _  (a : 'case0 list)  _  _  (_loc : FanLoc.t)  ->\n     (let cases = bar_of_list a in `Fun (_loc, cases) : 'exp ))\n",
              (Gram.mk_action
                 (fun _  (a : 'case0 list)  _  _  (_loc : FanLoc.t)  ->
                    (let cases = bar_of_list a in `Fun (_loc, cases) : 
                    'exp )))));
         ([`Skeyword "function";
          `Skeyword "[";
          `Slist1sep
            ((`Snterm (Gram.obj (case0 : 'case0 Gram.t ))), (`Skeyword "|"));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (a : 'case0 list)  _  _  (_loc : FanLoc.t)  ->\n     (let cases = bar_of_list a in `Fun (_loc, cases) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (a : 'case0 list)  _  _  (_loc : FanLoc.t)  ->
                   (let cases = bar_of_list a in `Fun (_loc, cases) : 
                   'exp )))));
         ([`Skeyword "fun"; `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
           ("Gram.mk_action (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'exp )))));
         ([`Skeyword "function";
          `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
           ("Gram.mk_action (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ")";
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _  (_loc : FanLoc.t) \n     -> (`ObjPat (_loc, p, cst) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : FanLoc.t)  -> (`ObjPat (_loc, p, cst) : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  _  (p : 'pat)  _  _  (_loc : FanLoc.t)  ->\n     (`ObjPatEnd (_loc, p) : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : FanLoc.t)  ->
                   (`ObjPatEnd (_loc, p) : 'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")";
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _  (p : 'pat)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : FanLoc.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'exp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : FanLoc.t)  ->\n     (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'exp )))));
         ([`Skeyword "object";
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  _  (_loc : FanLoc.t)  ->\n     (`Obj (_loc, cst) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : FanLoc.t)  ->
                   (`Obj (_loc, cst) : 'exp )))));
         ([`Skeyword "object"; `Skeyword "end"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`ObjEnd _loc : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`ObjEnd _loc : 'exp )))))]);
       ((Some "unary minus"), (Some `NA),
         [([`Skeyword "-"; `Sself],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (FanOps.mkumin _loc \"-\" e : 'exp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                    (FanOps.mkumin _loc "-" e : 'exp )))));
         ([`Skeyword "-."; `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (FanOps.mkumin _loc \"-.\" e : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (FanOps.mkumin _loc "-." e : 'exp )))))]);
       ((Some "apply"), (Some `LA),
         [([`Sself; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'exp)  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`App (_loc, e1, e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'exp)  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                    (`App (_loc, e1, e2) : 'exp )))));
         ([`Skeyword "assert"; `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  -> (`Assert (_loc, e) : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (`Assert (_loc, e) : 'exp )))));
         ([`Skeyword "new";
          `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->\n     (`New (_loc, i) : 'exp ))\n",
             (Gram.mk_action
                (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                   (`New (_loc, i) : 'exp )))));
         ([`Skeyword "lazy"; `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  -> (`Lazy (_loc, e) : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (`Lazy (_loc, e) : 'exp )))))]);
       ((Some "label"), (Some `NA),
         [([`Skeyword "~";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ":";
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, e) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                    -> (`Label (_loc, i, e) : 'exp )))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  -> (`LabelS (_loc, i) : 'exp ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`LabelS (_loc, i) : 'exp )))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), e) : 'exp )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, i)), e)\n\")\n",
             (Gram.mk_action
                (fun (e : 'exp)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), e) : 'exp )
                   | _ -> failwith "`Label (_loc, (`Lid (_loc, i)), e)\n"))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp )\n     | _ -> failwith \"`OptLabl (_loc, (`Lid (_loc, i)), e)\n\")\n",
             (Gram.mk_action
                (fun (e : 'exp)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp )
                   | _ -> failwith "`OptLabl (_loc, (`Lid (_loc, i)), e)\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, i, e) : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`OptLabl (_loc, i, e) : 'exp )))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLablS (_loc, i) : 'exp ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`OptLablS (_loc, i) : 'exp )))))]);
       ((Some "."), (Some `LA),
         [([`Sself; `Skeyword "."; `Skeyword "("; `Sself; `Skeyword ")"],
            ("Gram.mk_action\n  (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`ArrayDot (_loc, e1, e2) : 'exp ))\n",
              (Gram.mk_action
                 (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : FanLoc.t) 
                    -> (`ArrayDot (_loc, e1, e2) : 'exp )))));
         ([`Sself; `Skeyword "."; `Skeyword "["; `Sself; `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`StringDot (_loc, e1, e2) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`StringDot (_loc, e1, e2) : 'exp )))));
         ([`Sself;
          `Skeyword ".";
          `Skeyword "{";
          `Snterm (Gram.obj (comma_exp : 'comma_exp Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (e2 : 'comma_exp)  _  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (FanOps.bigarray_get _loc e1 e2 : 'exp ))\n",
             (Gram.mk_action
                (fun _  (e2 : 'comma_exp)  _  _  (e1 : 'exp) 
                   (_loc : FanLoc.t)  ->
                   (FanOps.bigarray_get _loc e1 e2 : 'exp )))));
         ([`Sself; `Skeyword "."; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->\n     (`Field (_loc, e1, e2) : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'exp)  _  (e1 : 'exp)  (_loc : FanLoc.t)  ->
                   (`Field (_loc, e1, e2) : 'exp )))));
         ([`Sself;
          `Skeyword "#";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (lab : 'a_lident)  _  (e : 'exp)  (_loc : FanLoc.t)  ->\n     (`Send (_loc, e, lab) : 'exp ))\n",
             (Gram.mk_action
                (fun (lab : 'a_lident)  _  (e : 'exp)  (_loc : FanLoc.t)  ->
                   (`Send (_loc, e, lab) : 'exp )))))]);
       ((Some "~-"), (Some `NA),
         [([`Skeyword "!"; `Sself],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Field (_loc, e, (`Lid (_loc, \"contents\"))) : 'exp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                    (`Field (_loc, e, (`Lid (_loc, "contents"))) : 'exp )))));
         ([`Snterm (Gram.obj (prefixop : 'prefixop Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (e : 'exp)  (f : 'prefixop)  (_loc : FanLoc.t)  ->\n     (`App (_loc, f, e) : 'exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  (f : 'prefixop)  (_loc : FanLoc.t)  ->
                   (`App (_loc, f, e) : 'exp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.exp_tag : 'exp )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.exp_tag\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `QUOTATION x ->
                        (AstQuotation.expand _loc x FanDyn.exp_tag : 
                        'exp )
                    | _ ->
                        failwith
                          "AstQuotation.expand _loc x FanDyn.exp_tag\n"))));
         ([`Stoken
             (((function
                | `Ant
                    (("exp"|""|"anti"|"`bool"|"par"|"seq"|"int"|"`int"
                      |"int32"|"`int32"|"int64"|"`int64"|"nativeint"
                      |"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"
                      |"vrn"),_)
                    -> true
                | _ -> false)),
               (`Normal,
                 "`Ant\n  ((\"exp\"|\"\"|\"anti\"|\"`bool\"|\"par\"|\"seq\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"\n    |\"`int64\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"|\"str\"|\"`str\"\n    |\"vrn\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant\n         ((\"exp\"|\"\"|\"anti\"|\"`bool\"|\"par\"|\"seq\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"\n           |\"int64\"|\"`int64\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"\n           |\"`chr\"|\"str\"|\"`str\"|\"vrn\" as n),s)\n         -> (mk_anti _loc ~c:\"exp\" n s : 'exp )\n     | _ -> failwith \"mk_anti _loc ~c:\"exp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant
                       (("exp"|""|"anti"|"`bool"|"par"|"seq"|"int"|"`int"
                         |"int32"|"`int32"|"int64"|"`int64"|"nativeint"
                         |"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"
                         |"vrn" as n),s)
                       -> (mk_anti _loc ~c:"exp" n s : 'exp )
                   | _ -> failwith "mk_anti _loc ~c:\"exp\" n s\n"))));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT (_,s) -> (`Int (_loc, s) : 'exp )\n     | _ -> failwith \"`Int (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT (_,s) -> (`Int (_loc, s) : 'exp )
                   | _ -> failwith "`Int (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT32 (_,_) -> true | _ -> false)),
               (`Normal, "`INT32 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT32 (_,s) -> (`Int32 (_loc, s) : 'exp )\n     | _ -> failwith \"`Int32 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT32 (_,s) -> (`Int32 (_loc, s) : 'exp )
                   | _ -> failwith "`Int32 (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT64 (_,_) -> true | _ -> false)),
               (`Normal, "`INT64 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT64 (_,s) -> (`Int64 (_loc, s) : 'exp )\n     | _ -> failwith \"`Int64 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT64 (_,s) -> (`Int64 (_loc, s) : 'exp )
                   | _ -> failwith "`Int64 (_loc, s)\n"))));
         ([`Stoken
             (((function | `Flo (_,_) -> true | _ -> false)),
               (`Normal, "`Flo (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Flo (_,s) -> (`Flo (_loc, s) : 'exp )\n     | _ -> failwith \"`Flo (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Flo (_,s) -> (`Flo (_loc, s) : 'exp )
                   | _ -> failwith "`Flo (_loc, s)\n"))));
         ([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (_,s) -> (`Chr (_loc, s) : 'exp )\n     | _ -> failwith \"`Chr (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `CHAR (_,s) -> (`Chr (_loc, s) : 'exp )
                   | _ -> failwith "`Chr (_loc, s)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'exp )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) -> (`Str (_loc, s) : 'exp )
                   | _ -> failwith "`Str (_loc, s)\n"))));
         ([`Stoken
             (((function | `NATIVEINT (_,_) -> true | _ -> false)),
               (`Normal, "`NATIVEINT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `NATIVEINT (_,s) -> (`Nativeint (_loc, s) : 'exp )\n     | _ -> failwith \"`Nativeint (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `NATIVEINT (_,s) -> (`Nativeint (_loc, s) : 'exp )
                   | _ -> failwith "`Nativeint (_loc, s)\n"))));
         ([`Stry
             (`Snterm
                (Gram.obj
                   (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                    Gram.t )));
          `Sself;
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  (i : 'module_longident_dot_lparen)  (_loc : FanLoc.t) \n     -> (`LetOpen (_loc, i, e) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  (i : 'module_longident_dot_lparen) 
                   (_loc : FanLoc.t)  -> (`LetOpen (_loc, i, e) : 'exp )))));
         ([`Snterm (Gram.obj (vid : 'vid Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'vid)  (_loc : FanLoc.t)  -> ((i : vid  :>exp) : 'exp ))\n",
             (Gram.mk_action
                (fun (i : 'vid)  (_loc : FanLoc.t)  ->
                   ((i : vid  :>exp) : 'exp )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, s) : 'exp ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'exp )))));
         ([`Skeyword "["; `Skeyword "]"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, \"[]\") : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, "[]") : 'exp )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_exp_for_list : 'sem_exp_for_list Gram.t ));
          `Skeyword "::";
          `Sself;
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (last : 'exp)  _  (mk_list : 'sem_exp_for_list)  _ \n     (_loc : FanLoc.t)  -> (mk_list last : 'exp ))\n",
             (Gram.mk_action
                (fun _  (last : 'exp)  _  (mk_list : 'sem_exp_for_list)  _ 
                   (_loc : FanLoc.t)  -> (mk_list last : 'exp )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_exp_for_list : 'sem_exp_for_list Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (mk_list : 'sem_exp_for_list)  _  (_loc : FanLoc.t)  ->\n     (mk_list (`Uid (_loc, \"[]\")) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (mk_list : 'sem_exp_for_list)  _  (_loc : FanLoc.t) 
                   -> (mk_list (`Uid (_loc, "[]")) : 'exp )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`ArrayEmpty _loc : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`ArrayEmpty _loc : 'exp )))));
         ([`Skeyword "[|";
          `Snterm (Gram.obj (sem_exp : 'sem_exp Gram.t ));
          `Skeyword "|]"],
           ("Gram.mk_action\n  (fun _  (el : 'sem_exp)  _  (_loc : FanLoc.t)  ->\n     (`Array (_loc, el) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (el : 'sem_exp)  _  (_loc : FanLoc.t)  ->
                   (`Array (_loc, el) : 'exp )))));
         ([`Skeyword "{";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"));
          `Skeyword "with";
          `Snterm (Gram.obj (label_exp_list : 'label_exp_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (el : 'label_exp_list)  _  (__fan_1 : [> FanToken.t])  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Lid x -> (`RecordWith (_loc, el, (`Lid (_loc, x))) : 'exp )\n     | _ -> failwith \"`RecordWith (_loc, el, (`Lid (_loc, x)))\n\")\n",
             (Gram.mk_action
                (fun _  (el : 'label_exp_list)  _  (__fan_1 : [> FanToken.t])
                    _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Lid x ->
                       (`RecordWith (_loc, el, (`Lid (_loc, x))) : 'exp )
                   | _ ->
                       failwith "`RecordWith (_loc, el, (`Lid (_loc, x)))\n"))));
         ([`Skeyword "{";
          `Snterm (Gram.obj (label_exp_list : 'label_exp_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (el : 'label_exp_list)  _  (_loc : FanLoc.t)  ->\n     (`Record (_loc, el) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (el : 'label_exp_list)  _  (_loc : FanLoc.t)  ->
                   (`Record (_loc, el) : 'exp )))));
         ([`Skeyword "{";
          `Skeyword "(";
          `Sself;
          `Skeyword ")";
          `Skeyword "with";
          `Snterm (Gram.obj (label_exp_list : 'label_exp_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (el : 'label_exp_list)  _  _  (e : 'exp)  _  _  (_loc : FanLoc.t) \n     -> (`RecordWith (_loc, el, e) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (el : 'label_exp_list)  _  _  (e : 'exp)  _  _ 
                   (_loc : FanLoc.t)  -> (`RecordWith (_loc, el, e) : 
                   'exp )))));
         ([`Skeyword "{<"; `Skeyword ">}"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`OvrInstEmpty _loc : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`OvrInstEmpty _loc : 'exp )))));
         ([`Skeyword "{<";
          `Snterm (Gram.obj (field_exp_list : 'field_exp_list Gram.t ));
          `Skeyword ">}"],
           ("Gram.mk_action\n  (fun _  (fel : 'field_exp_list)  _  (_loc : FanLoc.t)  ->\n     (`OvrInst (_loc, fel) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (fel : 'field_exp_list)  _  (_loc : FanLoc.t)  ->
                   (`OvrInst (_loc, fel) : 'exp )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, \"()\") : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, "()") : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, e, t) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, e, t) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_exp : 'comma_exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (el : 'comma_exp)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Par (_loc, (`Com (_loc, e, el))) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (el : 'comma_exp)  _  (e : 'exp)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Par (_loc, (`Com (_loc, e, el))) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ";";
          `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (seq : 'sequence)  _  (e : 'exp)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp )))));
         ([`Skeyword "("; `Sself; `Skeyword ";"; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  _  (e : 'exp)  _  (_loc : FanLoc.t)  -> (`Seq (_loc, e) : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (`Seq (_loc, e) : 'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : FanLoc.t) \n     -> (`Coercion (_loc, e, t, t2) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'exp)  _ 
                   (_loc : FanLoc.t)  -> (`Coercion (_loc, e, t, t2) : 
                   'exp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Subtype (_loc, e, t) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (`Subtype (_loc, e, t) : 'exp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (e : 'exp)  _  (_loc : FanLoc.t)  -> (e : 'exp ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (_loc : FanLoc.t)  -> (e : 'exp )))));
         ([`Skeyword "begin";
          `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->\n     (`Seq (_loc, seq) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                   (`Seq (_loc, seq) : 'exp )))));
         ([`Skeyword "begin"; `Skeyword "end"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, \"()\") : 'exp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, "()") : 'exp )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (mexp : 'mexp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (me : 'mexp)  _  _  (_loc : FanLoc.t)  ->\n     (`Package_exp (_loc, me) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (me : 'mexp)  _  _  (_loc : FanLoc.t)  ->
                   (`Package_exp (_loc, me) : 'exp )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (mexp : 'mexp Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pt : 'mtyp)  _  (me : 'mexp)  _  _  (_loc : FanLoc.t)  ->\n     (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 'exp ))\n",
             (Gram.mk_action
                (fun _  (pt : 'mtyp)  _  (me : 'mexp)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 
                   'exp )))))])]);
   Gram.extend_single (sequence : 'sequence Gram.t )
     (None,
       (None, None,
         [([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
            ("Gram.mk_action\n  (fun (k : 'sequence')  (e : 'exp)  _  (bi : 'binding)  (rf : 'opt_rec)  _ \n     (_loc : FanLoc.t)  -> (k (`LetIn (_loc, rf, bi, e)) : 'sequence ))\n",
              (Gram.mk_action
                 (fun (k : 'sequence')  (e : 'exp)  _  (bi : 'binding) 
                    (rf : 'opt_rec)  _  (_loc : FanLoc.t)  ->
                    (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "try";
          `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
          `Snterm (Gram.obj (binding : 'binding Gram.t ));
          `Skeyword "in";
          `Sself;
          `Skeyword "with";
          `Snterm (Gram.obj (case : 'case Gram.t ));
          `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
           ("Gram.mk_action\n  (fun (k : 'sequence')  (a : 'case)  _  (x : 'sequence)  _  (bi : 'binding) \n     (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->\n     (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence ))\n",
             (Gram.mk_action
                (fun (k : 'sequence')  (a : 'case)  _  (x : 'sequence)  _ 
                   (bi : 'binding)  (r : 'opt_rec)  _  _  (_loc : FanLoc.t) 
                   -> (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence )))));
         ([`Skeyword "let";
          `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
          `Snterm (Gram.obj (binding : 'binding Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (el : 'sequence)  _  (bi : 'binding)  (rf : 'opt_rec)  _ \n     (_loc : FanLoc.t)  ->\n     (`LetIn (_loc, rf, bi, (`Seq (_loc, el))) : 'sequence ))\n",
             (Gram.mk_action
                (fun (el : 'sequence)  _  (bi : 'binding)  (rf : 'opt_rec)  _
                    (_loc : FanLoc.t)  ->
                   (`LetIn (_loc, rf, bi, (`Seq (_loc, el))) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Snterm (Gram.obj (mbind0 : 'mbind0 Gram.t ));
          `Skeyword "in";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
           ("Gram.mk_action\n  (fun (k : 'sequence')  (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _\n      (_loc : FanLoc.t)  -> (k (`LetModule (_loc, m, mb, e)) : 'sequence ))\n",
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'exp)  _  (mb : 'mbind0) 
                   (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Snterm (Gram.obj (mbind0 : 'mbind0 Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (el : 'sequence)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`LetModule (_loc, m, mb, (`Seq (_loc, el))) : 'sequence ))\n",
             (Gram.mk_action
                (fun (el : 'sequence)  _  (mb : 'mbind0)  (m : 'a_uident)  _ 
                   _  (_loc : FanLoc.t)  ->
                   (`LetModule (_loc, m, mb, (`Seq (_loc, el))) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "open";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
          `Skeyword "in";
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'sequence)  _  (i : 'module_longident)  _  _  (_loc : FanLoc.t) \n     -> (`LetOpen (_loc, (i : vid  :>ident), e) : 'sequence ))\n",
             (Gram.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`LetOpen (_loc, (i : vid  :>ident), e) : 'sequence )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"exp;\" n s : 'sequence )\n     | _ -> failwith \"mk_anti _loc ~c:\"exp;\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (mk_anti _loc ~c:"exp;" n s : 'sequence )
                   | _ -> failwith "mk_anti _loc ~c:\"exp;\" n s\n"))));
         ([`Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
           ("Gram.mk_action\n  (fun (k : 'sequence')  (e : 'exp)  (_loc : FanLoc.t)  -> (k e : 'sequence ))\n",
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'exp)  (_loc : FanLoc.t)  ->
                   (k e : 'sequence )))))]));
   Gram.extend_single (sequence' : 'sequence' Gram.t )
     (None,
       (None, None,
         [([],
            ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' ))\n",
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Skeyword ";"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' )))));
         ([`Skeyword ";"; `Snterm (Gram.obj (sequence : 'sequence Gram.t ))],
           ("Gram.mk_action\n  (fun (el : 'sequence)  _  (_loc : FanLoc.t)  ->\n     (fun e  -> `Sem (_loc, e, el) : 'sequence' ))\n",
             (Gram.mk_action
                (fun (el : 'sequence)  _  (_loc : FanLoc.t)  ->
                   (fun e  -> `Sem (_loc, e, el) : 'sequence' )))))]));
   Gram.extend_single (infixop1 : 'infixop1 Gram.t )
     (None,
       (None, None,
         [([Gram.srules
              [([`Skeyword "&"],
                 ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__3 ))\n",
                   (Gram.mk_action
                      (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         (Gram.string_of_token x : 'e__3 )))));
              ([`Skeyword "&&"],
                ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__3 ))\n",
                  (Gram.mk_action
                     (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__3 )))))]],
            ("Gram.mk_action\n  (fun (x : 'e__3)  (_loc : FanLoc.t)  -> (`Lid (_loc, x) : 'infixop1 ))\n",
              (Gram.mk_action
                 (fun (x : 'e__3)  (_loc : FanLoc.t)  ->
                    (`Lid (_loc, x) : 'infixop1 )))))]));
   Gram.extend_single (infixop0 : 'infixop0 Gram.t )
     (None,
       (None, None,
         [([Gram.srules
              [([`Skeyword "or"],
                 ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                   (Gram.mk_action
                      (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         (Gram.string_of_token x : 'e__4 )))));
              ([`Skeyword "||"],
                ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                  (Gram.mk_action
                     (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__4 )))))]],
            ("Gram.mk_action\n  (fun (x : 'e__4)  (_loc : FanLoc.t)  -> (`Lid (_loc, x) : 'infixop0 ))\n",
              (Gram.mk_action
                 (fun (x : 'e__4)  (_loc : FanLoc.t)  ->
                    (`Lid (_loc, x) : 'infixop0 )))))]));
   Gram.extend_single (sem_exp_for_list : 'sem_exp_for_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (exp : 'exp Gram.t )); `Skeyword ";"; `Sself],
            ("Gram.mk_action\n  (fun (el : 'sem_exp_for_list)  _  (e : 'exp)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), (el acc)) : \n     'sem_exp_for_list ))\n",
              (Gram.mk_action
                 (fun (el : 'sem_exp_for_list)  _  (e : 'exp) 
                    (_loc : FanLoc.t)  ->
                    (fun acc  ->
                       `App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), e)),
                           (el acc)) : 'sem_exp_for_list )))));
         ([`Snterm (Gram.obj (exp : 'exp Gram.t )); `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  (_loc : FanLoc.t)  ->\n     (fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : \n     'sem_exp_for_list ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                   'sem_exp_for_list )))));
         ([`Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  (_loc : FanLoc.t)  ->\n     (fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), acc) : \n     'sem_exp_for_list ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), acc) : 
                   'sem_exp_for_list )))))]));
   Gram.extend_single (comma_exp : 'comma_exp Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'comma_exp)  _  (e1 : 'comma_exp)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, e1, e2) : 'comma_exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'comma_exp)  _  (e1 : 'comma_exp) 
                    (_loc : FanLoc.t)  -> (`Com (_loc, e1, e2) : 'comma_exp )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"exp,\" n s : 'comma_exp )\n     | _ -> failwith \"mk_anti _loc ~c:\"exp,\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (mk_anti _loc ~c:"exp," n s : 'comma_exp )
                   | _ -> failwith "mk_anti _loc ~c:\"exp,\" n s\n"))));
         ([`Snterml ((Gram.obj (exp : 'exp Gram.t )), "top")],
           ("Gram.mk_action (fun (e : 'exp)  (_loc : FanLoc.t)  -> (e : 'comma_exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  (_loc : FanLoc.t)  -> (e : 'comma_exp )))))]));
   Gram.extend_single (dummy : 'dummy Gram.t )
     (None,
       (None, None,
         [([],
            ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (() : 'dummy ))\n",
              (Gram.mk_action (fun (_loc : FanLoc.t)  -> (() : 'dummy )))))])));
  Gram.extend_single (with_exp_lang : 'with_exp_lang Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (lang : 'lang Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'exp)  _  (old : 'lang)  (_loc : FanLoc.t)  ->\n     (AstQuotation.default := old; x : 'with_exp_lang ))\n",
             (Gram.mk_action
                (fun (x : 'exp)  _  (old : 'lang)  (_loc : FanLoc.t)  ->
                   (AstQuotation.default := old; x : 'with_exp_lang )))))]));
  Gram.extend_single (with_stru_lang : 'with_stru_lang Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (lang : 'lang Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (stru : 'stru Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'stru)  _  (old : 'lang)  (_loc : FanLoc.t)  ->\n     (AstQuotation.default := old; x : 'with_stru_lang ))\n",
             (Gram.mk_action
                (fun (x : 'stru)  _  (old : 'lang)  (_loc : FanLoc.t)  ->
                   (AstQuotation.default := old; x : 'with_stru_lang )))))]));
  (Gram.extend_single (binding_quot : 'binding_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (binding : 'binding Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'binding)  (_loc : FanLoc.t)  -> (x : 'binding_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'binding)  (_loc : FanLoc.t)  ->
                    (x : 'binding_quot )))))]));
   Gram.extend_single (binding : 'binding Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant (("binding"|"list"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"binding\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"binding\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"binding\" n s : 'binding )\n     | _ -> failwith \"mk_anti _loc ~c:\"binding\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("binding"|"list" as n),s) ->
                        (mk_anti _loc ~c:"binding" n s : 'binding )
                    | _ -> failwith "mk_anti _loc ~c:\"binding\" n s\n"))));
         ([`Stoken
             (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"anti\"),_)"));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`Bind (_loc, (mk_anti _loc ~c:\"pat\" n s), e) : 'binding )\n     | _ -> failwith \"`Bind (_loc, (mk_anti _loc ~c:\"pat\" n s), e)\n\")\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"anti" as n),s) ->
                       (`Bind (_loc, (mk_anti _loc ~c:"pat" n s), e) : 
                       'binding )
                   | _ ->
                       failwith
                         "`Bind (_loc, (mk_anti _loc ~c:\"pat\" n s), e)\n"))));
         ([`Stoken
             (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"binding\" n s : 'binding )\n     | _ -> failwith \"mk_anti _loc ~c:\"binding\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"anti" as n),s) ->
                       (mk_anti _loc ~c:"binding" n s : 'binding )
                   | _ -> failwith "mk_anti _loc ~c:\"binding\" n s\n"))));
         ([`Sself; `Skeyword "and"; `Sself],
           ("Gram.mk_action\n  (fun (b2 : 'binding)  _  (b1 : 'binding)  (_loc : FanLoc.t)  ->\n     (`And (_loc, b1, b2) : 'binding ))\n",
             (Gram.mk_action
                (fun (b2 : 'binding)  _  (b1 : 'binding)  (_loc : FanLoc.t) 
                   -> (`And (_loc, b1, b2) : 'binding )))));
         ([`Snterm (Gram.obj (let_binding : 'let_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (b : 'let_binding)  (_loc : FanLoc.t)  -> (b : 'binding ))\n",
             (Gram.mk_action
                (fun (b : 'let_binding)  (_loc : FanLoc.t)  ->
                   (b : 'binding )))))]));
   Gram.extend_single (let_binding : 'let_binding Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
           `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'fun_binding)  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (`Bind (_loc, p, e) : 'let_binding ))\n",
              (Gram.mk_action
                 (fun (e : 'fun_binding)  (p : 'pat)  (_loc : FanLoc.t)  ->
                    (`Bind (_loc, p, e) : 'let_binding )))))])));
  (Gram.extend_single (case : 'case Gram.t )
     (None,
       (None, None,
         [([`Skeyword "[";
           `Slist1sep
             ((`Snterm (Gram.obj (case0 : 'case0 Gram.t ))), (`Skeyword "|"));
           `Skeyword "]"],
            ("Gram.mk_action\n  (fun _  (l : 'case0 list)  _  (_loc : FanLoc.t)  ->\n     (bar_of_list l : 'case ))\n",
              (Gram.mk_action
                 (fun _  (l : 'case0 list)  _  (_loc : FanLoc.t)  ->
                    (bar_of_list l : 'case )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword "->";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (`Case (_loc, p, e) : 'case ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (p : 'pat)  (_loc : FanLoc.t)  ->
                   (`Case (_loc, p, e) : 'case )))))]));
   Gram.extend_single (case0 : 'case0 Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant (("case"|"list"|"anti"|""),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"case\"|\"list\"|\"anti\"|\"\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"case\"|\"list\"|\"anti\"|\"\" as n),s) ->\n         (mk_anti _loc ~c:\"case\" n s : 'case0 )\n     | _ -> failwith \"mk_anti _loc ~c:\"case\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("case"|"list"|"anti"|"" as n),s) ->
                        (mk_anti _loc ~c:"case" n s : 'case0 )
                    | _ -> failwith "mk_anti _loc ~c:\"case\" n s\n"))));
         ([`Snterm (Gram.obj (pat_as_pat_opt : 'pat_as_pat_opt Gram.t ));
          `Skeyword "when";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword "->";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (w : 'exp)  _  (p : 'pat_as_pat_opt)  (_loc : FanLoc.t)\n      -> (`CaseWhen (_loc, p, w, e) : 'case0 ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (w : 'exp)  _  (p : 'pat_as_pat_opt) 
                   (_loc : FanLoc.t)  ->
                   (`CaseWhen (_loc, p, w, e) : 'case0 )))));
         ([`Snterm (Gram.obj (pat_as_pat_opt : 'pat_as_pat_opt Gram.t ));
          `Skeyword "->";
          `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (p : 'pat_as_pat_opt)  (_loc : FanLoc.t)  ->\n     (`Case (_loc, p, e) : 'case0 ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (p : 'pat_as_pat_opt)  (_loc : FanLoc.t) 
                   -> (`Case (_loc, p, e) : 'case0 )))))]));
   Gram.extend_single (case_quot : 'case_quot Gram.t )
     (None,
       (None, None,
         [([`Slist1sep
              ((`Snterm (Gram.obj (case0 : 'case0 Gram.t ))),
                (`Skeyword "|"))],
            ("Gram.mk_action\n  (fun (x : 'case0 list)  (_loc : FanLoc.t)  -> (bar_of_list x : 'case_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'case0 list)  (_loc : FanLoc.t)  ->
                    (bar_of_list x : 'case_quot )))))])));
  (Gram.extend_single (rec_exp_quot : 'rec_exp_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_exp_list : 'label_exp_list Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'label_exp_list)  (_loc : FanLoc.t)  -> (x : 'rec_exp_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'label_exp_list)  (_loc : FanLoc.t)  ->
                    (x : 'rec_exp_quot )))))]));
   Gram.extend_single (label_exp : 'label_exp Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant (("rec_exp"|""|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"rec_exp\"|\"\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"rec_exp\"|\"\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"rec_exp\" n s : 'label_exp )\n     | _ -> failwith \"mk_anti _loc ~c:\"rec_exp\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("rec_exp"|""|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"rec_exp" n s : 'label_exp )
                    | _ -> failwith "mk_anti _loc ~c:\"rec_exp\" n s\n"))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
          `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'fun_binding)  (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, i, e) : 'label_exp ))\n",
             (Gram.mk_action
                (fun (e : 'fun_binding)  (i : 'label_longident) 
                   (_loc : FanLoc.t)  ->
                   (`RecBind (_loc, i, e) : 'label_exp )))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, i, (`Lid (_loc, (FanOps.to_lid i)))) : 'label_exp ))\n",
             (Gram.mk_action
                (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (FanOps.to_lid i)))) : 
                   'label_exp )))))]));
   Gram.extend_single (field_exp : 'field_exp Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"bi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"bi\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"bi\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"rec_exp\" n s : 'field_exp )\n     | _ -> failwith \"mk_anti _loc ~c:\"rec_exp\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"bi"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"rec_exp" n s : 'field_exp )
                    | _ -> failwith "mk_anti _loc ~c:\"rec_exp\" n s\n"))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword "=";
          `Snterml ((Gram.obj (exp : 'exp Gram.t )), "top")],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (l : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, (l :>ident), e) : 'field_exp ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (l : 'a_lident)  (_loc : FanLoc.t)  ->
                   (`RecBind (_loc, (l :>ident), e) : 'field_exp )))))]));
   Gram.extend_single (label_exp_list : 'label_exp_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_exp : 'label_exp Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'label_exp_list)  _  (b1 : 'label_exp)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, b1, b2) : 'label_exp_list ))\n",
              (Gram.mk_action
                 (fun (b2 : 'label_exp_list)  _  (b1 : 'label_exp) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, b1, b2) : 'label_exp_list )))));
         ([`Snterm (Gram.obj (label_exp : 'label_exp Gram.t ));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (b1 : 'label_exp)  (_loc : FanLoc.t)  -> (b1 : 'label_exp_list ))\n",
             (Gram.mk_action
                (fun _  (b1 : 'label_exp)  (_loc : FanLoc.t)  ->
                   (b1 : 'label_exp_list )))));
         ([`Snterm (Gram.obj (label_exp : 'label_exp Gram.t ))],
           ("Gram.mk_action\n  (fun (b1 : 'label_exp)  (_loc : FanLoc.t)  -> (b1 : 'label_exp_list ))\n",
             (Gram.mk_action
                (fun (b1 : 'label_exp)  (_loc : FanLoc.t)  ->
                   (b1 : 'label_exp_list )))))]));
   Gram.extend_single (field_exp_list : 'field_exp_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (field_exp : 'field_exp Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'field_exp_list)  _  (b1 : 'field_exp)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, b1, b2) : 'field_exp_list ))\n",
              (Gram.mk_action
                 (fun (b2 : 'field_exp_list)  _  (b1 : 'field_exp) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, b1, b2) : 'field_exp_list )))));
         ([`Snterm (Gram.obj (field_exp : 'field_exp Gram.t ));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (b1 : 'field_exp)  (_loc : FanLoc.t)  -> (b1 : 'field_exp_list ))\n",
             (Gram.mk_action
                (fun _  (b1 : 'field_exp)  (_loc : FanLoc.t)  ->
                   (b1 : 'field_exp_list )))));
         ([`Snterm (Gram.obj (field_exp : 'field_exp Gram.t ))],
           ("Gram.mk_action\n  (fun (b1 : 'field_exp)  (_loc : FanLoc.t)  -> (b1 : 'field_exp_list ))\n",
             (Gram.mk_action
                (fun (b1 : 'field_exp)  (_loc : FanLoc.t)  ->
                   (b1 : 'field_exp_list )))))])));
  (let grammar_entry_create = Gram.mk in
   let pat_constr: 'pat_constr Gram.t = grammar_entry_create "pat_constr" in
   Gram.extend_single (pat_quot : 'pat_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
           `Skeyword ",";
           `Snterm (Gram.obj (comma_pat : 'comma_pat Gram.t ))],
            ("Gram.mk_action\n  (fun (y : 'comma_pat)  _  (x : 'pat)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, x, y) : 'pat_quot ))\n",
              (Gram.mk_action
                 (fun (y : 'comma_pat)  _  (x : 'pat)  (_loc : FanLoc.t)  ->
                    (`Com (_loc, x, y) : 'pat_quot )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ";";
          `Snterm (Gram.obj (sem_pat : 'sem_pat Gram.t ))],
           ("Gram.mk_action\n  (fun (y : 'sem_pat)  _  (x : 'pat)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, x, y) : 'pat_quot ))\n",
             (Gram.mk_action
                (fun (y : 'sem_pat)  _  (x : 'pat)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, x, y) : 'pat_quot )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action (fun (x : 'pat)  (_loc : FanLoc.t)  -> (x : 'pat_quot ))\n",
             (Gram.mk_action
                (fun (x : 'pat)  (_loc : FanLoc.t)  -> (x : 'pat_quot )))))]));
   Gram.extend_single (pat_as_pat_opt : 'pat_as_pat_opt Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
           `Skeyword "as";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (s : 'a_lident)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p1, s) : 'pat_as_pat_opt ))\n",
              (Gram.mk_action
                 (fun (s : 'a_lident)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->
                    (`Alias (_loc, p1, s) : 'pat_as_pat_opt )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'pat_as_pat_opt ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'pat_as_pat_opt )))))]));
   Gram.extend_single (pat_constr : 'pat_constr Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->\n     ((i : vid  :>pat) : 'pat_constr ))\n",
              (Gram.mk_action
                 (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                    ((i : vid  :>pat) : 'pat_constr )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->\n     ((`Vrn (_loc, s) : pat ) : 'pat_constr ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   ((`Vrn (_loc, s) : pat ) : 'pat_constr )))));
         ([`Stoken
             (((function
                | `Ant ((""|"pat"|"anti"|"vrn"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"pat\"|\"anti\"|\"vrn\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"pat\"|\"anti\"|\"vrn\" as n),s) ->\n         (mk_anti _loc ~c:\"pat\" n s : 'pat_constr )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"pat"|"anti"|"vrn" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'pat_constr )
                   | _ -> failwith "mk_anti _loc ~c:\"pat\" n s\n"))))]));
   Gram.extend (pat : 'pat Gram.t )
     (None,
       [((Some "|"), (Some `LA),
          [([`Sself; `Skeyword "|"; `Sself],
             ("Gram.mk_action\n  (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->\n     (`Bar (_loc, p1, p2) : 'pat ))\n",
               (Gram.mk_action
                  (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->
                     (`Bar (_loc, p1, p2) : 'pat )))))]);
       ((Some ".."), (Some `NA),
         [([`Sself; `Skeyword ".."; `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->\n     (`PaRng (_loc, p1, p2) : 'pat ))\n",
              (Gram.mk_action
                 (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->
                    (`PaRng (_loc, p1, p2) : 'pat )))))]);
       ((Some "apply"), (Some `LA),
         [([`Snterm (Gram.obj (pat_constr : 'pat_constr Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'pat)  (p1 : 'pat_constr)  (_loc : FanLoc.t)  ->\n     (match p2 with\n      | `Par (_loc,p) ->\n          List.fold_left (fun p1  p2  -> `App (_loc, p1, p2)) p1\n            (list_of_com p [])\n      | _ -> `App (_loc, p1, p2) : 'pat ))\n",
              (Gram.mk_action
                 (fun (p2 : 'pat)  (p1 : 'pat_constr)  (_loc : FanLoc.t)  ->
                    (match p2 with
                     | `Par (_loc,p) ->
                         List.fold_left (fun p1  p2  -> `App (_loc, p1, p2))
                           p1 (list_of_com p [])
                     | _ -> `App (_loc, p1, p2) : 'pat )))));
         ([`Snterm (Gram.obj (pat_constr : 'pat_constr Gram.t ))],
           ("Gram.mk_action (fun (p1 : 'pat_constr)  (_loc : FanLoc.t)  -> (p1 : 'pat ))\n",
             (Gram.mk_action
                (fun (p1 : 'pat_constr)  (_loc : FanLoc.t)  -> (p1 : 'pat )))));
         ([`Skeyword "lazy"; `Sself],
           ("Gram.mk_action\n  (fun (p : 'pat)  _  (_loc : FanLoc.t)  -> (`Lazy (_loc, p) : 'pat ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  _  (_loc : FanLoc.t)  ->
                   (`Lazy (_loc, p) : 'pat )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant
                     ((""|"pat"|"anti"|"par"|"int"|"`int"|"int32"|"`int32"
                       |"int64"|"`int64"|"vrn"|"nativeint"|"`nativeint"|"flo"
                       |"`flo"|"chr"|"`chr"|"str"|"`str"),_)
                     -> true
                 | _ -> false)),
                (`Normal,
                  "`Ant\n  ((\"\"|\"pat\"|\"anti\"|\"par\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"|\"`int64\"\n    |\"vrn\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"|\"str\"|\"`str\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant\n         ((\"\"|\"pat\"|\"anti\"|\"par\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"\n           |\"`int64\"|\"vrn\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"\n           |\"str\"|\"`str\" as n),s)\n         -> (mk_anti _loc ~c:\"pat\" n s : 'pat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant
                        ((""|"pat"|"anti"|"par"|"int"|"`int"|"int32"|"`int32"
                          |"int64"|"`int64"|"vrn"|"nativeint"|"`nativeint"
                          |"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s)
                        -> (mk_anti _loc ~c:"pat" n s : 'pat )
                    | _ -> failwith "mk_anti _loc ~c:\"pat\" n s\n"))));
         ([`Snterm (Gram.obj (vid : 'vid Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'vid)  (_loc : FanLoc.t)  -> ((i : vid  :>pat) : 'pat ))\n",
             (Gram.mk_action
                (fun (i : 'vid)  (_loc : FanLoc.t)  ->
                   ((i : vid  :>pat) : 'pat )))));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT (_,s) -> (`Int (_loc, s) : 'pat )\n     | _ -> failwith \"`Int (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT (_,s) -> (`Int (_loc, s) : 'pat )
                   | _ -> failwith "`Int (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT32 (_,_) -> true | _ -> false)),
               (`Normal, "`INT32 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT32 (_,s) -> (`Int32 (_loc, s) : 'pat )\n     | _ -> failwith \"`Int32 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT32 (_,s) -> (`Int32 (_loc, s) : 'pat )
                   | _ -> failwith "`Int32 (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT64 (_,_) -> true | _ -> false)),
               (`Normal, "`INT64 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT64 (_,s) -> (`Int64 (_loc, s) : 'pat )\n     | _ -> failwith \"`Int64 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT64 (_,s) -> (`Int64 (_loc, s) : 'pat )
                   | _ -> failwith "`Int64 (_loc, s)\n"))));
         ([`Stoken
             (((function | `Flo (_,_) -> true | _ -> false)),
               (`Normal, "`Flo (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Flo (_,s) -> (`Flo (_loc, s) : 'pat )\n     | _ -> failwith \"`Flo (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Flo (_,s) -> (`Flo (_loc, s) : 'pat )
                   | _ -> failwith "`Flo (_loc, s)\n"))));
         ([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (_,s) -> (`Chr (_loc, s) : 'pat )\n     | _ -> failwith \"`Chr (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `CHAR (_,s) -> (`Chr (_loc, s) : 'pat )
                   | _ -> failwith "`Chr (_loc, s)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'pat )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) -> (`Str (_loc, s) : 'pat )
                   | _ -> failwith "`Str (_loc, s)\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT (_,_) -> true | _ -> false)),
              (`Normal, "`INT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `INT (_,s) -> (`Int (_loc, (String.neg s)) : 'pat )\n     | _ -> failwith \"`Int (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `INT (_,s) -> (`Int (_loc, (String.neg s)) : 'pat )
                   | _ -> failwith "`Int (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT32 (_,_) -> true | _ -> false)),
              (`Normal, "`INT32 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `INT32 (_,s) -> (`Int32 (_loc, (String.neg s)) : 'pat )\n     | _ -> failwith \"`Int32 (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `INT32 (_,s) -> (`Int32 (_loc, (String.neg s)) : 'pat )
                   | _ -> failwith "`Int32 (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT64 (_,_) -> true | _ -> false)),
              (`Normal, "`INT64 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `INT64 (_,s) -> (`Int64 (_loc, (String.neg s)) : 'pat )\n     | _ -> failwith \"`Int64 (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `INT64 (_,s) -> (`Int64 (_loc, (String.neg s)) : 'pat )
                   | _ -> failwith "`Int64 (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `NATIVEINT (_,_) -> true | _ -> false)),
              (`Normal, "`NATIVEINT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `NATIVEINT (_,s) -> (`Nativeint (_loc, (String.neg s)) : 'pat )\n     | _ -> failwith \"`Nativeint (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `NATIVEINT (_,s) ->
                       (`Nativeint (_loc, (String.neg s)) : 'pat )
                   | _ -> failwith "`Nativeint (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Flo (_,_) -> true | _ -> false)),
              (`Normal, "`Flo (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Flo (_,s) -> (`Flo (_loc, (String.neg s)) : 'pat )\n     | _ -> failwith \"`Flo (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Flo (_,s) -> (`Flo (_loc, (String.neg s)) : 'pat )
                   | _ -> failwith "`Flo (_loc, (String.neg s))\n"))));
         ([`Skeyword "["; `Skeyword "]"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, \"[]\") : 'pat ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, "[]") : 'pat )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_pat_for_list : 'sem_pat_for_list Gram.t ));
          `Skeyword "::";
          `Sself;
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (last : 'pat)  _  (mk_list : 'sem_pat_for_list)  _ \n     (_loc : FanLoc.t)  -> (mk_list last : 'pat ))\n",
             (Gram.mk_action
                (fun _  (last : 'pat)  _  (mk_list : 'sem_pat_for_list)  _ 
                   (_loc : FanLoc.t)  -> (mk_list last : 'pat )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_pat_for_list : 'sem_pat_for_list Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (mk_list : 'sem_pat_for_list)  _  (_loc : FanLoc.t)  ->\n     (mk_list (`Uid (_loc, \"[]\")) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (mk_list : 'sem_pat_for_list)  _  (_loc : FanLoc.t) 
                   -> (mk_list (`Uid (_loc, "[]")) : 'pat )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`ArrayEmpty _loc : 'pat ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`ArrayEmpty _loc : 'pat )))));
         ([`Skeyword "[|";
          `Snterm (Gram.obj (sem_pat : 'sem_pat Gram.t ));
          `Skeyword "|]"],
           ("Gram.mk_action\n  (fun _  (pl : 'sem_pat)  _  (_loc : FanLoc.t)  ->\n     (`Array (_loc, pl) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (pl : 'sem_pat)  _  (_loc : FanLoc.t)  ->
                   (`Array (_loc, pl) : 'pat )))));
         ([`Skeyword "{";
          `Snterm (Gram.obj (label_pat_list : 'label_pat_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (pl : 'label_pat_list)  _  (_loc : FanLoc.t)  ->\n     (`Record (_loc, pl) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (pl : 'label_pat_list)  _  (_loc : FanLoc.t)  ->
                   (`Record (_loc, pl) : 'pat )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, \"()\") : 'pat ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, "()") : 'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleUnpack (_loc, m) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleUnpack (_loc, m) : 'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'pat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_4 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 'pat )\n     | _ -> failwith \"`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 
                       'pat )
                   | _ ->
                       failwith
                         "`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n"))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (p : 'pat)  _  (_loc : FanLoc.t)  -> (p : 'pat ))\n",
             (Gram.mk_action
                (fun _  (p : 'pat)  _  (_loc : FanLoc.t)  -> (p : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, p, t) : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "as";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p, s) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'pat)  _  (_loc : FanLoc.t) 
                   -> (`Alias (_loc, p, s) : 'pat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_pat : 'comma_pat Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pl : 'comma_pat)  _  (p : 'pat)  _  (_loc : FanLoc.t)  ->\n     (`Par (_loc, (`Com (_loc, p, pl))) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (pl : 'comma_pat)  _  (p : 'pat)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Par (_loc, (`Com (_loc, p, pl))) : 'pat )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, s) : 'pat ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'pat )))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'type_longident)  _  (_loc : FanLoc.t)  ->\n     (`ClassPath (_loc, i) : 'pat ))\n",
             (Gram.mk_action
                (fun (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                   (`ClassPath (_loc, i) : 'pat )))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.pat_tag : 'pat )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.pat_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.pat_tag : 'pat )
                   | _ ->
                       failwith "AstQuotation.expand _loc x FanDyn.pat_tag\n"))));
         ([`Skeyword "_"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'pat ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'pat )))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'pat)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), p) : 'pat )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, i)), p)\n\")\n",
             (Gram.mk_action
                (fun (p : 'pat)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), p) : 'pat )
                   | _ -> failwith "`Label (_loc, (`Lid (_loc, i)), p)\n"))));
         ([`Skeyword "~";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'pat)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, p) : 'pat ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Label (_loc, i, p) : 'pat )))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  -> (`LabelS (_loc, i) : 'pat ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`LabelS (_loc, i) : 'pat )))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 'pat )\n     | _ -> failwith \"`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n\")\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 
                       'pat )
                   | _ ->
                       failwith
                         "`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n"))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'pat_tcon)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'pat )\n     | _ -> failwith \"`OptLabl (_loc, (`Lid (_loc, i)), p)\n\")\n",
             (Gram.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'pat )
                   | _ -> failwith "`OptLabl (_loc, (`Lid (_loc, i)), p)\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`OptLablExpr (_loc, i, p, e) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : FanLoc.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'pat_tcon)  _  _ \n     (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     match __fan_6 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 'pat )\n     | _ -> failwith \"`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 
                       'pat )
                   | _ ->
                       failwith
                         "`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, i, p) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : FanLoc.t)  -> (`OptLabl (_loc, i, p) : 'pat )))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLablS (_loc, i) : 'pat ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`OptLablS (_loc, i) : 'pat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipat_tcon : 'ipat_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'ipat_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, (`Lid (_loc, \"\")), p) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : FanLoc.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'pat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipat_tcon : 'ipat_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e) : 'pat ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'pat )))))])]);
   Gram.extend_single (ipat : 'ipat Gram.t )
     (None,
       (None, None,
         [([`Skeyword "{";
           `Snterm (Gram.obj (label_pat_list : 'label_pat_list Gram.t ));
           `Skeyword "}"],
            ("Gram.mk_action\n  (fun _  (pl : 'label_pat_list)  _  (_loc : FanLoc.t)  ->\n     (`Record (_loc, pl) : 'ipat ))\n",
              (Gram.mk_action
                 (fun _  (pl : 'label_pat_list)  _  (_loc : FanLoc.t)  ->
                    (`Record (_loc, pl) : 'ipat )))));
         ([`Stoken
             (((function
                | `Ant ((""|"pat"|"anti"|"par"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"pat\"|\"anti\"|\"par\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"pat\"|\"anti\"|\"par\" as n),s) ->\n         (mk_anti _loc ~c:\"pat\" n s : 'ipat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"pat"|"anti"|"par" as n),s) ->
                       (mk_anti _loc ~c:"pat" n s : 'ipat )
                   | _ -> failwith "mk_anti _loc ~c:\"pat\" n s\n"))));
         ([`Skeyword "("; `Skeyword ")"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, \"()\") : 'ipat ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`Uid (_loc, "()") : 'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleUnpack (_loc, m) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleUnpack (_loc, m) : 'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'ipat )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_4 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 'ipat )\n     | _ -> failwith \"`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleConstraint (_loc, m, (mk_anti _loc n s)) : 
                       'ipat )
                   | _ ->
                       failwith
                         "`ModuleConstraint (_loc, m, (mk_anti _loc n s))\n"))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (p : 'ipat)  _  (_loc : FanLoc.t)  -> (p : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (p : 'ipat)  _  (_loc : FanLoc.t)  -> (p : 'ipat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (p : 'ipat)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'ipat)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, p, t) : 'ipat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "as";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (s : 'a_lident)  _  (p : 'ipat)  _  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p, s) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'ipat)  _  (_loc : FanLoc.t)
                    -> (`Alias (_loc, p, s) : 'ipat )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_ipat : 'comma_ipat Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pl : 'comma_ipat)  _  (p : 'ipat)  _  (_loc : FanLoc.t)  ->\n     (`Par (_loc, (`Com (_loc, p, pl))) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (pl : 'comma_ipat)  _  (p : 'ipat)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Par (_loc, (`Com (_loc, p, pl))) : 'ipat )))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'a_lident)  (_loc : FanLoc.t)  -> ((s : alident  :>pat) : 'ipat ))\n",
             (Gram.mk_action
                (fun (s : 'a_lident)  (_loc : FanLoc.t)  ->
                   ((s : alident  :>pat) : 'ipat )))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.pat_tag : 'ipat )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.pat_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.pat_tag : 'ipat )
                   | _ ->
                       failwith "AstQuotation.expand _loc x FanDyn.pat_tag\n"))));
         ([`Skeyword "_"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ipat )))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'ipat)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), p) : 'ipat )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, i)), p)\n\")\n",
             (Gram.mk_action
                (fun (p : 'ipat)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `LABEL i ->
                       (`Label (_loc, (`Lid (_loc, i)), p) : 'ipat )
                   | _ -> failwith "`Label (_loc, (`Lid (_loc, i)), p)\n"))));
         ([`Skeyword "~";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'ipat)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, p) : 'ipat ))\n",
             (Gram.mk_action
                (fun (p : 'ipat)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`Label (_loc, i, p) : 'ipat )))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  -> (`LabelS (_loc, i) : 'ipat ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`LabelS (_loc, i) : 'ipat )))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 'ipat )\n     | _ -> failwith \"`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n\")\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`OptLablExpr (_loc, (`Lid (_loc, i)), p, e) : 
                       'ipat )
                   | _ ->
                       failwith
                         "`OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n"))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'pat_tcon)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'ipat )\n     | _ -> failwith \"`OptLabl (_loc, (`Lid (_loc, i)), p)\n\")\n",
             (Gram.mk_action
                (fun _  (p : 'pat_tcon)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), p) : 'ipat )
                   | _ -> failwith "`OptLabl (_loc, (`Lid (_loc, i)), p)\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`OptLablExpr (_loc, i, p, e) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _  (i : 'a_lident)
                    _  (_loc : FanLoc.t)  ->
                   (`OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'pat_tcon)  _  _ \n     (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     match __fan_6 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 'ipat )\n     | _ -> failwith \"`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'pat_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`OptLablExpr (_loc, i, p, (mk_anti _loc n s)) : 
                       'ipat )
                   | _ ->
                       failwith
                         "`OptLablExpr (_loc, i, p, (mk_anti _loc n s))\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (pat_tcon : 'pat_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, i, p) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : FanLoc.t)  -> (`OptLabl (_loc, i, p) : 'ipat )))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLablS (_loc, i) : 'ipat ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`OptLablS (_loc, i) : 'ipat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipat_tcon : 'ipat_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'ipat_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, (`Lid (_loc, \"\")), p) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (p : 'ipat_tcon)  _  _  (_loc : FanLoc.t)  ->
                   (`OptLabl (_loc, (`Lid (_loc, "")), p) : 'ipat )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipat_tcon : 'ipat_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (exp : 'exp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e) : 'ipat ))\n",
             (Gram.mk_action
                (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`OptLablExpr (_loc, (`Lid (_loc, "")), p, e) : 'ipat )))))]));
   Gram.extend_single (sem_pat : 'sem_pat Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("list",_) -> true | _ -> false)),
                (`Normal, "`Ant (\"list\",_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"pat;\" n s : 'sem_pat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat;\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("list" as n),s) ->
                        (mk_anti _loc ~c:"pat;" n s : 'sem_pat )
                    | _ -> failwith "mk_anti _loc ~c:\"pat;\" n s\n"))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t )); `Skeyword ";"; `Sself],
           ("Gram.mk_action\n  (fun (p2 : 'sem_pat)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, p2) : 'sem_pat ))\n",
             (Gram.mk_action
                (fun (p2 : 'sem_pat)  _  (p1 : 'pat)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, p1, p2) : 'sem_pat )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t )); `Skeyword ";"],
           ("Gram.mk_action (fun _  (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'sem_pat ))\n",
             (Gram.mk_action
                (fun _  (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'sem_pat )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'sem_pat ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'sem_pat )))))]));
   Gram.extend_single (sem_pat_for_list : 'sem_pat_for_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (pat : 'pat Gram.t )); `Skeyword ";"; `Sself],
            ("Gram.mk_action\n  (fun (pl : 'sem_pat_for_list)  _  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), (pl acc)) : \n     'sem_pat_for_list ))\n",
              (Gram.mk_action
                 (fun (pl : 'sem_pat_for_list)  _  (p : 'pat) 
                    (_loc : FanLoc.t)  ->
                    (fun acc  ->
                       `App
                         (_loc, (`App (_loc, (`Uid (_loc, "::")), p)),
                           (pl acc)) : 'sem_pat_for_list )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t )); `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc) : \n     'sem_pat_for_list ))\n",
             (Gram.mk_action
                (fun _  (p : 'pat)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'pat)  (_loc : FanLoc.t)  ->\n     (fun acc  -> `App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), p)), acc) : \n     'sem_pat_for_list ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App (_loc, (`App (_loc, (`Uid (_loc, "::")), p)), acc) : 
                   'sem_pat_for_list )))))]));
   Gram.extend_single (pat_tcon : 'pat_tcon Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (p : 'pat)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'pat_tcon ))\n",
              (Gram.mk_action
                 (fun (t : 'ctyp)  _  (p : 'pat)  (_loc : FanLoc.t)  ->
                    (`Constraint (_loc, p, t) : 'pat_tcon )))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'pat_tcon ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'pat_tcon )))))]));
   Gram.extend_single (ipat_tcon : 'ipat_tcon Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) -> (mk_anti _loc ~c:\"pat\" n s : 'ipat_tcon )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"anti" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'ipat_tcon )
                    | _ -> failwith "mk_anti _loc ~c:\"pat\" n s\n"))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i : alident  :>pat) : 'ipat_tcon ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                   ((i : alident  :>pat) : 'ipat_tcon )))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((`Constraint (_loc, (i : alident  :>pat), t) : pat ) : 'ipat_tcon ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : FanLoc.t)  ->
                   ((`Constraint (_loc, (i : alident  :>pat), t) : pat ) : 
                   'ipat_tcon )))))]));
   Gram.extend_single (comma_ipat : 'comma_ipat Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'comma_ipat)  _  (p1 : 'comma_ipat)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, p1, p2) : 'comma_ipat ))\n",
              (Gram.mk_action
                 (fun (p2 : 'comma_ipat)  _  (p1 : 'comma_ipat) 
                    (_loc : FanLoc.t)  ->
                    (`Com (_loc, p1, p2) : 'comma_ipat )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"pat,\" n s : 'comma_ipat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat,\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (mk_anti _loc ~c:"pat," n s : 'comma_ipat )
                   | _ -> failwith "mk_anti _loc ~c:\"pat,\" n s\n"))));
         ([`Snterm (Gram.obj (ipat : 'ipat Gram.t ))],
           ("Gram.mk_action (fun (p : 'ipat)  (_loc : FanLoc.t)  -> (p : 'comma_ipat ))\n",
             (Gram.mk_action
                (fun (p : 'ipat)  (_loc : FanLoc.t)  -> (p : 'comma_ipat )))))]));
   Gram.extend_single (comma_pat : 'comma_pat Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'comma_pat)  _  (p1 : 'comma_pat)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, p1, p2) : 'comma_pat ))\n",
              (Gram.mk_action
                 (fun (p2 : 'comma_pat)  _  (p1 : 'comma_pat) 
                    (_loc : FanLoc.t)  -> (`Com (_loc, p1, p2) : 'comma_pat )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"pat,\" n s : 'comma_pat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat,\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (mk_anti _loc ~c:"pat," n s : 'comma_pat )
                   | _ -> failwith "mk_anti _loc ~c:\"pat,\" n s\n"))));
         ([`Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'comma_pat ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  (_loc : FanLoc.t)  -> (p : 'comma_pat )))))]));
   Gram.extend_single (label_pat_list : 'label_pat_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_pat : 'label_pat Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'label_pat_list)  _  (p1 : 'label_pat)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, p2) : 'label_pat_list ))\n",
              (Gram.mk_action
                 (fun (p2 : 'label_pat_list)  _  (p1 : 'label_pat) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_pat_list )))));
         ([`Snterm (Gram.obj (label_pat : 'label_pat Gram.t ));
          `Skeyword ";";
          `Skeyword "_"],
           ("Gram.mk_action\n  (fun _  _  (p1 : 'label_pat)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list ))\n",
             (Gram.mk_action
                (fun _  _  (p1 : 'label_pat)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Snterm (Gram.obj (label_pat : 'label_pat Gram.t ));
          `Skeyword ";";
          `Skeyword "_";
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  _  _  (p1 : 'label_pat)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list ))\n",
             (Gram.mk_action
                (fun _  _  _  (p1 : 'label_pat)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Snterm (Gram.obj (label_pat : 'label_pat Gram.t ));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (p1 : 'label_pat)  (_loc : FanLoc.t)  -> (p1 : 'label_pat_list ))\n",
             (Gram.mk_action
                (fun _  (p1 : 'label_pat)  (_loc : FanLoc.t)  ->
                   (p1 : 'label_pat_list )))));
         ([`Snterm (Gram.obj (label_pat : 'label_pat Gram.t ))],
           ("Gram.mk_action\n  (fun (p1 : 'label_pat)  (_loc : FanLoc.t)  -> (p1 : 'label_pat_list ))\n",
             (Gram.mk_action
                (fun (p1 : 'label_pat)  (_loc : FanLoc.t)  ->
                   (p1 : 'label_pat_list )))))]));
   Gram.extend_single (label_pat : 'label_pat Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"pat"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"pat\"|\"anti\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"pat\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"pat\" n s : 'label_pat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"pat"|"anti" as n),s) ->
                        (mk_anti _loc ~c:"pat" n s : 'label_pat )
                    | _ -> failwith "mk_anti _loc ~c:\"pat\" n s\n"))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"pat;\" n s : 'label_pat )\n     | _ -> failwith \"mk_anti _loc ~c:\"pat;\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (mk_anti _loc ~c:"pat;" n s : 'label_pat )
                   | _ -> failwith "mk_anti _loc ~c:\"pat;\" n s\n"))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (pat : 'pat Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'pat)  _  (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, i, p) : 'label_pat ))\n",
             (Gram.mk_action
                (fun (p : 'pat)  _  (i : 'label_longident)  (_loc : FanLoc.t)
                    -> (`RecBind (_loc, i, p) : 'label_pat )))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, i, (`Lid (_loc, (FanOps.to_lid i)))) : 'label_pat ))\n",
             (Gram.mk_action
                (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                   (`RecBind (_loc, i, (`Lid (_loc, (FanOps.to_lid i)))) : 
                   'label_pat )))))])));
  (Gram.extend_single (luident : 'luident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Lid i -> (i : 'luident ) | _ -> failwith \"i\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Lid i -> (i : 'luident )
                    | _ -> failwith "i\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid i -> (i : 'luident ) | _ -> failwith \"i\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (i : 'luident )
                   | _ -> failwith "i\n"))))]));
   Gram.extend_single (aident : 'aident Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  -> ((i :>ident) : 'aident ))\n",
              (Gram.mk_action
                 (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                    ((i :>ident) : 'aident )))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_uident)  (_loc : FanLoc.t)  -> ((i :>ident) : 'aident ))\n",
             (Gram.mk_action
                (fun (i : 'a_uident)  (_loc : FanLoc.t)  ->
                   ((i :>ident) : 'aident )))))]));
   Gram.extend_single (astr : 'astr Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`C (_loc, i) : 'astr )\n     | _ -> failwith \"`C (_loc, i)\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Lid i -> (`C (_loc, i) : 'astr )
                    | _ -> failwith "`C (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`C (_loc, i) : 'astr )\n     | _ -> failwith \"`C (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`C (_loc, i) : 'astr )
                   | _ -> failwith "`C (_loc, i)\n"))));
         ([`Stoken
             (((function | `Ant ((""|"vrn"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"vrn\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"vrn\" as n),s) -> (mk_anti _loc n s : 'astr )\n     | _ -> failwith \"mk_anti _loc n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"vrn" as n),s) -> (mk_anti _loc n s : 'astr )
                   | _ -> failwith "mk_anti _loc n s\n"))))]));
   Gram.extend (ident_quot : 'ident_quot Gram.t )
     (None,
       [((Some "."), None,
          [([`Sself; `Skeyword "."; `Sself],
             ("Gram.mk_action\n  (fun (j : 'ident_quot)  _  (i : 'ident_quot)  (_loc : FanLoc.t)  ->\n     (`Dot (_loc, i, j) : 'ident_quot ))\n",
               (Gram.mk_action
                  (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                     (_loc : FanLoc.t)  -> (`Dot (_loc, i, j) : 'ident_quot )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'ident_quot )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"lid\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"lid\" as n),s) -> (mk_anti _loc ~c:\"ident\" n s : 'ident_quot )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident_quot )
                   | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (i : 'ident_quot)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i) : 'ident_quot )\n     | _ -> failwith \"`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n\")\n",
             (Gram.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         "`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'ident_quot )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'ident_quot )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'ident_quot )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`Uid (_loc, i) : 'ident_quot )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (j : 'ident_quot)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Uid s -> (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident_quot )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, s)), j)\n\")\n",
             (Gram.mk_action
                (fun (j : 'ident_quot)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid s ->
                       (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident_quot )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, s)), j)\n"))));
         ([`Skeyword "("; `Sself; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _  (_loc : FanLoc.t)  ->\n     (`Apply (_loc, i, j) : 'ident_quot ))\n",
             (Gram.mk_action
                (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                   (_loc : FanLoc.t)  -> (`Apply (_loc, i, j) : 'ident_quot )))))])]);
   Gram.extend_single (ident : 'ident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'ident )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'ident )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"lid\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"lid\" as n),s) -> (mk_anti _loc ~c:\"ident\" n s : 'ident )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'ident )
                   | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (i : 'ident)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i) : 'ident )\n     | _ -> failwith \"`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n\")\n",
             (Gram.mk_action
                (fun (i : 'ident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'ident )
                   | _ ->
                       failwith
                         "`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'ident )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'ident )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'ident )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`Uid (_loc, i) : 'ident )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (j : 'ident)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid s -> (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, s)), j)\n\")\n",
             (Gram.mk_action
                (fun (j : 'ident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid s -> (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, s)), j)\n"))))]));
   Gram.extend_single (vid : 'vid Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'vid )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'vid )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"lid\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"lid\" as n),s) -> (mk_anti _loc ~c:\"ident\" n s : 'vid )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'vid )
                   | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (i : 'vid)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i) : 'vid )\n     | _ -> failwith \"`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n\")\n",
             (Gram.mk_action
                (fun (i : 'vid)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), i) : 
                       'vid )
                   | _ ->
                       failwith
                         "`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), i)\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'vid )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'vid )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'vid )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`Uid (_loc, i) : 'vid )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (j : 'vid)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid s -> (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, s)), j)\n\")\n",
             (Gram.mk_action
                (fun (j : 'vid)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid s -> (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, s)), j)\n"))))]));
   Gram.extend_single (uident : 'uident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid s -> (`Uid (_loc, s) : 'uident )\n     | _ -> failwith \"`Uid (_loc, s)\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Uid s -> (`Uid (_loc, s) : 'uident )
                    | _ -> failwith "`Uid (_loc, s)\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"uident\" n s : 'uident )\n     | _ -> failwith \"mk_anti _loc ~c:\"uident\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (mk_anti _loc ~c:"uident" n s : 'uident )
                   | _ -> failwith "mk_anti _loc ~c:\"uident\" n s\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'uident)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid s -> (dot (`Uid (_loc, s)) l : 'uident )\n     | _ -> failwith \"dot (`Uid (_loc, s)) l\n\")\n",
             (Gram.mk_action
                (fun (l : 'uident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid s -> (dot (`Uid (_loc, s)) l : 'uident )
                   | _ -> failwith "dot (`Uid (_loc, s)) l\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (i : 'uident)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (dot (mk_anti _loc ~c:\"uident\" n s) i : 'uident )\n     | _ -> failwith \"dot (mk_anti _loc ~c:\"uident\" n s) i\n\")\n",
             (Gram.mk_action
                (fun (i : 'uident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (dot (mk_anti _loc ~c:"uident" n s) i : 'uident )
                   | _ -> failwith "dot (mk_anti _loc ~c:\"uident\" n s) i\n"))))]));
   Gram.extend_single (dot_namespace : 'dot_namespace Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"));
           `Skeyword ".";
           `Sself],
            ("Gram.mk_action\n  (fun (xs : 'dot_namespace)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (i :: xs : 'dot_namespace )\n     | _ -> failwith \"i :: xs\n\")\n",
              (Gram.mk_action
                 (fun (xs : 'dot_namespace)  _  (__fan_0 : [> FanToken.t]) 
                    (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Uid i -> (i :: xs : 'dot_namespace )
                    | _ -> failwith "i :: xs\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> ([i] : 'dot_namespace )\n     | _ -> failwith \"[i]\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> ([i] : 'dot_namespace )
                   | _ -> failwith "[i]\n"))))]));
   Gram.extend_single (dot_lstrings : 'dot_lstrings Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (((`Sub []), i) : 'dot_lstrings )\n     | _ -> failwith \"((`Sub []), i)\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Lid i -> (((`Sub []), i) : 'dot_lstrings )
                    | _ -> failwith "((`Sub []), i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'dot_lstrings)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)\n      ->\n     match __fan_0 with\n     | `Uid i ->\n         ((match xs with\n           | (`Sub xs,v) -> ((`Sub (i :: xs)), v)\n           | _ -> raise (XStream.Error \"impossible dot_lstrings\")) : \n         'dot_lstrings )\n     | _ ->\n         failwith\n           \"match xs with\n| (`Sub xs,v) -> ((`Sub (i :: xs)), v)\n| _ -> raise (XStream.Error \"impossible dot_lstrings\")\n\")\n",
             (Gram.mk_action
                (fun (xs : 'dot_lstrings)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i ->
                       ((match xs with
                         | (`Sub xs,v) -> ((`Sub (i :: xs)), v)
                         | _ ->
                             raise (XStream.Error "impossible dot_lstrings")) : 
                       'dot_lstrings )
                   | _ ->
                       failwith
                         "match xs with\n| (`Sub xs,v) -> ((`Sub (i :: xs)), v)\n| _ -> raise (XStream.Error \"impossible dot_lstrings\")\n"))));
         ([`Skeyword ".";
          `Stoken
            (((function | `Uid _ -> true | _ -> false)), (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'dot_lstrings)  _  (__fan_1 : [> FanToken.t])  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Uid i ->\n         ((match xs with\n           | (`Sub xs,v) -> ((`Absolute (i :: xs)), v)\n           | _ -> raise (XStream.Error \"impossible dot_lstrings\")) : \n         'dot_lstrings )\n     | _ ->\n         failwith\n           \"match xs with\n| (`Sub xs,v) -> ((`Absolute (i :: xs)), v)\n| _ -> raise (XStream.Error \"impossible dot_lstrings\")\n\")\n",
             (Gram.mk_action
                (fun (xs : 'dot_lstrings)  _  (__fan_1 : [> FanToken.t])  _ 
                   (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Uid i ->
                       ((match xs with
                         | (`Sub xs,v) -> ((`Absolute (i :: xs)), v)
                         | _ ->
                             raise (XStream.Error "impossible dot_lstrings")) : 
                       'dot_lstrings )
                   | _ ->
                       failwith
                         "match xs with\n| (`Sub xs,v) -> ((`Absolute (i :: xs)), v)\n| _ -> raise (XStream.Error \"impossible dot_lstrings\")\n"))))]));
   Gram.extend_single
     (module_longident_dot_lparen : 'module_longident_dot_lparen Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
           `Skeyword ".";
           `Skeyword "("],
            ("Gram.mk_action\n  (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'module_longident_dot_lparen )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident_dot_lparen )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'module_longident_dot_lparen)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i ->\n         (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident_dot_lparen )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, i)), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i ->
                       (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident_dot_lparen )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, i)), l)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Skeyword "("],
           ("Gram.mk_action\n  (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'module_longident_dot_lparen )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i ->
                       (`Uid (_loc, i) : 'module_longident_dot_lparen )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Ant (("uid"|""),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"uid\"|\"\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'module_longident_dot_lparen)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"uid\"|\"\" as n),s) ->\n         (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : 'module_longident_dot_lparen )\n     | _ -> failwith \"`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("uid"|"" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         "`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n"))))]));
   Gram.extend_single (module_longident : 'module_longident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'module_longident )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, i)), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i ->
                       (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, i)), l)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'module_longident )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`Uid (_loc, i) : 'module_longident )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Ant ((""|"uid"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"uid\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'module_longident )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"uid" as n),s) ->
                       (mk_anti _loc ~c:"ident" n s : 'module_longident )
                   | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Ant ((""|"uid"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : 'module_longident )\n     | _ -> failwith \"`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'module_longident )
                   | _ ->
                       failwith
                         "`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n"))))]));
   Gram.extend
     (module_longident_with_app : 'module_longident_with_app Gram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("Gram.mk_action\n  (fun (j : 'module_longident_with_app)  (i : 'module_longident_with_app) \n     (_loc : FanLoc.t)  ->\n     (`Apply (_loc, i, j) : 'module_longident_with_app ))\n",
               (Gram.mk_action
                  (fun (j : 'module_longident_with_app) 
                     (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                     (`Apply (_loc, i, j) : 'module_longident_with_app )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("Gram.mk_action\n  (fun (j : 'module_longident_with_app)  _  (i : 'module_longident_with_app) \n     (_loc : FanLoc.t)  -> (`Dot (_loc, i, j) : 'module_longident_with_app ))\n",
              (Gram.mk_action
                 (fun (j : 'module_longident_with_app)  _ 
                    (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                    (`Dot (_loc, i, j) : 'module_longident_with_app )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'module_longident_with_app )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'module_longident_with_app )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'module_longident_with_app )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`Uid (_loc, i) : 'module_longident_with_app )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (i : 'module_longident_with_app)  _  (_loc : FanLoc.t)  ->\n     (i : 'module_longident_with_app ))\n",
             (Gram.mk_action
                (fun _  (i : 'module_longident_with_app)  _ 
                   (_loc : FanLoc.t)  -> (i : 'module_longident_with_app )))))])]);
   Gram.extend (type_longident : 'type_longident Gram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("Gram.mk_action\n  (fun (j : 'type_longident)  (i : 'type_longident)  (_loc : FanLoc.t)  ->\n     (`Apply (_loc, i, j) : 'type_longident ))\n",
               (Gram.mk_action
                  (fun (j : 'type_longident)  (i : 'type_longident) 
                     (_loc : FanLoc.t)  ->
                     (`Apply (_loc, i, j) : 'type_longident )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("Gram.mk_action\n  (fun (j : 'type_longident)  _  (i : 'type_longident)  (_loc : FanLoc.t)  ->\n     (`Dot (_loc, i, j) : 'type_longident ))\n",
              (Gram.mk_action
                 (fun (j : 'type_longident)  _  (i : 'type_longident) 
                    (_loc : FanLoc.t)  ->
                    (`Dot (_loc, i, j) : 'type_longident )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"|"lid"),_) -> true
                 | _ -> false)),
                (`Normal,
                  "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"|\"lid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"|\"lid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'type_longident )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid"|"lid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'type_longident )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'type_longident )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'type_longident )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Uid (_loc, i) : 'type_longident )\n     | _ -> failwith \"`Uid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i -> (`Uid (_loc, i) : 'type_longident )
                   | _ -> failwith "`Uid (_loc, i)\n"))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (i : 'type_longident)  _  (_loc : FanLoc.t)  ->\n     (i : 'type_longident ))\n",
             (Gram.mk_action
                (fun _  (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                   (i : 'type_longident )))))])]);
   Gram.extend_single (label_longident : 'label_longident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"lid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"lid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"lid\" as n),s) ->\n         (mk_anti _loc ~c:\"ident\" n s : 'label_longident )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"lid" as n),s) ->
                        (mk_anti _loc ~c:"ident" n s : 'label_longident )
                    | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'label_longident )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'label_longident )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid i -> (`Dot (_loc, (`Uid (_loc, i)), l) : 'label_longident )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, i)), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid i ->
                       (`Dot (_loc, (`Uid (_loc, i)), l) : 'label_longident )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, i)), l)\n"))));
         ([`Stoken
             (((function | `Ant ((""|"uid"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l) : 'label_longident )\n     | _ -> failwith \"`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"uid" as n),s) ->
                       (`Dot (_loc, (mk_anti _loc ~c:"ident" n s), l) : 
                       'label_longident )
                   | _ ->
                       failwith
                         "`Dot (_loc, (mk_anti _loc ~c:\"ident\" n s), l)\n"))))]));
   Gram.extend_single (cltyp_longident : 'cltyp_longident Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'type_longident)  (_loc : FanLoc.t)  -> (x : 'cltyp_longident ))\n",
              (Gram.mk_action
                 (fun (x : 'type_longident)  (_loc : FanLoc.t)  ->
                    (x : 'cltyp_longident )))))]));
   Gram.extend_single (val_longident : 'val_longident Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'ident)  (_loc : FanLoc.t)  -> (x : 'val_longident ))\n",
              (Gram.mk_action
                 (fun (x : 'ident)  (_loc : FanLoc.t)  ->
                    (x : 'val_longident )))))]));
   Gram.extend_single (class_longident : 'class_longident Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'label_longident)  (_loc : FanLoc.t)  -> (x : 'class_longident ))\n",
              (Gram.mk_action
                 (fun (x : 'label_longident)  (_loc : FanLoc.t)  ->
                    (x : 'class_longident )))))]));
   Gram.extend_single (method_opt_override : 'method_opt_override Gram.t )
     (None,
       (None, None,
         [([`Skeyword "method"; `Skeyword "!"],
            ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Override _loc : 'method_opt_override ))\n",
              (Gram.mk_action
                 (fun _  _  (_loc : FanLoc.t)  ->
                    (`Override _loc : 'method_opt_override )))));
         ([`Skeyword "method";
          `Stoken
            (((function
               | `Ant ((""|"override"|"anti"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"override\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Ant ((\"\"|\"override\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"override_flag\" n s : 'method_opt_override )\n     | _ -> failwith \"mk_anti _loc ~c:\"override_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Ant ((""|"override"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"override_flag" n s : 'method_opt_override )
                   | _ -> failwith "mk_anti _loc ~c:\"override_flag\" n s\n"))));
         ([`Skeyword "method"],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (`OvNil _loc : 'method_opt_override ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (`OvNil _loc : 'method_opt_override )))))]));
   Gram.extend_single (opt_override : 'opt_override Gram.t )
     (None,
       (None, None,
         [([`Skeyword "!"],
            ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (`Override _loc : 'opt_override ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (`Override _loc : 'opt_override )))));
         ([`Stoken
             (((function
                | `Ant (("!"|"override"|"anti"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"!\"|\"override\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"!\"|\"override\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"override_flag\" n s : 'opt_override )\n     | _ -> failwith \"mk_anti _loc ~c:\"override_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("!"|"override"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"override_flag" n s : 'opt_override )
                   | _ -> failwith "mk_anti _loc ~c:\"override_flag\" n s\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`OvNil _loc : 'opt_override ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`OvNil _loc : 'opt_override )))))]));
   Gram.extend_single
     (value_val_opt_override : 'value_val_opt_override Gram.t )
     (None,
       (None, None,
         [([`Skeyword "val"; `Skeyword "!"],
            ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (`Override _loc : 'value_val_opt_override ))\n",
              (Gram.mk_action
                 (fun _  _  (_loc : FanLoc.t)  ->
                    (`Override _loc : 'value_val_opt_override )))));
         ([`Skeyword "val";
          `Stoken
            (((function
               | `Ant ((""|"override"|"anti"|"!"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"override\"|\"anti\"|\"!\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Ant ((\"\"|\"override\"|\"anti\"|\"!\" as n),s) ->\n         (mk_anti _loc ~c:\"override_flag\" n s : 'value_val_opt_override )\n     | _ -> failwith \"mk_anti _loc ~c:\"override_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Ant ((""|"override"|"anti"|"!" as n),s) ->
                       (mk_anti _loc ~c:"override_flag" n s : 'value_val_opt_override )
                   | _ -> failwith "mk_anti _loc ~c:\"override_flag\" n s\n"))));
         ([`Skeyword "val"],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (`OvNil _loc : 'value_val_opt_override ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (`OvNil _loc : 'value_val_opt_override )))))]));
   Gram.extend_single (direction_flag : 'direction_flag Gram.t )
     (None,
       (None, None,
         [([`Skeyword "to"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`To _loc : 'direction_flag ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  -> (`To _loc : 'direction_flag )))));
         ([`Skeyword "downto"],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (`Downto _loc : 'direction_flag ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (`Downto _loc : 'direction_flag )))));
         ([`Stoken
             (((function | `Ant (("to"|"anti"|""),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"to\"|\"anti\"|\"\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"to\"|\"anti\"|\"\" as n),s) ->\n         (mk_anti _loc ~c:\"direction_flag\" n s : 'direction_flag )\n     | _ -> failwith \"mk_anti _loc ~c:\"direction_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("to"|"anti"|"" as n),s) ->
                       (mk_anti _loc ~c:"direction_flag" n s : 'direction_flag )
                   | _ -> failwith "mk_anti _loc ~c:\"direction_flag\" n s\n"))))]));
   Gram.extend_single (opt_private : 'opt_private Gram.t )
     (None,
       (None, None,
         [([`Skeyword "private"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Private _loc : 'opt_private ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (`Private _loc : 'opt_private )))));
         ([`Stoken
             (((function | `Ant (("private"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"private\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"private\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"private_flag\" n s : 'opt_private )\n     | _ -> failwith \"mk_anti _loc ~c:\"private_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("private"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"private_flag" n s : 'opt_private )
                   | _ -> failwith "mk_anti _loc ~c:\"private_flag\" n s\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`PrNil _loc : 'opt_private ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`PrNil _loc : 'opt_private )))))]));
   Gram.extend_single (opt_mutable : 'opt_mutable Gram.t )
     (None,
       (None, None,
         [([`Skeyword "mutable"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Mutable _loc : 'opt_mutable ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (`Mutable _loc : 'opt_mutable )))));
         ([`Stoken
             (((function | `Ant (("mutable"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"mutable\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"mutable\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"mutable_flag\" n s : 'opt_mutable )\n     | _ -> failwith \"mk_anti _loc ~c:\"mutable_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("mutable"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"mutable_flag" n s : 'opt_mutable )
                   | _ -> failwith "mk_anti _loc ~c:\"mutable_flag\" n s\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`MuNil _loc : 'opt_mutable ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`MuNil _loc : 'opt_mutable )))))]));
   Gram.extend_single (opt_virtual : 'opt_virtual Gram.t )
     (None,
       (None, None,
         [([`Skeyword "virtual"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Virtual _loc : 'opt_virtual ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (`Virtual _loc : 'opt_virtual )))));
         ([`Stoken
             (((function | `Ant (("virtual"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"virtual\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"virtual\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"virtual_flag\" n s : 'opt_virtual )\n     | _ -> failwith \"mk_anti _loc ~c:\"virtual_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("virtual"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"virtual_flag" n s : 'opt_virtual )
                   | _ -> failwith "mk_anti _loc ~c:\"virtual_flag\" n s\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`ViNil _loc : 'opt_virtual ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`ViNil _loc : 'opt_virtual )))))]));
   Gram.extend_single (opt_dot_dot : 'opt_dot_dot Gram.t )
     (None,
       (None, None,
         [([`Skeyword ".."],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`RowVar _loc : 'opt_dot_dot ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  -> (`RowVar _loc : 'opt_dot_dot )))));
         ([`Stoken
             (((function | `Ant ((".."|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"..\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"..\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"row_var_flag\" n s : 'opt_dot_dot )\n     | _ -> failwith \"mk_anti _loc ~c:\"row_var_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((".."|"anti" as n),s) ->
                       (mk_anti _loc ~c:"row_var_flag" n s : 'opt_dot_dot )
                   | _ -> failwith "mk_anti _loc ~c:\"row_var_flag\" n s\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`RvNil _loc : 'opt_dot_dot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`RvNil _loc : 'opt_dot_dot )))))]));
   Gram.extend_single (opt_rec : 'opt_rec Gram.t )
     (None,
       (None, None,
         [([`Skeyword "rec"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Recursive _loc : 'opt_rec ))\n",
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  -> (`Recursive _loc : 'opt_rec )))));
         ([`Stoken
             (((function | `Ant (("rec"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"rec\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"rec\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"rec_flag\" n s : 'opt_rec )\n     | _ -> failwith \"mk_anti _loc ~c:\"rec_flag\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("rec"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"rec_flag" n s : 'opt_rec )
                   | _ -> failwith "mk_anti _loc ~c:\"rec_flag\" n s\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`ReNil _loc : 'opt_rec ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`ReNil _loc : 'opt_rec )))))]));
   Gram.extend_single (a_lident : 'a_lident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"lid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"lid\" as n),s) ->\n         (mk_anti _loc ~c:\"a_lident\" n s : 'a_lident )\n     | _ -> failwith \"mk_anti _loc ~c:\"a_lident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"lid" as n),s) ->
                        (mk_anti _loc ~c:"a_lident" n s : 'a_lident )
                    | _ -> failwith "mk_anti _loc ~c:\"a_lident\" n s\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid s -> (`Lid (_loc, s) : 'a_lident )\n     | _ -> failwith \"`Lid (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid s -> (`Lid (_loc, s) : 'a_lident )
                   | _ -> failwith "`Lid (_loc, s)\n"))))]));
   Gram.extend_single (a_uident : 'a_uident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"uid"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"uid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (mk_anti _loc ~c:\"a_uident\" n s : 'a_uident )\n     | _ -> failwith \"mk_anti _loc ~c:\"a_uident\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"uid" as n),s) ->
                        (mk_anti _loc ~c:"a_uident" n s : 'a_uident )
                    | _ -> failwith "mk_anti _loc ~c:\"a_uident\" n s\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid s -> (`Uid (_loc, s) : 'a_uident )\n     | _ -> failwith \"`Uid (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid s -> (`Uid (_loc, s) : 'a_uident )
                   | _ -> failwith "`Uid (_loc, s)\n"))))]));
   Gram.extend_single (string_list : 'string_list Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (`Normal, "`Ant (\"\",_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant (\"\",s) -> (mk_anti _loc \"str_list\" s : 'string_list )\n     | _ -> failwith \"mk_anti _loc \"str_list\" s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ("",s) ->
                        (mk_anti _loc "str_list" s : 'string_list )
                    | _ -> failwith "mk_anti _loc \"str_list\" s\n"))));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"\",_)"));
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant (\"\",s) -> (`App (_loc, (mk_anti _loc \"\" s), xs) : 'string_list )\n     | _ -> failwith \"`App (_loc, (mk_anti _loc \"\" s), xs)\n\")\n",
             (Gram.mk_action
                (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ("",s) ->
                       (`App (_loc, (mk_anti _loc "" s), xs) : 'string_list )
                   | _ -> failwith "`App (_loc, (mk_anti _loc \"\" s), xs)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,x) -> (`Str (_loc, x) : 'string_list )\n     | _ -> failwith \"`Str (_loc, x)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,x) -> (`Str (_loc, x) : 'string_list )
                   | _ -> failwith "`Str (_loc, x)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"));
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,x) -> (`App (_loc, (`Str (_loc, x)), xs) : 'string_list )\n     | _ -> failwith \"`App (_loc, (`Str (_loc, x)), xs)\n\")\n",
             (Gram.mk_action
                (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,x) ->
                       (`App (_loc, (`Str (_loc, x)), xs) : 'string_list )
                   | _ -> failwith "`App (_loc, (`Str (_loc, x)), xs)\n"))))]));
   Gram.extend_single (rec_flag_quot : 'rec_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'opt_rec)  (_loc : FanLoc.t)  -> (x : 'rec_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'opt_rec)  (_loc : FanLoc.t)  ->
                    (x : 'rec_flag_quot )))))]));
   Gram.extend_single (direction_flag_quot : 'direction_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (direction_flag : 'direction_flag Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'direction_flag)  (_loc : FanLoc.t)  ->\n     (x : 'direction_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'direction_flag)  (_loc : FanLoc.t)  ->
                    (x : 'direction_flag_quot )))))]));
   Gram.extend_single (mutable_flag_quot : 'mutable_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'opt_mutable)  (_loc : FanLoc.t)  -> (x : 'mutable_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'opt_mutable)  (_loc : FanLoc.t)  ->
                    (x : 'mutable_flag_quot )))))]));
   Gram.extend_single (private_flag_quot : 'private_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_private : 'opt_private Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'opt_private)  (_loc : FanLoc.t)  -> (x : 'private_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'opt_private)  (_loc : FanLoc.t)  ->
                    (x : 'private_flag_quot )))))]));
   Gram.extend_single (virtual_flag_quot : 'virtual_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'opt_virtual)  (_loc : FanLoc.t)  -> (x : 'virtual_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'opt_virtual)  (_loc : FanLoc.t)  ->
                    (x : 'virtual_flag_quot )))))]));
   Gram.extend_single (row_var_flag_quot : 'row_var_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'opt_dot_dot)  (_loc : FanLoc.t)  -> (x : 'row_var_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'opt_dot_dot)  (_loc : FanLoc.t)  ->
                    (x : 'row_var_flag_quot )))))]));
   Gram.extend_single (override_flag_quot : 'override_flag_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_override : 'opt_override Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'opt_override)  (_loc : FanLoc.t)  -> (x : 'override_flag_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'opt_override)  (_loc : FanLoc.t)  ->
                    (x : 'override_flag_quot )))))]));
   Gram.extend_single (pat_eoi : 'pat_eoi Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (pat : 'pat Gram.t ));
           `Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  (x : 'pat)  (_loc : FanLoc.t)  ->\n     match __fan_1 with | `EOI -> (x : 'pat_eoi ) | _ -> failwith \"x\n\")\n",
              (Gram.mk_action
                 (fun (__fan_1 : [> FanToken.t])  (x : 'pat) 
                    (_loc : FanLoc.t)  ->
                    match __fan_1 with
                    | `EOI -> (x : 'pat_eoi )
                    | _ -> failwith "x\n"))))]));
   Gram.extend_single (exp_eoi : 'exp_eoi Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  (x : 'exp)  (_loc : FanLoc.t)  ->\n     match __fan_1 with | `EOI -> (x : 'exp_eoi ) | _ -> failwith \"x\n\")\n",
              (Gram.mk_action
                 (fun (__fan_1 : [> FanToken.t])  (x : 'exp) 
                    (_loc : FanLoc.t)  ->
                    match __fan_1 with
                    | `EOI -> (x : 'exp_eoi )
                    | _ -> failwith "x\n"))))])));
  (Gram.extend_single (implem : 'implem Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Skeyword ";;"],
            ("Gram.mk_action\n  (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (([`Directive (_loc, n, dp)], (Some _loc)) : 'implem ))\n",
              (Gram.mk_action
                 (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t) 
                    -> (([`Directive (_loc, n, dp)], (Some _loc)) : 'implem )))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ";;"],
           ("Gram.mk_action\n  (fun _  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (([`DirectiveSimple (_loc, n)], (Some _loc)) : 'implem ))\n",
             (Gram.mk_action
                (fun _  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (([`DirectiveSimple (_loc, n)], (Some _loc)) : 'implem )))));
         ([`Skeyword "#";
          `Skeyword "import";
          `Snterm (Gram.obj (dot_namespace : 'dot_namespace Gram.t ));
          `Skeyword ";;"],
           ("Gram.mk_action\n  (fun _  (x : 'dot_namespace)  _  _  (_loc : FanLoc.t)  ->\n     (FanToken.paths := ((`Absolute x) :: (FanToken.paths.contents));\n      ([`DirectiveSimple (_loc, (`Lid (_loc, \"import\")))], (Some _loc)) : \n     'implem ))\n",
             (Gram.mk_action
                (fun _  (x : 'dot_namespace)  _  _  (_loc : FanLoc.t)  ->
                   (FanToken.paths := ((`Absolute x) ::
                      (FanToken.paths.contents));
                    ([`DirectiveSimple (_loc, (`Lid (_loc, "import")))],
                      (Some _loc)) : 'implem )))));
         ([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";"; `Sself],
           ("Gram.mk_action\n  (fun ((sil,stopped) : 'implem)  _  (si : 'stru)  (_loc : FanLoc.t)  ->\n     (((si :: sil), stopped) : 'implem ))\n",
             (Gram.mk_action
                (fun ((sil,stopped) : 'implem)  _  (si : 'stru) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'implem )))));
         ([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";;"; `Sself],
           ("Gram.mk_action\n  (fun ((sil,stopped) : 'implem)  _  (si : 'stru)  (_loc : FanLoc.t)  ->\n     (((si :: sil), stopped) : 'implem ))\n",
             (Gram.mk_action
                (fun ((sil,stopped) : 'implem)  _  (si : 'stru) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'implem )))));
         ([`Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `EOI -> (([], None) : 'implem )\n     | _ -> failwith \"([], None)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `EOI -> (([], None) : 'implem )
                   | _ -> failwith "([], None)\n"))))]));
   Gram.extend_single (strus : 'strus Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"stri"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"stri\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc n ~c:\"stru\" s : 'strus )\n     | _ -> failwith \"mk_anti _loc n ~c:\"stru\" s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
                        (mk_anti _loc n ~c:"stru" s : 'strus )
                    | _ -> failwith "mk_anti _loc n ~c:\"stru\" s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"stri"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (st : 'strus)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"stri\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st) : 'strus )\n     | _ -> failwith \"`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n\")\n",
             (Gram.mk_action
                (fun (st : 'strus)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc n ~c:"stru" s), st) : 
                       'strus )
                   | _ ->
                       failwith
                         "`Sem (_loc, (mk_anti _loc n ~c:\"stru\" s), st)\n"))));
         ([`Slist1
             (Gram.srules
                [([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";"],
                   ("Gram.mk_action (fun _  (st : 'stru)  (_loc : FanLoc.t)  -> (st : 'e__5 ))\n",
                     (Gram.mk_action
                        (fun _  (st : 'stru)  (_loc : FanLoc.t)  ->
                           (st : 'e__5 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__5 list)  (_loc : FanLoc.t)  -> (sem_of_list l : 'strus ))\n",
             (Gram.mk_action
                (fun (l : 'e__5 list)  (_loc : FanLoc.t)  ->
                   (sem_of_list l : 'strus )))));
         ([`Slist1
             (Gram.srules
                [([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";;"],
                   ("Gram.mk_action (fun _  (st : 'stru)  (_loc : FanLoc.t)  -> (st : 'e__6 ))\n",
                     (Gram.mk_action
                        (fun _  (st : 'stru)  (_loc : FanLoc.t)  ->
                           (st : 'e__6 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__6 list)  (_loc : FanLoc.t)  -> (sem_of_list l : 'strus ))\n",
             (Gram.mk_action
                (fun (l : 'e__6 list)  (_loc : FanLoc.t)  ->
                   (sem_of_list l : 'strus )))))]));
   Gram.extend_single (top_phrase : 'top_phrase Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Skeyword ";;"],
            ("Gram.mk_action\n  (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (Some (`Directive (_loc, n, dp)) : 'top_phrase ))\n",
              (Gram.mk_action
                 (fun _  (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t) 
                    -> (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ";;"],
           ("Gram.mk_action\n  (fun _  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (Some (`DirectiveSimple (_loc, n)) : 'top_phrase ))\n",
             (Gram.mk_action
                (fun _  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (Some (`DirectiveSimple (_loc, n)) : 'top_phrase )))));
         ([`Skeyword "#";
          `Skeyword "import";
          `Snterm (Gram.obj (dot_namespace : 'dot_namespace Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'dot_namespace)  _  _  (_loc : FanLoc.t)  ->\n     (FanToken.paths := ((`Absolute x) :: (FanToken.paths.contents)); None : \n     'top_phrase ))\n",
             (Gram.mk_action
                (fun (x : 'dot_namespace)  _  _  (_loc : FanLoc.t)  ->
                   (FanToken.paths := ((`Absolute x) ::
                      (FanToken.paths.contents));
                    None : 'top_phrase )))));
         ([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (st : 'stru)  (_loc : FanLoc.t)  -> (Some st : 'top_phrase ))\n",
             (Gram.mk_action
                (fun _  (st : 'stru)  (_loc : FanLoc.t)  ->
                   (Some st : 'top_phrase )))));
         ([`Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `EOI -> (None : 'top_phrase )\n     | _ -> failwith \"None\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `EOI -> (None : 'top_phrase )
                   | _ -> failwith "None\n"))))]));
   Gram.extend_single (stru_quot : 'stru_quot Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Directive (_loc, n, dp) : 'stru_quot ))\n",
              (Gram.mk_action
                 (fun (dp : 'exp)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->
                    (`Directive (_loc, n, dp) : 'stru_quot )))));
         ([`Skeyword "#"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`DirectiveSimple (_loc, n) : 'stru_quot ))\n",
             (Gram.mk_action
                (fun (n : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`DirectiveSimple (_loc, n) : 'stru_quot )))));
         ([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";"; `Sself],
           ("Gram.mk_action\n  (fun (st2 : 'stru_quot)  _  (st1 : 'stru)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, st1, st2) : 'stru_quot ))\n",
             (Gram.mk_action
                (fun (st2 : 'stru_quot)  _  (st1 : 'stru)  (_loc : FanLoc.t) 
                   -> (`Sem (_loc, st1, st2) : 'stru_quot )))));
         ([`Snterm (Gram.obj (stru : 'stru Gram.t ))],
           ("Gram.mk_action (fun (st : 'stru)  (_loc : FanLoc.t)  -> (st : 'stru_quot ))\n",
             (Gram.mk_action
                (fun (st : 'stru)  (_loc : FanLoc.t)  -> (st : 'stru_quot )))))]));
   Gram.extend (stru : 'stru Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "exception";
            `Snterm
              (Gram.obj
                 (constructor_declaration : 'constructor_declaration Gram.t ))],
             ("Gram.mk_action\n  (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Exception (_loc, t) : 'stru ))\n",
               (Gram.mk_action
                  (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t) 
                     -> (`Exception (_loc, t) : 'stru )))));
          ([`Skeyword "external";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (string_list : 'string_list Gram.t ))],
            ("Gram.mk_action\n  (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`External (_loc, i, t, sl) : 'stru ))\n",
              (Gram.mk_action
                 (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)
                     _  (_loc : FanLoc.t)  ->
                    (`External (_loc, i, t, sl) : 'stru )))));
          ([`Skeyword "include"; `Snterm (Gram.obj (mexp : 'mexp Gram.t ))],
            ("Gram.mk_action\n  (fun (me : 'mexp)  _  (_loc : FanLoc.t)  -> (`Include (_loc, me) : 'stru ))\n",
              (Gram.mk_action
                 (fun (me : 'mexp)  _  (_loc : FanLoc.t)  ->
                    (`Include (_loc, me) : 'stru )))));
          ([`Skeyword "module";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Snterm (Gram.obj (mbind0 : 'mbind0 Gram.t ))],
            ("Gram.mk_action\n  (fun (mb : 'mbind0)  (i : 'a_uident)  _  (_loc : FanLoc.t)  ->\n     (`Module (_loc, i, mb) : 'stru ))\n",
              (Gram.mk_action
                 (fun (mb : 'mbind0)  (i : 'a_uident)  _  (_loc : FanLoc.t) 
                    -> (`Module (_loc, i, mb) : 'stru )))));
          ([`Skeyword "module";
           `Skeyword "rec";
           `Snterm (Gram.obj (mbind : 'mbind Gram.t ))],
            ("Gram.mk_action\n  (fun (mb : 'mbind)  _  _  (_loc : FanLoc.t)  ->\n     (`RecModule (_loc, mb) : 'stru ))\n",
              (Gram.mk_action
                 (fun (mb : 'mbind)  _  _  (_loc : FanLoc.t)  ->
                    (`RecModule (_loc, mb) : 'stru )))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ))],
            ("Gram.mk_action\n  (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleType (_loc, i, mt) : 'stru ))\n",
              (Gram.mk_action
                 (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`ModuleType (_loc, i, mt) : 'stru )))));
          ([`Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->\n     (`Open (_loc, (i : vid  :>ident)) : 'stru ))\n",
              (Gram.mk_action
                 (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                    (`Open (_loc, (i : vid  :>ident)) : 'stru )))));
          ([`Skeyword "type";
           `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
            ("Gram.mk_action\n  (fun (td : 'type_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Type (_loc, td) : 'stru ))\n",
              (Gram.mk_action
                 (fun (td : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                    (`Type (_loc, td) : 'stru )))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'exp)  _  (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) \n     -> (`StExp (_loc, (`LetIn (_loc, r, bi, x))) : 'stru ))\n",
              (Gram.mk_action
                 (fun (x : 'exp)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                    (_loc : FanLoc.t)  ->
                    (`StExp (_loc, (`LetIn (_loc, r, bi, x))) : 'stru )))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ))],
            ("Gram.mk_action\n  (fun (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t)  ->\n     (match bi with\n      | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n      | _ -> `Value (_loc, r, bi) : 'stru ))\n",
              (Gram.mk_action
                 (fun (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) 
                    ->
                    (match bi with
                     | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                     | _ -> `Value (_loc, r, bi) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Snterm (Gram.obj (mbind0 : 'mbind0 Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 'stru ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  (i : 'module_longident)  _  _  (_loc : FanLoc.t)  ->\n     (let i = (i : vid  :>ident) in `StExp (_loc, (`LetOpen (_loc, i, e))) : \n     'stru ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (let i = (i : vid  :>ident) in
                     `StExp (_loc, (`LetOpen (_loc, i, e))) : 'stru )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Skeyword "with";
           `Snterm (Gram.obj (case : 'case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'binding)  (r : 'opt_rec)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a))) : 'stru ))\n",
              (Gram.mk_action
                 (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'binding) 
                    (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->
                    (`StExp (_loc, (`LetTryInWith (_loc, r, bi, x, a))) : 
                    'stru )))));
          ([`Skeyword "class";
           `Snterm
             (Gram.obj (class_declaration : 'class_declaration Gram.t ))],
            ("Gram.mk_action\n  (fun (cd : 'class_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Class (_loc, cd) : 'stru ))\n",
              (Gram.mk_action
                 (fun (cd : 'class_declaration)  _  (_loc : FanLoc.t)  ->
                    (`Class (_loc, cd) : 'stru )))));
          ([`Skeyword "class";
           `Skeyword "type";
           `Snterm
             (Gram.obj (cltyp_declaration : 'cltyp_declaration Gram.t ))],
            ("Gram.mk_action\n  (fun (ctd : 'cltyp_declaration)  _  _  (_loc : FanLoc.t)  ->\n     (`ClassType (_loc, ctd) : 'stru ))\n",
              (Gram.mk_action
                 (fun (ctd : 'cltyp_declaration)  _  _  (_loc : FanLoc.t)  ->
                    (`ClassType (_loc, ctd) : 'stru )))));
          ([`Stoken
              (((function
                 | `Ant ((""|"stri"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"stri\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"stru\" n s : 'stru )\n     | _ -> failwith \"mk_anti _loc ~c:\"stru\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"stru" n s : 'stru )
                    | _ -> failwith "mk_anti _loc ~c:\"stru\" n s\n"))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.stru_tag : 'stru )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.stru_tag\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `QUOTATION x ->
                        (AstQuotation.expand _loc x FanDyn.stru_tag : 
                        'stru )
                    | _ ->
                        failwith
                          "AstQuotation.expand _loc x FanDyn.stru_tag\n"))));
          ([`Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  (_loc : FanLoc.t)  -> (`StExp (_loc, e) : 'stru ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  (_loc : FanLoc.t)  ->
                    (`StExp (_loc, e) : 'stru )))))])]));
  (Gram.extend_single (clsigi_quot : 'clsigi_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (clsigi : 'clsigi Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (x2 : 'clsigi_quot)  _  (x1 : 'clsigi)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, x1, x2) : 'clsigi_quot ))\n",
              (Gram.mk_action
                 (fun (x2 : 'clsigi_quot)  _  (x1 : 'clsigi) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, x1, x2) : 'clsigi_quot )))));
         ([`Snterm (Gram.obj (clsigi : 'clsigi Gram.t ))],
           ("Gram.mk_action (fun (x : 'clsigi)  (_loc : FanLoc.t)  -> (x : 'clsigi_quot ))\n",
             (Gram.mk_action
                (fun (x : 'clsigi)  (_loc : FanLoc.t)  -> (x : 'clsigi_quot )))))]));
   Gram.extend_single (class_signature : 'class_signature Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"csg"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"csg\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"clsigi\" n s : 'class_signature )\n     | _ -> failwith \"mk_anti _loc ~c:\"clsigi\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"csg"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"clsigi" n s : 'class_signature )
                    | _ -> failwith "mk_anti _loc ~c:\"clsigi\" n s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"csg"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (csg : 'class_signature)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"csg\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg) : 'class_signature )\n     | _ -> failwith \"`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg)\n\")\n",
             (Gram.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"csg"|"anti"|"list" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc ~c:"clsigi" n s), csg) : 
                       'class_signature )
                   | _ ->
                       failwith
                         "`Sem (_loc, (mk_anti _loc ~c:\"clsigi\" n s), csg)\n"))));
         ([`Slist1
             (Gram.srules
                [([`Snterm (Gram.obj (clsigi : 'clsigi Gram.t ));
                  `Skeyword ";"],
                   ("Gram.mk_action (fun _  (csg : 'clsigi)  (_loc : FanLoc.t)  -> (csg : 'e__7 ))\n",
                     (Gram.mk_action
                        (fun _  (csg : 'clsigi)  (_loc : FanLoc.t)  ->
                           (csg : 'e__7 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__7 list)  (_loc : FanLoc.t)  ->\n     (sem_of_list l : 'class_signature ))\n",
             (Gram.mk_action
                (fun (l : 'e__7 list)  (_loc : FanLoc.t)  ->
                   (sem_of_list l : 'class_signature )))))]));
   Gram.extend_single (clsigi : 'clsigi Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"csg"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"csg\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"clsigi\" n s : 'clsigi )\n     | _ -> failwith \"mk_anti _loc ~c:\"clsigi\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"csg"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"clsigi" n s : 'clsigi )
                    | _ -> failwith "mk_anti _loc ~c:\"clsigi\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.clsigi_tag : 'clsigi )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.clsigi_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.clsigi_tag : 
                       'clsigi )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.clsigi_tag\n"))));
         ([`Skeyword "inherit"; `Snterm (Gram.obj (cltyp : 'cltyp Gram.t ))],
           ("Gram.mk_action\n  (fun (cs : 'cltyp)  _  (_loc : FanLoc.t)  ->\n     (`SigInherit (_loc, cs) : 'clsigi ))\n",
             (Gram.mk_action
                (fun (cs : 'cltyp)  _  (_loc : FanLoc.t)  ->
                   (`SigInherit (_loc, cs) : 'clsigi )))));
         ([`Skeyword "val";
          `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
          `Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) \n     (mf : 'opt_mutable)  _  (_loc : FanLoc.t)  ->\n     (`CgVal (_loc, l, mf, mv, t) : 'clsigi ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) 
                   (mf : 'opt_mutable)  _  (_loc : FanLoc.t)  ->
                   (`CgVal (_loc, l, mf, mv, t) : 'clsigi )))));
         ([`Skeyword "method";
          `Skeyword "virtual";
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _  _ \n     (_loc : FanLoc.t)  -> (`VirMeth (_loc, l, pf, t) : 'clsigi ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   _  (_loc : FanLoc.t)  ->
                   (`VirMeth (_loc, l, pf, t) : 'clsigi )))));
         ([`Skeyword "method";
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ \n     (_loc : FanLoc.t)  -> (`Method (_loc, l, pf, t) : 'clsigi ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Method (_loc, l, pf, t) : 'clsigi )))));
         ([`Skeyword "constraint";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, t1, t2) : 'clsigi ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, t1, t2) : 'clsigi )))))])));
  (Gram.extend_single (class_structure : 'class_structure Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"cst"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cst\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"cstru\" n s : 'class_structure )\n     | _ -> failwith \"mk_anti _loc ~c:\"cstru\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"cst"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"cstru" n s : 'class_structure )
                    | _ -> failwith "mk_anti _loc ~c:\"cstru\" n s\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"cst"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (cst : 'class_structure)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cst\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (mk_anti _loc ~c:\"cstru\" n s), cst) : 'class_structure )\n     | _ -> failwith \"`Sem (_loc, (mk_anti _loc ~c:\"cstru\" n s), cst)\n\")\n",
             (Gram.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"cst"|"anti"|"list" as n),s) ->
                       (`Sem (_loc, (mk_anti _loc ~c:"cstru" n s), cst) : 
                       'class_structure )
                   | _ ->
                       failwith
                         "`Sem (_loc, (mk_anti _loc ~c:\"cstru\" n s), cst)\n"))));
         ([`Slist1
             (Gram.srules
                [([`Snterm (Gram.obj (cstru : 'cstru Gram.t ));
                  `Skeyword ";"],
                   ("Gram.mk_action (fun _  (cst : 'cstru)  (_loc : FanLoc.t)  -> (cst : 'e__8 ))\n",
                     (Gram.mk_action
                        (fun _  (cst : 'cstru)  (_loc : FanLoc.t)  ->
                           (cst : 'e__8 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__8 list)  (_loc : FanLoc.t)  ->\n     (sem_of_list l : 'class_structure ))\n",
             (Gram.mk_action
                (fun (l : 'e__8 list)  (_loc : FanLoc.t)  ->
                   (sem_of_list l : 'class_structure )))))]));
   Gram.extend_single (cstru : 'cstru Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"cst"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cst\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"cstru\" n s : 'cstru )\n     | _ -> failwith \"mk_anti _loc ~c:\"cstru\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"cst"|"anti"|"list" as n),s) ->
                        (mk_anti _loc ~c:"cstru" n s : 'cstru )
                    | _ -> failwith "mk_anti _loc ~c:\"cstru\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.cstru_tag : 'cstru )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.cstru_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.cstru_tag : 
                       'cstru )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.cstru_tag\n"))));
         ([`Skeyword "inherit";
          `Snterm (Gram.obj (opt_override : 'opt_override Gram.t ));
          `Snterm (Gram.obj (clexp : 'clexp Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'clexp)  (o : 'opt_override)  _  (_loc : FanLoc.t)  ->\n     (`Inherit (_loc, o, ce) : 'cstru ))\n",
             (Gram.mk_action
                (fun (ce : 'clexp)  (o : 'opt_override)  _  (_loc : FanLoc.t)
                    -> (`Inherit (_loc, o, ce) : 'cstru )))));
         ([`Skeyword "inherit";
          `Snterm (Gram.obj (opt_override : 'opt_override Gram.t ));
          `Snterm (Gram.obj (clexp : 'clexp Gram.t ));
          `Skeyword "as";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (ce : 'clexp)  (o : 'opt_override)  _ \n     (_loc : FanLoc.t)  -> (`InheritAs (_loc, o, ce, i) : 'cstru ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (ce : 'clexp)  (o : 'opt_override) 
                   _  (_loc : FanLoc.t)  ->
                   (`InheritAs (_loc, o, ce, i) : 'cstru )))));
         ([`Snterm
             (Gram.obj
                (value_val_opt_override : 'value_val_opt_override Gram.t ));
          `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (cvalue_binding : 'cvalue_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'cvalue_binding)  (lab : 'a_lident)  (mf : 'opt_mutable) \n     (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->\n     (`CrVal (_loc, lab, o, mf, e) : 'cstru ))\n",
             (Gram.mk_action
                (fun (e : 'cvalue_binding)  (lab : 'a_lident) 
                   (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                   (_loc : FanLoc.t)  ->
                   (`CrVal (_loc, lab, o, mf, e) : 'cstru )))));
         ([`Snterm
             (Gram.obj
                (value_val_opt_override : 'value_val_opt_override Gram.t ));
          `Skeyword "virtual";
          `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (mf : 'opt_mutable)  _ \n     (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->\n     (match o with\n      | `OvNil _ -> `CrVvr (_loc, l, mf, t)\n      | _ ->\n          raise (XStream.Error \"override (!) is incompatible with virtual\") : \n     'cstru ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mf : 'opt_mutable)  _ 
                   (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->
                   (match o with
                    | `OvNil _ -> `CrVvr (_loc, l, mf, t)
                    | _ ->
                        raise
                          (XStream.Error
                             "override (!) is incompatible with virtual") : 
                   'cstru )))));
         ([`Snterm
             (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
          `Skeyword "virtual";
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ \n     (o : 'method_opt_override)  (_loc : FanLoc.t)  ->\n     (match o with\n      | `OvNil _ -> `VirMeth (_loc, l, pf, t)\n      | _ ->\n          raise (XStream.Error \"override (!) is incompatible with virtual\") : \n     'cstru ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (o : 'method_opt_override)  (_loc : FanLoc.t)  ->
                   (match o with
                    | `OvNil _ -> `VirMeth (_loc, l, pf, t)
                    | _ ->
                        raise
                          (XStream.Error
                             "override (!) is incompatible with virtual") : 
                   'cstru )))));
         ([`Snterm
             (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'fun_binding)  (t : 'ctyp)  _  (l : 'a_lident) \n     (pf : 'opt_private)  (o : 'method_opt_override)  (_loc : FanLoc.t)  ->\n     (`CrMth (_loc, l, o, pf, e, t) : 'cstru ))\n",
             (Gram.mk_action
                (fun (e : 'fun_binding)  (t : 'ctyp)  _  (l : 'a_lident) 
                   (pf : 'opt_private)  (o : 'method_opt_override) 
                   (_loc : FanLoc.t)  ->
                   (`CrMth (_loc, l, o, pf, e, t) : 'cstru )))));
         ([`Snterm
             (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'fun_binding)  (l : 'a_lident)  (pf : 'opt_private) \n     (o : 'method_opt_override)  (_loc : FanLoc.t)  ->\n     (`CrMthS (_loc, l, o, pf, e) : 'cstru ))\n",
             (Gram.mk_action
                (fun (e : 'fun_binding)  (l : 'a_lident)  (pf : 'opt_private)
                    (o : 'method_opt_override)  (_loc : FanLoc.t)  ->
                   (`CrMthS (_loc, l, o, pf, e) : 'cstru )))));
         ([`Skeyword "constraint";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, t1, t2) : 'cstru ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, t1, t2) : 'cstru )))));
         ([`Skeyword "initializer"; `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (se : 'exp)  _  (_loc : FanLoc.t)  ->\n     (`Initializer (_loc, se) : 'cstru ))\n",
             (Gram.mk_action
                (fun (se : 'exp)  _  (_loc : FanLoc.t)  ->
                   (`Initializer (_loc, se) : 'cstru )))))]));
   Gram.extend_single (cstru_quot : 'cstru_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (cstru : 'cstru Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (x2 : 'cstru_quot)  _  (x1 : 'cstru)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, x1, x2) : 'cstru_quot ))\n",
              (Gram.mk_action
                 (fun (x2 : 'cstru_quot)  _  (x1 : 'cstru)  (_loc : FanLoc.t)
                     -> (`Sem (_loc, x1, x2) : 'cstru_quot )))));
         ([`Snterm (Gram.obj (cstru : 'cstru Gram.t ))],
           ("Gram.mk_action (fun (x : 'cstru)  (_loc : FanLoc.t)  -> (x : 'cstru_quot ))\n",
             (Gram.mk_action
                (fun (x : 'cstru)  (_loc : FanLoc.t)  -> (x : 'cstru_quot )))))])));
  (Gram.extend_single (clexp_quot : 'clexp_quot Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (ce2 : 'clexp_quot)  _  (ce1 : 'clexp_quot)  (_loc : FanLoc.t)  ->\n     (`And (_loc, ce1, ce2) : 'clexp_quot ))\n",
              (Gram.mk_action
                 (fun (ce2 : 'clexp_quot)  _  (ce1 : 'clexp_quot) 
                    (_loc : FanLoc.t)  ->
                    (`And (_loc, ce1, ce2) : 'clexp_quot )))));
         ([`Sself; `Skeyword "="; `Sself],
           ("Gram.mk_action\n  (fun (ce2 : 'clexp_quot)  _  (ce1 : 'clexp_quot)  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, ce1, ce2) : 'clexp_quot ))\n",
             (Gram.mk_action
                (fun (ce2 : 'clexp_quot)  _  (ce1 : 'clexp_quot) 
                   (_loc : FanLoc.t)  ->
                   (`Eq (_loc, ce1, ce2) : 'clexp_quot )))));
         ([`Snterm (Gram.obj (clexp : 'clexp Gram.t ))],
           ("Gram.mk_action (fun (x : 'clexp)  (_loc : FanLoc.t)  -> (x : 'clexp_quot ))\n",
             (Gram.mk_action
                (fun (x : 'clexp)  (_loc : FanLoc.t)  -> (x : 'clexp_quot )))))]));
   Gram.extend_single (class_declaration : 'class_declaration Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration) \n     (_loc : FanLoc.t)  -> (`And (_loc, c1, c2) : 'class_declaration ))\n",
              (Gram.mk_action
                 (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration)
                     (_loc : FanLoc.t)  ->
                    (`And (_loc, c1, c2) : 'class_declaration )))));
         ([`Stoken
             (((function
                | `Ant ((""|"cdcl"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"cdcl\"|\"anti\"|\"list\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cdcl\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"clexp\" n s : 'class_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"clexp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"cdcl"|"anti"|"list" as n),s) ->
                       (mk_anti _loc ~c:"clexp" n s : 'class_declaration )
                   | _ -> failwith "mk_anti _loc ~c:\"clexp\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.clexp_tag : 'class_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.clexp_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.clexp_tag : 
                       'class_declaration )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.clexp_tag\n"))));
         ([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword "[";
          `Snterm
            (Gram.obj (comma_type_parameter : 'comma_type_parameter Gram.t ));
          `Skeyword "]";
          `Snterm (Gram.obj (class_fun_binding : 'class_fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_fun_binding)  _  (x : 'comma_type_parameter)  _ \n     (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, (`ClassCon (_loc, mv, (i :>ident), x)), ce) : 'class_declaration ))\n",
             (Gram.mk_action
                (fun (ce : 'class_fun_binding)  _ 
                   (x : 'comma_type_parameter)  _  (i : 'a_lident) 
                   (mv : 'opt_virtual)  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, (`ClassCon (_loc, mv, (i :>ident), x)), ce) : 
                   'class_declaration )))));
         ([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (class_fun_binding : 'class_fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_fun_binding)  (i : 'a_lident)  (mv : 'opt_virtual) \n     (_loc : FanLoc.t)  ->\n     (`Eq (_loc, (`ClassConS (_loc, mv, (i :>ident))), ce) : 'class_declaration ))\n",
             (Gram.mk_action
                (fun (ce : 'class_fun_binding)  (i : 'a_lident) 
                   (mv : 'opt_virtual)  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, (`ClassConS (_loc, mv, (i :>ident))), ce) : 
                   'class_declaration )))))]));
   Gram.extend_single (class_fun_binding : 'class_fun_binding Gram.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Gram.obj (clexp : 'clexp Gram.t ))],
            ("Gram.mk_action\n  (fun (ce : 'clexp)  _  (_loc : FanLoc.t)  -> (ce : 'class_fun_binding ))\n",
              (Gram.mk_action
                 (fun (ce : 'clexp)  _  (_loc : FanLoc.t)  ->
                    (ce : 'class_fun_binding )))));
         ([`Skeyword ":";
          `Snterm (Gram.obj (cltyp_plus : 'cltyp_plus Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (clexp : 'clexp Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'clexp)  _  (ct : 'cltyp_plus)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, ce, ct) : 'class_fun_binding ))\n",
             (Gram.mk_action
                (fun (ce : 'clexp)  _  (ct : 'cltyp_plus)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, ce, ct) : 'class_fun_binding )))));
         ([`Snterm (Gram.obj (ipat : 'ipat Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (cfb : 'class_fun_binding)  (p : 'ipat)  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, cfb) : 'class_fun_binding ))\n",
             (Gram.mk_action
                (fun (cfb : 'class_fun_binding)  (p : 'ipat) 
                   (_loc : FanLoc.t)  ->
                   (`CeFun (_loc, p, cfb) : 'class_fun_binding )))))]));
   Gram.extend_single (class_fun_def : 'class_fun_def Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (ipat : 'ipat Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (ce : 'class_fun_def)  (p : 'ipat)  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, ce) : 'class_fun_def ))\n",
              (Gram.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  (_loc : FanLoc.t) 
                    -> (`CeFun (_loc, p, ce) : 'class_fun_def )))));
         ([`Skeyword "->"; `Snterm (Gram.obj (clexp : 'clexp Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'clexp)  _  (_loc : FanLoc.t)  -> (ce : 'class_fun_def ))\n",
             (Gram.mk_action
                (fun (ce : 'clexp)  _  (_loc : FanLoc.t)  ->
                   (ce : 'class_fun_def )))))]));
   Gram.extend (clexp : 'clexp Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "fun";
            `Snterm (Gram.obj (ipat : 'ipat Gram.t ));
            `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
             ("Gram.mk_action\n  (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, ce) : 'clexp ))\n",
               (Gram.mk_action
                  (fun (ce : 'class_fun_def)  (p : 'ipat)  _ 
                     (_loc : FanLoc.t)  -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Skeyword "function";
           `Snterm (Gram.obj (ipat : 'ipat Gram.t ));
           `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
            ("Gram.mk_action\n  (fun (ce : 'class_fun_def)  (p : 'ipat)  _  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, ce) : 'clexp ))\n",
              (Gram.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipat)  _ 
                    (_loc : FanLoc.t)  -> (`CeFun (_loc, p, ce) : 'clexp )))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Sself],
            ("Gram.mk_action\n  (fun (ce : 'clexp)  _  (bi : 'binding)  (rf : 'opt_rec)  _ \n     (_loc : FanLoc.t)  -> (`LetIn (_loc, rf, bi, ce) : 'clexp ))\n",
              (Gram.mk_action
                 (fun (ce : 'clexp)  _  (bi : 'binding)  (rf : 'opt_rec)  _ 
                    (_loc : FanLoc.t)  ->
                    (`LetIn (_loc, rf, bi, ce) : 'clexp )))))]);
       ((Some "apply"), (Some `NA),
         [([`Sself; `Snterml ((Gram.obj (exp : 'exp Gram.t )), "label")],
            ("Gram.mk_action\n  (fun (e : 'exp)  (ce : 'clexp)  (_loc : FanLoc.t)  ->\n     (`CeApp (_loc, ce, e) : 'clexp ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  (ce : 'clexp)  (_loc : FanLoc.t)  ->
                    (`CeApp (_loc, ce, e) : 'clexp )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ((""|"cexp"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"cexp\"|\"anti\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cexp\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"clexp\" n s : 'clexp )\n     | _ -> failwith \"mk_anti _loc ~c:\"clexp\" n s\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"cexp"|"anti" as n),s) ->
                        (mk_anti _loc ~c:"clexp" n s : 'clexp )
                    | _ -> failwith "mk_anti _loc ~c:\"clexp\" n s\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.clexp_tag : 'clexp )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.clexp_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x FanDyn.clexp_tag : 
                       'clexp )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x FanDyn.clexp_tag\n"))));
         ([`Snterm
             (Gram.obj
                (class_longident_and_param : 'class_longident_and_param
                                               Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_longident_and_param)  (_loc : FanLoc.t)  ->\n     (ce : 'clexp ))\n",
             (Gram.mk_action
                (fun (ce : 'class_longident_and_param)  (_loc : FanLoc.t)  ->
                   (ce : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ")";
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _  (_loc : FanLoc.t) \n     -> (`ObjPat (_loc, p, cst) : 'clexp ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                   (_loc : FanLoc.t)  -> (`ObjPat (_loc, p, cst) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  _  (p : 'pat)  _  _  (_loc : FanLoc.t)  ->\n     (`ObjPatEnd (_loc, p) : 'clexp ))\n",
             (Gram.mk_action
                (fun _  _  (p : 'pat)  _  _  (_loc : FanLoc.t)  ->
                   (`ObjPatEnd (_loc, p) : 'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")";
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _  (p : 'pat)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 'clexp ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                   (p : 'pat)  _  _  (_loc : FanLoc.t)  ->
                   (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                   'clexp )))));
         ([`Skeyword "object";
          `Skeyword "(";
          `Snterm (Gram.obj (pat : 'pat Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")";
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _  (_loc : FanLoc.t)  ->\n     (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'clexp ))\n",
             (Gram.mk_action
                (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 'clexp )))));
         ([`Skeyword "object";
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  _  (_loc : FanLoc.t)  ->\n     (`Obj (_loc, cst) : 'clexp ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure)  _  (_loc : FanLoc.t)  ->
                   (`Obj (_loc, cst) : 'clexp )))));
         ([`Skeyword "object"; `Skeyword "end"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`ObjEnd _loc : 'clexp ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> (`ObjEnd _loc : 'clexp )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (cltyp : 'cltyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (ct : 'cltyp)  _  (ce : 'clexp)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, ce, ct) : 'clexp ))\n",
             (Gram.mk_action
                (fun _  (ct : 'cltyp)  _  (ce : 'clexp)  _  (_loc : FanLoc.t)
                    -> (`Constraint (_loc, ce, ct) : 'clexp )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (ce : 'clexp)  _  (_loc : FanLoc.t)  -> (ce : 'clexp ))\n",
             (Gram.mk_action
                (fun _  (ce : 'clexp)  _  (_loc : FanLoc.t)  ->
                   (ce : 'clexp )))))])]);
   Gram.extend_single
     (class_longident_and_param : 'class_longident_and_param Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ));
           `Skeyword "[";
           `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
           `Skeyword "]"],
            ("Gram.mk_action\n  (fun _  (t : 'comma_ctyp)  _  (ci : 'class_longident)  (_loc : FanLoc.t) \n     ->\n     (`ClassCon (_loc, (`ViNil _loc), ci, t) : 'class_longident_and_param ))\n",
              (Gram.mk_action
                 (fun _  (t : 'comma_ctyp)  _  (ci : 'class_longident) 
                    (_loc : FanLoc.t)  ->
                    (`ClassCon (_loc, (`ViNil _loc), ci, t) : 'class_longident_and_param )))));
         ([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (ci : 'class_longident)  (_loc : FanLoc.t)  ->\n     (`ClassConS (_loc, (`ViNil _loc), ci) : 'class_longident_and_param ))\n",
             (Gram.mk_action
                (fun (ci : 'class_longident)  (_loc : FanLoc.t)  ->
                   (`ClassConS (_loc, (`ViNil _loc), ci) : 'class_longident_and_param )))))])));
  Gram.extend_single (class_description : 'class_description Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("Gram.mk_action\n  (fun (cd2 : 'class_description)  _  (cd1 : 'class_description) \n     (_loc : FanLoc.t)  -> (`And (_loc, cd1, cd2) : 'class_description ))\n",
             (Gram.mk_action
                (fun (cd2 : 'class_description)  _ 
                   (cd1 : 'class_description)  (_loc : FanLoc.t)  ->
                   (`And (_loc, cd1, cd2) : 'class_description )))));
        ([`Stoken
            (((function
               | `Ant ((""|"typ"|"anti"|"list"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"cltyp\" n s : 'class_description )\n     | _ -> failwith \"mk_anti _loc ~c:\"cltyp\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ"|"anti"|"list" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'class_description )
                  | _ -> failwith "mk_anti _loc ~c:\"cltyp\" n s\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.cltyp_tag : 'class_description )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.cltyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x FanDyn.cltyp_tag : 
                      'class_description )
                  | _ ->
                      failwith
                        "AstQuotation.expand _loc x FanDyn.cltyp_tag\n"))));
        ([`Snterm
            (Gram.obj (class_info_for_cltyp : 'class_info_for_cltyp Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (cltyp_plus : 'cltyp_plus Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'cltyp_plus)  _  (ci : 'class_info_for_cltyp)  (_loc : FanLoc.t)\n      -> (`CtCol (_loc, ci, ct) : 'class_description ))\n",
            (Gram.mk_action
               (fun (ct : 'cltyp_plus)  _  (ci : 'class_info_for_cltyp) 
                  (_loc : FanLoc.t)  ->
                  (`CtCol (_loc, ci, ct) : 'class_description )))))]));
  Gram.extend_single (cltyp_declaration : 'cltyp_declaration Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("Gram.mk_action\n  (fun (cd2 : 'cltyp_declaration)  _  (cd1 : 'cltyp_declaration) \n     (_loc : FanLoc.t)  -> (`And (_loc, cd1, cd2) : 'cltyp_declaration ))\n",
             (Gram.mk_action
                (fun (cd2 : 'cltyp_declaration)  _ 
                   (cd1 : 'cltyp_declaration)  (_loc : FanLoc.t)  ->
                   (`And (_loc, cd1, cd2) : 'cltyp_declaration )))));
        ([`Stoken
            (((function
               | `Ant ((""|"typ"|"anti"|"list"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\"|\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"cltyp\" n s : 'cltyp_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"cltyp\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ"|"anti"|"list" as n),s) ->
                      (mk_anti _loc ~c:"cltyp" n s : 'cltyp_declaration )
                  | _ -> failwith "mk_anti _loc ~c:\"cltyp\" n s\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.cltyp_tag : 'cltyp_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.cltyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x FanDyn.cltyp_tag : 
                      'cltyp_declaration )
                  | _ ->
                      failwith
                        "AstQuotation.expand _loc x FanDyn.cltyp_tag\n"))));
        ([`Snterm
            (Gram.obj (class_info_for_cltyp : 'class_info_for_cltyp Gram.t ));
         `Skeyword "=";
         `Snterm (Gram.obj (cltyp : 'cltyp Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'cltyp)  _  (ci : 'class_info_for_cltyp)  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, ci, ct) : 'cltyp_declaration ))\n",
            (Gram.mk_action
               (fun (ct : 'cltyp)  _  (ci : 'class_info_for_cltyp) 
                  (_loc : FanLoc.t)  ->
                  (`Eq (_loc, ci, ct) : 'cltyp_declaration )))))]));
  Gram.extend_single (class_info_for_cltyp : 'class_info_for_cltyp Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword "[";
          `Snterm
            (Gram.obj (comma_type_parameter : 'comma_type_parameter Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (x : 'comma_type_parameter)  _  (i : 'a_lident) \n     (mv : 'opt_virtual)  (_loc : FanLoc.t)  ->\n     (`ClassCon (_loc, mv, (i :>ident), x) : 'class_info_for_cltyp ))\n",
             (Gram.mk_action
                (fun _  (x : 'comma_type_parameter)  _  (i : 'a_lident) 
                   (mv : 'opt_virtual)  (_loc : FanLoc.t)  ->
                   (`ClassCon (_loc, mv, (i :>ident), x) : 'class_info_for_cltyp )))));
        ([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : FanLoc.t)  ->\n     (`ClassConS (_loc, mv, (i :>ident)) : 'class_info_for_cltyp ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (mv : 'opt_virtual)  (_loc : FanLoc.t) 
                  ->
                  (`ClassConS (_loc, mv, (i :>ident)) : 'class_info_for_cltyp )))))]));
  Gram.extend_single (cltyp_quot : 'cltyp_quot Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("Gram.mk_action\n  (fun (ct2 : 'cltyp_quot)  _  (ct1 : 'cltyp_quot)  (_loc : FanLoc.t)  ->\n     (`And (_loc, ct1, ct2) : 'cltyp_quot ))\n",
             (Gram.mk_action
                (fun (ct2 : 'cltyp_quot)  _  (ct1 : 'cltyp_quot) 
                   (_loc : FanLoc.t)  ->
                   (`And (_loc, ct1, ct2) : 'cltyp_quot )))));
        ([`Sself; `Skeyword "="; `Sself],
          ("Gram.mk_action\n  (fun (ct2 : 'cltyp_quot)  _  (ct1 : 'cltyp_quot)  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, ct1, ct2) : 'cltyp_quot ))\n",
            (Gram.mk_action
               (fun (ct2 : 'cltyp_quot)  _  (ct1 : 'cltyp_quot) 
                  (_loc : FanLoc.t)  -> (`Eq (_loc, ct1, ct2) : 'cltyp_quot )))));
        ([`Sself; `Skeyword ":"; `Sself],
          ("Gram.mk_action\n  (fun (ct2 : 'cltyp_quot)  _  (ct1 : 'cltyp_quot)  (_loc : FanLoc.t)  ->\n     (`CtCol (_loc, ct1, ct2) : 'cltyp_quot ))\n",
            (Gram.mk_action
               (fun (ct2 : 'cltyp_quot)  _  (ct1 : 'cltyp_quot) 
                  (_loc : FanLoc.t)  ->
                  (`CtCol (_loc, ct1, ct2) : 'cltyp_quot )))));
        ([`Stoken
            (((function | `Ant ("virtual",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"virtual\",_)"));
         `Snterm (Gram.obj (ident : 'ident Gram.t ));
         `Skeyword "[";
         `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (t : 'comma_ctyp)  _  (i : 'ident)  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"virtual\" as n),s) ->\n         (let anti = mk_anti _loc ~c:\"cltyp\" n s in\n          `ClassCon (_loc, anti, i, t) : 'cltyp_quot )\n     | _ ->\n         failwith\n           \"let anti = mk_anti _loc ~c:\"cltyp\" n s in `ClassCon (_loc, anti, i, t)\n\")\n",
            (Gram.mk_action
               (fun _  (t : 'comma_ctyp)  _  (i : 'ident) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("virtual" as n),s) ->
                      (let anti = mk_anti _loc ~c:"cltyp" n s in
                       `ClassCon (_loc, anti, i, t) : 'cltyp_quot )
                  | _ ->
                      failwith
                        "let anti = mk_anti _loc ~c:\"cltyp\" n s in `ClassCon (_loc, anti, i, t)\n"))));
        ([`Stoken
            (((function | `Ant ("virtual",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"virtual\",_)"));
         `Snterm (Gram.obj (ident : 'ident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'ident)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"virtual\" as n),s) ->\n         (let anti = mk_anti _loc ~c:\"cltyp\" n s in\n          `ClassConS (_loc, anti, i) : 'cltyp_quot )\n     | _ ->\n         failwith\n           \"let anti = mk_anti _loc ~c:\"cltyp\" n s in `ClassConS (_loc, anti, i)\n\")\n",
            (Gram.mk_action
               (fun (i : 'ident)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("virtual" as n),s) ->
                      (let anti = mk_anti _loc ~c:"cltyp" n s in
                       `ClassConS (_loc, anti, i) : 'cltyp_quot )
                  | _ ->
                      failwith
                        "let anti = mk_anti _loc ~c:\"cltyp\" n s in `ClassConS (_loc, anti, i)\n"))));
        ([`Snterm (Gram.obj (cltyp_plus : 'cltyp_plus Gram.t ))],
          ("Gram.mk_action\n  (fun (x : 'cltyp_plus)  (_loc : FanLoc.t)  -> (x : 'cltyp_quot ))\n",
            (Gram.mk_action
               (fun (x : 'cltyp_plus)  (_loc : FanLoc.t)  ->
                  (x : 'cltyp_quot )))))]));
  Gram.extend_single (cltyp_plus : 'cltyp_plus Gram.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "]";
          `Skeyword "->";
          `Sself],
           ("Gram.mk_action\n  (fun (ct : 'cltyp_plus)  _  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`CtFun (_loc, t, ct) : 'cltyp_plus ))\n",
             (Gram.mk_action
                (fun (ct : 'cltyp_plus)  _  _  (t : 'ctyp)  _ 
                   (_loc : FanLoc.t)  ->
                   (`CtFun (_loc, t, ct) : 'cltyp_plus )))));
        ([`Snterm (Gram.obj (cltyp : 'cltyp Gram.t ))],
          ("Gram.mk_action (fun (ct : 'cltyp)  (_loc : FanLoc.t)  -> (ct : 'cltyp_plus ))\n",
            (Gram.mk_action
               (fun (ct : 'cltyp)  (_loc : FanLoc.t)  -> (ct : 'cltyp_plus )))))]));
  Gram.extend_single (cltyp : 'cltyp Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"ctyp"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"ctyp\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"ctyp\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"cltyp\" n s : 'cltyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"cltyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"ctyp"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"cltyp" n s : 'cltyp )
                   | _ -> failwith "mk_anti _loc ~c:\"cltyp\" n s\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.cltyp_tag : 'cltyp )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.cltyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x FanDyn.cltyp_tag : 
                      'cltyp )
                  | _ ->
                      failwith
                        "AstQuotation.expand _loc x FanDyn.cltyp_tag\n"))));
        ([`Snterm
            (Gram.obj
               (cltyp_longident_and_param : 'cltyp_longident_and_param Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'cltyp_longident_and_param)  (_loc : FanLoc.t)  ->\n     (ct : 'cltyp ))\n",
            (Gram.mk_action
               (fun (ct : 'cltyp_longident_and_param)  (_loc : FanLoc.t)  ->
                  (ct : 'cltyp )))));
        ([`Skeyword "object";
         `Skeyword "(";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
         `Skeyword ")";
         `Snterm (Gram.obj (class_signature : 'class_signature Gram.t ));
         `Skeyword "end"],
          ("Gram.mk_action\n  (fun _  (csg : 'class_signature)  _  (t : 'ctyp)  _  _  (_loc : FanLoc.t) \n     -> (`ObjTy (_loc, t, csg) : 'cltyp ))\n",
            (Gram.mk_action
               (fun _  (csg : 'class_signature)  _  (t : 'ctyp)  _  _ 
                  (_loc : FanLoc.t)  -> (`ObjTy (_loc, t, csg) : 'cltyp )))));
        ([`Skeyword "object";
         `Snterm (Gram.obj (class_signature : 'class_signature Gram.t ));
         `Skeyword "end"],
          ("Gram.mk_action\n  (fun _  (csg : 'class_signature)  _  (_loc : FanLoc.t)  ->\n     (`Obj (_loc, csg) : 'cltyp ))\n",
            (Gram.mk_action
               (fun _  (csg : 'class_signature)  _  (_loc : FanLoc.t)  ->
                  (`Obj (_loc, csg) : 'cltyp )))));
        ([`Skeyword "object";
         `Skeyword "(";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  _  (_loc : FanLoc.t)  ->\n     (`ObjTyEnd (_loc, t) : 'cltyp ))\n",
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  _  (_loc : FanLoc.t)  ->
                  (`ObjTyEnd (_loc, t) : 'cltyp )))));
        ([`Skeyword "object"; `Skeyword "end"],
          ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> (`ObjEnd _loc : 'cltyp ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  -> (`ObjEnd _loc : 'cltyp )))))]));
  Gram.extend_single
    (cltyp_longident_and_param : 'cltyp_longident_and_param Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (cltyp_longident : 'cltyp_longident Gram.t ));
          `Skeyword "[";
          `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (t : 'comma_ctyp)  _  (i : 'cltyp_longident)  (_loc : FanLoc.t)  ->\n     (`ClassCon (_loc, (`ViNil _loc), i, t) : 'cltyp_longident_and_param ))\n",
             (Gram.mk_action
                (fun _  (t : 'comma_ctyp)  _  (i : 'cltyp_longident) 
                   (_loc : FanLoc.t)  ->
                   (`ClassCon (_loc, (`ViNil _loc), i, t) : 'cltyp_longident_and_param )))));
        ([`Snterm (Gram.obj (cltyp_longident : 'cltyp_longident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'cltyp_longident)  (_loc : FanLoc.t)  ->\n     (`ClassConS (_loc, (`ViNil _loc), i) : 'cltyp_longident_and_param ))\n",
            (Gram.mk_action
               (fun (i : 'cltyp_longident)  (_loc : FanLoc.t)  ->
                  (`ClassConS (_loc, (`ViNil _loc), i) : 'cltyp_longident_and_param )))))]))

let apply_ctyp () =
  Gram.extend_single (ctyp_quot : 'ctyp_quot Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "*";
          `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (y : 'star_ctyp)  _  (x : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`Sta (_loc, x, y) : 'ctyp_quot ))\n",
             (Gram.mk_action
                (fun (y : 'star_ctyp)  _  (x : 'ctyp)  (_loc : FanLoc.t)  ->
                   (`Sta (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action (fun (x : 'ctyp)  (_loc : FanLoc.t)  -> (x : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (x : 'ctyp)  (_loc : FanLoc.t)  -> (x : 'ctyp_quot )))))]));
  Gram.extend_single (unquoted_typevars : 'unquoted_typevars Gram.t )
    (None,
      (None, None,
        [([`Sself; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) \n     (_loc : FanLoc.t)  -> (`App (_loc, t1, t2) : 'unquoted_typevars ))\n",
             (Gram.mk_action
                (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) 
                   (_loc : FanLoc.t)  ->
                   (`App (_loc, t1, t2) : 'unquoted_typevars )))));
        ([`Stoken
            (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp\" n s : 'unquoted_typevars )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'unquoted_typevars )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.ctyp_tag : 'unquoted_typevars )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x FanDyn.ctyp_tag : 'unquoted_typevars )
                  | _ ->
                      failwith "AstQuotation.expand _loc x FanDyn.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i :>ctyp) : 'unquoted_typevars ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  ((i :>ctyp) : 'unquoted_typevars )))))]));
  Gram.extend_single (type_parameter : 'type_parameter Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\" as n),s) ->\n         (mk_anti _loc n s : 'type_parameter )\n     | _ -> failwith \"mk_anti _loc n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ"|"anti" as n),s) ->
                       (mk_anti _loc n s : 'type_parameter )
                   | _ -> failwith "mk_anti _loc n s\n"))));
        ([`Skeyword "'"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Normal _loc), i) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'type_parameter )))));
        ([`Skeyword "+";
         `Skeyword "'";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Positive _loc), i) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Positive _loc), i) : 'type_parameter )))));
        ([`Skeyword "-";
         `Skeyword "'";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Negative _loc), i) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Negative _loc), i) : 'type_parameter )))));
        ([`Skeyword "+"; `Skeyword "_"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (`QuoteAny (_loc, (`Positive _loc)) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (`QuoteAny (_loc, (`Positive _loc)) : 'type_parameter )))));
        ([`Skeyword "-"; `Skeyword "_"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (`QuoteAny (_loc, (`Negative _loc)) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (`QuoteAny (_loc, (`Negative _loc)) : 'type_parameter )))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'type_parameter ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'type_parameter )))))]));
  Gram.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Gram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Gram.obj (type_parameters : 'type_parameters Gram.t ));
          `Skeyword ")";
          `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'type_longident)  _  (tpl : 'type_parameters)  _ \n     (_loc : FanLoc.t)  -> (tpl (i :>ctyp) : 'type_longident_and_parameters ))\n",
             (Gram.mk_action
                (fun (i : 'type_longident)  _  (tpl : 'type_parameters)  _ 
                   (_loc : FanLoc.t)  ->
                   (tpl (i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ));
         `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'type_longident)  (tpl : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters ))\n",
            (Gram.mk_action
               (fun (i : 'type_longident)  (tpl : 'type_parameter) 
                  (_loc : FanLoc.t)  ->
                  (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters )))));
        ([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'type_longident)  (_loc : FanLoc.t)  ->\n     ((i :>ctyp) : 'type_longident_and_parameters ))\n",
            (Gram.mk_action
               (fun (i : 'type_longident)  (_loc : FanLoc.t)  ->
                  ((i :>ctyp) : 'type_longident_and_parameters )))));
        ([`Stoken
            (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (mk_anti _loc n s ~c:\"ctyp\" : 'type_longident_and_parameters )\n     | _ -> failwith \"mk_anti _loc n s ~c:\"ctyp\"\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"anti" as n),s) ->
                      (mk_anti _loc n s ~c:"ctyp" : 'type_longident_and_parameters )
                  | _ -> failwith "mk_anti _loc n s ~c:\"ctyp\"\n"))))]));
  Gram.extend_single (type_parameters : 'type_parameters Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'type_parameters)  (t1 : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 'type_parameters ))\n",
             (Gram.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : FanLoc.t)  ->
                   (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 
                   'type_parameters )))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (fun acc  -> `App (_loc, acc, (t :>ctyp)) : 'type_parameters ))\n",
            (Gram.mk_action
               (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (fun acc  -> `App (_loc, acc, (t :>ctyp)) : 'type_parameters )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (fun t  -> t : 'type_parameters ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (fun t  -> t : 'type_parameters )))))]));
  Gram.extend_single (meth_list : 'meth_list Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun ((ml,v) : 'meth_list)  _  (m : 'meth_decl)  (_loc : FanLoc.t)  ->\n     (((`Sem (_loc, m, ml)), v) : 'meth_list ))\n",
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  _  (m : 'meth_decl) 
                   (_loc : FanLoc.t)  ->
                   (((`Sem (_loc, m, ml)), v) : 'meth_list )))));
        ([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
         `Skeyword ";";
         `Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
          ("Gram.mk_action\n  (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl)  (_loc : FanLoc.t)  ->\n     ((m, v) : 'meth_list ))\n",
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl) 
                  (_loc : FanLoc.t)  -> ((m, v) : 'meth_list )))));
        ([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
         `Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
          ("Gram.mk_action\n  (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : FanLoc.t)  ->\n     ((m, v) : 'meth_list ))\n",
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : FanLoc.t) 
                  -> ((m, v) : 'meth_list )))))]));
  Gram.extend_single (meth_decl : 'meth_decl Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) -> (mk_anti _loc ~c:\"ctyp\" n s : 'meth_decl )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'meth_decl )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"ctyp;\" n s : 'meth_decl )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp;\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctyp;" n s : 'meth_decl )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp;\" n s\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`TyCol (_loc, lab, t) : 'meth_decl ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`TyCol (_loc, lab, t) : 'meth_decl )))))]));
  Gram.extend_single (opt_meth_list : 'opt_meth_list Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (meth_list : 'meth_list Gram.t ))],
           ("Gram.mk_action\n  (fun ((ml,v) : 'meth_list)  (_loc : FanLoc.t)  ->\n     (`TyObj (_loc, ml, v) : 'opt_meth_list ))\n",
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  (_loc : FanLoc.t)  ->
                   (`TyObj (_loc, ml, v) : 'opt_meth_list )))));
        ([`Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
          ("Gram.mk_action\n  (fun (v : 'opt_dot_dot)  (_loc : FanLoc.t)  ->\n     (`TyObjEnd (_loc, v) : 'opt_meth_list ))\n",
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  (_loc : FanLoc.t)  ->
                  (`TyObjEnd (_loc, v) : 'opt_meth_list )))))]));
  Gram.extend_single (row_field : 'row_field Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) -> (mk_anti _loc ~c:\"ctyp\" n s : 'row_field )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'row_field )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"ctyp|\" n s : 'row_field )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp|\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctyp|" n s : 'row_field )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp|\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("vrn",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"vrn\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"vrn\" as n),s) ->\n         (`TyVrn (_loc, (mk_anti _loc ~c:\"ctyp\" n s)) : 'row_field )\n     | _ -> failwith \"`TyVrn (_loc, (mk_anti _loc ~c:\"ctyp\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("vrn" as n),s) ->
                      (`TyVrn (_loc, (mk_anti _loc ~c:"ctyp" n s)) : 
                      'row_field )
                  | _ ->
                      failwith
                        "`TyVrn (_loc, (mk_anti _loc ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("vrn",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"vrn\",_)"));
         `Skeyword "of";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"vrn\" as n),s) ->\n         (`TyVrnOf (_loc, (mk_anti _loc ~c:\"ctyp\" n s), t) : 'row_field )\n     | _ -> failwith \"`TyVrnOf (_loc, (mk_anti _loc ~c:\"ctyp\" n s), t)\n\")\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("vrn" as n),s) ->
                      (`TyVrnOf (_loc, (mk_anti _loc ~c:"ctyp" n s), t) : 
                      'row_field )
                  | _ ->
                      failwith
                        "`TyVrnOf (_loc, (mk_anti _loc ~c:\"ctyp\" n s), t)\n"))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'row_field)  _  (t1 : 'row_field)  (_loc : FanLoc.t)  ->\n     (`Bar (_loc, t1, t2) : 'row_field ))\n",
            (Gram.mk_action
               (fun (t2 : 'row_field)  _  (t1 : 'row_field) 
                  (_loc : FanLoc.t)  -> (`Bar (_loc, t1, t2) : 'row_field )))));
        ([`Skeyword "`"; `Snterm (Gram.obj (astr : 'astr Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'astr)  _  (_loc : FanLoc.t)  -> (`TyVrn (_loc, i) : 'row_field ))\n",
            (Gram.mk_action
               (fun (i : 'astr)  _  (_loc : FanLoc.t)  ->
                  (`TyVrn (_loc, i) : 'row_field )))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (astr : 'astr Gram.t ));
         `Skeyword "of";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'astr)  _  (_loc : FanLoc.t)  ->\n     (`TyVrnOf (_loc, i, t) : 'row_field ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'astr)  _  (_loc : FanLoc.t)  ->
                  (`TyVrnOf (_loc, i, t) : 'row_field )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (`Ctyp (_loc, t) : 'row_field ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  ->
                  (`Ctyp (_loc, t) : 'row_field )))))]));
  Gram.extend_single (name_tags : 'name_tags Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) -> (mk_anti _loc ~c:\"ctyp\" n s : 'name_tags )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'name_tags )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Sself; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : FanLoc.t)  ->\n     (`App (_loc, t1, t2) : 'name_tags ))\n",
            (Gram.mk_action
               (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : FanLoc.t) 
                  -> (`App (_loc, t1, t2) : 'name_tags )))));
        ([`Skeyword "`"; `Snterm (Gram.obj (astr : 'astr Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'astr)  _  (_loc : FanLoc.t)  -> (`TyVrn (_loc, i) : 'name_tags ))\n",
            (Gram.mk_action
               (fun (i : 'astr)  _  (_loc : FanLoc.t)  ->
                  (`TyVrn (_loc, i) : 'name_tags )))))]));
  Gram.extend_single (type_declaration : 'type_declaration Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp\" n s : 'type_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ"|"anti" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'type_declaration )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"ctypand\" n s : 'type_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctypand\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctypand" n s : 'type_declaration )
                  | _ -> failwith "mk_anti _loc ~c:\"ctypand\" n s\n"))));
        ([`Sself; `Skeyword "and"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) \n     (_loc : FanLoc.t)  -> (`And (_loc, t1, t2) : 'type_declaration ))\n",
            (Gram.mk_action
               (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) 
                  (_loc : FanLoc.t)  ->
                  (`And (_loc, t1, t2) : 'type_declaration )))));
        ([`Snterm
            (Gram.obj
               (type_ident_and_parameters : 'type_ident_and_parameters Gram.t ));
         `Skeyword "=";
         `Snterm (Gram.obj (type_info : 'type_info Gram.t ));
         `Slist0 (`Snterm (Gram.obj (constrain : 'constrain Gram.t )))],
          ("Gram.mk_action\n  (fun (cl : 'constrain list)  (tk : 'type_info)  _ \n     ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t)  ->\n     (`TyDcl\n        (_loc, n, tpl, tk,\n          (match cl with\n           | [] -> `None _loc\n           | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration ))\n",
            (Gram.mk_action
               (fun (cl : 'constrain list)  (tk : 'type_info)  _ 
                  ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t) 
                  ->
                  (`TyDcl
                     (_loc, n, tpl, tk,
                       (match cl with
                        | [] -> `None _loc
                        | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))));
        ([`Snterm
            (Gram.obj
               (type_ident_and_parameters : 'type_ident_and_parameters Gram.t ));
         `Slist0 (`Snterm (Gram.obj (constrain : 'constrain Gram.t )))],
          ("Gram.mk_action\n  (fun (cl : 'constrain list)  ((n,tpl) : 'type_ident_and_parameters) \n     (_loc : FanLoc.t)  ->\n     (`TyAbstr\n        (_loc, n, tpl,\n          (match cl with\n           | [] -> `None _loc\n           | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration ))\n",
            (Gram.mk_action
               (fun (cl : 'constrain list) 
                  ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t) 
                  ->
                  (`TyAbstr
                     (_loc, n, tpl,
                       (match cl with
                        | [] -> `None _loc
                        | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))))]));
  Gram.extend_single (type_info : 'type_info Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (type_repr : 'type_repr Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'type_repr)  (_loc : FanLoc.t)  ->\n     (`TyRepr (_loc, (`PrNil _loc), t2) : 'type_info ))\n",
             (Gram.mk_action
                (fun (t2 : 'type_repr)  (_loc : FanLoc.t)  ->
                   (`TyRepr (_loc, (`PrNil _loc), t2) : 'type_info )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
         `Skeyword "=";
         `Snterm (Gram.obj (type_repr : 'type_repr Gram.t ))],
          ("Gram.mk_action\n  (fun (t2 : 'type_repr)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`TyMan (_loc, t1, (`PrNil _loc), t2) : 'type_info ))\n",
            (Gram.mk_action
               (fun (t2 : 'type_repr)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                  (`TyMan (_loc, t1, (`PrNil _loc), t2) : 'type_info )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`TyEq (_loc, (`PrNil _loc), t1) : 'type_info ))\n",
            (Gram.mk_action
               (fun (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                  (`TyEq (_loc, (`PrNil _loc), t1) : 'type_info )))));
        ([`Skeyword "private"; `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`TyEq (_loc, (`Private _loc), t1) : 'type_info ))\n",
            (Gram.mk_action
               (fun (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                  (`TyEq (_loc, (`Private _loc), t1) : 'type_info )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
         `Skeyword "=";
         `Skeyword "private";
         `Snterm (Gram.obj (type_repr : 'type_repr Gram.t ))],
          ("Gram.mk_action\n  (fun (t2 : 'type_repr)  _  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`TyMan (_loc, t1, (`Private _loc), t2) : 'type_info ))\n",
            (Gram.mk_action
               (fun (t2 : 'type_repr)  _  _  (t1 : 'ctyp)  (_loc : FanLoc.t) 
                  -> (`TyMan (_loc, t1, (`Private _loc), t2) : 'type_info )))));
        ([`Skeyword "private";
         `Snterm (Gram.obj (type_repr : 'type_repr Gram.t ))],
          ("Gram.mk_action\n  (fun (t2 : 'type_repr)  _  (_loc : FanLoc.t)  ->\n     (`TyRepr (_loc, (`Private _loc), t2) : 'type_info ))\n",
            (Gram.mk_action
               (fun (t2 : 'type_repr)  _  (_loc : FanLoc.t)  ->
                  (`TyRepr (_loc, (`Private _loc), t2) : 'type_info )))))]));
  Gram.extend_single (type_repr : 'type_repr Gram.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Snterm
            (Gram.obj
               (constructor_declarations : 'constructor_declarations Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (t : 'constructor_declarations)  _  (_loc : FanLoc.t)  ->\n     (`Sum (_loc, t) : 'type_repr ))\n",
             (Gram.mk_action
                (fun _  (t : 'constructor_declarations)  _  (_loc : FanLoc.t)
                    -> (`Sum (_loc, t) : 'type_repr )))));
        ([`Skeyword "{";
         `Snterm
           (Gram.obj
              (label_declaration_list : 'label_declaration_list Gram.t ));
         `Skeyword "}"],
          ("Gram.mk_action\n  (fun _  (t : 'label_declaration_list)  _  (_loc : FanLoc.t)  ->\n     (`Record (_loc, t) : 'type_repr ))\n",
            (Gram.mk_action
               (fun _  (t : 'label_declaration_list)  _  (_loc : FanLoc.t) 
                  -> (`Record (_loc, t) : 'type_repr )))))]));
  Gram.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Gram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Slist1sep
            ((`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))),
              (`Skeyword ","));
          `Skeyword ")";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (tpl : 'type_parameter list)  _  (_loc : FanLoc.t)\n      ->\n     ((i, (`Some (_loc, (com_of_list (tpl :>decl_params list))))) : 'type_ident_and_parameters ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (tpl : 'type_parameter list)  _ 
                   (_loc : FanLoc.t)  ->
                   ((i,
                      (`Some (_loc, (com_of_list (tpl :>decl_params list))))) : 
                   'type_ident_and_parameters )))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ));
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (t : 'type_parameter)  (_loc : FanLoc.t)  ->\n     ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (t : 'type_parameter)  (_loc : FanLoc.t)
                   ->
                  ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters )))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i, (`None _loc)) : 'type_ident_and_parameters ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  ((i, (`None _loc)) : 'type_ident_and_parameters )))))]));
  Gram.extend_single (constrain : 'constrain Gram.t )
    (None,
      (None, None,
        [([`Skeyword "constraint";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, t1, t2) : 'constrain ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, t1, t2) : 'constrain )))))]));
  Gram.extend_single (typevars : 'typevars Gram.t )
    (None,
      (None, None,
        [([`Sself; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : FanLoc.t)  ->\n     (`App (_loc, t1, t2) : 'typevars ))\n",
             (Gram.mk_action
                (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : FanLoc.t) 
                   -> (`App (_loc, t1, t2) : 'typevars )))));
        ([`Stoken
            (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) -> (mk_anti _loc ~c:\"ctyp\" n s : 'typevars )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'typevars )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"forall\" n s : 'typevars )\n     | _ -> failwith \"mk_anti _loc ~c:\"forall\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"forall" n s : 'typevars )
                  | _ -> failwith "mk_anti _loc ~c:\"forall\" n s\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x FanDyn.ctyp_tag : 'typevars )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x FanDyn.ctyp_tag : 'typevars )
                  | _ ->
                      failwith "AstQuotation.expand _loc x FanDyn.ctyp_tag\n"))));
        ([`Skeyword "'"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Normal _loc), i) : 'typevars ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Normal _loc), i) : 'typevars )))))]));
  Gram.extend (ctyp : 'ctyp Gram.t )
    (None,
      [((Some "alias"), (Some `LA),
         [([`Sself;
           `Skeyword "as";
           `Skeyword "'";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'a_lident)  _  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, t1, i) : 'ctyp ))\n",
              (Gram.mk_action
                 (fun (i : 'a_lident)  _  _  (t1 : 'ctyp)  (_loc : FanLoc.t) 
                    -> (`Alias (_loc, t1, i) : 'ctyp )))))]);
      ((Some "forall"), (Some `LA),
        [([`Skeyword "!";
          `Snterm (Gram.obj (typevars : 'typevars Gram.t ));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _  (_loc : FanLoc.t)  ->\n     (`TyPol (_loc, t1, t2) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _  (_loc : FanLoc.t) 
                   -> (`TyPol (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "arrow"), (Some `RA),
        [([`Sself; `Skeyword "->"; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`Arrow (_loc, t1, t2) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (`Arrow (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "label"), (Some `NA),
        [([`Skeyword "~";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, t) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`Label (_loc, i, t) : 'ctyp )))));
        ([`Stoken
            (((function | `LABEL _ -> true | _ -> false)),
              (`Normal, "`LABEL _"));
         `Skeyword ":";
         `Sself],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL s -> (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, s)), t)\n\")\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LABEL s -> (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ -> failwith "`Label (_loc, (`Lid (_loc, s)), t)\n"))));
        ([`Stoken
            (((function | `OPTLABEL _ -> true | _ -> false)),
              (`Normal, "`OPTLABEL _"));
         `Sself],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL s -> (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp )\n     | _ -> failwith \"`OptLabl (_loc, (`Lid (_loc, s)), t)\n\")\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `OPTLABEL s ->
                      (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ -> failwith "`OptLabl (_loc, (`Lid (_loc, s)), t)\n"))));
        ([`Skeyword "?";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Sself],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, i, t) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`OptLabl (_loc, i, t) : 'ctyp )))))]);
      ((Some "apply"), (Some `LA),
        [([`Sself; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (let t = `App (_loc, t1, t2) in\n      try (ident_of_ctyp t :>ctyp) with | Invalid_argument _ -> t : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (let t = `App (_loc, t1, t2) in
                    try (ident_of_ctyp t :>ctyp)
                    with | Invalid_argument _ -> t : 'ctyp )))))]);
      ((Some "."), (Some `LA),
        [([`Sself; `Skeyword "."; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (try `Dot (_loc, (ident_of_ctyp t1 : ident ), (ident_of_ctyp t2))\n      with | Invalid_argument s -> raise (XStream.Error s) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (try
                      `Dot
                        (_loc, (ident_of_ctyp t1 : ident ),
                          (ident_of_ctyp t2))
                    with | Invalid_argument s -> raise (XStream.Error s) : 
                   'ctyp )))))]);
      ((Some "simple"), None,
        [([`Skeyword "'"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Normal _loc), i) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Quote (_loc, (`Normal _loc), i) : 'ctyp )))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ctyp )))));
        ([`Stoken
            (((function
               | `Ant ((""|"typ"|"anti"|"par"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"|\"par\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\"|\"par\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp\" n s : 'ctyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ"|"anti"|"par" as n),s) ->
                      (mk_anti _loc ~c:"ctyp" n s : 'ctyp )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("id",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"id\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"id\" as n),s) -> (mk_anti _loc ~c:\"ident\" n s : 'ctyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"ident\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("id" as n),s) ->
                      (mk_anti _loc ~c:"ident" n s : 'ctyp )
                  | _ -> failwith "mk_anti _loc ~c:\"ident\" n s\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x FanDyn.ctyp_tag : 'ctyp )\n     | _ -> failwith \"AstQuotation.expand _loc x FanDyn.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x FanDyn.ctyp_tag : 'ctyp )
                  | _ ->
                      failwith "AstQuotation.expand _loc x FanDyn.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  -> ((i :>ctyp) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  ((i :>ctyp) : 'ctyp )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_uident)  (_loc : FanLoc.t)  -> ((i :>ctyp) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (i : 'a_uident)  (_loc : FanLoc.t)  ->
                  ((i :>ctyp) : 'ctyp )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword "*";
         `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _ 
                  (_loc : FanLoc.t)  ->
                  (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'ctyp )))));
        ([`Skeyword "[=";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->\n     (`PolyEq (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->
                  (`PolyEq (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[>";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->\n     (`PolySup (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->
                  (`PolySup (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->\n     (`PolyInf (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->
                  (`PolyInf (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword ">";
         `Snterm (Gram.obj (name_tags : 'name_tags Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->\n     (`PolyInfSup (_loc, rfl, ntl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                  (_loc : FanLoc.t)  ->
                  (`PolyInfSup (_loc, rfl, ntl) : 'ctyp )))));
        ([`Skeyword "#";
         `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->\n     (`ClassPath (_loc, i) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                  (`ClassPath (_loc, i) : 'ctyp )))));
        ([`Skeyword "<";
         `Snterm (Gram.obj (opt_meth_list : 'opt_meth_list Gram.t ));
         `Skeyword ">"],
          ("Gram.mk_action\n  (fun _  (t : 'opt_meth_list)  _  (_loc : FanLoc.t)  -> (t : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (t : 'opt_meth_list)  _  (_loc : FanLoc.t)  ->
                  (t : 'ctyp )))));
        ([`Skeyword "(";
         `Skeyword "module";
         `Snterm (Gram.obj (mtyp : 'mtyp Gram.t ));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (p : 'mtyp)  _  _  (_loc : FanLoc.t)  ->\n     (`Package (_loc, p) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (p : 'mtyp)  _  _  (_loc : FanLoc.t)  ->
                  (`Package (_loc, p) : 'ctyp )))))])]);
  Gram.extend_single (star_ctyp : 'star_ctyp Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) -> (mk_anti _loc ~c:\"ctyp\" n s : 'star_ctyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'star_ctyp )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) -> (mk_anti _loc ~c:\"ctyp*\" n s : 'star_ctyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp*\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctyp*" n s : 'star_ctyp )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp*\" n s\n"))));
        ([`Sself; `Skeyword "*"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp)  (_loc : FanLoc.t)  ->\n     (`Sta (_loc, t1, t2) : 'star_ctyp ))\n",
            (Gram.mk_action
               (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp) 
                  (_loc : FanLoc.t)  -> (`Sta (_loc, t1, t2) : 'star_ctyp )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'star_ctyp ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'star_ctyp )))))]));
  Gram.extend_single
    (constructor_declarations : 'constructor_declarations Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp\" n s : 'constructor_declarations )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'constructor_declarations )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp|\" n s : 'constructor_declarations )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp|\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctyp|" n s : 'constructor_declarations )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp|\" n s\n"))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'constructor_declarations)  _  (t1 : 'constructor_declarations) \n     (_loc : FanLoc.t)  -> (`Bar (_loc, t1, t2) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (t2 : 'constructor_declarations)  _ 
                  (t1 : 'constructor_declarations)  (_loc : FanLoc.t)  ->
                  (`Bar (_loc, t1, t2) : 'constructor_declarations )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
         `Skeyword "of";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'constructor_arg_list)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Of (_loc, s, t) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : FanLoc.t)  ->
                  (`Of (_loc, s, t) : 'constructor_declarations )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (let (tl,rt) = FanOps.to_generalized t in\n      `TyCol\n        (_loc, s,\n          (match tl with\n           | [] -> rt\n           | _ -> `Arrow (_loc, (sta_of_list tl), rt))) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->
                  (let (tl,rt) = FanOps.to_generalized t in
                   `TyCol
                     (_loc, s,
                       (match tl with
                        | [] -> rt
                        | _ -> `Arrow (_loc, (sta_of_list tl), rt))) : 
                  'constructor_declarations )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
          ("Gram.mk_action\n  (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     ((s :>or_ctyp) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->
                  ((s :>or_ctyp) : 'constructor_declarations )))))]));
  Gram.extend_single
    (constructor_declaration : 'constructor_declaration Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp\" n s : 'constructor_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'constructor_declaration )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
         `Skeyword "of";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'constructor_arg_list)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Of (_loc, (s :>vid), t) : 'constructor_declaration ))\n",
            (Gram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : FanLoc.t)  ->
                  (`Of (_loc, (s :>vid), t) : 'constructor_declaration )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
          ("Gram.mk_action\n  (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     ((s :>of_ctyp) : 'constructor_declaration ))\n",
            (Gram.mk_action
               (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->
                  ((s :>of_ctyp) : 'constructor_declaration )))))]));
  Gram.extend_single (constructor_arg_list : 'constructor_arg_list Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp*\" n s : 'constructor_arg_list )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp*\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (mk_anti _loc ~c:"ctyp*" n s : 'constructor_arg_list )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp*\" n s\n"))));
        ([`Sself; `Skeyword "*"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'constructor_arg_list)  _  (t1 : 'constructor_arg_list) \n     (_loc : FanLoc.t)  -> (`Sta (_loc, t1, t2) : 'constructor_arg_list ))\n",
            (Gram.mk_action
               (fun (t2 : 'constructor_arg_list)  _ 
                  (t1 : 'constructor_arg_list)  (_loc : FanLoc.t)  ->
                  (`Sta (_loc, t1, t2) : 'constructor_arg_list )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'constructor_arg_list ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  ->
                  (t : 'constructor_arg_list )))))]));
  Gram.extend_single
    (label_declaration_list : 'label_declaration_list Gram.t )
    (None,
      (None, None,
        [([`Snterm
             (Gram.obj (label_declaration : 'label_declaration Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'label_declaration_list)  _  (t1 : 'label_declaration) \n     (_loc : FanLoc.t)  -> (`Sem (_loc, t1, t2) : 'label_declaration_list ))\n",
             (Gram.mk_action
                (fun (t2 : 'label_declaration_list)  _ 
                   (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, t1, t2) : 'label_declaration_list )))));
        ([`Snterm (Gram.obj (label_declaration : 'label_declaration Gram.t ));
         `Skeyword ";"],
          ("Gram.mk_action\n  (fun _  (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->\n     (t1 : 'label_declaration_list ))\n",
            (Gram.mk_action
               (fun _  (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                  (t1 : 'label_declaration_list )))));
        ([`Snterm (Gram.obj (label_declaration : 'label_declaration Gram.t ))],
          ("Gram.mk_action\n  (fun (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->\n     (t1 : 'label_declaration_list ))\n",
            (Gram.mk_action
               (fun (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                  (t1 : 'label_declaration_list )))))]));
  Gram.extend_single (label_declaration : 'label_declaration Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp\" n s : 'label_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp\" n s\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (mk_anti _loc ~c:"ctyp" n s : 'label_declaration )
                   | _ -> failwith "mk_anti _loc ~c:\"ctyp\" n s\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp;\" n s : 'label_declaration )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp;\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctyp;" n s : 'label_declaration )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp;\" n s\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`TyCol (_loc, s, t) : 'label_declaration ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`TyCol (_loc, s, t) : 'label_declaration )))));
        ([`Skeyword "mutable";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (s : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`TyColMut (_loc, s, t) : 'label_declaration ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`TyColMut (_loc, s, t) : 'label_declaration )))))]));
  Gram.extend_single (comma_type_parameter : 'comma_type_parameter Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'comma_type_parameter)  _  (t1 : 'comma_type_parameter) \n     (_loc : FanLoc.t)  -> (`Com (_loc, t1, t2) : 'comma_type_parameter ))\n",
             (Gram.mk_action
                (fun (t2 : 'comma_type_parameter)  _ 
                   (t1 : 'comma_type_parameter)  (_loc : FanLoc.t)  ->
                   (`Com (_loc, t1, t2) : 'comma_type_parameter )))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp,\" n s : 'comma_type_parameter )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp,\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (mk_anti _loc ~c:"ctyp," n s : 'comma_type_parameter )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp,\" n s\n"))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter ))\n",
            (Gram.mk_action
               (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter )))))]));
  Gram.extend_single (comma_ctyp : 'comma_ctyp Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, t1, t2) : 'comma_ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : FanLoc.t)  -> (`Com (_loc, t1, t2) : 'comma_ctyp )))));
        ([`Stoken
            (((function | `Ant (("list"|""),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"list\"|\"\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\"|\"\" as n),s) ->\n         (mk_anti _loc ~c:\"ctyp,\" n s : 'comma_ctyp )\n     | _ -> failwith \"mk_anti _loc ~c:\"ctyp,\" n s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list"|"" as n),s) ->
                      (mk_anti _loc ~c:"ctyp," n s : 'comma_ctyp )
                  | _ -> failwith "mk_anti _loc ~c:\"ctyp,\" n s\n"))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (`Ctyp (_loc, t) : 'comma_ctyp ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  ->
                  (`Ctyp (_loc, t) : 'comma_ctyp )))))]))

let _ =
  AstParsers.register_parser ("revise", (fun ()  -> apply (); apply_ctyp ()))