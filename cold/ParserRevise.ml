open Ast
open PreCast.Syntax
open Lib
open LibUtil
open FanUtil
open GramLib
let help_sequences () =
  Printf.eprintf
    "New syntax:\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\n    while e do e1; e2; ... ; en done\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\nOld syntax (still supported):\n    begin e1; e2; ... ; en end\n    while e begin e1; e2; ... ; en end\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\nVery old (no more supported) syntax:\n    do e1; e2; ... ; en-1; return en\n    while e do e1; e2; ... ; en; done\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\n";
  flush stderr;
  exit 1
let pos_exprs = Gram.mk "pos_exprs"
let apply () =
  Options.add
    ("-help_seq", (FanArg.Unit help_sequences),
      "Print explanations about new sequences and exit.");
  (Gram.clear amp_ctyp;
   Gram.clear and_ctyp;
   Gram.clear match_case;
   Gram.clear match_case0;
   Gram.clear match_case_quot;
   Gram.clear binding;
   Gram.clear binding_quot;
   Gram.clear rec_binding_quot;
   Gram.clear class_declaration;
   Gram.clear class_description;
   Gram.clear class_expr;
   Gram.clear class_expr_quot;
   Gram.clear class_fun_binding;
   Gram.clear class_fun_def;
   Gram.clear class_info_for_class_expr;
   Gram.clear class_info_for_class_type;
   Gram.clear class_longident;
   Gram.clear class_longident_and_param;
   Gram.clear class_name_and_param;
   Gram.clear class_sig_item;
   Gram.clear class_sig_item_quot;
   Gram.clear class_signature;
   Gram.clear class_str_item;
   Gram.clear class_str_item_quot;
   Gram.clear class_structure;
   Gram.clear class_type;
   Gram.clear class_type_declaration;
   Gram.clear class_type_longident;
   Gram.clear class_type_longident_and_param;
   Gram.clear class_type_plus;
   Gram.clear class_type_quot;
   Gram.clear comma_ctyp;
   Gram.clear comma_expr;
   Gram.clear comma_ipatt;
   Gram.clear comma_patt;
   Gram.clear comma_type_parameter;
   Gram.clear constrain;
   Gram.clear constructor_arg_list;
   Gram.clear constructor_declaration;
   Gram.clear constructor_declarations;
   Gram.clear ctyp;
   Gram.clear ctyp_quot;
   Gram.clear cvalue_binding;
   Gram.clear direction_flag;
   Gram.clear dummy;
   Gram.clear eq_expr;
   Gram.clear expr;
   Gram.clear expr_eoi;
   Gram.clear expr_quot;
   Gram.clear field_expr;
   Gram.clear field_expr_list;
   Gram.clear fun_binding;
   Gram.clear fun_def;
   Gram.clear ident;
   Gram.clear ident_quot;
   Gram.clear implem;
   Gram.clear interf;
   Gram.clear ipatt;
   Gram.clear ipatt_tcon;
   Gram.clear patt_tcon;
   Gram.clear label_declaration;
   Gram.clear label_declaration_list;
   Gram.clear label_expr_list;
   Gram.clear label_expr;
   Gram.clear label_longident;
   Gram.clear label_patt;
   Gram.clear label_patt_list;
   Gram.clear let_binding;
   Gram.clear meth_list;
   Gram.clear meth_decl;
   Gram.clear module_binding;
   Gram.clear module_binding0;
   Gram.clear module_binding_quot;
   Gram.clear module_declaration;
   Gram.clear module_expr;
   Gram.clear module_expr_quot;
   Gram.clear module_longident;
   Gram.clear module_longident_with_app;
   Gram.clear module_rec_declaration;
   Gram.clear module_type;
   Gram.clear module_type_quot;
   Gram.clear more_ctyp;
   Gram.clear name_tags;
   Gram.clear opt_as_lident;
   Gram.clear opt_class_self_patt;
   Gram.clear opt_class_self_type;
   Gram.clear opt_comma_ctyp;
   Gram.clear opt_dot_dot;
   Gram.clear opt_expr;
   Gram.clear opt_meth_list;
   Gram.clear opt_mutable;
   Gram.clear opt_polyt;
   Gram.clear opt_private;
   Gram.clear opt_rec;
   Gram.clear opt_virtual;
   Gram.clear patt;
   Gram.clear patt_as_patt_opt;
   Gram.clear patt_eoi;
   Gram.clear patt_quot;
   Gram.clear row_field;
   Gram.clear sem_expr;
   Gram.clear sem_expr_for_list;
   Gram.clear sem_patt;
   Gram.clear sem_patt_for_list;
   Gram.clear semi;
   Gram.clear sequence;
   Gram.clear sig_item;
   Gram.clear sig_item_quot;
   Gram.clear sig_items;
   Gram.clear star_ctyp;
   Gram.clear str_item;
   Gram.clear str_item_quot;
   Gram.clear str_items;
   Gram.clear top_phrase;
   Gram.clear type_declaration;
   Gram.clear type_ident_and_parameters;
   Gram.clear type_longident;
   Gram.clear type_longident_and_parameters;
   Gram.clear type_parameter;
   Gram.clear type_parameters;
   Gram.clear typevars;
   Gram.clear val_longident;
   Gram.clear with_constr;
   Gram.clear with_constr_quot;
   Gram.clear lang);
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
  Gram.setup_parser sem_expr
    (let symb1 = Gram.parse_origin_tokens expr in
     let symb (__strm : _ XStream.t) =
       match XStream.peek __strm with
       | Some (`Ant (("list" as n),s),_loc) ->
           (XStream.junk __strm; `Ant (_loc, (mk_anti ~c:"expr;" n s)))
       | _ -> symb1 __strm in
     let rec kont al (__strm : _ XStream.t) =
       match XStream.peek __strm with
       | Some (`KEYWORD ";",_) ->
           (XStream.junk __strm;
            (let a =
               try symb __strm
               with | XStream.Failure  -> raise (XStream.Error "") in
             let s = __strm in
             let _loc = FanLoc.merge (FanAst.loc_of al) (FanAst.loc_of a) in
             kont (`Sem (_loc, al, a)) s))
       | _ -> al in
     fun (__strm : _ XStream.t)  -> let a = symb __strm in kont a __strm);
  (Gram.extend_single (module_expr_quot : 'module_expr_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'module_expr)  (_loc : FanLoc.t)  -> (x : 'module_expr_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'module_expr)  (_loc : FanLoc.t)  ->
                    (x : 'module_expr_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'module_expr_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'module_expr_quot )))))]));
   Gram.extend (module_binding0 : 'module_binding0 Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword ")";
            `Sself],
             ("Gram.mk_action\n  (fun (mb : 'module_binding0)  _  (mt : 'module_type)  _  (m : 'a_uident)  _\n      (_loc : FanLoc.t)  -> (`Functor (_loc, m, mt, mb) : 'module_binding0 ))\n",
               (Gram.mk_action
                  (fun (mb : 'module_binding0)  _  (mt : 'module_type)  _ 
                     (m : 'a_uident)  _  (_loc : FanLoc.t)  ->
                     (`Functor (_loc, m, mt, mb) : 'module_binding0 )))));
          ([`Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (me : 'module_expr)  _  (mt : 'module_type)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, me, mt) : 'module_binding0 ))\n",
              (Gram.mk_action
                 (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                    (_loc : FanLoc.t)  ->
                    (`Constraint (_loc, me, mt) : 'module_binding0 )))));
          ([`Skeyword "=";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  -> (me : 'module_binding0 ))\n",
              (Gram.mk_action
                 (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                    (me : 'module_binding0 )))))])]);
   Gram.extend (module_expr : 'module_expr Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             ("Gram.mk_action\n  (fun (me : 'module_expr)  _  _  (t : 'module_type)  _  (i : 'a_uident)  _ \n     _  (_loc : FanLoc.t)  -> (`Functor (_loc, i, t, me) : 'module_expr ))\n",
               (Gram.mk_action
                  (fun (me : 'module_expr)  _  _  (t : 'module_type)  _ 
                     (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                     (`Functor (_loc, i, t, me) : 'module_expr )))));
          ([`Skeyword "struct";
           `Snterm (Gram.obj (str_items : 'str_items Gram.t ));
           `Skeyword "end"],
            ("Gram.mk_action\n  (fun _  (st : 'str_items)  _  (_loc : FanLoc.t)  ->\n     (`Struct (_loc, st) : 'module_expr ))\n",
              (Gram.mk_action
                 (fun _  (st : 'str_items)  _  (_loc : FanLoc.t)  ->
                    (`Struct (_loc, st) : 'module_expr )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("Gram.mk_action\n  (fun (me2 : 'module_expr)  (me1 : 'module_expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, me1, me2) : 'module_expr ))\n",
              (Gram.mk_action
                 (fun (me2 : 'module_expr)  (me1 : 'module_expr) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, me1, me2) : 'module_expr )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"mexp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"mexp\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"module_expr\" n s)) : 'module_expr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"module_expr\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"mexp"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"module_expr" n s)) : 
                        'module_expr )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"module_expr\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.module_expr_tag : 'module_expr )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.module_expr_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.module_expr_tag : 
                       'module_expr )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.module_expr_tag\n"))));
         ([`Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, i) : 'module_expr ))\n",
             (Gram.mk_action
                (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                   (`Id (_loc, i) : 'module_expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (mt : 'module_type)  _  (me : 'module_expr)  _  (_loc : FanLoc.t) \n     -> (`Constraint (_loc, me, mt) : 'module_expr ))\n",
             (Gram.mk_action
                (fun _  (mt : 'module_type)  _  (me : 'module_expr)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, me, mt) : 'module_expr )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (me : 'module_expr)  _  (_loc : FanLoc.t)  -> (me : 'module_expr ))\n",
             (Gram.mk_action
                (fun _  (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                   (me : 'module_expr )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  _  (_loc : FanLoc.t)  ->\n     (`PackageModule (_loc, e) : 'module_expr ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  _  (_loc : FanLoc.t)  ->
                   (`PackageModule (_loc, e) : 'module_expr )))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'module_type)  _  (e : 'expr)  _  _  (_loc : FanLoc.t)  ->\n     (`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : \n     'module_expr ))\n",
             (Gram.mk_action
                (fun _  (p : 'module_type)  _  (e : 'expr)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`PackageModule
                      (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                   'module_expr )))))])]));
  (Gram.extend_single (module_binding_quot : 'module_binding_quot Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'module_binding_quot)  _  (b1 : 'module_binding_quot) \n     (_loc : FanLoc.t)  -> (`And (_loc, b1, b2) : 'module_binding_quot ))\n",
              (Gram.mk_action
                 (fun (b2 : 'module_binding_quot)  _ 
                    (b1 : 'module_binding_quot)  (_loc : FanLoc.t)  ->
                    (`And (_loc, b1, b2) : 'module_binding_quot )))));
         ([`Stoken
             (((function
                | `Ant (("module_binding"|"anti"|""),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"module_binding\"|\"anti\"|\"\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"module_binding\"|\"anti\"|\"\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"module_binding\" n s)) : 'module_binding_quot )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"module_binding\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("module_binding"|"anti"|"" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_binding_quot )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"module_binding\" n s))\n"))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'module_type)  _  (m : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, m, mt) : 'module_binding_quot ))\n",
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (m : 'a_uident) 
                   (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, m, mt) : 'module_binding_quot )))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (me : 'module_expr)  _  (mt : 'module_type)  _  (m : 'a_uident) \n     (_loc : FanLoc.t)  ->\n     (`ModuleBind (_loc, m, mt, me) : 'module_binding_quot ))\n",
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                   (m : 'a_uident)  (_loc : FanLoc.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'module_binding_quot )))));
         ([],
           ("Gram.mk_action\n  (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'module_binding_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  ->
                   (`Nil _loc : 'module_binding_quot )))))]));
   Gram.extend_single (module_binding : 'module_binding Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'module_binding)  _  (b1 : 'module_binding)  (_loc : FanLoc.t) \n     -> (`And (_loc, b1, b2) : 'module_binding ))\n",
              (Gram.mk_action
                 (fun (b2 : 'module_binding)  _  (b1 : 'module_binding) 
                    (_loc : FanLoc.t)  ->
                    (`And (_loc, b1, b2) : 'module_binding )))));
         ([`Stoken
             (((function
                | `Ant (("module_binding"|"anti"|"list"|""),_) -> true
                | _ -> false)),
               (`Normal,
                 "`Ant ((\"module_binding\"|\"anti\"|\"list\"|\"\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"module_binding\"|\"anti\"|\"list\"|\"\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"module_binding\" n s)) : 'module_binding )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"module_binding\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("module_binding"|"anti"|"list"|"" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_binding )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"module_binding\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.module_binding_tag : 'module_binding )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.module_binding_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.module_binding_tag : 
                       'module_binding )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.module_binding_tag\n"))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (me : 'module_expr)  _  (mt : 'module_type)  _  (m : 'a_uident) \n     (_loc : FanLoc.t)  -> (`ModuleBind (_loc, m, mt, me) : 'module_binding ))\n",
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                   (m : 'a_uident)  (_loc : FanLoc.t)  ->
                   (`ModuleBind (_loc, m, mt, me) : 'module_binding )))))]));
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
                | `Ant ((""|"module_binding"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal,
                 "`Ant ((\"\"|\"module_binding\"|\"anti\"|\"list\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"module_binding\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"module_binding\" n s)) : 'module_rec_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"module_binding\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"module_binding"|"anti"|"list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_rec_declaration )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"module_binding\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.module_binding_tag : 'module_rec_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.module_binding_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.module_binding_tag : 
                       'module_rec_declaration )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.module_binding_tag\n"))));
         ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'module_type)  _  (m : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, m, mt) : 'module_rec_declaration ))\n",
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (m : 'a_uident) 
                   (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, m, mt) : 'module_rec_declaration )))))])));
  (Gram.extend_single (with_constr_quot : 'with_constr_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (with_constr : 'with_constr Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'with_constr)  (_loc : FanLoc.t)  -> (x : 'with_constr_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'with_constr)  (_loc : FanLoc.t)  ->
                    (x : 'with_constr_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'with_constr_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'with_constr_quot )))))]));
   Gram.extend_single (with_constr : 'with_constr Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (wc2 : 'with_constr)  _  (wc1 : 'with_constr)  (_loc : FanLoc.t)  ->\n     (`And (_loc, wc1, wc2) : 'with_constr ))\n",
              (Gram.mk_action
                 (fun (wc2 : 'with_constr)  _  (wc1 : 'with_constr) 
                    (_loc : FanLoc.t)  ->
                    (`And (_loc, wc1, wc2) : 'with_constr )))));
         ([`Stoken
             (((function
                | `Ant ((""|"with_constr"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"with_constr\"|\"anti\"|\"list\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"with_constr\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"with_constr\" n s)) : 'with_constr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"with_constr\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"with_constr"|"anti"|"list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"with_constr" n s)) : 
                       'with_constr )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"with_constr\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.with_constr_tag : 'with_constr )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.with_constr_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.with_constr_tag : 
                       'with_constr )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.with_constr_tag\n"))));
         ([`Skeyword "type";
          `Snterm
            (Gram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters)  _ \n     (_loc : FanLoc.t)  -> (`TypeEq (_loc, t1, t2) : 'with_constr ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : FanLoc.t)  ->
                   (`TypeEq (_loc, t1, t2) : 'with_constr )))));
         ([`Skeyword "type";
          `Snterm
            (Gram.obj
               (type_longident_and_parameters : 'type_longident_and_parameters
                                                  Gram.t ));
          `Skeyword ":=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters)  _ \n     (_loc : FanLoc.t)  -> (`TypeSubst (_loc, t1, t2) : 'with_constr ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : FanLoc.t)  ->
                   (`TypeSubst (_loc, t1, t2) : 'with_constr )))));
         ([`Skeyword "module";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
          `Skeyword "=";
          `Snterm
            (Gram.obj
               (module_longident_with_app : 'module_longident_with_app Gram.t ))],
           ("Gram.mk_action\n  (fun (i2 : 'module_longident_with_app)  _  (i1 : 'module_longident)  _ \n     (_loc : FanLoc.t)  -> (`ModuleEq (_loc, i1, i2) : 'with_constr ))\n",
             (Gram.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (`ModuleEq (_loc, i1, i2) : 'with_constr )))));
         ([`Skeyword "module";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
          `Skeyword ":=";
          `Snterm
            (Gram.obj
               (module_longident_with_app : 'module_longident_with_app Gram.t ))],
           ("Gram.mk_action\n  (fun (i2 : 'module_longident_with_app)  _  (i1 : 'module_longident)  _ \n     (_loc : FanLoc.t)  -> (`ModuleSubst (_loc, i1, i2) : 'with_constr ))\n",
             (Gram.mk_action
                (fun (i2 : 'module_longident_with_app)  _ 
                   (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (`ModuleSubst (_loc, i1, i2) : 'with_constr )))))])));
  (Gram.extend (module_type : 'module_type Gram.t )
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
             ("Gram.mk_action\n  (fun (mt : 'module_type)  _  _  (t : 'module_type)  _  (i : 'a_uident)  _ \n     _  (_loc : FanLoc.t)  -> (`MtFun (_loc, i, t, mt) : 'module_type ))\n",
               (Gram.mk_action
                  (fun (mt : 'module_type)  _  _  (t : 'module_type)  _ 
                     (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                     (`MtFun (_loc, i, t, mt) : 'module_type )))))]);
       ((Some "with"), None,
         [([`Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (with_constr : 'with_constr Gram.t ))],
            ("Gram.mk_action\n  (fun (wc : 'with_constr)  _  (mt : 'module_type)  (_loc : FanLoc.t)  ->\n     (`With (_loc, mt, wc) : 'module_type ))\n",
              (Gram.mk_action
                 (fun (wc : 'with_constr)  _  (mt : 'module_type) 
                    (_loc : FanLoc.t)  ->
                    (`With (_loc, mt, wc) : 'module_type )))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            ("Gram.mk_action\n  (fun (mt2 : 'module_type)  (mt1 : 'module_type)  (_loc : FanLoc.t)  ->\n     (ModuleType.app0 mt1 mt2 : 'module_type ))\n",
              (Gram.mk_action
                 (fun (mt2 : 'module_type)  (mt1 : 'module_type) 
                    (_loc : FanLoc.t)  ->
                    (ModuleType.app0 mt1 mt2 : 'module_type )))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            ("Gram.mk_action\n  (fun (mt2 : 'module_type)  _  (mt1 : 'module_type)  (_loc : FanLoc.t)  ->\n     (ModuleType.acc0 mt1 mt2 : 'module_type ))\n",
              (Gram.mk_action
                 (fun (mt2 : 'module_type)  _  (mt1 : 'module_type) 
                    (_loc : FanLoc.t)  ->
                    (ModuleType.acc0 mt1 mt2 : 'module_type )))))]);
       ((Some "sig"), None,
         [([`Skeyword "sig";
           `Snterm (Gram.obj (sig_items : 'sig_items Gram.t ));
           `Skeyword "end"],
            ("Gram.mk_action\n  (fun _  (sg : 'sig_items)  _  (_loc : FanLoc.t)  ->\n     (`Sig (_loc, sg) : 'module_type ))\n",
              (Gram.mk_action
                 (fun _  (sg : 'sig_items)  _  (_loc : FanLoc.t)  ->
                    (`Sig (_loc, sg) : 'module_type )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant ((""|"mtyp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"mtyp\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"mtyp\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"module_type\" n s)) : 'module_type )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"module_type\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"mtyp"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"module_type" n s)) : 
                        'module_type )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"module_type\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.module_type_tag : 'module_type )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.module_type_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.module_type_tag : 
                       'module_type )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.module_type_tag\n"))));
         ([`Snterm
             (Gram.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, i) : 'module_type ))\n",
             (Gram.mk_action
                (fun (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                   (`Id (_loc, i) : 'module_type )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (mt : 'module_type)  _  (_loc : FanLoc.t)  -> (mt : 'module_type ))\n",
             (Gram.mk_action
                (fun _  (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                   (mt : 'module_type )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Skeyword "of";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (me : 'module_expr)  _  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleTypeOf (_loc, me) : 'module_type ))\n",
             (Gram.mk_action
                (fun (me : 'module_expr)  _  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleTypeOf (_loc, me) : 'module_type )))))])]);
   Gram.extend (module_declaration : 'module_declaration Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             ("Gram.mk_action\n  (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->\n     (mt : 'module_declaration ))\n",
               (Gram.mk_action
                  (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                     (mt : 'module_declaration )))));
          ([`Skeyword "(";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword ")";
           `Sself],
            ("Gram.mk_action\n  (fun (mt : 'module_declaration)  _  (t : 'module_type)  _  (i : 'a_uident) \n     _  (_loc : FanLoc.t)  ->\n     (`MtFun (_loc, i, t, mt) : 'module_declaration ))\n",
              (Gram.mk_action
                 (fun (mt : 'module_declaration)  _  (t : 'module_type)  _ 
                    (i : 'a_uident)  _  (_loc : FanLoc.t)  ->
                    (`MtFun (_loc, i, t, mt) : 'module_declaration )))))])]);
   Gram.extend_single (module_type_quot : 'module_type_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'module_type)  (_loc : FanLoc.t)  -> (x : 'module_type_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'module_type)  (_loc : FanLoc.t)  ->
                    (x : 'module_type_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'module_type_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'module_type_quot )))))])));
  (Gram.extend_single (sig_item_quot : 'sig_item_quot Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (dp : 'opt_expr)  (s : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Directive (_loc, s, dp) : 'sig_item_quot ))\n",
              (Gram.mk_action
                 (fun (dp : 'opt_expr)  (s : 'a_lident)  _  (_loc : FanLoc.t)
                     -> (`Directive (_loc, s, dp) : 'sig_item_quot )))));
         ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (sg2 : 'sig_item_quot)  _  (sg1 : 'sig_item)  (_loc : FanLoc.t)  ->\n     (match sg2 with | `Nil _loc -> sg1 | _ -> `Sem (_loc, sg1, sg2) : \n     'sig_item_quot ))\n",
             (Gram.mk_action
                (fun (sg2 : 'sig_item_quot)  _  (sg1 : 'sig_item) 
                   (_loc : FanLoc.t)  ->
                   (match sg2 with
                    | `Nil _loc -> sg1
                    | _ -> `Sem (_loc, sg1, sg2) : 'sig_item_quot )))));
         ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ))],
           ("Gram.mk_action\n  (fun (sg : 'sig_item)  (_loc : FanLoc.t)  -> (sg : 'sig_item_quot ))\n",
             (Gram.mk_action
                (fun (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                   (sg : 'sig_item_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'sig_item_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'sig_item_quot )))))]));
   Gram.extend_single (sig_item : 'sig_item Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"sigi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"sigi\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"sig_item\" n s)) : 'sig_item )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"sig_item\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"sigi"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"sig_item" n s)) : 'sig_item )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"sig_item\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.sig_item_tag : 'sig_item )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.sig_item_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.sig_item_tag : 
                       'sig_item )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.sig_item_tag\n"))));
         ([`Skeyword "exception";
          `Snterm
            (Gram.obj
               (constructor_declaration : 'constructor_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Exception (_loc, t) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->
                   (`Exception (_loc, t) : 'sig_item )))));
         ([`Skeyword "external";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (string_list : 'string_list Gram.t ))],
           ("Gram.mk_action\n  (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`External (_loc, i, t, sl) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident) 
                   _  (_loc : FanLoc.t)  ->
                   (`External (_loc, i, t, sl) : 'sig_item )))));
         ([`Skeyword "include";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->\n     (`Include (_loc, mt) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                   (`Include (_loc, mt) : 'sig_item )))));
         ([`Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Snterm
            (Gram.obj (module_declaration : 'module_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'module_declaration)  (i : 'a_uident)  _  (_loc : FanLoc.t)  ->\n     (`Module (_loc, i, mt) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (mt : 'module_declaration)  (i : 'a_uident)  _ 
                   (_loc : FanLoc.t)  -> (`Module (_loc, i, mt) : 'sig_item )))));
         ([`Skeyword "module";
          `Skeyword "rec";
          `Snterm
            (Gram.obj
               (module_rec_declaration : 'module_rec_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (mb : 'module_rec_declaration)  _  _  (_loc : FanLoc.t)  ->\n     (`RecModule (_loc, mb) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (mb : 'module_rec_declaration)  _  _  (_loc : FanLoc.t) 
                   -> (`RecModule (_loc, mb) : 'sig_item )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
           ("Gram.mk_action\n  (fun (mt : 'module_type)  _  (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleType (_loc, i, mt) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (i : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ModuleType (_loc, i, mt) : 'sig_item )))));
         ([`Skeyword "import";
          `Snterm (Gram.obj (dot_namespace : 'dot_namespace Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'dot_namespace)  _  (_loc : FanLoc.t)  ->\n     (FanToken.paths := ((`Absolute x) :: (FanToken.paths.contents));\n      `Nil _loc : 'sig_item ))\n",
             (Gram.mk_action
                (fun (x : 'dot_namespace)  _  (_loc : FanLoc.t)  ->
                   (FanToken.paths := ((`Absolute x) ::
                      (FanToken.paths.contents));
                    `Nil _loc : 'sig_item )))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleType (_loc, i, (`Nil _loc)) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleType (_loc, i, (`Nil _loc)) : 'sig_item )))));
         ([`Skeyword "open";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->\n     (`Open (_loc, i) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (`Open (_loc, i) : 'sig_item )))));
         ([`Skeyword "type";
          `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Type (_loc, t) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                   (`Type (_loc, t) : 'sig_item )))));
         ([`Skeyword "val";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Val (_loc, i, t) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`Val (_loc, i, t) : 'sig_item )))));
         ([`Skeyword "class";
          `Snterm (Gram.obj (class_description : 'class_description Gram.t ))],
           ("Gram.mk_action\n  (fun (cd : 'class_description)  _  (_loc : FanLoc.t)  ->\n     (`Class (_loc, cd) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (cd : 'class_description)  _  (_loc : FanLoc.t)  ->
                   (`Class (_loc, cd) : 'sig_item )))));
         ([`Skeyword "class";
          `Skeyword "type";
          `Snterm
            (Gram.obj
               (class_type_declaration : 'class_type_declaration Gram.t ))],
           ("Gram.mk_action\n  (fun (ctd : 'class_type_declaration)  _  _  (_loc : FanLoc.t)  ->\n     (`ClassType (_loc, ctd) : 'sig_item ))\n",
             (Gram.mk_action
                (fun (ctd : 'class_type_declaration)  _  _  (_loc : FanLoc.t)
                    -> (`ClassType (_loc, ctd) : 'sig_item )))))]));
   Gram.extend_single (interf : 'interf Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
           `Skeyword ";;"],
            ("Gram.mk_action\n  (fun _  (dp : 'opt_expr)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (([`Directive (_loc, n, dp)], (Some _loc)) : 'interf ))\n",
              (Gram.mk_action
                 (fun _  (dp : 'opt_expr)  (n : 'a_lident)  _ 
                    (_loc : FanLoc.t)  ->
                    (([`Directive (_loc, n, dp)], (Some _loc)) : 'interf )))));
         ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun ((sil,stopped) : 'interf)  _  (si : 'sig_item)  (_loc : FanLoc.t)  ->\n     (((si :: sil), stopped) : 'interf ))\n",
             (Gram.mk_action
                (fun ((sil,stopped) : 'interf)  _  (si : 'sig_item) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'interf )))));
         ([`Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `EOI -> (([], None) : 'interf )\n     | _ -> failwith \"([], None)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `EOI -> (([], None) : 'interf )
                   | _ -> failwith "([], None)\n"))))]));
   Gram.extend_single (sig_items : 'sig_items Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"sigi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"sigi\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti n ~c:\"sig_item\" s)) : 'sig_items )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n ~c:\"sig_item\" s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"sigi"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti n ~c:"sig_item" s)) : 'sig_items )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti n ~c:\"sig_item\" s))\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"sigi"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (sg : 'sig_items)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"sigi\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (`Ant (_loc, (mk_anti n ~c:\"sig_item\" s))), sg) : \n         'sig_items )\n     | _ ->\n         failwith\n           \"`Sem (_loc, (`Ant (_loc, (mk_anti n ~c:\"sig_item\" s))), sg)\n\")\n",
             (Gram.mk_action
                (fun (sg : 'sig_items)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"sigi"|"anti"|"list" as n),s) ->
                       (`Sem
                          (_loc, (`Ant (_loc, (mk_anti n ~c:"sig_item" s))),
                            sg) : 'sig_items )
                   | _ ->
                       failwith
                         "`Sem (_loc, (`Ant (_loc, (mk_anti n ~c:\"sig_item\" s))), sg)\n"))));
         ([`Slist0
             (Gram.srules
                [([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   ("Gram.mk_action (fun _  (sg : 'sig_item)  (_loc : FanLoc.t)  -> (sg : 'e__1 ))\n",
                     (Gram.mk_action
                        (fun _  (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                           (sg : 'e__1 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__1 list)  (_loc : FanLoc.t)  ->\n     (FanAst.sem_of_list l : 'sig_items ))\n",
             (Gram.mk_action
                (fun (l : 'e__1 list)  (_loc : FanLoc.t)  ->
                   (FanAst.sem_of_list l : 'sig_items )))))])));
  (let grammar_entry_create = Gram.mk in
   let fun_def_patt: 'fun_def_patt Gram.t =
     grammar_entry_create "fun_def_patt" in
   Gram.extend_single (expr_quot : 'expr_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword ",";
           `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (e2 : 'comma_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, e1, e2) : 'expr_quot ))\n",
              (Gram.mk_action
                 (fun (e2 : 'comma_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                    -> (`Com (_loc, e1, e2) : 'expr_quot )))));
         ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ";";
          `Snterm (Gram.obj (sem_expr : 'sem_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e2 : 'sem_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, e1, e2) : 'expr_quot ))\n",
             (Gram.mk_action
                (fun (e2 : 'sem_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, e1, e2) : 'expr_quot )))));
         ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'expr_quot ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'expr_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'expr_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'expr_quot )))))]));
   Gram.extend_single (cvalue_binding : 'cvalue_binding Gram.t )
     (None,
       (None, None,
         [([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (_loc : FanLoc.t)  -> (e : 'cvalue_binding ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (e : 'cvalue_binding )))));
         ([`Skeyword ":";
          `Skeyword "type";
          `Snterm (Gram.obj (unquoted_typevars : 'unquoted_typevars Gram.t ));
          `Skeyword ".";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (t2 : 'ctyp)  _  (t1 : 'unquoted_typevars)  _  _ \n     (_loc : FanLoc.t)  ->\n     (let u = `TyPol (_loc, t1, t2) in `Constraint (_loc, e, u) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (t2 : 'ctyp)  _ 
                   (t1 : 'unquoted_typevars)  _  _  (_loc : FanLoc.t)  ->
                   (let u = `TyPol (_loc, t1, t2) in `Constraint (_loc, e, u) : 
                   'cvalue_binding )))));
         ([`Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, e, t) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, e, t) : 'cvalue_binding )))));
         ([`Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (match t with\n      | `TyPol (_loc,_,_) -> raise (XStream.Error \"unexpected polytype here\")\n      | _ -> `Coercion (_loc, e, t, t2) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _ 
                   (_loc : FanLoc.t)  ->
                   (match t with
                    | `TyPol (_loc,_,_) ->
                        raise (XStream.Error "unexpected polytype here")
                    | _ -> `Coercion (_loc, e, t, t2) : 'cvalue_binding )))));
         ([`Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Coercion (_loc, e, (`Nil _loc), t) : 'cvalue_binding ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Coercion (_loc, e, (`Nil _loc), t) : 'cvalue_binding )))))]));
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
          ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (e : 'fun_binding)  (p : 'ipatt)  (_loc : FanLoc.t)  ->\n     (`Fun (_loc, (`Case (_loc, p, (`Nil _loc), e))) : 'fun_binding ))\n",
              (Gram.mk_action
                 (fun (e : 'fun_binding)  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                    (`Fun (_loc, (`Case (_loc, p, (`Nil _loc), e))) : 
                    'fun_binding )))));
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
   Gram.extend_single (pos_exprs : 'pos_exprs Gram.t )
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
            ("Gram.mk_action\n  (fun (xys : 'e__2 list)  (_loc : FanLoc.t)  ->\n     (let old = AstQuotation.map.contents in\n      AstQuotation.map := (SMap.add_list xys old); old : 'pos_exprs ))\n",
              (Gram.mk_action
                 (fun (xys : 'e__2 list)  (_loc : FanLoc.t)  ->
                    (let old = AstQuotation.map.contents in
                     AstQuotation.map := (SMap.add_list xys old); old : 
                    'pos_exprs )))))]));
   Gram.extend_single (fun_def_patt : 'fun_def_patt Gram.t )
     (None,
       (None, None,
         [([`Skeyword "(";
           `Skeyword "type";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ")"],
            ("Gram.mk_action\n  (fun _  (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_patt ))\n",
              (Gram.mk_action
                 (fun _  (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->
                    (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_patt )))));
         ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'ipatt)  (_loc : FanLoc.t)  ->\n     (fun e  -> `Fun (_loc, (`Case (_loc, p, (`Nil _loc), e))) : 'fun_def_patt ))\n",
             (Gram.mk_action
                (fun (p : 'ipatt)  (_loc : FanLoc.t)  ->
                   (fun e  -> `Fun (_loc, (`Case (_loc, p, (`Nil _loc), e))) : 
                   'fun_def_patt )))));
         ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
          `Skeyword "when";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (w : 'expr)  _  (p : 'ipatt)  (_loc : FanLoc.t)  ->\n     (fun e  -> `Fun (_loc, (`Case (_loc, p, w, e))) : 'fun_def_patt ))\n",
             (Gram.mk_action
                (fun (w : 'expr)  _  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                   (fun e  -> `Fun (_loc, (`Case (_loc, p, w, e))) : 
                   'fun_def_patt )))))]));
   Gram.extend (fun_def : 'fun_def Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Snterm (Gram.obj (fun_def_patt : 'fun_def_patt Gram.t ));
            `Skeyword "->";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             ("Gram.mk_action\n  (fun (e : 'expr)  _  (f : 'fun_def_patt)  (_loc : FanLoc.t)  ->\n     (f e : 'fun_def ))\n",
               (Gram.mk_action
                  (fun (e : 'expr)  _  (f : 'fun_def_patt)  (_loc : FanLoc.t)
                      -> (f e : 'fun_def )))));
          ([`Snterm (Gram.obj (fun_def_patt : 'fun_def_patt Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'fun_def)  (f : 'fun_def_patt)  (_loc : FanLoc.t)  ->\n     (f e : 'fun_def ))\n",
              (Gram.mk_action
                 (fun (e : 'fun_def)  (f : 'fun_def_patt)  (_loc : FanLoc.t) 
                    -> (f e : 'fun_def )))))])]);
   Gram.extend_single (opt_expr : 'opt_expr Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
            ("Gram.mk_action (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'opt_expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'opt_expr )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_expr ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_expr )))))]));
   Gram.extend (expr : 'expr Gram.t )
     (None,
       [((Some "top"), (Some `RA),
          [([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Sself],
             ("Gram.mk_action\n  (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) \n     -> (`LetIn (_loc, r, bi, x) : 'expr ))\n",
               (Gram.mk_action
                  (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                     (_loc : FanLoc.t)  -> (`LetIn (_loc, r, bi, x) : 
                     'expr )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
           `Skeyword "in";
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (mb : 'module_binding0)  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  -> (`LetModule (_loc, m, mb, e) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (mb : 'module_binding0) 
                    (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                    (`LetModule (_loc, m, mb, e) : 'expr )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (i : 'module_longident)  _  _  (_loc : FanLoc.t)  ->\n     (`LetOpen (_loc, i, e) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (i : 'module_longident)  _  _ 
                    (_loc : FanLoc.t)  -> (`LetOpen (_loc, i, e) : 'expr )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'match_case)  _  (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec) \n     _  _  (_loc : FanLoc.t)  ->\n     (`App\n        (_loc,\n          (`Try\n             (_loc,\n               (`LetIn\n                  (_loc, r, bi,\n                    (`Fun\n                       (_loc,\n                         (`Case\n                            (_loc, (`Id (_loc, (`Uid (_loc, \"()\")))),\n                              (`Nil _loc), x)))))),\n               (FanAst.match_pre#match_case a))),\n          (`Id (_loc, (`Uid (_loc, \"()\"))))) : 'expr ))\n",
              (Gram.mk_action
                 (fun (a : 'match_case)  _  (x : 'expr)  _  (bi : 'binding) 
                    (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->
                    (`App
                       (_loc,
                         (`Try
                            (_loc,
                              (`LetIn
                                 (_loc, r, bi,
                                   (`Fun
                                      (_loc,
                                        (`Case
                                           (_loc,
                                             (`Id (_loc, (`Uid (_loc, "()")))),
                                             (`Nil _loc), x)))))),
                              (FanAst.match_pre#match_case a))),
                         (`Id (_loc, (`Uid (_loc, "()"))))) : 'expr )))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Match (_loc, e, a) : 'expr ))\n",
              (Gram.mk_action
                 (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t)
                     -> (`Match (_loc, e, a) : 'expr )))));
          ([`Skeyword "try";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Try (_loc, e, a) : 'expr ))\n",
              (Gram.mk_action
                 (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t)
                     -> (`Try (_loc, e, a) : 'expr )))));
          ([`Skeyword "if";
           `Sself;
           `Skeyword "then";
           `Sself;
           `Skeyword "else";
           `Sself],
            ("Gram.mk_action\n  (fun (e3 : 'expr)  _  (e2 : 'expr)  _  (e1 : 'expr)  _  (_loc : FanLoc.t) \n     -> (`IfThenElse (_loc, e1, e2, e3) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e3 : 'expr)  _  (e2 : 'expr)  _  (e1 : 'expr)  _ 
                    (_loc : FanLoc.t)  ->
                    (`IfThenElse (_loc, e1, e2, e3) : 'expr )))));
          ([`Skeyword "if"; `Sself; `Skeyword "then"; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`IfThen (_loc, e1, e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  _  (e1 : 'expr)  _  (_loc : FanLoc.t)  ->
                    (`IfThen (_loc, e1, e2) : 'expr )))));
          ([`Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->\n     (Expr.mksequence ~loc:_loc seq : 'expr ))\n",
              (Gram.mk_action
                 (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                    (Expr.mksequence ~loc:_loc seq : 'expr )))));
          ([`Skeyword "with";
           `Snterm (Gram.obj (lang : 'lang Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (x : 'expr)  (old : 'lang)  _  (_loc : FanLoc.t)  ->\n     (AstQuotation.default := old; x : 'expr ))\n",
              (Gram.mk_action
                 (fun (x : 'expr)  (old : 'lang)  _  (_loc : FanLoc.t)  ->
                    (AstQuotation.default := old; x : 'expr )))));
          ([`Skeyword "with";
           `Skeyword "{";
           `Snterm (Gram.obj (pos_exprs : 'pos_exprs Gram.t ));
           `Skeyword "}";
           `Sself],
            ("Gram.mk_action\n  (fun (x : 'expr)  _  (old : 'pos_exprs)  _  _  (_loc : FanLoc.t)  ->\n     (AstQuotation.map := old; x : 'expr ))\n",
              (Gram.mk_action
                 (fun (x : 'expr)  _  (old : 'pos_exprs)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (AstQuotation.map := old; x : 'expr )))));
          ([`Skeyword "for";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword "=";
           `Sself;
           `Snterm (Gram.obj (direction_flag : 'direction_flag Gram.t ));
           `Sself;
           `Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (e2 : 'expr)  (df : 'direction_flag) \n     (e1 : 'expr)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`For (_loc, i, e1, e2, df, seq) : 'expr ))\n",
              (Gram.mk_action
                 (fun _  (seq : 'sequence)  _  (e2 : 'expr) 
                    (df : 'direction_flag)  (e1 : 'expr)  _  (i : 'a_lident) 
                    _  (_loc : FanLoc.t)  ->
                    (`For (_loc, i, e1, e2, df, seq) : 'expr )))));
          ([`Skeyword "while";
           `Sself;
           `Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`While (_loc, e, seq) : 'expr ))\n",
              (Gram.mk_action
                 (fun _  (seq : 'sequence)  _  (e : 'expr)  _ 
                    (_loc : FanLoc.t)  -> (`While (_loc, e, seq) : 'expr )))))]);
       ((Some ":="), (Some `NA),
         [([`Sself; `Skeyword ":="; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`Assign\n        (_loc, (`Dot (_loc, e1, (`Id (_loc, (`Lid (_loc, \"contents\")))))),\n          e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                    (`Assign
                       (_loc,
                         (`Dot
                            (_loc, e1,
                              (`Id (_loc, (`Lid (_loc, "contents")))))), e2) : 
                    'expr )))));
         ([`Sself; `Skeyword "<-"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (match Expr.bigarray_set _loc e1 e2 with\n      | Some e -> e\n      | None  -> `Assign (_loc, e1, e2) : 'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (match Expr.bigarray_set _loc e1 e2 with
                    | Some e -> e
                    | None  -> `Assign (_loc, e1, e2) : 'expr )))))]);
       ((Some "||"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop0 : 'infixop0 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop0)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  (op : 'infixop0)  (e1 : 'expr) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "&&"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop1 : 'infixop1 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop1)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  (op : 'infixop1)  (e1 : 'expr) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "<"), (Some `LA),
         [([`Sself;
           `Snterm (Gram.obj (infixop2 : 'infixop2 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop2)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  (op : 'infixop2)  (e1 : 'expr) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "^"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop3 : 'infixop3 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop3)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  (op : 'infixop3)  (e1 : 'expr) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "+"), (Some `LA),
         [([`Sself;
           `Snterm (Gram.obj (infixop4 : 'infixop4 Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop4)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  (op : 'infixop4)  (e1 : 'expr) 
                    (_loc : FanLoc.t)  ->
                    (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "*"), (Some `LA),
         [([`Sself; `Skeyword "land"; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"land\")))), e1)), e2) : \n     'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                    (`App
                       (_loc,
                         (`App
                            (_loc, (`Id (_loc, (`Lid (_loc, "land")))), e1)),
                         e2) : 'expr )))));
         ([`Sself; `Skeyword "lor"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"lor\")))), e1)), e2) : \n     'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`App
                      (_loc,
                        (`App (_loc, (`Id (_loc, (`Lid (_loc, "lor")))), e1)),
                        e2) : 'expr )))));
         ([`Sself; `Skeyword "lxor"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"lxor\")))), e1)), e2) : \n     'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`App
                      (_loc,
                        (`App (_loc, (`Id (_loc, (`Lid (_loc, "lxor")))), e1)),
                        e2) : 'expr )))));
         ([`Sself; `Skeyword "mod"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"mod\")))), e1)), e2) : \n     'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`App
                      (_loc,
                        (`App (_loc, (`Id (_loc, (`Lid (_loc, "mod")))), e1)),
                        e2) : 'expr )))));
         ([`Sself; `Snterm (Gram.obj (infixop5 : 'infixop5 Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop5)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop5)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "**"), (Some `RA),
         [([`Sself; `Skeyword "asr"; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"asr\")))), e1)), e2) : \n     'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                    (`App
                       (_loc,
                         (`App (_loc, (`Id (_loc, (`Lid (_loc, "asr")))), e1)),
                         e2) : 'expr )))));
         ([`Sself; `Skeyword "lsl"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"lsl\")))), e1)), e2) : \n     'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`App
                      (_loc,
                        (`App (_loc, (`Id (_loc, (`Lid (_loc, "lsl")))), e1)),
                        e2) : 'expr )))));
         ([`Sself; `Skeyword "lsr"; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"lsr\")))), e1)), e2) : \n     'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`App
                      (_loc,
                        (`App (_loc, (`Id (_loc, (`Lid (_loc, "lsr")))), e1)),
                        e2) : 'expr )))));
         ([`Sself; `Snterm (Gram.obj (infixop6 : 'infixop6 Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  (op : 'infixop6)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop6)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (`App (_loc, (`App (_loc, op, e1)), e2) : 'expr )))))]);
       ((Some "obj"), (Some `RA),
         [([`Skeyword "fun";
           `Skeyword "[";
           `Slist0sep
             ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
               (`Skeyword "|"));
           `Skeyword "]"],
            ("Gram.mk_action\n  (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->\n     (`Fun (_loc, (FanAst.or_of_list a)) : 'expr ))\n",
              (Gram.mk_action
                 (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->
                    (`Fun (_loc, (FanAst.or_of_list a)) : 'expr )))));
         ([`Skeyword "function";
          `Skeyword "[";
          `Slist0sep
            ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
              (`Skeyword "|"));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->\n     (`Fun (_loc, (FanAst.or_of_list a)) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->
                   (`Fun (_loc, (FanAst.or_of_list a)) : 'expr )))));
         ([`Skeyword "fun"; `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
           ("Gram.mk_action (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr )))));
         ([`Skeyword "function";
          `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
           ("Gram.mk_action (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr )))));
         ([`Skeyword "object";
          `Snterm
            (Gram.obj (opt_class_self_patt : 'opt_class_self_patt Gram.t ));
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  (csp : 'opt_class_self_patt)  _ \n     (_loc : FanLoc.t)  -> (`Obj (_loc, csp, cst) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure) 
                   (csp : 'opt_class_self_patt)  _  (_loc : FanLoc.t)  ->
                   (`Obj (_loc, csp, cst) : 'expr )))))]);
       ((Some "unary minus"), (Some `NA),
         [([`Skeyword "-"; `Sself],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (Expr.mkumin _loc \"-\" e : 'expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (Expr.mkumin _loc "-" e : 'expr )))));
         ([`Skeyword "-."; `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (Expr.mkumin _loc \"-.\" e : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (Expr.mkumin _loc "-." e : 'expr )))))]);
       ((Some "apply"), (Some `LA),
         [([`Sself; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'expr)  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`App (_loc, e1, e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'expr)  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                    (`App (_loc, e1, e2) : 'expr )))));
         ([`Skeyword "assert"; `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (_loc : FanLoc.t)  -> (Expr.mkassert _loc e : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (Expr.mkassert _loc e : 'expr )))));
         ([`Skeyword "new";
          `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->\n     (`New (_loc, i) : 'expr ))\n",
             (Gram.mk_action
                (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                   (`New (_loc, i) : 'expr )))));
         ([`Skeyword "lazy"; `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (_loc : FanLoc.t)  -> (`Lazy (_loc, e) : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (`Lazy (_loc, e) : 'expr )))))]);
       ((Some "label"), (Some `NA),
         [([`Skeyword "~";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ":";
           `Sself],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, e) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                    -> (`Label (_loc, i, e) : 'expr )))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, (`Nil _loc)) : 'expr ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Label (_loc, i, (`Nil _loc)) : 'expr )))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), e) : 'expr )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, i)), e)\n\")\n",
             (Gram.mk_action
                (fun (e : 'expr)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `LABEL i ->
                       (`Label (_loc, (`Lid (_loc, i)), e) : 'expr )
                   | _ -> failwith "`Label (_loc, (`Lid (_loc, i)), e)\n"))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'expr )\n     | _ -> failwith \"`OptLabl (_loc, (`Lid (_loc, i)), e)\n\")\n",
             (Gram.mk_action
                (fun (e : 'expr)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'expr )
                   | _ -> failwith "`OptLabl (_loc, (`Lid (_loc, i)), e)\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, i, e) : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`OptLabl (_loc, i, e) : 'expr )))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`OptLabl (_loc, i, (`Nil _loc)) : 'expr ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`OptLabl (_loc, i, (`Nil _loc)) : 'expr )))))]);
       ((Some "."), (Some `LA),
         [([`Sself; `Skeyword "."; `Skeyword "("; `Sself; `Skeyword ")"],
            ("Gram.mk_action\n  (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`ArrayDot (_loc, e1, e2) : 'expr ))\n",
              (Gram.mk_action
                 (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                    -> (`ArrayDot (_loc, e1, e2) : 'expr )))));
         ([`Sself; `Skeyword "."; `Skeyword "["; `Sself; `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`StringDot (_loc, e1, e2) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                   -> (`StringDot (_loc, e1, e2) : 'expr )))));
         ([`Sself;
          `Skeyword ".";
          `Skeyword "{";
          `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (e2 : 'comma_expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (Expr.bigarray_get _loc e1 e2 : 'expr ))\n",
             (Gram.mk_action
                (fun _  (e2 : 'comma_expr)  _  _  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Expr.bigarray_get _loc e1 e2 : 'expr )))));
         ([`Sself; `Skeyword "."; `Sself],
           ("Gram.mk_action\n  (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->\n     (`Dot (_loc, e1, e2) : 'expr ))\n",
             (Gram.mk_action
                (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (`Dot (_loc, e1, e2) : 'expr )))));
         ([`Sself;
          `Skeyword "#";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (lab : 'a_lident)  _  (e : 'expr)  (_loc : FanLoc.t)  ->\n     (`Send (_loc, e, lab) : 'expr ))\n",
             (Gram.mk_action
                (fun (lab : 'a_lident)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                   (`Send (_loc, e, lab) : 'expr )))))]);
       ((Some "~-"), (Some `NA),
         [([`Skeyword "!"; `Sself],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Dot (_loc, e, (`Id (_loc, (`Lid (_loc, \"contents\"))))) : 'expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (`Dot (_loc, e, (`Id (_loc, (`Lid (_loc, "contents"))))) : 
                    'expr )))));
         ([`Snterm (Gram.obj (prefixop : 'prefixop Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (e : 'expr)  (f : 'prefixop)  (_loc : FanLoc.t)  ->\n     (`App (_loc, f, e) : 'expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  (f : 'prefixop)  (_loc : FanLoc.t)  ->
                   (`App (_loc, f, e) : 'expr )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x DynAst.expr_tag : 'expr )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.expr_tag\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `QUOTATION x ->
                        (AstQuotation.expand _loc x DynAst.expr_tag : 
                        'expr )
                    | _ ->
                        failwith
                          "AstQuotation.expand _loc x DynAst.expr_tag\n"))));
         ([`Stoken
             (((function
                | `Ant
                    (("exp"|""|"anti"|"`bool"|"tup"|"seq"|"int"|"`int"
                      |"int32"|"`int32"|"int64"|"`int64"|"nativeint"
                      |"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"
                      |"vrn"),_)
                    -> true
                | _ -> false)),
               (`Normal,
                 "`Ant\n  ((\"exp\"|\"\"|\"anti\"|\"`bool\"|\"tup\"|\"seq\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"\n    |\"`int64\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"|\"str\"|\"`str\"\n    |\"vrn\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant\n         ((\"exp\"|\"\"|\"anti\"|\"`bool\"|\"tup\"|\"seq\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"\n           |\"int64\"|\"`int64\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"\n           |\"`chr\"|\"str\"|\"`str\"|\"vrn\" as n),s)\n         -> (`Ant (_loc, (mk_anti ~c:\"expr\" n s)) : 'expr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"expr\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant
                       (("exp"|""|"anti"|"`bool"|"tup"|"seq"|"int"|"`int"
                         |"int32"|"`int32"|"int64"|"`int64"|"nativeint"
                         |"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"
                         |"vrn" as n),s)
                       -> (`Ant (_loc, (mk_anti ~c:"expr" n s)) : 'expr )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"expr\" n s))\n"))));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT (_,s) -> (`Int (_loc, s) : 'expr )\n     | _ -> failwith \"`Int (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT (_,s) -> (`Int (_loc, s) : 'expr )
                   | _ -> failwith "`Int (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT32 (_,_) -> true | _ -> false)),
               (`Normal, "`INT32 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT32 (_,s) -> (`Int32 (_loc, s) : 'expr )\n     | _ -> failwith \"`Int32 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT32 (_,s) -> (`Int32 (_loc, s) : 'expr )
                   | _ -> failwith "`Int32 (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT64 (_,_) -> true | _ -> false)),
               (`Normal, "`INT64 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT64 (_,s) -> (`Int64 (_loc, s) : 'expr )\n     | _ -> failwith \"`Int64 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT64 (_,s) -> (`Int64 (_loc, s) : 'expr )
                   | _ -> failwith "`Int64 (_loc, s)\n"))));
         ([`Stoken
             (((function | `Flo (_,_) -> true | _ -> false)),
               (`Normal, "`Flo (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Flo (_,s) -> (`Flo (_loc, s) : 'expr )\n     | _ -> failwith \"`Flo (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Flo (_,s) -> (`Flo (_loc, s) : 'expr )
                   | _ -> failwith "`Flo (_loc, s)\n"))));
         ([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (_,s) -> (`Chr (_loc, s) : 'expr )\n     | _ -> failwith \"`Chr (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `CHAR (_,s) -> (`Chr (_loc, s) : 'expr )
                   | _ -> failwith "`Chr (_loc, s)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'expr )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) -> (`Str (_loc, s) : 'expr )
                   | _ -> failwith "`Str (_loc, s)\n"))));
         ([`Stoken
             (((function | `NATIVEINT (_,_) -> true | _ -> false)),
               (`Normal, "`NATIVEINT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `NATIVEINT (_,s) -> (`NativeInt (_loc, s) : 'expr )\n     | _ -> failwith \"`NativeInt (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `NATIVEINT (_,s) -> (`NativeInt (_loc, s) : 'expr )
                   | _ -> failwith "`NativeInt (_loc, s)\n"))));
         ([`Stry
             (`Snterm
                (Gram.obj
                   (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                    Gram.t )));
          `Sself;
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  (i : 'module_longident_dot_lparen)  (_loc : FanLoc.t) \n     -> (`LetOpen (_loc, i, e) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  (i : 'module_longident_dot_lparen) 
                   (_loc : FanLoc.t)  -> (`LetOpen (_loc, i, e) : 'expr )))));
         ([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'ident)  (_loc : FanLoc.t)  -> (`Id (_loc, i) : 'expr ))\n",
             (Gram.mk_action
                (fun (i : 'ident)  (_loc : FanLoc.t)  ->
                   (`Id (_loc, i) : 'expr )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, s) : 'expr ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'expr )))));
         ([`Skeyword "["; `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Id (_loc, (`Uid (_loc, \"[]\"))) : 'expr ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "[]"))) : 'expr )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
          `Skeyword "::";
          `Sself;
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (last : 'expr)  _  (mk_list : 'sem_expr_for_list)  _ \n     (_loc : FanLoc.t)  -> (mk_list last : 'expr ))\n",
             (Gram.mk_action
                (fun _  (last : 'expr)  _  (mk_list : 'sem_expr_for_list)  _ 
                   (_loc : FanLoc.t)  -> (mk_list last : 'expr )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (mk_list : 'sem_expr_for_list)  _  (_loc : FanLoc.t)  ->\n     (mk_list (`Id (_loc, (`Uid (_loc, \"[]\")))) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (mk_list : 'sem_expr_for_list)  _  (_loc : FanLoc.t) 
                   -> (mk_list (`Id (_loc, (`Uid (_loc, "[]")))) : 'expr )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Array (_loc, (`Nil _loc)) : 'expr ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Array (_loc, (`Nil _loc)) : 'expr )))));
         ([`Skeyword "[|";
          `Snterm (Gram.obj (sem_expr : 'sem_expr Gram.t ));
          `Skeyword "|]"],
           ("Gram.mk_action\n  (fun _  (el : 'sem_expr)  _  (_loc : FanLoc.t)  ->\n     (`Array (_loc, el) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (el : 'sem_expr)  _  (_loc : FanLoc.t)  ->
                   (`Array (_loc, el) : 'expr )))));
         ([`Skeyword "{";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"));
          `Skeyword "with";
          `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (el : 'label_expr_list)  _  (__fan_1 : [> FanToken.t])  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Lid x ->\n         (`Record (_loc, el, (`Id (_loc, (`Lid (_loc, x))))) : 'expr )\n     | _ -> failwith \"`Record (_loc, el, (`Id (_loc, (`Lid (_loc, x)))))\n\")\n",
             (Gram.mk_action
                (fun _  (el : 'label_expr_list)  _ 
                   (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Lid x ->
                       (`RecordWith (_loc, el, (`Id (_loc, (`Lid (_loc, x))))) : 
                       'expr )
                   | _ ->
                       failwith
                         "`Record (_loc, el, (`Id (_loc, (`Lid (_loc, x)))))\n"))));
         ([`Skeyword "{";
          `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (el : 'label_expr_list)  _  (_loc : FanLoc.t)  ->\n     (`Record (_loc, el, (`Nil _loc)) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (el : 'label_expr_list)  _  (_loc : FanLoc.t)  ->
                   (`Record (_loc, el) : 'expr )))));
         ([`Skeyword "{";
          `Skeyword "(";
          `Sself;
          `Skeyword ")";
          `Skeyword "with";
          `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (el : 'label_expr_list)  _  _  (e : 'expr)  _  _  (_loc : FanLoc.t)\n      -> (`Record (_loc, el, e) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (el : 'label_expr_list)  _  _  (e : 'expr)  _  _ 
                   (_loc : FanLoc.t)  -> (`RecordWith (_loc, el, e) : 'expr )))));
         ([`Skeyword "{<"; `Skeyword ">}"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`OvrInst (_loc, (`Nil _loc)) : 'expr ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`OvrInst (_loc, (`Nil _loc)) : 'expr )))));
         ([`Skeyword "{<";
          `Snterm (Gram.obj (field_expr_list : 'field_expr_list Gram.t ));
          `Skeyword ">}"],
           ("Gram.mk_action\n  (fun _  (fel : 'field_expr_list)  _  (_loc : FanLoc.t)  ->\n     (`OvrInst (_loc, fel) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (fel : 'field_expr_list)  _  (_loc : FanLoc.t)  ->
                   (`OvrInst (_loc, fel) : 'expr )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Id (_loc, (`Uid (_loc, \"()\"))) : 'expr ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "()"))) : 'expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, e, t) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, e, t) : 'expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (el : 'comma_expr)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Tup (_loc, (`Com (_loc, e, el))) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (el : 'comma_expr)  _  (e : 'expr)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Tup (_loc, (`Com (_loc, e, el))) : 'expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ";";
          `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (Expr.mksequence ~loc:_loc (`Sem (_loc, e, seq)) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (seq : 'sequence)  _  (e : 'expr)  _ 
                   (_loc : FanLoc.t)  ->
                   (Expr.mksequence ~loc:_loc (`Sem (_loc, e, seq)) : 
                   'expr )))));
         ([`Skeyword "("; `Sself; `Skeyword ";"; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (Expr.mksequence ~loc:_loc e : 'expr ))\n",
             (Gram.mk_action
                (fun _  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (Expr.mksequence ~loc:_loc e : 'expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t) \n     -> (`Coercion (_loc, e, t, t2) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'expr)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Coercion (_loc, e, t, t2) : 'expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Coercion (_loc, e, (`Nil _loc), t) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (`Coercion (_loc, e, (`Nil _loc), t) : 'expr )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  -> (e : 'expr ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  -> (e : 'expr )))));
         ([`Skeyword "begin";
          `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->\n     (Expr.mksequence ~loc:_loc seq : 'expr ))\n",
             (Gram.mk_action
                (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                   (Expr.mksequence ~loc:_loc seq : 'expr )))));
         ([`Skeyword "begin"; `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Id (_loc, (`Uid (_loc, \"()\"))) : 'expr ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "()"))) : 'expr )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (me : 'module_expr)  _  _  (_loc : FanLoc.t)  ->\n     (`Package_expr (_loc, me) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (me : 'module_expr)  _  _  (_loc : FanLoc.t)  ->
                   (`Package_expr (_loc, me) : 'expr )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pt : 'module_type)  _  (me : 'module_expr)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`Package_expr (_loc, (`Constraint (_loc, me, pt))) : 'expr ))\n",
             (Gram.mk_action
                (fun _  (pt : 'module_type)  _  (me : 'module_expr)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`Package_expr (_loc, (`Constraint (_loc, me, pt))) : 
                   'expr )))))])]);
   Gram.extend_single (sequence : 'sequence Gram.t )
     (None,
       (None, None,
         [([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
            ("Gram.mk_action\n  (fun (k : 'sequence')  (e : 'expr)  _  (bi : 'binding)  (rf : 'opt_rec)  _ \n     (_loc : FanLoc.t)  -> (k (`LetIn (_loc, rf, bi, e)) : 'sequence ))\n",
              (Gram.mk_action
                 (fun (k : 'sequence')  (e : 'expr)  _  (bi : 'binding) 
                    (rf : 'opt_rec)  _  (_loc : FanLoc.t)  ->
                    (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "try";
          `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
          `Snterm (Gram.obj (binding : 'binding Gram.t ));
          `Skeyword "in";
          `Sself;
          `Skeyword "with";
          `Snterm (Gram.obj (match_case : 'match_case Gram.t ));
          `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
           ("Gram.mk_action\n  (fun (k : 'sequence')  (a : 'match_case)  _  (x : 'sequence)  _ \n     (bi : 'binding)  (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->\n     (k\n        (`App\n           (_loc,\n             (`Try\n                (_loc,\n                  (`LetIn\n                     (_loc, r, bi,\n                       (`Fun\n                          (_loc,\n                            (`Case\n                               (_loc, (`Id (_loc, (`Uid (_loc, \"()\")))),\n                                 (`Nil _loc), x)))))),\n                  (FanAst.match_pre#match_case a))),\n             (`Id (_loc, (`Uid (_loc, \"()\")))))) : 'sequence ))\n",
             (Gram.mk_action
                (fun (k : 'sequence')  (a : 'match_case)  _  (x : 'sequence) 
                   _  (bi : 'binding)  (r : 'opt_rec)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (k
                      (`App
                         (_loc,
                           (`Try
                              (_loc,
                                (`LetIn
                                   (_loc, r, bi,
                                     (`Fun
                                        (_loc,
                                          (`Case
                                             (_loc,
                                               (`Id
                                                  (_loc, (`Uid (_loc, "()")))),
                                               (`Nil _loc), x)))))),
                                (FanAst.match_pre#match_case a))),
                           (`Id (_loc, (`Uid (_loc, "()")))))) : 'sequence )))));
         ([`Skeyword "let";
          `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
          `Snterm (Gram.obj (binding : 'binding Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (el : 'sequence)  _  (bi : 'binding)  (rf : 'opt_rec)  _ \n     (_loc : FanLoc.t)  ->\n     (`LetIn (_loc, rf, bi, (Expr.mksequence ~loc:_loc el)) : 'sequence ))\n",
             (Gram.mk_action
                (fun (el : 'sequence)  _  (bi : 'binding)  (rf : 'opt_rec)  _
                    (_loc : FanLoc.t)  ->
                   (`LetIn (_loc, rf, bi, (Expr.mksequence ~loc:_loc el)) : 
                   'sequence )))));
         ([`Skeyword "let";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
          `Skeyword "in";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
           ("Gram.mk_action\n  (fun (k : 'sequence')  (e : 'expr)  _  (mb : 'module_binding0) \n     (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (k (`LetModule (_loc, m, mb, e)) : 'sequence ))\n",
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'expr)  _ 
                   (mb : 'module_binding0)  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
         ([`Skeyword "let";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
          `Skeyword ";";
          `Sself],
           ("Gram.mk_action\n  (fun (el : 'sequence)  _  (mb : 'module_binding0)  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`LetModule (_loc, m, mb, (Expr.mksequence ~loc:_loc el)) : 'sequence ))\n",
             (Gram.mk_action
                (fun (el : 'sequence)  _  (mb : 'module_binding0) 
                   (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`LetModule (_loc, m, mb, (Expr.mksequence ~loc:_loc el)) : 
                   'sequence )))));
         ([`Skeyword "let";
          `Skeyword "open";
          `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
          `Skeyword "in";
          `Sself],
           ("Gram.mk_action\n  (fun (e : 'sequence)  _  (i : 'module_longident)  _  _  (_loc : FanLoc.t) \n     -> (`LetOpen (_loc, i, e) : 'sequence ))\n",
             (Gram.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                   (_loc : FanLoc.t)  -> (`LetOpen (_loc, i, e) : 'sequence )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"expr;\" n s)) : 'sequence )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"expr;\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"expr;" n s)) : 'sequence )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"expr;\" n s))\n"))));
         ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
           ("Gram.mk_action\n  (fun (k : 'sequence')  (e : 'expr)  (_loc : FanLoc.t)  ->\n     (k e : 'sequence ))\n",
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'expr)  (_loc : FanLoc.t)  ->
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
            ("Gram.mk_action\n  (fun (x : 'e__3)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (`Lid (_loc, x))) : 'infixop1 ))\n",
              (Gram.mk_action
                 (fun (x : 'e__3)  (_loc : FanLoc.t)  ->
                    (`Id (_loc, (`Lid (_loc, x))) : 'infixop1 )))))]));
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
            ("Gram.mk_action\n  (fun (x : 'e__4)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (`Lid (_loc, x))) : 'infixop0 ))\n",
              (Gram.mk_action
                 (fun (x : 'e__4)  (_loc : FanLoc.t)  ->
                    (`Id (_loc, (`Lid (_loc, x))) : 'infixop0 )))))]));
   Gram.extend_single (sem_expr_for_list : 'sem_expr_for_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (expr : 'expr Gram.t )); `Skeyword ";"; `Sself],
            ("Gram.mk_action\n  (fun (el : 'sem_expr_for_list)  _  (e : 'expr)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App\n          (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, \"::\")))), e)),\n            (el acc)) : 'sem_expr_for_list ))\n",
              (Gram.mk_action
                 (fun (el : 'sem_expr_for_list)  _  (e : 'expr) 
                    (_loc : FanLoc.t)  ->
                    (fun acc  ->
                       `App
                         (_loc,
                           (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                           (el acc)) : 'sem_expr_for_list )))));
         ([`Snterm (Gram.obj (expr : 'expr Gram.t )); `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, \"::\")))), e)), acc) : \n     'sem_expr_for_list ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App
                        (_loc,
                          (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                          acc) : 'sem_expr_for_list )))));
         ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, \"::\")))), e)), acc) : \n     'sem_expr_for_list ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App
                        (_loc,
                          (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), e)),
                          acc) : 'sem_expr_for_list )))))]));
   Gram.extend_single (comma_expr : 'comma_expr Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("Gram.mk_action\n  (fun (e2 : 'comma_expr)  _  (e1 : 'comma_expr)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, e1, e2) : 'comma_expr ))\n",
              (Gram.mk_action
                 (fun (e2 : 'comma_expr)  _  (e1 : 'comma_expr) 
                    (_loc : FanLoc.t)  ->
                    (`Com (_loc, e1, e2) : 'comma_expr )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"expr,\" n s)) : 'comma_expr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"expr,\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"expr," n s)) : 'comma_expr )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"expr,\" n s))\n"))));
         ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
           ("Gram.mk_action (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'comma_expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'comma_expr )))))]));
   Gram.extend_single (dummy : 'dummy Gram.t )
     (None,
       (None, None,
         [([],
            ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (() : 'dummy ))\n",
              (Gram.mk_action (fun (_loc : FanLoc.t)  -> (() : 'dummy )))))])));
  (Gram.extend_single (binding_quot : 'binding_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (binding : 'binding Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'binding)  (_loc : FanLoc.t)  -> (x : 'binding_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'binding)  (_loc : FanLoc.t)  ->
                    (x : 'binding_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'binding_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'binding_quot )))))]));
   Gram.extend_single (binding : 'binding Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant (("binding"|"list"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"binding\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"binding\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"binding\" n s)) : 'binding )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"binding\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("binding"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"binding" n s)) : 'binding )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"binding\" n s))\n"))));
         ([`Stoken
             (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"anti\"),_)"));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`Bind (_loc, (`Ant (_loc, (mk_anti ~c:\"patt\" n s))), e) : 'binding )\n     | _ ->\n         failwith\n           \"`Bind (_loc, (`Ant (_loc, (mk_anti ~c:\"patt\" n s))), e)\n\")\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"anti" as n),s) ->
                       (`Bind
                          (_loc, (`Ant (_loc, (mk_anti ~c:"patt" n s))), e) : 
                       'binding )
                   | _ ->
                       failwith
                         "`Bind (_loc, (`Ant (_loc, (mk_anti ~c:\"patt\" n s))), e)\n"))));
         ([`Stoken
             (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"binding\" n s)) : 'binding )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"binding\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"binding" n s)) : 'binding )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"binding\" n s))\n"))));
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
         [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'fun_binding)  (p : 'patt)  (_loc : FanLoc.t)  ->\n     (`Bind (_loc, p, e) : 'let_binding ))\n",
              (Gram.mk_action
                 (fun (e : 'fun_binding)  (p : 'patt)  (_loc : FanLoc.t)  ->
                    (`Bind (_loc, p, e) : 'let_binding )))))])));
  (Gram.extend_single (match_case : 'match_case Gram.t )
     (None,
       (None, None,
         [([`Skeyword "[";
           `Slist0sep
             ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
               (`Skeyword "|"));
           `Skeyword "]"],
            ("Gram.mk_action\n  (fun _  (l : 'match_case0 list)  _  (_loc : FanLoc.t)  ->\n     (FanAst.or_of_list l : 'match_case ))\n",
              (Gram.mk_action
                 (fun _  (l : 'match_case0 list)  _  (_loc : FanLoc.t)  ->
                    (FanAst.or_of_list l : 'match_case )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
          `Skeyword "->";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (p : 'patt)  (_loc : FanLoc.t)  ->\n     (`Case (_loc, p, (`Nil _loc), e) : 'match_case ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (`Case (_loc, p, (`Nil _loc), e) : 'match_case )))))]));
   Gram.extend_single (match_case0 : 'match_case0 Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant (("match_case"|"list"|"anti"|""),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"match_case\"|\"list\"|\"anti\"|\"\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"match_case\"|\"list\"|\"anti\"|\"\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"match_case\" n s)) : 'match_case0 )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"match_case\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("match_case"|"list"|"anti"|"" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"match_case" n s)) : 
                        'match_case0 )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"match_case\" n s))\n"))));
         ([`Snterm (Gram.obj (patt_as_patt_opt : 'patt_as_patt_opt Gram.t ));
          `Skeyword "when";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword "->";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (w : 'expr)  _  (p : 'patt_as_patt_opt) \n     (_loc : FanLoc.t)  -> (`Case (_loc, p, w, e) : 'match_case0 ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (w : 'expr)  _  (p : 'patt_as_patt_opt) 
                   (_loc : FanLoc.t)  ->
                   (`Case (_loc, p, w, e) : 'match_case0 )))));
         ([`Snterm (Gram.obj (patt_as_patt_opt : 'patt_as_patt_opt Gram.t ));
          `Skeyword "->";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (p : 'patt_as_patt_opt)  (_loc : FanLoc.t)  ->\n     (`Case (_loc, p, (`Nil _loc), e) : 'match_case0 ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (p : 'patt_as_patt_opt) 
                   (_loc : FanLoc.t)  ->
                   (`Case (_loc, p, (`Nil _loc), e) : 'match_case0 )))))]));
   Gram.extend_single (match_case_quot : 'match_case_quot Gram.t )
     (None,
       (None, None,
         [([`Slist0sep
              ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
                (`Skeyword "|"))],
            ("Gram.mk_action\n  (fun (x : 'match_case0 list)  (_loc : FanLoc.t)  ->\n     (FanAst.or_of_list x : 'match_case_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'match_case0 list)  (_loc : FanLoc.t)  ->
                    (FanAst.or_of_list x : 'match_case_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'match_case_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'match_case_quot )))))])));
  (Gram.extend_single (rec_binding_quot : 'rec_binding_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'label_expr_list)  (_loc : FanLoc.t)  -> (x : 'rec_binding_quot ))\n",
              (Gram.mk_action
                 (fun (x : 'label_expr_list)  (_loc : FanLoc.t)  ->
                    (x : 'rec_binding_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'rec_binding_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'rec_binding_quot )))))]));
   Gram.extend_single (label_expr : 'label_expr Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant (("rec_binding"|""|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal,
                  "`Ant ((\"rec_binding\"|\"\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"rec_binding\"|\"\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"rec_binding\" n s)) : 'label_expr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"rec_binding\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("rec_binding"|""|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                        'label_expr )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"rec_binding\" n s))\n"))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
          `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'fun_binding)  (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, i, e) : 'label_expr ))\n",
             (Gram.mk_action
                (fun (e : 'fun_binding)  (i : 'label_longident) 
                   (_loc : FanLoc.t)  ->
                   (`RecBind (_loc, i, e) : 'label_expr )))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, i, (`Id (_loc, (`Lid (_loc, (Ident.to_lid i)))))) : \n     'label_expr ))\n",
             (Gram.mk_action
                (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                   (`RecBind
                      (_loc, i,
                        (`Id (_loc, (`Lid (_loc, (Ident.to_lid i)))))) : 
                   'label_expr )))))]));
   Gram.extend_single (field_expr : 'field_expr Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"bi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"bi\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"bi\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"rec_binding\" n s)) : 'field_expr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"rec_binding\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"bi"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                        'field_expr )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"rec_binding\" n s))\n"))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword "=";
          `Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
           ("Gram.mk_action\n  (fun (e : 'expr)  _  (l : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`RecBind (_loc, (l :>ident), e) : 'field_expr ))\n",
             (Gram.mk_action
                (fun (e : 'expr)  _  (l : 'a_lident)  (_loc : FanLoc.t)  ->
                   (`RecBind (_loc, (l :>ident), e) : 'field_expr )))))]));
   Gram.extend_single (label_expr_list : 'label_expr_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'label_expr_list)  _  (b1 : 'label_expr)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, b1, b2) : 'label_expr_list ))\n",
              (Gram.mk_action
                 (fun (b2 : 'label_expr_list)  _  (b1 : 'label_expr) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, b1, b2) : 'label_expr_list )))));
         ([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (b1 : 'label_expr)  (_loc : FanLoc.t)  -> (b1 : 'label_expr_list ))\n",
             (Gram.mk_action
                (fun _  (b1 : 'label_expr)  (_loc : FanLoc.t)  ->
                   (b1 : 'label_expr_list )))));
         ([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (b1 : 'label_expr)  (_loc : FanLoc.t)  -> (b1 : 'label_expr_list ))\n",
             (Gram.mk_action
                (fun (b1 : 'label_expr)  (_loc : FanLoc.t)  ->
                   (b1 : 'label_expr_list )))))]));
   Gram.extend_single (field_expr_list : 'field_expr_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (b2 : 'field_expr_list)  _  (b1 : 'field_expr)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, b1, b2) : 'field_expr_list ))\n",
              (Gram.mk_action
                 (fun (b2 : 'field_expr_list)  _  (b1 : 'field_expr) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, b1, b2) : 'field_expr_list )))));
         ([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (b1 : 'field_expr)  (_loc : FanLoc.t)  -> (b1 : 'field_expr_list ))\n",
             (Gram.mk_action
                (fun _  (b1 : 'field_expr)  (_loc : FanLoc.t)  ->
                   (b1 : 'field_expr_list )))));
         ([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (b1 : 'field_expr)  (_loc : FanLoc.t)  -> (b1 : 'field_expr_list ))\n",
             (Gram.mk_action
                (fun (b1 : 'field_expr)  (_loc : FanLoc.t)  ->
                   (b1 : 'field_expr_list )))))])));
  (let grammar_entry_create = Gram.mk in
   let patt_constr: 'patt_constr Gram.t = grammar_entry_create "patt_constr" in
   Gram.extend_single (patt_quot : 'patt_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword ",";
           `Snterm (Gram.obj (comma_patt : 'comma_patt Gram.t ))],
            ("Gram.mk_action\n  (fun (y : 'comma_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, x, y) : 'patt_quot ))\n",
              (Gram.mk_action
                 (fun (y : 'comma_patt)  _  (x : 'patt)  (_loc : FanLoc.t) 
                    -> (`Com (_loc, x, y) : 'patt_quot )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
          `Skeyword ";";
          `Snterm (Gram.obj (sem_patt : 'sem_patt Gram.t ))],
           ("Gram.mk_action\n  (fun (y : 'sem_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, x, y) : 'patt_quot ))\n",
             (Gram.mk_action
                (fun (y : 'sem_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, x, y) : 'patt_quot )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action\n  (fun (y : 'patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->\n     (let i =\n        match x with\n        | `Ant (loc,s) -> `Ant (loc, s)\n        | p -> FanAst.ident_of_patt p in\n      `PaEq (_loc, i, y) : 'patt_quot ))\n",
             (Gram.mk_action
                (fun (y : 'patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                   (let i =
                      match x with
                      | `Ant (loc,s) -> `Ant (loc, s)
                      | p -> FanAst.ident_of_patt p in
                    `PaEq (_loc, i, y) : 'patt_quot )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action (fun (x : 'patt)  (_loc : FanLoc.t)  -> (x : 'patt_quot ))\n",
             (Gram.mk_action
                (fun (x : 'patt)  (_loc : FanLoc.t)  -> (x : 'patt_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'patt_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'patt_quot )))))]));
   Gram.extend_single (patt_as_patt_opt : 'patt_as_patt_opt Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword "as";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (s : 'a_lident)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p1, s) : 'patt_as_patt_opt ))\n",
              (Gram.mk_action
                 (fun (s : 'a_lident)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                    (`Alias (_loc, p1, s) : 'patt_as_patt_opt )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'patt_as_patt_opt ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  (_loc : FanLoc.t)  ->
                   (p : 'patt_as_patt_opt )))))]));
   Gram.extend_single (opt_class_self_patt : 'opt_class_self_patt Gram.t )
     (None,
       (None, None,
         [([`Skeyword "(";
           `Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword ")"],
            ("Gram.mk_action\n  (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  -> (p : 'opt_class_self_patt ))\n",
              (Gram.mk_action
                 (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                    (p : 'opt_class_self_patt )))));
         ([`Skeyword "(";
          `Snterm (Gram.obj (patt : 'patt Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'opt_class_self_patt ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, p, t) : 'opt_class_self_patt )))));
         ([],
           ("Gram.mk_action\n  (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_class_self_patt ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  ->
                   (`Nil _loc : 'opt_class_self_patt )))))]));
   Gram.extend_single (patt_constr : 'patt_constr Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, i) : 'patt_constr ))\n",
              (Gram.mk_action
                 (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                    (`Id (_loc, i) : 'patt_constr )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->\n     (`Vrn (_loc, s) : 'patt_constr ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'patt_constr )))));
         ([`Stoken
             (((function
                | `Ant ((""|"pat"|"anti"|"vrn"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"pat\"|\"anti\"|\"vrn\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"pat\"|\"anti\"|\"vrn\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt\" n s)) : 'patt_constr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"pat"|"anti"|"vrn" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"patt" n s)) : 'patt_constr )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n"))))]));
   Gram.extend (patt : 'patt Gram.t )
     (None,
       [((Some "|"), (Some `LA),
          [([`Sself; `Skeyword "|"; `Sself],
             ("Gram.mk_action\n  (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->\n     (`Or (_loc, p1, p2) : 'patt ))\n",
               (Gram.mk_action
                  (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                     (`Or (_loc, p1, p2) : 'patt )))))]);
       ((Some ".."), (Some `NA),
         [([`Sself; `Skeyword ".."; `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->\n     (`PaRng (_loc, p1, p2) : 'patt ))\n",
              (Gram.mk_action
                 (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                    (`PaRng (_loc, p1, p2) : 'patt )))))]);
       ((Some "apply"), (Some `LA),
         [([`Snterm (Gram.obj (patt_constr : 'patt_constr Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'patt)  (p1 : 'patt_constr)  (_loc : FanLoc.t)  ->\n     (match p2 with\n      | `Tup (_loc,p) ->\n          List.fold_left (fun p1  p2  -> `App (_loc, p1, p2)) p1\n            (FanAst.list_of_com' p [])\n      | _ -> `App (_loc, p1, p2) : 'patt ))\n",
              (Gram.mk_action
                 (fun (p2 : 'patt)  (p1 : 'patt_constr)  (_loc : FanLoc.t) 
                    ->
                    (match p2 with
                     | `Tup (_loc,p) ->
                         List.fold_left (fun p1  p2  -> `App (_loc, p1, p2))
                           p1 (FanAst.list_of_com' p [])
                     | _ -> `App (_loc, p1, p2) : 'patt )))));
         ([`Snterm (Gram.obj (patt_constr : 'patt_constr Gram.t ))],
           ("Gram.mk_action (fun (p1 : 'patt_constr)  (_loc : FanLoc.t)  -> (p1 : 'patt ))\n",
             (Gram.mk_action
                (fun (p1 : 'patt_constr)  (_loc : FanLoc.t)  -> (p1 : 'patt )))));
         ([`Skeyword "lazy"; `Sself],
           ("Gram.mk_action\n  (fun (p : 'patt)  _  (_loc : FanLoc.t)  -> (`Lazy (_loc, p) : 'patt ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (`Lazy (_loc, p) : 'patt )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `Ant
                     ((""|"pat"|"anti"|"tup"|"int"|"`int"|"int32"|"`int32"
                       |"int64"|"`int64"|"vrn"|"nativeint"|"`nativeint"|"flo"
                       |"`flo"|"chr"|"`chr"|"str"|"`str"),_)
                     -> true
                 | _ -> false)),
                (`Normal,
                  "`Ant\n  ((\"\"|\"pat\"|\"anti\"|\"tup\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"|\"`int64\"\n    |\"vrn\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"|\"str\"|\"`str\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant\n         ((\"\"|\"pat\"|\"anti\"|\"tup\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"\n           |\"`int64\"|\"vrn\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"\n           |\"str\"|\"`str\" as n),s)\n         -> (`Ant (_loc, (mk_anti ~c:\"patt\" n s)) : 'patt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant
                        ((""|"pat"|"anti"|"tup"|"int"|"`int"|"int32"|"`int32"
                          |"int64"|"`int64"|"vrn"|"nativeint"|"`nativeint"
                          |"flo"|"`flo"|"chr"|"`chr"|"str"|"`str" as n),s)
                        -> (`Ant (_loc, (mk_anti ~c:"patt" n s)) : 'patt )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n"))));
         ([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'ident)  (_loc : FanLoc.t)  -> (`Id (_loc, i) : 'patt ))\n",
             (Gram.mk_action
                (fun (i : 'ident)  (_loc : FanLoc.t)  ->
                   (`Id (_loc, i) : 'patt )))));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT (_,s) -> (`Int (_loc, s) : 'patt )\n     | _ -> failwith \"`Int (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT (_,s) -> (`Int (_loc, s) : 'patt )
                   | _ -> failwith "`Int (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT32 (_,_) -> true | _ -> false)),
               (`Normal, "`INT32 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT32 (_,s) -> (`Int32 (_loc, s) : 'patt )\n     | _ -> failwith \"`Int32 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT32 (_,s) -> (`Int32 (_loc, s) : 'patt )
                   | _ -> failwith "`Int32 (_loc, s)\n"))));
         ([`Stoken
             (((function | `INT64 (_,_) -> true | _ -> false)),
               (`Normal, "`INT64 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT64 (_,s) -> (`Int64 (_loc, s) : 'patt )\n     | _ -> failwith \"`Int64 (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `INT64 (_,s) -> (`Int64 (_loc, s) : 'patt )
                   | _ -> failwith "`Int64 (_loc, s)\n"))));
         ([`Stoken
             (((function | `Flo (_,_) -> true | _ -> false)),
               (`Normal, "`Flo (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Flo (_,s) -> (`Flo (_loc, s) : 'patt )\n     | _ -> failwith \"`Flo (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Flo (_,s) -> (`Flo (_loc, s) : 'patt )
                   | _ -> failwith "`Flo (_loc, s)\n"))));
         ([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (_,s) -> (`Chr (_loc, s) : 'patt )\n     | _ -> failwith \"`Chr (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `CHAR (_,s) -> (`Chr (_loc, s) : 'patt )
                   | _ -> failwith "`Chr (_loc, s)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'patt )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) -> (`Str (_loc, s) : 'patt )
                   | _ -> failwith "`Str (_loc, s)\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT (_,_) -> true | _ -> false)),
              (`Normal, "`INT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `INT (_,s) -> (`Int (_loc, (String.neg s)) : 'patt )\n     | _ -> failwith \"`Int (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `INT (_,s) -> (`Int (_loc, (String.neg s)) : 'patt )
                   | _ -> failwith "`Int (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT32 (_,_) -> true | _ -> false)),
              (`Normal, "`INT32 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `INT32 (_,s) -> (`Int32 (_loc, (String.neg s)) : 'patt )\n     | _ -> failwith \"`Int32 (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `INT32 (_,s) -> (`Int32 (_loc, (String.neg s)) : 'patt )
                   | _ -> failwith "`Int32 (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT64 (_,_) -> true | _ -> false)),
              (`Normal, "`INT64 (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `INT64 (_,s) -> (`Int64 (_loc, (String.neg s)) : 'patt )\n     | _ -> failwith \"`Int64 (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `INT64 (_,s) -> (`Int64 (_loc, (String.neg s)) : 'patt )
                   | _ -> failwith "`Int64 (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `NATIVEINT (_,_) -> true | _ -> false)),
              (`Normal, "`NATIVEINT (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `NATIVEINT (_,s) -> (`Int64 (_loc, (String.neg s)) : 'patt )\n     | _ -> failwith \"`Int64 (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `NATIVEINT (_,s) ->
                       (`Int64 (_loc, (String.neg s)) : 'patt )
                   | _ -> failwith "`Int64 (_loc, (String.neg s))\n"))));
         ([`Skeyword "-";
          `Stoken
            (((function | `Flo (_,_) -> true | _ -> false)),
              (`Normal, "`Flo (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Flo (_,s) -> (`Flo (_loc, (String.neg s)) : 'patt )\n     | _ -> failwith \"`Flo (_loc, (String.neg s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Flo (_,s) -> (`Flo (_loc, (String.neg s)) : 'patt )
                   | _ -> failwith "`Flo (_loc, (String.neg s))\n"))));
         ([`Skeyword "["; `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Id (_loc, (`Uid (_loc, \"[]\"))) : 'patt ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "[]"))) : 'patt )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_patt_for_list : 'sem_patt_for_list Gram.t ));
          `Skeyword "::";
          `Sself;
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (last : 'patt)  _  (mk_list : 'sem_patt_for_list)  _ \n     (_loc : FanLoc.t)  -> (mk_list last : 'patt ))\n",
             (Gram.mk_action
                (fun _  (last : 'patt)  _  (mk_list : 'sem_patt_for_list)  _ 
                   (_loc : FanLoc.t)  -> (mk_list last : 'patt )))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_patt_for_list : 'sem_patt_for_list Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (mk_list : 'sem_patt_for_list)  _  (_loc : FanLoc.t)  ->\n     (mk_list (`Id (_loc, (`Uid (_loc, \"[]\")))) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (mk_list : 'sem_patt_for_list)  _  (_loc : FanLoc.t) 
                   -> (mk_list (`Id (_loc, (`Uid (_loc, "[]")))) : 'patt )))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Array (_loc, (`Nil _loc)) : 'patt ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Array (_loc, (`Nil _loc)) : 'patt )))));
         ([`Skeyword "[|";
          `Snterm (Gram.obj (sem_patt : 'sem_patt Gram.t ));
          `Skeyword "|]"],
           ("Gram.mk_action\n  (fun _  (pl : 'sem_patt)  _  (_loc : FanLoc.t)  ->\n     (`Array (_loc, pl) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (pl : 'sem_patt)  _  (_loc : FanLoc.t)  ->
                   (`Array (_loc, pl) : 'patt )))));
         ([`Skeyword "{";
          `Snterm (Gram.obj (label_patt_list : 'label_patt_list Gram.t ));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->\n     (`PaRec (_loc, pl) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->
                   (`PaRec (_loc, pl) : 'patt )))));
         ([`Skeyword "("; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Id (_loc, (`Uid (_loc, \"()\"))) : 'patt ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "()"))) : 'patt )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleUnpack (_loc, m, `None) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleUnpack (_loc, m, `None) : 'patt )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pt : 'module_type)  _  (m : 'a_uident)  _  _  (_loc : FanLoc.t) \n     -> (`ModuleUnpack (_loc, m, (`Some (`Package (_loc, pt)))) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (pt : 'module_type)  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ModuleUnpack (_loc, m, (`Some (`Package (_loc, pt)))) : 
                   'patt )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_4 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s)))) : 'patt )\n     | _ ->\n         failwith \"`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s))))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s)))) : 
                       'patt )
                   | _ ->
                       failwith
                         "`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s))))\n"))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  -> (p : 'patt ))\n",
             (Gram.mk_action
                (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  -> (p : 'patt )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, p, t) : 'patt )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "as";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (s : 'a_lident)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p, s) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'patt)  _  (_loc : FanLoc.t)
                    -> (`Alias (_loc, p, s) : 'patt )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_patt : 'comma_patt Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pl : 'comma_patt)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->\n     (`Tup (_loc, (`Com (_loc, p, pl))) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (pl : 'comma_patt)  _  (p : 'patt)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Tup (_loc, (`Com (_loc, p, pl))) : 'patt )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, s) : 'patt ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'patt )))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'type_longident)  _  (_loc : FanLoc.t)  ->\n     (`ClassPath (_loc, i) : 'patt ))\n",
             (Gram.mk_action
                (fun (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                   (`ClassPath (_loc, i) : 'patt )))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x DynAst.patt_tag : 'patt )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.patt_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.patt_tag : 
                       'patt )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.patt_tag\n"))));
         ([`Skeyword "_"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'patt ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'patt )))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'patt)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), p) : 'patt )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, i)), p)\n\")\n",
             (Gram.mk_action
                (fun (p : 'patt)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `LABEL i ->
                       (`Label (_loc, (`Lid (_loc, i)), p) : 'patt )
                   | _ -> failwith "`Label (_loc, (`Lid (_loc, i)), p)\n"))));
         ([`Skeyword "~";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'patt)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, p) : 'patt ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`Label (_loc, i, p) : 'patt )))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, (`Nil _loc)) : 'patt ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Label (_loc, i, (`Nil _loc)) : 'patt )))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL i ->\n         (`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e)) : 'patt )\n     | _ -> failwith \"`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e))\n\")\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e)) : 
                       'patt )
                   | _ ->
                       failwith
                         "`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e))\n"))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'patt_tcon)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`PaOlbi (_loc, (`Lid (_loc, i)), p, `None) : 'patt )\n     | _ -> failwith \"`PaOlbi (_loc, (`Lid (_loc, i)), p, `None)\n\")\n",
             (Gram.mk_action
                (fun _  (p : 'patt_tcon)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`PaOlbi (_loc, (`Lid (_loc, i)), p, `None) : 
                       'patt )
                   | _ ->
                       failwith
                         "`PaOlbi (_loc, (`Lid (_loc, i)), p, `None)\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`PaOlbi (_loc, i, p, (`Some e)) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, i, p, (`Some e)) : 'patt )))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'patt_tcon)  _  _ \n     (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     match __fan_6 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s)))) : 'patt )\n     | _ -> failwith \"`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s))))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'patt_tcon)  _  _
                    (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s)))) : 
                       'patt )
                   | _ ->
                       failwith
                         "`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s))))\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'patt_tcon)  _  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, i, p, `None) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (p : 'patt_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, i, p, `None) : 'patt )))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, i, (`Nil _loc), `None) : 'patt ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, i, (`Nil _loc), `None) : 'patt )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, (`Lid (_loc, \"\")), p, `None) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, (`Lid (_loc, "")), p, `None) : 'patt )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, (`Lid (_loc, \"\")), p, (`Some e)) : 'patt ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, (`Lid (_loc, "")), p, (`Some e)) : 
                   'patt )))))])]);
   Gram.extend_single (ipatt : 'ipatt Gram.t )
     (None,
       (None, None,
         [([`Skeyword "{";
           `Snterm (Gram.obj (label_patt_list : 'label_patt_list Gram.t ));
           `Skeyword "}"],
            ("Gram.mk_action\n  (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->\n     (`PaRec (_loc, pl) : 'ipatt ))\n",
              (Gram.mk_action
                 (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->
                    (`PaRec (_loc, pl) : 'ipatt )))));
         ([`Stoken
             (((function
                | `Ant ((""|"pat"|"anti"|"tup"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"pat\"|\"anti\"|\"tup\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"pat\"|\"anti\"|\"tup\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt\" n s)) : 'ipatt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"pat"|"anti"|"tup" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"patt" n s)) : 'ipatt )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n"))));
         ([`Skeyword "("; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (`Uid (_loc, \"()\"))) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "()"))) : 'ipatt )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleUnpack (_loc, m, `None) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                   (`ModuleUnpack (_loc, m, `None) : 'ipatt )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pt : 'module_type)  _  (m : 'a_uident)  _  _  (_loc : FanLoc.t) \n     -> (`ModuleUnpack (_loc, m, (`Some (`Package (_loc, pt)))) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (pt : 'module_type)  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`ModuleUnpack (_loc, m, (`Some (`Package (_loc, pt)))) : 
                   'ipatt )))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
          `Skeyword ":";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_4 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s)))) : 'ipatt )\n     | _ ->\n         failwith \"`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s))))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_4 : [> FanToken.t])  _  (m : 'a_uident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   match __fan_4 with
                   | `Ant (("opt" as n),s) ->
                       (`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s)))) : 
                       'ipatt )
                   | _ ->
                       failwith
                         "`ModuleUnpack (_loc, m, (`Ant (_loc, (mk_anti n s))))\n"))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action (fun _  (p : 'ipatt)  _  (_loc : FanLoc.t)  -> (p : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (p : 'ipatt)  _  (_loc : FanLoc.t)  -> (p : 'ipatt )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (p : 'ipatt)  _  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'ipatt)  _  (_loc : FanLoc.t) 
                   -> (`Constraint (_loc, p, t) : 'ipatt )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "as";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (s : 'a_lident)  _  (p : 'ipatt)  _  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p, s) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (s : 'a_lident)  _  (p : 'ipatt)  _ 
                   (_loc : FanLoc.t)  -> (`Alias (_loc, p, s) : 'ipatt )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_ipatt : 'comma_ipatt Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (pl : 'comma_ipatt)  _  (p : 'ipatt)  _  (_loc : FanLoc.t)  ->\n     (`Tup (_loc, (`Com (_loc, p, pl))) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (pl : 'comma_ipatt)  _  (p : 'ipatt)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Tup (_loc, (`Com (_loc, p, pl))) : 'ipatt )))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (s :>ident)) : 'ipatt ))\n",
             (Gram.mk_action
                (fun (s : 'a_lident)  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (s :>ident)) : 'ipatt )))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x DynAst.patt_tag : 'ipatt )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.patt_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.patt_tag : 
                       'ipatt )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.patt_tag\n"))));
         ([`Skeyword "_"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ipatt )))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'ipatt)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `LABEL i -> (`Label (_loc, (`Lid (_loc, i)), p) : 'ipatt )\n     | _ -> failwith \"`Label (_loc, (`Lid (_loc, i)), p)\n\")\n",
             (Gram.mk_action
                (fun (p : 'ipatt)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `LABEL i ->
                       (`Label (_loc, (`Lid (_loc, i)), p) : 'ipatt )
                   | _ -> failwith "`Label (_loc, (`Lid (_loc, i)), p)\n"))));
         ([`Skeyword "~";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Sself],
           ("Gram.mk_action\n  (fun (p : 'ipatt)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, p) : 'ipatt ))\n",
             (Gram.mk_action
                (fun (p : 'ipatt)  _  (i : 'a_lident)  _  (_loc : FanLoc.t) 
                   -> (`Label (_loc, i, p) : 'ipatt )))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Label (_loc, i, (`Nil _loc)) : 'ipatt ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Label (_loc, i, (`Nil _loc)) : 'ipatt )))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL i ->\n         (`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e)) : 'ipatt )\n     | _ -> failwith \"`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e))\n\")\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e)) : 
                       'ipatt )
                   | _ ->
                       failwith
                         "`PaOlbi (_loc, (`Lid (_loc, i)), p, (`Some e))\n"))));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'patt_tcon)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `OPTLABEL i -> (`PaOlbi (_loc, (`Lid (_loc, i)), p, `None) : 'ipatt )\n     | _ -> failwith \"`PaOlbi (_loc, (`Lid (_loc, i)), p, `None)\n\")\n",
             (Gram.mk_action
                (fun _  (p : 'patt_tcon)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `OPTLABEL i ->
                       (`PaOlbi (_loc, (`Lid (_loc, i)), p, `None) : 
                       'ipatt )
                   | _ ->
                       failwith
                         "`PaOlbi (_loc, (`Lid (_loc, i)), p, `None)\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`PaOlbi (_loc, i, p, (`Some e)) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  _ 
                   (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, i, p, (`Some e)) : 'ipatt )))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword "=";
          `Stoken
            (((function | `Ant ("opt",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"opt\",_)"));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'patt_tcon)  _  _ \n     (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     match __fan_6 with\n     | `Ant ((\"opt\" as n),s) ->\n         (`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s)))) : 'ipatt )\n     | _ -> failwith \"`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s))))\n\")\n",
             (Gram.mk_action
                (fun _  (__fan_6 : [> FanToken.t])  _  (p : 'patt_tcon)  _  _
                    (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   match __fan_6 with
                   | `Ant (("opt" as n),s) ->
                       (`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s)))) : 
                       'ipatt )
                   | _ ->
                       failwith
                         "`PaOlbi (_loc, i, p, (`Ant (_loc, (mk_anti n s))))\n"))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'patt_tcon)  _  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, i, p, `None) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (p : 'patt_tcon)  _  _  (i : 'a_lident)  _ 
                   (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, i, p, `None) : 'ipatt )))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, i, (`Nil _loc), `None) : 'ipatt ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, i, (`Nil _loc), `None) : 'ipatt )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, (`Lid (_loc, \"\")), p, `None) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, (`Lid (_loc, "")), p, `None) : 'ipatt )))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->\n     (`PaOlbi (_loc, (`Lid (_loc, \"\")), p, (`Some e)) : 'ipatt ))\n",
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (`PaOlbi (_loc, (`Lid (_loc, "")), p, (`Some e)) : 
                   'ipatt )))))]));
   Gram.extend_single (sem_patt : 'sem_patt Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ("list",_) -> true | _ -> false)),
                (`Normal, "`Ant (\"list\",_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt;\" n s)) : 'sem_patt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt;\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"patt;" n s)) : 'sem_patt )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"patt;\" n s))\n"))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"; `Sself],
           ("Gram.mk_action\n  (fun (p2 : 'sem_patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, p2) : 'sem_patt ))\n",
             (Gram.mk_action
                (fun (p2 : 'sem_patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, p1, p2) : 'sem_patt )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"],
           ("Gram.mk_action (fun _  (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt ))\n",
             (Gram.mk_action
                (fun _  (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt )))))]));
   Gram.extend_single (sem_patt_for_list : 'sem_patt_for_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"; `Sself],
            ("Gram.mk_action\n  (fun (pl : 'sem_patt_for_list)  _  (p : 'patt)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App\n          (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, \"::\")))), p)),\n            (pl acc)) : 'sem_patt_for_list ))\n",
              (Gram.mk_action
                 (fun (pl : 'sem_patt_for_list)  _  (p : 'patt) 
                    (_loc : FanLoc.t)  ->
                    (fun acc  ->
                       `App
                         (_loc,
                           (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), p)),
                           (pl acc)) : 'sem_patt_for_list )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (p : 'patt)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, \"::\")))), p)), acc) : \n     'sem_patt_for_list ))\n",
             (Gram.mk_action
                (fun _  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App
                        (_loc,
                          (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), p)),
                          acc) : 'sem_patt_for_list )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'patt)  (_loc : FanLoc.t)  ->\n     (fun acc  ->\n        `App (_loc, (`App (_loc, (`Id (_loc, (`Uid (_loc, \"::\")))), p)), acc) : \n     'sem_patt_for_list ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      `App
                        (_loc,
                          (`App (_loc, (`Id (_loc, (`Uid (_loc, "::")))), p)),
                          acc) : 'sem_patt_for_list )))))]));
   Gram.extend_single (patt_tcon : 'patt_tcon Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (p : 'patt)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, p, t) : 'patt_tcon ))\n",
              (Gram.mk_action
                 (fun (t : 'ctyp)  _  (p : 'patt)  (_loc : FanLoc.t)  ->
                    (`Constraint (_loc, p, t) : 'patt_tcon )))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'patt_tcon ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'patt_tcon )))))]));
   Gram.extend_single (ipatt_tcon : 'ipatt_tcon Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt\" n s)) : 'ipatt_tcon )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"anti" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"patt" n s)) : 'ipatt_tcon )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n"))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (i :>ident)) : 'ipatt_tcon ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (i :>ident)) : 'ipatt_tcon )))));
         ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`Constraint (_loc, (`Id (_loc, (i :>ident))), t) : 'ipatt_tcon ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : FanLoc.t)  ->
                   (`Constraint (_loc, (`Id (_loc, (i :>ident))), t) : 
                   'ipatt_tcon )))))]));
   Gram.extend_single (comma_ipatt : 'comma_ipatt Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'comma_ipatt)  _  (p1 : 'comma_ipatt)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, p1, p2) : 'comma_ipatt ))\n",
              (Gram.mk_action
                 (fun (p2 : 'comma_ipatt)  _  (p1 : 'comma_ipatt) 
                    (_loc : FanLoc.t)  ->
                    (`Com (_loc, p1, p2) : 'comma_ipatt )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt,\" n s)) : 'comma_ipatt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt,\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"patt," n s)) : 'comma_ipatt )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"patt,\" n s))\n"))));
         ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
           ("Gram.mk_action (fun (p : 'ipatt)  (_loc : FanLoc.t)  -> (p : 'comma_ipatt ))\n",
             (Gram.mk_action
                (fun (p : 'ipatt)  (_loc : FanLoc.t)  -> (p : 'comma_ipatt )))))]));
   Gram.extend_single (comma_patt : 'comma_patt Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword ","; `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'comma_patt)  _  (p1 : 'comma_patt)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, p1, p2) : 'comma_patt ))\n",
              (Gram.mk_action
                 (fun (p2 : 'comma_patt)  _  (p1 : 'comma_patt) 
                    (_loc : FanLoc.t)  ->
                    (`Com (_loc, p1, p2) : 'comma_patt )))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt,\" n s)) : 'comma_patt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt,\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"patt," n s)) : 'comma_patt )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"patt,\" n s))\n"))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'comma_patt ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'comma_patt )))))]));
   Gram.extend_single (label_patt_list : 'label_patt_list Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
           `Skeyword ";";
           `Sself],
            ("Gram.mk_action\n  (fun (p2 : 'label_patt_list)  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, p2) : 'label_patt_list ))\n",
              (Gram.mk_action
                 (fun (p2 : 'label_patt_list)  _  (p1 : 'label_patt) 
                    (_loc : FanLoc.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_patt_list )))));
         ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
          `Skeyword ";";
          `Skeyword "_"],
           ("Gram.mk_action\n  (fun _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, (`Any _loc)) : 'label_patt_list ))\n",
             (Gram.mk_action
                (fun _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_patt_list )))));
         ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
          `Skeyword ";";
          `Skeyword "_";
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, p1, (`Any _loc)) : 'label_patt_list ))\n",
             (Gram.mk_action
                (fun _  _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_patt_list )))));
         ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (p1 : 'label_patt)  (_loc : FanLoc.t)  -> (p1 : 'label_patt_list ))\n",
             (Gram.mk_action
                (fun _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (p1 : 'label_patt_list )))));
         ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ))],
           ("Gram.mk_action\n  (fun (p1 : 'label_patt)  (_loc : FanLoc.t)  -> (p1 : 'label_patt_list ))\n",
             (Gram.mk_action
                (fun (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (p1 : 'label_patt_list )))))]));
   Gram.extend_single (label_patt : 'label_patt Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"pat"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"pat\"|\"anti\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"pat\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt\" n s)) : 'label_patt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"pat"|"anti" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"patt" n s)) : 'label_patt )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"patt\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.patt_tag : 'label_patt )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.patt_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.patt_tag : 
                       'label_patt )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.patt_tag\n"))));
         ([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"patt;\" n s)) : 'label_patt )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"patt;\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"patt;" n s)) : 'label_patt )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"patt;\" n s))\n"))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (patt : 'patt Gram.t ))],
           ("Gram.mk_action\n  (fun (p : 'patt)  _  (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`PaEq (_loc, i, p) : 'label_patt ))\n",
             (Gram.mk_action
                (fun (p : 'patt)  _  (i : 'label_longident) 
                   (_loc : FanLoc.t)  -> (`PaEq (_loc, i, p) : 'label_patt )))));
         ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->\n     (`PaEq (_loc, i, (`Id (_loc, (`Lid (_loc, (Ident.to_lid i)))))) : \n     'label_patt ))\n",
             (Gram.mk_action
                (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                   (`PaEq
                      (_loc, i,
                        (`Id (_loc, (`Lid (_loc, (Ident.to_lid i)))))) : 
                   'label_patt )))))])));
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
             (((function | `Ant (_,_) -> true | _ -> false)),
               (`Normal, "`Ant (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant (n,s) -> (`Ant (_loc, (mk_anti n s)) : 'astr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (n,s) -> (`Ant (_loc, (mk_anti n s)) : 'astr )
                   | _ -> failwith "`Ant (_loc, (mk_anti n s))\n"))))]));
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
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'ident_quot )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'ident_quot )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"lid\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"lid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'ident_quot )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'ident_quot )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (i : 'ident_quot)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), i) : 'ident_quot )\n     | _ ->\n         failwith\n           \"`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), i)\n\")\n",
             (Gram.mk_action
                (fun (i : 'ident_quot)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (`Dot
                          (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), i) : 
                       'ident_quot )
                   | _ ->
                       failwith
                         "`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), i)\n"))));
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
           ("Gram.mk_action\n  (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _  (_loc : FanLoc.t)  ->\n     (`App (_loc, i, j) : 'ident_quot ))\n",
             (Gram.mk_action
                (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                   (_loc : FanLoc.t)  -> (`App (_loc, i, j) : 'ident_quot )))))])]);
   Gram.extend_single (ident : 'ident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'ident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'ident )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
         ([`Stoken
             (((function | `Ant ("lid",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"lid\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"lid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'ident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("lid" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'ident )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (i : 'ident)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), i) : 'ident )\n     | _ ->\n         failwith\n           \"`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), i)\n\")\n",
             (Gram.mk_action
                (fun (i : 'ident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (`Dot
                          (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), i) : 
                       'ident )
                   | _ ->
                       failwith
                         "`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), i)\n"))));
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
            ("Gram.mk_action\n  (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'module_longident_dot_lparen )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident_dot_lparen )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (l : 'module_longident_dot_lparen)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"uid\"|\"\" as n),s) ->\n         (`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l) : 'module_longident_dot_lparen )\n     | _ ->\n         failwith\n           \"`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("uid"|"" as n),s) ->
                       (`Dot
                          (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), l) : 
                       'module_longident_dot_lparen )
                   | _ ->
                       failwith
                         "`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l)\n"))))]));
   Gram.extend_single (module_longident : 'module_longident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"id"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'module_longident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'module_longident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"uid" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
         ([`Stoken
             (((function | `Ant ((""|"uid"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l) : 'module_longident )\n     | _ ->\n         failwith\n           \"`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"uid" as n),s) ->
                       (`Dot
                          (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), l) : 
                       'module_longident )
                   | _ ->
                       failwith
                         "`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l)\n"))))]));
   Gram.extend
     (module_longident_with_app : 'module_longident_with_app Gram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             ("Gram.mk_action\n  (fun (j : 'module_longident_with_app)  (i : 'module_longident_with_app) \n     (_loc : FanLoc.t)  -> (`App (_loc, i, j) : 'module_longident_with_app ))\n",
               (Gram.mk_action
                  (fun (j : 'module_longident_with_app) 
                     (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                     (`App (_loc, i, j) : 'module_longident_with_app )))))]);
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
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'module_longident_with_app )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident_with_app )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
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
             ("Gram.mk_action\n  (fun (j : 'type_longident)  (i : 'type_longident)  (_loc : FanLoc.t)  ->\n     (`App (_loc, i, j) : 'type_longident ))\n",
               (Gram.mk_action
                  (fun (j : 'type_longident)  (i : 'type_longident) 
                     (_loc : FanLoc.t)  ->
                     (`App (_loc, i, j) : 'type_longident )))))]);
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
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"|\"lid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'type_longident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"uid"|"lid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'type_longident )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
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
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"id\"|\"anti\"|\"list\"|\"lid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ident\" n s)) : 'label_longident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"id"|"anti"|"list"|"lid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"ident" n s)) : 'label_longident )
                    | _ ->
                        failwith "`Ant (_loc, (mk_anti ~c:\"ident\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l) : 'label_longident )\n     | _ ->\n         failwith\n           \"`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l)\n\")\n",
             (Gram.mk_action
                (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"uid" as n),s) ->
                       (`Dot
                          (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s))), l) : 
                       'label_longident )
                   | _ ->
                       failwith
                         "`Dot (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))), l)\n"))))]));
   Gram.extend_single (class_type_longident : 'class_type_longident Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'type_longident)  (_loc : FanLoc.t)  ->\n     (x : 'class_type_longident ))\n",
              (Gram.mk_action
                 (fun (x : 'type_longident)  (_loc : FanLoc.t)  ->
                    (x : 'class_type_longident )))))]));
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
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Ant ((\"\"|\"override\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"override_flag\" n s)) : 'method_opt_override )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"override_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Ant ((""|"override"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"override_flag" n s)) : 
                       'method_opt_override )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"override_flag\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"!\"|\"override\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"override_flag\" n s)) : 'opt_override )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"override_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("!"|"override"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"override_flag" n s)) : 
                       'opt_override )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"override_flag\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Ant ((\"\"|\"override\"|\"anti\"|\"!\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"override_flag\" n s)) : 'value_val_opt_override )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"override_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Ant ((""|"override"|"anti"|"!" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"override_flag" n s)) : 
                       'value_val_opt_override )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"override_flag\" n s))\n"))));
         ([`Skeyword "val"],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (`OvNil _loc : 'value_val_opt_override ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (`OvNil _loc : 'value_val_opt_override )))))]));
   Gram.extend_single (opt_as_lident : 'opt_as_lident Gram.t )
     (None,
       (None, None,
         [([`Skeyword "as";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  -> (`Some i : 'opt_as_lident ))\n",
              (Gram.mk_action
                 (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                    (`Some i : 'opt_as_lident )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`None : 'opt_as_lident ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`None : 'opt_as_lident )))));
         ([`Stoken
             (((function | `Ant ((""|"as"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"as\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"as\" as n),s) ->\n         (`Ant (_loc, (mk_anti n s)) : 'opt_as_lident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"as" as n),s) ->
                       (`Ant (_loc, (mk_anti n s)) : 'opt_as_lident )
                   | _ -> failwith "`Ant (_loc, (mk_anti n s))\n"))))]));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"to\"|\"anti\"|\"\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"direction_flag\" n s)) : 'direction_flag )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"direction_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("to"|"anti"|"" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"direction_flag" n s)) : 
                       'direction_flag )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"direction_flag\" n s))\n"))))]));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"private\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"private_flag\" n s)) : 'opt_private )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"private_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("private"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"private_flag" n s)) : 
                       'opt_private )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"private_flag\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"mutable\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"mutable_flag\" n s)) : 'opt_mutable )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"mutable_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("mutable"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"mutable_flag" n s)) : 
                       'opt_mutable )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"mutable_flag\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"virtual\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"virtual_flag\" n s)) : 'opt_virtual )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"virtual_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("virtual"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"virtual_flag" n s)) : 
                       'opt_virtual )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"virtual_flag\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"..\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"row_var_flag\" n s)) : 'opt_dot_dot )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"row_var_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((".."|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"row_var_flag" n s)) : 
                       'opt_dot_dot )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"row_var_flag\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"rec\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"rec_flag\" n s)) : 'opt_rec )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"rec_flag\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("rec"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"rec_flag" n s)) : 'opt_rec )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"rec_flag\" n s))\n"))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`ReNil _loc : 'opt_rec ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`ReNil _loc : 'opt_rec )))))]));
   Gram.extend_single (a_string : 'a_string Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"lid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"lid\" as n),s) -> (`Ant (_loc, (mk_anti n s)) : 'a_string )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"lid" as n),s) ->
                        (`Ant (_loc, (mk_anti n s)) : 'a_string )
                    | _ -> failwith "`Ant (_loc, (mk_anti n s))\n"))));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid s -> (`C (_loc, s) : 'a_string )\n     | _ -> failwith \"`C (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid s -> (`C (_loc, s) : 'a_string )
                   | _ -> failwith "`C (_loc, s)\n"))));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid s -> (`C (_loc, s) : 'a_string )\n     | _ -> failwith \"`C (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid s -> (`C (_loc, s) : 'a_string )
                   | _ -> failwith "`C (_loc, s)\n"))))]));
   Gram.extend_single (a_lident : 'a_lident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Ant ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"lid\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"lid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"a_lident\" n s)) : 'a_lident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"a_lident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"lid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"a_lident" n s)) : 'a_lident )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"a_lident\" n s))\n"))));
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
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"uid\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"a_uident\" n s)) : 'a_uident )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"a_uident\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"uid" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"a_uident" n s)) : 'a_uident )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"a_uident\" n s))\n"))));
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
              (((function | `Ant ((""|"str_list"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"str_list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"str_list\"),s) ->\n         (`Ant (_loc, (mk_anti \"str_list\" s)) : 'string_list )\n     | _ -> failwith \"`Ant (_loc, (mk_anti \"str_list\" s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"str_list"),s) ->
                        (`Ant (_loc, (mk_anti "str_list" s)) : 'string_list )
                    | _ -> failwith "`Ant (_loc, (mk_anti \"str_list\" s))\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"));
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,x) -> (`LCons (x, xs) : 'string_list )\n     | _ -> failwith \"`LCons (x, xs)\n\")\n",
             (Gram.mk_action
                (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,x) -> (`LCons (x, xs) : 'string_list )
                   | _ -> failwith "`LCons (x, xs)\n"))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,x) -> (`LCons (x, `LNil) : 'string_list )\n     | _ -> failwith \"`LCons (x, `LNil)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,x) -> (`LCons (x, `LNil) : 'string_list )
                   | _ -> failwith "`LCons (x, `LNil)\n"))))]));
   Gram.extend_single (semi : 'semi Gram.t )
     (None,
       (None, None,
         [([`Skeyword ";"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'semi ))\n",
              (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'semi )))))]));
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
   Gram.extend_single (patt_eoi : 'patt_eoi Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  (x : 'patt)  (_loc : FanLoc.t)  ->\n     match __fan_1 with | `EOI -> (x : 'patt_eoi ) | _ -> failwith \"x\n\")\n",
              (Gram.mk_action
                 (fun (__fan_1 : [> FanToken.t])  (x : 'patt) 
                    (_loc : FanLoc.t)  ->
                    match __fan_1 with
                    | `EOI -> (x : 'patt_eoi )
                    | _ -> failwith "x\n"))))]));
   Gram.extend_single (expr_eoi : 'expr_eoi Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  (x : 'expr)  (_loc : FanLoc.t)  ->\n     match __fan_1 with | `EOI -> (x : 'expr_eoi ) | _ -> failwith \"x\n\")\n",
              (Gram.mk_action
                 (fun (__fan_1 : [> FanToken.t])  (x : 'expr) 
                    (_loc : FanLoc.t)  ->
                    match __fan_1 with
                    | `EOI -> (x : 'expr_eoi )
                    | _ -> failwith "x\n"))))])));
  (Gram.extend_single (implem : 'implem Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
           `Skeyword ";;"],
            ("Gram.mk_action\n  (fun _  (dp : 'opt_expr)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (([`Directive (_loc, n, dp)], (Some _loc)) : 'implem ))\n",
              (Gram.mk_action
                 (fun _  (dp : 'opt_expr)  (n : 'a_lident)  _ 
                    (_loc : FanLoc.t)  ->
                    (([`Directive (_loc, n, dp)], (Some _loc)) : 'implem )))));
         ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun ((sil,stopped) : 'implem)  _  (si : 'str_item)  (_loc : FanLoc.t)  ->\n     (((si :: sil), stopped) : 'implem ))\n",
             (Gram.mk_action
                (fun ((sil,stopped) : 'implem)  _  (si : 'str_item) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'implem )))));
         ([`Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `EOI -> (([], None) : 'implem )\n     | _ -> failwith \"([], None)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `EOI -> (([], None) : 'implem )
                   | _ -> failwith "([], None)\n"))))]));
   Gram.extend_single (str_items : 'str_items Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"stri"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"stri\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti n ~c:\"str_item\" s)) : 'str_items )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n ~c:\"str_item\" s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti n ~c:"str_item" s)) : 'str_items )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti n ~c:\"str_item\" s))\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"stri"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (st : 'str_items)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"stri\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (`Ant (_loc, (mk_anti n ~c:\"str_item\" s))), st) : \n         'str_items )\n     | _ ->\n         failwith\n           \"`Sem (_loc, (`Ant (_loc, (mk_anti n ~c:\"str_item\" s))), st)\n\")\n",
             (Gram.mk_action
                (fun (st : 'str_items)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
                       (`Sem
                          (_loc, (`Ant (_loc, (mk_anti n ~c:"str_item" s))),
                            st) : 'str_items )
                   | _ ->
                       failwith
                         "`Sem (_loc, (`Ant (_loc, (mk_anti n ~c:\"str_item\" s))), st)\n"))));
         ([`Slist0
             (Gram.srules
                [([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   ("Gram.mk_action (fun _  (st : 'str_item)  (_loc : FanLoc.t)  -> (st : 'e__5 ))\n",
                     (Gram.mk_action
                        (fun _  (st : 'str_item)  (_loc : FanLoc.t)  ->
                           (st : 'e__5 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__5 list)  (_loc : FanLoc.t)  ->\n     (FanAst.sem_of_list l : 'str_items ))\n",
             (Gram.mk_action
                (fun (l : 'e__5 list)  (_loc : FanLoc.t)  ->
                   (FanAst.sem_of_list l : 'str_items )))))]));
   Gram.extend_single (top_phrase : 'top_phrase Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
           `Skeyword ";;"],
            ("Gram.mk_action\n  (fun _  (dp : 'opt_expr)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (Some (`Directive (_loc, n, dp)) : 'top_phrase ))\n",
              (Gram.mk_action
                 (fun _  (dp : 'opt_expr)  (n : 'a_lident)  _ 
                    (_loc : FanLoc.t)  ->
                    (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
         ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
          `Snterm (Gram.obj (semi : 'semi Gram.t ))],
           ("Gram.mk_action\n  (fun _  (st : 'str_item)  (_loc : FanLoc.t)  -> (Some st : 'top_phrase ))\n",
             (Gram.mk_action
                (fun _  (st : 'str_item)  (_loc : FanLoc.t)  ->
                   (Some st : 'top_phrase )))));
         ([`Stoken
             (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `EOI -> (None : 'top_phrase )\n     | _ -> failwith \"None\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `EOI -> (None : 'top_phrase )
                   | _ -> failwith "None\n"))))]));
   Gram.extend_single (str_item_quot : 'str_item_quot Gram.t )
     (None,
       (None, None,
         [([`Skeyword "#";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (dp : 'opt_expr)  (n : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Directive (_loc, n, dp) : 'str_item_quot ))\n",
              (Gram.mk_action
                 (fun (dp : 'opt_expr)  (n : 'a_lident)  _  (_loc : FanLoc.t)
                     -> (`Directive (_loc, n, dp) : 'str_item_quot )))));
         ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (st2 : 'str_item_quot)  _  (st1 : 'str_item)  (_loc : FanLoc.t)  ->\n     (match st2 with | `Nil _loc -> st1 | _ -> `Sem (_loc, st1, st2) : \n     'str_item_quot ))\n",
             (Gram.mk_action
                (fun (st2 : 'str_item_quot)  _  (st1 : 'str_item) 
                   (_loc : FanLoc.t)  ->
                   (match st2 with
                    | `Nil _loc -> st1
                    | _ -> `Sem (_loc, st1, st2) : 'str_item_quot )))));
         ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ))],
           ("Gram.mk_action\n  (fun (st : 'str_item)  (_loc : FanLoc.t)  -> (st : 'str_item_quot ))\n",
             (Gram.mk_action
                (fun (st : 'str_item)  (_loc : FanLoc.t)  ->
                   (st : 'str_item_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'str_item_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'str_item_quot )))))]));
   Gram.extend (str_item : 'str_item Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "exception";
            `Snterm
              (Gram.obj
                 (constructor_declaration : 'constructor_declaration Gram.t ))],
             ("Gram.mk_action\n  (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Exception (_loc, t) : 'str_item ))\n",
               (Gram.mk_action
                  (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t) 
                     -> (`Exception (_loc, t) : 'str_item )))));
          ([`Skeyword "external";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (string_list : 'string_list Gram.t ))],
            ("Gram.mk_action\n  (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)  _ \n     (_loc : FanLoc.t)  -> (`External (_loc, i, t, sl) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_lident)
                     _  (_loc : FanLoc.t)  ->
                    (`External (_loc, i, t, sl) : 'str_item )))));
          ([`Skeyword "include";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->\n     (`Include (_loc, me) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                    (`Include (_loc, me) : 'str_item )))));
          ([`Skeyword "module";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ))],
            ("Gram.mk_action\n  (fun (mb : 'module_binding0)  (i : 'a_uident)  _  (_loc : FanLoc.t)  ->\n     (`Module (_loc, i, mb) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (mb : 'module_binding0)  (i : 'a_uident)  _ 
                    (_loc : FanLoc.t)  ->
                    (`Module (_loc, i, mb) : 'str_item )))));
          ([`Skeyword "module";
           `Skeyword "rec";
           `Snterm (Gram.obj (module_binding : 'module_binding Gram.t ))],
            ("Gram.mk_action\n  (fun (mb : 'module_binding)  _  _  (_loc : FanLoc.t)  ->\n     (`RecModule (_loc, mb) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (mb : 'module_binding)  _  _  (_loc : FanLoc.t)  ->
                    (`RecModule (_loc, mb) : 'str_item )))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            ("Gram.mk_action\n  (fun (mt : 'module_type)  _  (i : 'a_uident)  _  _  (_loc : FanLoc.t)  ->\n     (`ModuleType (_loc, i, mt) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (mt : 'module_type)  _  (i : 'a_uident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`ModuleType (_loc, i, mt) : 'str_item )))));
          ([`Skeyword "import";
           `Snterm (Gram.obj (dot_namespace : 'dot_namespace Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'dot_namespace)  _  (_loc : FanLoc.t)  ->\n     (FanToken.paths := ((`Absolute x) :: (FanToken.paths.contents));\n      `Nil _loc : 'str_item ))\n",
              (Gram.mk_action
                 (fun (x : 'dot_namespace)  _  (_loc : FanLoc.t)  ->
                    (FanToken.paths := ((`Absolute x) ::
                       (FanToken.paths.contents));
                     `Nil _loc : 'str_item )))));
          ([`Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->\n     (`Open (_loc, i) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                    (`Open (_loc, i) : 'str_item )))));
          ([`Skeyword "type";
           `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
            ("Gram.mk_action\n  (fun (td : 'type_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Type (_loc, td) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (td : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                    (`Type (_loc, td) : 'str_item )))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) \n     -> (`StExp (_loc, (`LetIn (_loc, r, bi, x))) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                    (_loc : FanLoc.t)  ->
                    (`StExp (_loc, (`LetIn (_loc, r, bi, x))) : 'str_item )))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ))],
            ("Gram.mk_action\n  (fun (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t)  ->\n     (match bi with\n      | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n      | _ -> `Value (_loc, r, bi) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) 
                    ->
                    (match bi with
                     | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                     | _ -> `Value (_loc, r, bi) : 'str_item )))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (mb : 'module_binding0)  (m : 'a_uident)  _  _ \n     (_loc : FanLoc.t)  ->\n     (`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (mb : 'module_binding0) 
                    (m : 'a_uident)  _  _  (_loc : FanLoc.t)  ->
                    (`StExp (_loc, (`LetModule (_loc, m, mb, e))) : 'str_item )))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'expr)  _  (i : 'module_longident)  _  _  (_loc : FanLoc.t)  ->\n     (`StExp (_loc, (`LetOpen (_loc, i, e))) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  _  (i : 'module_longident)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`StExp (_loc, (`LetOpen (_loc, i, e))) : 'str_item )))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            ("Gram.mk_action\n  (fun (a : 'match_case)  _  (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec) \n     _  _  (_loc : FanLoc.t)  ->\n     (`StExp\n        (_loc,\n          (`App\n             (_loc,\n               (`Try\n                  (_loc,\n                    (`LetIn\n                       (_loc, r, bi,\n                         (`Fun\n                            (_loc,\n                              (`Case\n                                 (_loc, (`Id (_loc, (`Uid (_loc, \"()\")))),\n                                   (`Nil _loc), x)))))),\n                    (FanAst.match_pre#match_case a))),\n               (`Id (_loc, (`Uid (_loc, \"()\"))))))) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (a : 'match_case)  _  (x : 'expr)  _  (bi : 'binding) 
                    (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->
                    (`StExp
                       (_loc,
                         (`App
                            (_loc,
                              (`Try
                                 (_loc,
                                   (`LetIn
                                      (_loc, r, bi,
                                        (`Fun
                                           (_loc,
                                             (`Case
                                                (_loc,
                                                  (`Id
                                                     (_loc,
                                                       (`Uid (_loc, "()")))),
                                                  (`Nil _loc), x)))))),
                                   (FanAst.match_pre#match_case a))),
                              (`Id (_loc, (`Uid (_loc, "()"))))))) : 
                    'str_item )))));
          ([`Skeyword "class";
           `Snterm
             (Gram.obj (class_declaration : 'class_declaration Gram.t ))],
            ("Gram.mk_action\n  (fun (cd : 'class_declaration)  _  (_loc : FanLoc.t)  ->\n     (`Class (_loc, cd) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (cd : 'class_declaration)  _  (_loc : FanLoc.t)  ->
                    (`Class (_loc, cd) : 'str_item )))));
          ([`Skeyword "class";
           `Skeyword "type";
           `Snterm
             (Gram.obj
                (class_type_declaration : 'class_type_declaration Gram.t ))],
            ("Gram.mk_action\n  (fun (ctd : 'class_type_declaration)  _  _  (_loc : FanLoc.t)  ->\n     (`ClassType (_loc, ctd) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (ctd : 'class_type_declaration)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (`ClassType (_loc, ctd) : 'str_item )))));
          ([`Stoken
              (((function
                 | `Ant ((""|"stri"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"stri\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"str_item\" n s)) : 'str_item )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"str_item\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"stri"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"str_item" n s)) : 'str_item )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"str_item\" n s))\n"))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.str_item_tag : 'str_item )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.str_item_tag\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `QUOTATION x ->
                        (AstQuotation.expand _loc x DynAst.str_item_tag : 
                        'str_item )
                    | _ ->
                        failwith
                          "AstQuotation.expand _loc x DynAst.str_item_tag\n"))));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'expr)  (_loc : FanLoc.t)  -> (`StExp (_loc, e) : 'str_item ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                    (`StExp (_loc, e) : 'str_item )))))])]));
  (Gram.extend_single (class_sig_item_quot : 'class_sig_item_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (class_sig_item : 'class_sig_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (x2 : 'class_sig_item_quot)  _  (x1 : 'class_sig_item) \n     (_loc : FanLoc.t)  ->\n     (match x2 with | `Nil _loc -> x1 | _ -> `Sem (_loc, x1, x2) : 'class_sig_item_quot ))\n",
              (Gram.mk_action
                 (fun (x2 : 'class_sig_item_quot)  _  (x1 : 'class_sig_item) 
                    (_loc : FanLoc.t)  ->
                    (match x2 with
                     | `Nil _loc -> x1
                     | _ -> `Sem (_loc, x1, x2) : 'class_sig_item_quot )))));
         ([`Snterm (Gram.obj (class_sig_item : 'class_sig_item Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'class_sig_item)  (_loc : FanLoc.t)  ->\n     (x : 'class_sig_item_quot ))\n",
             (Gram.mk_action
                (fun (x : 'class_sig_item)  (_loc : FanLoc.t)  ->
                   (x : 'class_sig_item_quot )))));
         ([],
           ("Gram.mk_action\n  (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'class_sig_item_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  ->
                   (`Nil _loc : 'class_sig_item_quot )))))]));
   Gram.extend_single (class_signature : 'class_signature Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"csg"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"csg\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s)) : 'class_signature )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"csg"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"class_sig_item" n s)) : 
                        'class_signature )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"csg"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (csg : 'class_signature)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"csg\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))), csg) : \n         'class_signature )\n     | _ ->\n         failwith\n           \"`Sem (_loc, (`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))), csg)\n\")\n",
             (Gram.mk_action
                (fun (csg : 'class_signature)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"csg"|"anti"|"list" as n),s) ->
                       (`Sem
                          (_loc,
                            (`Ant (_loc, (mk_anti ~c:"class_sig_item" n s))),
                            csg) : 'class_signature )
                   | _ ->
                       failwith
                         "`Sem (_loc, (`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))), csg)\n"))));
         ([`Slist0
             (Gram.srules
                [([`Snterm
                     (Gram.obj (class_sig_item : 'class_sig_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   ("Gram.mk_action\n  (fun _  (csg : 'class_sig_item)  (_loc : FanLoc.t)  -> (csg : 'e__6 ))\n",
                     (Gram.mk_action
                        (fun _  (csg : 'class_sig_item)  (_loc : FanLoc.t) 
                           -> (csg : 'e__6 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__6 list)  (_loc : FanLoc.t)  ->\n     (FanAst.sem_of_list l : 'class_signature ))\n",
             (Gram.mk_action
                (fun (l : 'e__6 list)  (_loc : FanLoc.t)  ->
                   (FanAst.sem_of_list l : 'class_signature )))))]));
   Gram.extend_single (class_sig_item : 'class_sig_item Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"csg"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"csg\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s)) : 'class_sig_item )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"csg"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"class_sig_item" n s)) : 
                        'class_sig_item )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"class_sig_item\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_sig_item_tag : 'class_sig_item )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_sig_item_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.class_sig_item_tag : 
                       'class_sig_item )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.class_sig_item_tag\n"))));
         ([`Skeyword "inherit";
          `Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
           ("Gram.mk_action\n  (fun (cs : 'class_type)  _  (_loc : FanLoc.t)  ->\n     (`SigInherit (_loc, cs) : 'class_sig_item ))\n",
             (Gram.mk_action
                (fun (cs : 'class_type)  _  (_loc : FanLoc.t)  ->
                   (`SigInherit (_loc, cs) : 'class_sig_item )))));
         ([`Skeyword "val";
          `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
          `Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) \n     (mf : 'opt_mutable)  _  (_loc : FanLoc.t)  ->\n     (`CgVal (_loc, l, mf, mv, t) : 'class_sig_item ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mv : 'opt_virtual) 
                   (mf : 'opt_mutable)  _  (_loc : FanLoc.t)  ->
                   (`CgVal (_loc, l, mf, mv, t) : 'class_sig_item )))));
         ([`Skeyword "method";
          `Skeyword "virtual";
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _  _ \n     (_loc : FanLoc.t)  -> (`CgVir (_loc, l, pf, t) : 'class_sig_item ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   _  (_loc : FanLoc.t)  ->
                   (`CgVir (_loc, l, pf, t) : 'class_sig_item )))));
         ([`Skeyword "method";
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ \n     (_loc : FanLoc.t)  -> (`Method (_loc, l, pf, t) : 'class_sig_item ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (_loc : FanLoc.t)  ->
                   (`Method (_loc, l, pf, t) : 'class_sig_item )))));
         ([`Skeyword "constraint";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, t1, t2) : 'class_sig_item ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, t1, t2) : 'class_sig_item )))))])));
  (Gram.extend_single (class_structure : 'class_structure Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"cst"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cst\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s)) : 'class_structure )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"cst"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"class_str_item" n s)) : 
                        'class_structure )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))\n"))));
         ([`Stoken
             (((function
                | `Ant ((""|"cst"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"));
          `Snterm (Gram.obj (semi : 'semi Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (cst : 'class_structure)  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cst\"|\"anti\"|\"list\" as n),s) ->\n         (`Sem (_loc, (`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))), cst) : \n         'class_structure )\n     | _ ->\n         failwith\n           \"`Sem (_loc, (`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))), cst)\n\")\n",
             (Gram.mk_action
                (fun (cst : 'class_structure)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"cst"|"anti"|"list" as n),s) ->
                       (`Sem
                          (_loc,
                            (`Ant (_loc, (mk_anti ~c:"class_str_item" n s))),
                            cst) : 'class_structure )
                   | _ ->
                       failwith
                         "`Sem (_loc, (`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))), cst)\n"))));
         ([`Slist0
             (Gram.srules
                [([`Snterm
                     (Gram.obj (class_str_item : 'class_str_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   ("Gram.mk_action\n  (fun _  (cst : 'class_str_item)  (_loc : FanLoc.t)  -> (cst : 'e__7 ))\n",
                     (Gram.mk_action
                        (fun _  (cst : 'class_str_item)  (_loc : FanLoc.t) 
                           -> (cst : 'e__7 )))))])],
           ("Gram.mk_action\n  (fun (l : 'e__7 list)  (_loc : FanLoc.t)  ->\n     (FanAst.sem_of_list l : 'class_structure ))\n",
             (Gram.mk_action
                (fun (l : 'e__7 list)  (_loc : FanLoc.t)  ->
                   (FanAst.sem_of_list l : 'class_structure )))))]));
   Gram.extend_single (class_str_item : 'class_str_item Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function
                 | `Ant ((""|"cst"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cst\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s)) : 'class_str_item )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"cst"|"anti"|"list" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"class_str_item" n s)) : 
                        'class_str_item )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"class_str_item\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_str_item_tag : 'class_str_item )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_str_item_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.class_str_item_tag : 
                       'class_str_item )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.class_str_item_tag\n"))));
         ([`Skeyword "inherit";
          `Snterm (Gram.obj (opt_override : 'opt_override Gram.t ));
          `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ));
          `Snterm (Gram.obj (opt_as_lident : 'opt_as_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (pb : 'opt_as_lident)  (ce : 'class_expr)  (o : 'opt_override)  _ \n     (_loc : FanLoc.t)  -> (`Inherit (_loc, o, ce, pb) : 'class_str_item ))\n",
             (Gram.mk_action
                (fun (pb : 'opt_as_lident)  (ce : 'class_expr) 
                   (o : 'opt_override)  _  (_loc : FanLoc.t)  ->
                   (`Inherit (_loc, o, ce, pb) : 'class_str_item )))));
         ([`Snterm
             (Gram.obj
                (value_val_opt_override : 'value_val_opt_override Gram.t ));
          `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (cvalue_binding : 'cvalue_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'cvalue_binding)  (lab : 'a_lident)  (mf : 'opt_mutable) \n     (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->\n     (`CrVal (_loc, lab, o, mf, e) : 'class_str_item ))\n",
             (Gram.mk_action
                (fun (e : 'cvalue_binding)  (lab : 'a_lident) 
                   (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                   (_loc : FanLoc.t)  ->
                   (`CrVal (_loc, lab, o, mf, e) : 'class_str_item )))));
         ([`Snterm
             (Gram.obj
                (value_val_opt_override : 'value_val_opt_override Gram.t ));
          `Skeyword "virtual";
          `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (mf : 'opt_mutable)  _ \n     (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->\n     (match o with\n      | `OvNil _ -> `CrVvr (_loc, l, mf, t)\n      | _ ->\n          raise (XStream.Error \"override (!) is incompatible with virtual\") : \n     'class_str_item ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (mf : 'opt_mutable)  _ 
                   (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->
                   (match o with
                    | `OvNil _ -> `CrVvr (_loc, l, mf, t)
                    | _ ->
                        raise
                          (XStream.Error
                             "override (!) is incompatible with virtual") : 
                   'class_str_item )))));
         ([`Snterm
             (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
          `Skeyword "virtual";
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ \n     (o : 'method_opt_override)  (_loc : FanLoc.t)  ->\n     (match o with\n      | `OvNil _ -> `CrVir (_loc, l, pf, t)\n      | _ ->\n          raise (XStream.Error \"override (!) is incompatible with virtual\") : \n     'class_str_item ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (l : 'a_lident)  (pf : 'opt_private)  _ 
                   (o : 'method_opt_override)  (_loc : FanLoc.t)  ->
                   (match o with
                    | `OvNil _ -> `CrVir (_loc, l, pf, t)
                    | _ ->
                        raise
                          (XStream.Error
                             "override (!) is incompatible with virtual") : 
                   'class_str_item )))));
         ([`Snterm
             (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
          `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
          `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Snterm (Gram.obj (opt_polyt : 'opt_polyt Gram.t ));
          `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'fun_binding)  (topt : 'opt_polyt)  (l : 'a_lident) \n     (pf : 'opt_private)  (o : 'method_opt_override)  (_loc : FanLoc.t)  ->\n     (`CrMth (_loc, l, o, pf, e, topt) : 'class_str_item ))\n",
             (Gram.mk_action
                (fun (e : 'fun_binding)  (topt : 'opt_polyt)  (l : 'a_lident)
                    (pf : 'opt_private)  (o : 'method_opt_override) 
                   (_loc : FanLoc.t)  ->
                   (`CrMth (_loc, l, o, pf, e, topt) : 'class_str_item )))));
         ([`Skeyword "constraint";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Eq (_loc, t1, t2) : 'class_str_item ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, t1, t2) : 'class_str_item )))));
         ([`Skeyword "initializer";
          `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           ("Gram.mk_action\n  (fun (se : 'expr)  _  (_loc : FanLoc.t)  ->\n     (`Initializer (_loc, se) : 'class_str_item ))\n",
             (Gram.mk_action
                (fun (se : 'expr)  _  (_loc : FanLoc.t)  ->
                   (`Initializer (_loc, se) : 'class_str_item )))))]));
   Gram.extend_single (class_str_item_quot : 'class_str_item_quot Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (class_str_item : 'class_str_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            ("Gram.mk_action\n  (fun (x2 : 'class_str_item_quot)  _  (x1 : 'class_str_item) \n     (_loc : FanLoc.t)  ->\n     (match x2 with | `Nil _loc -> x1 | _ -> `Sem (_loc, x1, x2) : 'class_str_item_quot ))\n",
              (Gram.mk_action
                 (fun (x2 : 'class_str_item_quot)  _  (x1 : 'class_str_item) 
                    (_loc : FanLoc.t)  ->
                    (match x2 with
                     | `Nil _loc -> x1
                     | _ -> `Sem (_loc, x1, x2) : 'class_str_item_quot )))));
         ([`Snterm (Gram.obj (class_str_item : 'class_str_item Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'class_str_item)  (_loc : FanLoc.t)  ->\n     (x : 'class_str_item_quot ))\n",
             (Gram.mk_action
                (fun (x : 'class_str_item)  (_loc : FanLoc.t)  ->
                   (x : 'class_str_item_quot )))));
         ([],
           ("Gram.mk_action\n  (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'class_str_item_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  ->
                   (`Nil _loc : 'class_str_item_quot )))))])));
  (Gram.extend_single (class_expr_quot : 'class_expr_quot Gram.t )
     (None,
       (None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            ("Gram.mk_action\n  (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) \n     (_loc : FanLoc.t)  -> (`And (_loc, ce1, ce2) : 'class_expr_quot ))\n",
              (Gram.mk_action
                 (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) 
                    (_loc : FanLoc.t)  ->
                    (`And (_loc, ce1, ce2) : 'class_expr_quot )))));
         ([`Sself; `Skeyword "="; `Sself],
           ("Gram.mk_action\n  (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) \n     (_loc : FanLoc.t)  -> (`Eq (_loc, ce1, ce2) : 'class_expr_quot ))\n",
             (Gram.mk_action
                (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) 
                   (_loc : FanLoc.t)  ->
                   (`Eq (_loc, ce1, ce2) : 'class_expr_quot )))));
         ([`Skeyword "virtual";
          `Snterm
            (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
           ("Gram.mk_action\n  (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t)  ->\n     (`CeCon (_loc, (`Virtual _loc), (i :>ident), ot) : 'class_expr_quot ))\n",
             (Gram.mk_action
                (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t) 
                   ->
                   (`CeCon (_loc, (`Virtual _loc), (i :>ident), ot) : 
                   'class_expr_quot )))));
         ([`Stoken
             (((function | `Ant ("virtual",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"virtual\",_)"));
          `Snterm (Gram.obj (ident : 'ident Gram.t ));
          `Snterm (Gram.obj (opt_comma_ctyp : 'opt_comma_ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (ot : 'opt_comma_ctyp)  (i : 'ident)  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"virtual\" as n),s) ->\n         (let anti = `Ant (_loc, (mk_anti ~c:\"class_expr\" n s)) in\n          `CeCon (_loc, anti, i, ot) : 'class_expr_quot )\n     | _ ->\n         failwith\n           \"let anti = `Ant (_loc, (mk_anti ~c:\"class_expr\" n s)) in\n`CeCon (_loc, anti, i, ot)\n\")\n",
             (Gram.mk_action
                (fun (ot : 'opt_comma_ctyp)  (i : 'ident) 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("virtual" as n),s) ->
                       (let anti = `Ant (_loc, (mk_anti ~c:"class_expr" n s)) in
                        `CeCon (_loc, anti, i, ot) : 'class_expr_quot )
                   | _ ->
                       failwith
                         "let anti = `Ant (_loc, (mk_anti ~c:\"class_expr\" n s)) in\n`CeCon (_loc, anti, i, ot)\n"))));
         ([`Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (x : 'class_expr)  (_loc : FanLoc.t)  -> (x : 'class_expr_quot ))\n",
             (Gram.mk_action
                (fun (x : 'class_expr)  (_loc : FanLoc.t)  ->
                   (x : 'class_expr_quot )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'class_expr_quot ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'class_expr_quot )))))]));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cdcl\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_expr\" n s)) : 'class_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_expr\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"cdcl"|"anti"|"list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"class_expr" n s)) : 
                       'class_declaration )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"class_expr\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_expr_tag : 'class_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_expr_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.class_expr_tag : 
                       'class_declaration )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.class_expr_tag\n"))));
         ([`Snterm
             (Gram.obj
                (class_info_for_class_expr : 'class_info_for_class_expr
                                               Gram.t ));
          `Snterm (Gram.obj (class_fun_binding : 'class_fun_binding Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_fun_binding)  (ci : 'class_info_for_class_expr) \n     (_loc : FanLoc.t)  -> (`Eq (_loc, ci, ce) : 'class_declaration ))\n",
             (Gram.mk_action
                (fun (ce : 'class_fun_binding) 
                   (ci : 'class_info_for_class_expr)  (_loc : FanLoc.t)  ->
                   (`Eq (_loc, ci, ce) : 'class_declaration )))))]));
   Gram.extend_single (class_fun_binding : 'class_fun_binding Gram.t )
     (None,
       (None, None,
         [([`Skeyword "=";
           `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
            ("Gram.mk_action\n  (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->\n     (ce : 'class_fun_binding ))\n",
              (Gram.mk_action
                 (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                    (ce : 'class_fun_binding )))));
         ([`Skeyword ":";
          `Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_expr)  _  (ct : 'class_type_plus)  _  (_loc : FanLoc.t) \n     -> (`CeTyc (_loc, ce, ct) : 'class_fun_binding ))\n",
             (Gram.mk_action
                (fun (ce : 'class_expr)  _  (ct : 'class_type_plus)  _ 
                   (_loc : FanLoc.t)  ->
                   (`CeTyc (_loc, ce, ct) : 'class_fun_binding )))));
         ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t )); `Sself],
           ("Gram.mk_action\n  (fun (cfb : 'class_fun_binding)  (p : 'ipatt)  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, cfb) : 'class_fun_binding ))\n",
             (Gram.mk_action
                (fun (cfb : 'class_fun_binding)  (p : 'ipatt) 
                   (_loc : FanLoc.t)  ->
                   (`CeFun (_loc, p, cfb) : 'class_fun_binding )))))]));
   Gram.extend_single
     (class_info_for_class_expr : 'class_info_for_class_expr Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
           `Snterm
             (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
            ("Gram.mk_action\n  (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) \n     (_loc : FanLoc.t)  ->\n     (`CeCon (_loc, mv, (i :>ident), ot) : 'class_info_for_class_expr ))\n",
              (Gram.mk_action
                 (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) 
                    (_loc : FanLoc.t)  ->
                    (`CeCon (_loc, mv, (i :>ident), ot) : 'class_info_for_class_expr )))))]));
   Gram.extend_single (class_fun_def : 'class_fun_def Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t )); `Sself],
            ("Gram.mk_action\n  (fun (ce : 'class_fun_def)  (p : 'ipatt)  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, ce) : 'class_fun_def ))\n",
              (Gram.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipatt)  (_loc : FanLoc.t) 
                    -> (`CeFun (_loc, p, ce) : 'class_fun_def )))));
         ([`Skeyword "->";
          `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  -> (ce : 'class_fun_def ))\n",
             (Gram.mk_action
                (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                   (ce : 'class_fun_def )))))]));
   Gram.extend (class_expr : 'class_expr Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "fun";
            `Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
            `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
             ("Gram.mk_action\n  (fun (ce : 'class_fun_def)  (p : 'ipatt)  _  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, ce) : 'class_expr ))\n",
               (Gram.mk_action
                  (fun (ce : 'class_fun_def)  (p : 'ipatt)  _ 
                     (_loc : FanLoc.t)  ->
                     (`CeFun (_loc, p, ce) : 'class_expr )))));
          ([`Skeyword "function";
           `Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
           `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
            ("Gram.mk_action\n  (fun (ce : 'class_fun_def)  (p : 'ipatt)  _  (_loc : FanLoc.t)  ->\n     (`CeFun (_loc, p, ce) : 'class_expr ))\n",
              (Gram.mk_action
                 (fun (ce : 'class_fun_def)  (p : 'ipatt)  _ 
                    (_loc : FanLoc.t)  ->
                    (`CeFun (_loc, p, ce) : 'class_expr )))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Sself],
            ("Gram.mk_action\n  (fun (ce : 'class_expr)  _  (bi : 'binding)  (rf : 'opt_rec)  _ \n     (_loc : FanLoc.t)  -> (`CeLet (_loc, rf, bi, ce) : 'class_expr ))\n",
              (Gram.mk_action
                 (fun (ce : 'class_expr)  _  (bi : 'binding)  (rf : 'opt_rec)
                     _  (_loc : FanLoc.t)  ->
                    (`CeLet (_loc, rf, bi, ce) : 'class_expr )))))]);
       ((Some "apply"), (Some `NA),
         [([`Sself; `Snterml ((Gram.obj (expr : 'expr Gram.t )), "label")],
            ("Gram.mk_action\n  (fun (e : 'expr)  (ce : 'class_expr)  (_loc : FanLoc.t)  ->\n     (`CeApp (_loc, ce, e) : 'class_expr ))\n",
              (Gram.mk_action
                 (fun (e : 'expr)  (ce : 'class_expr)  (_loc : FanLoc.t)  ->
                    (`CeApp (_loc, ce, e) : 'class_expr )))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `Ant ((""|"cexp"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"\"|\"cexp\"|\"anti\"),_)"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"cexp\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_expr\" n s)) : 'class_expr )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_expr\" n s))\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Ant ((""|"cexp"|"anti" as n),s) ->
                        (`Ant (_loc, (mk_anti ~c:"class_expr" n s)) : 
                        'class_expr )
                    | _ ->
                        failwith
                          "`Ant (_loc, (mk_anti ~c:\"class_expr\" n s))\n"))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_expr_tag : 'class_expr )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_expr_tag\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `QUOTATION x ->
                       (AstQuotation.expand _loc x DynAst.class_expr_tag : 
                       'class_expr )
                   | _ ->
                       failwith
                         "AstQuotation.expand _loc x DynAst.class_expr_tag\n"))));
         ([`Snterm
             (Gram.obj
                (class_longident_and_param : 'class_longident_and_param
                                               Gram.t ))],
           ("Gram.mk_action\n  (fun (ce : 'class_longident_and_param)  (_loc : FanLoc.t)  ->\n     (ce : 'class_expr ))\n",
             (Gram.mk_action
                (fun (ce : 'class_longident_and_param)  (_loc : FanLoc.t)  ->
                   (ce : 'class_expr )))));
         ([`Skeyword "object";
          `Snterm
            (Gram.obj (opt_class_self_patt : 'opt_class_self_patt Gram.t ));
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           ("Gram.mk_action\n  (fun _  (cst : 'class_structure)  (csp : 'opt_class_self_patt)  _ \n     (_loc : FanLoc.t)  -> (`Obj (_loc, csp, cst) : 'class_expr ))\n",
             (Gram.mk_action
                (fun _  (cst : 'class_structure) 
                   (csp : 'opt_class_self_patt)  _  (_loc : FanLoc.t)  ->
                   (`Obj (_loc, csp, cst) : 'class_expr )))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (class_type : 'class_type Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (ct : 'class_type)  _  (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->\n     (`CeTyc (_loc, ce, ct) : 'class_expr ))\n",
             (Gram.mk_action
                (fun _  (ct : 'class_type)  _  (ce : 'class_expr)  _ 
                   (_loc : FanLoc.t)  ->
                   (`CeTyc (_loc, ce, ct) : 'class_expr )))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (ce : 'class_expr)  _  (_loc : FanLoc.t)  -> (ce : 'class_expr ))\n",
             (Gram.mk_action
                (fun _  (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                   (ce : 'class_expr )))))])]);
   Gram.extend_single
     (class_longident_and_param : 'class_longident_and_param Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ));
           `Skeyword "[";
           `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
           `Skeyword "]"],
            ("Gram.mk_action\n  (fun _  (t : 'comma_ctyp)  _  (ci : 'class_longident)  (_loc : FanLoc.t) \n     -> (`CeCon (_loc, (`ViNil _loc), ci, t) : 'class_longident_and_param ))\n",
              (Gram.mk_action
                 (fun _  (t : 'comma_ctyp)  _  (ci : 'class_longident) 
                    (_loc : FanLoc.t)  ->
                    (`CeCon (_loc, (`ViNil _loc), ci, t) : 'class_longident_and_param )))));
         ([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
           ("Gram.mk_action\n  (fun (ci : 'class_longident)  (_loc : FanLoc.t)  ->\n     (`CeCon (_loc, (`ViNil _loc), ci, (`Nil _loc)) : 'class_longident_and_param ))\n",
             (Gram.mk_action
                (fun (ci : 'class_longident)  (_loc : FanLoc.t)  ->
                   (`CeCon (_loc, (`ViNil _loc), ci, (`Nil _loc)) : 'class_longident_and_param )))))])));
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
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_type\" n s)) : 'class_description )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_type\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ"|"anti"|"list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"class_type" n s)) : 'class_description )
                  | _ ->
                      failwith
                        "`Ant (_loc, (mk_anti ~c:\"class_type\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_type_tag : 'class_description )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_type_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.class_type_tag : 
                      'class_description )
                  | _ ->
                      failwith
                        "AstQuotation.expand _loc x DynAst.class_type_tag\n"))));
        ([`Snterm
            (Gram.obj
               (class_info_for_class_type : 'class_info_for_class_type Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'class_type_plus)  _  (ci : 'class_info_for_class_type) \n     (_loc : FanLoc.t)  -> (`CtCol (_loc, ci, ct) : 'class_description ))\n",
            (Gram.mk_action
               (fun (ct : 'class_type_plus)  _ 
                  (ci : 'class_info_for_class_type)  (_loc : FanLoc.t)  ->
                  (`CtCol (_loc, ci, ct) : 'class_description )))))]));
  Gram.extend_single
    (class_type_declaration : 'class_type_declaration Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("Gram.mk_action\n  (fun (cd2 : 'class_type_declaration)  _  (cd1 : 'class_type_declaration) \n     (_loc : FanLoc.t)  -> (`And (_loc, cd1, cd2) : 'class_type_declaration ))\n",
             (Gram.mk_action
                (fun (cd2 : 'class_type_declaration)  _ 
                   (cd1 : 'class_type_declaration)  (_loc : FanLoc.t)  ->
                   (`And (_loc, cd1, cd2) : 'class_type_declaration )))));
        ([`Stoken
            (((function
               | `Ant ((""|"typ"|"anti"|"list"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\"|\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_type\" n s)) : 'class_type_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_type\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ"|"anti"|"list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"class_type" n s)) : 'class_type_declaration )
                  | _ ->
                      failwith
                        "`Ant (_loc, (mk_anti ~c:\"class_type\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_type_tag : 'class_type_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_type_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.class_type_tag : 
                      'class_type_declaration )
                  | _ ->
                      failwith
                        "AstQuotation.expand _loc x DynAst.class_type_tag\n"))));
        ([`Snterm
            (Gram.obj
               (class_info_for_class_type : 'class_info_for_class_type Gram.t ));
         `Skeyword "=";
         `Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'class_type)  _  (ci : 'class_info_for_class_type) \n     (_loc : FanLoc.t)  -> (`CtEq (_loc, ci, ct) : 'class_type_declaration ))\n",
            (Gram.mk_action
               (fun (ct : 'class_type)  _  (ci : 'class_info_for_class_type) 
                  (_loc : FanLoc.t)  ->
                  (`CtEq (_loc, ci, ct) : 'class_type_declaration )))))]));
  Gram.extend_single
    (class_info_for_class_type : 'class_info_for_class_type Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
          `Snterm
            (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
           ("Gram.mk_action\n  (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) \n     (_loc : FanLoc.t)  ->\n     (`CtCon (_loc, mv, (i :>ident), ot) : 'class_info_for_class_type ))\n",
             (Gram.mk_action
                (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) 
                   (_loc : FanLoc.t)  ->
                   (`CtCon (_loc, mv, (i :>ident), ot) : 'class_info_for_class_type )))))]));
  Gram.extend_single (class_type_quot : 'class_type_quot Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "and"; `Sself],
           ("Gram.mk_action\n  (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) \n     (_loc : FanLoc.t)  -> (`And (_loc, ct1, ct2) : 'class_type_quot ))\n",
             (Gram.mk_action
                (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                   (_loc : FanLoc.t)  ->
                   (`And (_loc, ct1, ct2) : 'class_type_quot )))));
        ([`Sself; `Skeyword "="; `Sself],
          ("Gram.mk_action\n  (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) \n     (_loc : FanLoc.t)  -> (`CtEq (_loc, ct1, ct2) : 'class_type_quot ))\n",
            (Gram.mk_action
               (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                  (_loc : FanLoc.t)  ->
                  (`CtEq (_loc, ct1, ct2) : 'class_type_quot )))));
        ([`Sself; `Skeyword ":"; `Sself],
          ("Gram.mk_action\n  (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) \n     (_loc : FanLoc.t)  -> (`CtCol (_loc, ct1, ct2) : 'class_type_quot ))\n",
            (Gram.mk_action
               (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                  (_loc : FanLoc.t)  ->
                  (`CtCol (_loc, ct1, ct2) : 'class_type_quot )))));
        ([`Skeyword "virtual";
         `Snterm
           (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
          ("Gram.mk_action\n  (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t)  ->\n     (`CtCon (_loc, (`Virtual _loc), (i :>ident), ot) : 'class_type_quot ))\n",
            (Gram.mk_action
               (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t) 
                  ->
                  (`CtCon (_loc, (`Virtual _loc), (i :>ident), ot) : 
                  'class_type_quot )))));
        ([`Stoken
            (((function | `Ant ("virtual",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"virtual\",_)"));
         `Snterm (Gram.obj (ident : 'ident Gram.t ));
         `Snterm (Gram.obj (opt_comma_ctyp : 'opt_comma_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (ot : 'opt_comma_ctyp)  (i : 'ident)  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"virtual\" as n),s) ->\n         (let anti = `Ant (_loc, (mk_anti ~c:\"class_type\" n s)) in\n          `CtCon (_loc, anti, i, ot) : 'class_type_quot )\n     | _ ->\n         failwith\n           \"let anti = `Ant (_loc, (mk_anti ~c:\"class_type\" n s)) in\n`CtCon (_loc, anti, i, ot)\n\")\n",
            (Gram.mk_action
               (fun (ot : 'opt_comma_ctyp)  (i : 'ident) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("virtual" as n),s) ->
                      (let anti = `Ant (_loc, (mk_anti ~c:"class_type" n s)) in
                       `CtCon (_loc, anti, i, ot) : 'class_type_quot )
                  | _ ->
                      failwith
                        "let anti = `Ant (_loc, (mk_anti ~c:\"class_type\" n s)) in\n`CtCon (_loc, anti, i, ot)\n"))));
        ([`Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ))],
          ("Gram.mk_action\n  (fun (x : 'class_type_plus)  (_loc : FanLoc.t)  -> (x : 'class_type_quot ))\n",
            (Gram.mk_action
               (fun (x : 'class_type_plus)  (_loc : FanLoc.t)  ->
                  (x : 'class_type_quot )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'class_type_quot ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'class_type_quot )))))]));
  Gram.extend_single (class_type_plus : 'class_type_plus Gram.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "]";
          `Skeyword "->";
          `Sself],
           ("Gram.mk_action\n  (fun (ct : 'class_type_plus)  _  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`CtFun (_loc, t, ct) : 'class_type_plus ))\n",
             (Gram.mk_action
                (fun (ct : 'class_type_plus)  _  _  (t : 'ctyp)  _ 
                   (_loc : FanLoc.t)  ->
                   (`CtFun (_loc, t, ct) : 'class_type_plus )))));
        ([`Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'class_type)  (_loc : FanLoc.t)  -> (ct : 'class_type_plus ))\n",
            (Gram.mk_action
               (fun (ct : 'class_type)  (_loc : FanLoc.t)  ->
                  (ct : 'class_type_plus )))))]));
  Gram.extend_single (class_type : 'class_type Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"ctyp"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"ctyp\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"ctyp\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"class_type\" n s)) : 'class_type )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"class_type\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"ctyp"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"class_type" n s)) : 
                       'class_type )
                   | _ ->
                       failwith
                         "`Ant (_loc, (mk_anti ~c:\"class_type\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.class_type_tag : 'class_type )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.class_type_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.class_type_tag : 
                      'class_type )
                  | _ ->
                      failwith
                        "AstQuotation.expand _loc x DynAst.class_type_tag\n"))));
        ([`Snterm
            (Gram.obj
               (class_type_longident_and_param : 'class_type_longident_and_param
                                                   Gram.t ))],
          ("Gram.mk_action\n  (fun (ct : 'class_type_longident_and_param)  (_loc : FanLoc.t)  ->\n     (ct : 'class_type ))\n",
            (Gram.mk_action
               (fun (ct : 'class_type_longident_and_param)  (_loc : FanLoc.t)
                   -> (ct : 'class_type )))));
        ([`Skeyword "object";
         `Snterm
           (Gram.obj (opt_class_self_type : 'opt_class_self_type Gram.t ));
         `Snterm (Gram.obj (class_signature : 'class_signature Gram.t ));
         `Skeyword "end"],
          ("Gram.mk_action\n  (fun _  (csg : 'class_signature)  (cst : 'opt_class_self_type)  _ \n     (_loc : FanLoc.t)  -> (`CtSig (_loc, cst, csg) : 'class_type ))\n",
            (Gram.mk_action
               (fun _  (csg : 'class_signature)  (cst : 'opt_class_self_type)
                   _  (_loc : FanLoc.t)  ->
                  (`CtSig (_loc, cst, csg) : 'class_type )))))]));
  Gram.extend_single
    (class_type_longident_and_param : 'class_type_longident_and_param Gram.t )
    (None,
      (None, None,
        [([`Snterm
             (Gram.obj (class_type_longident : 'class_type_longident Gram.t ));
          `Skeyword "[";
          `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (t : 'comma_ctyp)  _  (i : 'class_type_longident) \n     (_loc : FanLoc.t)  ->\n     (`CtCon (_loc, (`ViNil _loc), i, t) : 'class_type_longident_and_param ))\n",
             (Gram.mk_action
                (fun _  (t : 'comma_ctyp)  _  (i : 'class_type_longident) 
                   (_loc : FanLoc.t)  ->
                   (`CtCon (_loc, (`ViNil _loc), i, t) : 'class_type_longident_and_param )))));
        ([`Snterm
            (Gram.obj (class_type_longident : 'class_type_longident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'class_type_longident)  (_loc : FanLoc.t)  ->\n     (`CtCon (_loc, (`ViNil _loc), i, (`Nil _loc)) : 'class_type_longident_and_param ))\n",
            (Gram.mk_action
               (fun (i : 'class_type_longident)  (_loc : FanLoc.t)  ->
                  (`CtCon (_loc, (`ViNil _loc), i, (`Nil _loc)) : 'class_type_longident_and_param )))))]))
let apply_ctyp () =
  Gram.extend_single (ctyp_quot : 'ctyp_quot Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
          `Skeyword ",";
          `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (y : 'comma_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, x, y) : 'ctyp_quot ))\n",
             (Gram.mk_action
                (fun (y : 'comma_ctyp)  _  (x : 'more_ctyp) 
                   (_loc : FanLoc.t)  -> (`Com (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword ";";
         `Snterm
           (Gram.obj
              (label_declaration_list : 'label_declaration_list Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'label_declaration_list)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) \n     -> (`Sem (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'label_declaration_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (`Sem (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "|";
         `Snterm
           (Gram.obj
              (constructor_declarations : 'constructor_declarations Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'constructor_declarations)  _  (x : 'more_ctyp) \n     (_loc : FanLoc.t)  -> (`Or (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'constructor_declarations)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (`Or (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "of";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) \n     -> (`Of (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (`Of (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "of";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ));
         `Skeyword "|";
         `Snterm
           (Gram.obj
              (constructor_declarations : 'constructor_declarations Gram.t ))],
          ("Gram.mk_action\n  (fun (z : 'constructor_declarations)  _  (y : 'constructor_arg_list)  _ \n     (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`Or (_loc, (`Of (_loc, x, y)), z) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (z : 'constructor_declarations)  _ 
                  (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  ->
                  (`Or (_loc, (`Of (_loc, x, y)), z) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "of";
         `Skeyword "&";
         `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'amp_ctyp)  _  _  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`TyOfAmp (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'amp_ctyp)  _  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (`TyOfAmp (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "of";
         `Skeyword "&";
         `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ));
         `Skeyword "|";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ))],
          ("Gram.mk_action\n  (fun (z : 'row_field)  _  (y : 'amp_ctyp)  _  _  (x : 'more_ctyp) \n     (_loc : FanLoc.t)  ->\n     (`Or (_loc, (`TyOfAmp (_loc, x, y)), z) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (z : 'row_field)  _  (y : 'amp_ctyp)  _  _ 
                  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->
                  (`Or (_loc, (`TyOfAmp (_loc, x, y)), z) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'more_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`TyCol (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'more_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                  -> (`TyCol (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword ";";
         `Snterm
           (Gram.obj
              (label_declaration_list : 'label_declaration_list Gram.t ))],
          ("Gram.mk_action\n  (fun (z : 'label_declaration_list)  _  (y : 'more_ctyp)  _ \n     (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`Sem (_loc, (`TyCol (_loc, x, y)), z) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (z : 'label_declaration_list)  _  (y : 'more_ctyp)  _ 
                  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->
                  (`Sem (_loc, (`TyCol (_loc, x, y)), z) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "*";
         `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'star_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`Sta (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'star_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                  -> (`Sta (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "&";
         `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'amp_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->\n     (`Amp (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'amp_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                  -> (`Amp (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
         `Skeyword "and";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
          ("Gram.mk_action\n  (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) \n     -> (`And (_loc, x, y) : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (`And (_loc, x, y) : 'ctyp_quot )))));
        ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (x : 'more_ctyp)  (_loc : FanLoc.t)  -> (x : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (x : 'more_ctyp)  (_loc : FanLoc.t)  -> (x : 'ctyp_quot )))));
        ([`Skeyword "type";
         `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  -> (t : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                  (t : 'ctyp_quot )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'ctyp_quot ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'ctyp_quot )))))]));
  Gram.extend_single (more_ctyp : 'more_ctyp Gram.t )
    (None,
      (None, None,
        [([`Skeyword "mutable"; `Sself],
           ("Gram.mk_action\n  (fun (x : 'more_ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Mut (_loc, x) : 'more_ctyp ))\n",
             (Gram.mk_action
                (fun (x : 'more_ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Mut (_loc, x) : 'more_ctyp )))));
        ([`Skeyword "`"; `Snterm (Gram.obj (astr : 'astr Gram.t ))],
          ("Gram.mk_action\n  (fun (x : 'astr)  _  (_loc : FanLoc.t)  -> (`TyVrn (_loc, x) : 'more_ctyp ))\n",
            (Gram.mk_action
               (fun (x : 'astr)  _  (_loc : FanLoc.t)  ->
                  (`TyVrn (_loc, x) : 'more_ctyp )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action (fun (x : 'ctyp)  (_loc : FanLoc.t)  -> (x : 'more_ctyp ))\n",
            (Gram.mk_action
               (fun (x : 'ctyp)  (_loc : FanLoc.t)  -> (x : 'more_ctyp )))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
          ("Gram.mk_action\n  (fun (x : 'type_parameter)  (_loc : FanLoc.t)  -> (x : 'more_ctyp ))\n",
            (Gram.mk_action
               (fun (x : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (x : 'more_ctyp )))))]));
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
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'unquoted_typevars )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'unquoted_typevars )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'unquoted_typevars )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'unquoted_typevars )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (i :>ident)) : 'unquoted_typevars ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`Id (_loc, (i :>ident)) : 'unquoted_typevars )))))]));
  Gram.extend_single (type_parameter : 'type_parameter Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti n s)) : 'type_parameter )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti n s)) : 'type_parameter )
                   | _ -> failwith "`Ant (_loc, (mk_anti n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'type_parameter )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'type_parameter )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Skeyword "'"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Normal _loc), (`Some i)) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Normal _loc), (`Some i)) : 'type_parameter )))));
        ([`Skeyword "+";
         `Skeyword "'";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Positive _loc), (`Some i)) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Positive _loc), (`Some i)) : 'type_parameter )))));
        ([`Skeyword "-";
         `Skeyword "'";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Negative _loc), (`Some i)) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Negative _loc), (`Some i)) : 'type_parameter )))));
        ([`Skeyword "+"; `Skeyword "_"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Positive _loc), `None) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Positive _loc), `None) : 'type_parameter )))));
        ([`Skeyword "-"; `Skeyword "_"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Negative _loc), `None) : 'type_parameter ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Negative _loc), `None) : 'type_parameter )))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'type_parameter ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'type_parameter )))))]));
  Gram.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ));
          `Snterm (Gram.obj (type_parameters : 'type_parameters Gram.t ))],
           ("Gram.mk_action\n  (fun (tpl : 'type_parameters)  (i : 'type_longident)  (_loc : FanLoc.t)  ->\n     (tpl (`Id (_loc, i)) : 'type_longident_and_parameters ))\n",
             (Gram.mk_action
                (fun (tpl : 'type_parameters)  (i : 'type_longident) 
                   (_loc : FanLoc.t)  ->
                   (tpl (`Id (_loc, i)) : 'type_longident_and_parameters )))));
        ([`Stoken
            (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti n s ~c:\"ctyp\")) : 'type_longident_and_parameters )\n     | _ -> failwith \"`Ant (_loc, (mk_anti n s ~c:\"ctyp\"))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"anti" as n),s) ->
                      (`Ant (_loc, (mk_anti n s ~c:"ctyp")) : 'type_longident_and_parameters )
                  | _ -> failwith "`Ant (_loc, (mk_anti n s ~c:\"ctyp\"))\n"))))]));
  Gram.extend_single (type_parameters : 'type_parameters Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'type_parameters)  (t1 : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (fun acc  -> t2 (`App (_loc, acc, t1)) : 'type_parameters ))\n",
             (Gram.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : FanLoc.t)  ->
                   (fun acc  -> t2 (`App (_loc, acc, t1)) : 'type_parameters )))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (fun acc  -> `App (_loc, acc, t) : 'type_parameters ))\n",
            (Gram.mk_action
               (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (fun acc  -> `App (_loc, acc, t) : 'type_parameters )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (fun t  -> t : 'type_parameters ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (fun t  -> t : 'type_parameters )))))]));
  Gram.extend_single (opt_class_self_type : 'opt_class_self_type Gram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'opt_class_self_type ))\n",
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (t : 'opt_class_self_type )))));
        ([],
          ("Gram.mk_action\n  (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_class_self_type ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_class_self_type )))))]));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'meth_decl )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'meth_decl )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp;\" n s)) : 'meth_decl )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp;\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp;" n s)) : 'meth_decl )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp;\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'meth_decl )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'meth_decl )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`TyCol (_loc, (`Id (_loc, (lab :>ident))), t) : 'meth_decl ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`TyCol (_loc, (`Id (_loc, (lab :>ident))), t) : 'meth_decl )))))]));
  Gram.extend_single (opt_meth_list : 'opt_meth_list Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (meth_list : 'meth_list Gram.t ))],
           ("Gram.mk_action\n  (fun ((ml,v) : 'meth_list)  (_loc : FanLoc.t)  ->\n     (`TyObj (_loc, ml, v) : 'opt_meth_list ))\n",
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  (_loc : FanLoc.t)  ->
                   (`TyObj (_loc, ml, v) : 'opt_meth_list )))));
        ([`Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
          ("Gram.mk_action\n  (fun (v : 'opt_dot_dot)  (_loc : FanLoc.t)  ->\n     (`TyObj (_loc, (`Nil _loc), v) : 'opt_meth_list ))\n",
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  (_loc : FanLoc.t)  ->
                  (`TyObj (_loc, (`Nil _loc), v) : 'opt_meth_list )))))]));
  Gram.extend_single (row_field : 'row_field Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'row_field )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'row_field )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp|\" n s)) : 'row_field )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp|\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp|" n s)) : 'row_field )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp|\" n s))\n"))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'row_field)  _  (t1 : 'row_field)  (_loc : FanLoc.t)  ->\n     (`Or (_loc, t1, t2) : 'row_field ))\n",
            (Gram.mk_action
               (fun (t2 : 'row_field)  _  (t1 : 'row_field) 
                  (_loc : FanLoc.t)  -> (`Or (_loc, t1, t2) : 'row_field )))));
        ([`Skeyword "`"; `Snterm (Gram.obj (astr : 'astr Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'astr)  _  (_loc : FanLoc.t)  -> (`TyVrn (_loc, i) : 'row_field ))\n",
            (Gram.mk_action
               (fun (i : 'astr)  _  (_loc : FanLoc.t)  ->
                  (`TyVrn (_loc, i) : 'row_field )))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (astr : 'astr Gram.t ));
         `Skeyword "of";
         `Skeyword "&";
         `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'amp_ctyp)  _  _  (i : 'astr)  _  (_loc : FanLoc.t)  ->\n     (`TyOfAmp (_loc, (`TyVrn (_loc, i)), t) : 'row_field ))\n",
            (Gram.mk_action
               (fun (t : 'amp_ctyp)  _  _  (i : 'astr)  _  (_loc : FanLoc.t) 
                  -> (`TyOfAmp (_loc, (`TyVrn (_loc, i)), t) : 'row_field )))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (astr : 'astr Gram.t ));
         `Skeyword "of";
         `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'amp_ctyp)  _  (i : 'astr)  _  (_loc : FanLoc.t)  ->\n     (`Of (_loc, (`TyVrn (_loc, i)), t) : 'row_field ))\n",
            (Gram.mk_action
               (fun (t : 'amp_ctyp)  _  (i : 'astr)  _  (_loc : FanLoc.t)  ->
                  (`Of (_loc, (`TyVrn (_loc, i)), t) : 'row_field )))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'row_field ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'row_field )))))]));
  Gram.extend_single (amp_ctyp : 'amp_ctyp Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword "&"; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'amp_ctyp)  _  (t1 : 'amp_ctyp)  (_loc : FanLoc.t)  ->\n     (`Amp (_loc, t1, t2) : 'amp_ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'amp_ctyp)  _  (t1 : 'amp_ctyp)  (_loc : FanLoc.t)
                    -> (`Amp (_loc, t1, t2) : 'amp_ctyp )))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp&\" n s)) : 'amp_ctyp )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp&\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp&" n s)) : 'amp_ctyp )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp&\" n s))\n"))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'amp_ctyp ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'amp_ctyp )))))]));
  Gram.extend_single (name_tags : 'name_tags Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'name_tags )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'name_tags )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
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
  Gram.extend_single (opt_polyt : 'opt_polyt Gram.t )
    (None,
      (None, None,
        [([`Skeyword ":"; `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action (fun (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'opt_polyt ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'opt_polyt )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_polyt ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_polyt )))))]));
  Gram.extend_single (type_declaration : 'type_declaration Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'type_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ"|"anti" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'type_declaration )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctypand\" n s)) : 'type_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctypand\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctypand" n s)) : 'type_declaration )
                  | _ ->
                      failwith "`Ant (_loc, (mk_anti ~c:\"ctypand\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'type_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'type_declaration )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
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
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
         `Slist0 (`Snterm (Gram.obj (constrain : 'constrain Gram.t )))],
          ("Gram.mk_action\n  (fun (cl : 'constrain list)  (tk : 'ctyp)  _ \n     ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t)  ->\n     (`TyDcl (_loc, n, tpl, tk, cl) : 'type_declaration ))\n",
            (Gram.mk_action
               (fun (cl : 'constrain list)  (tk : 'ctyp)  _ 
                  ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t) 
                  -> (`TyDcl (_loc, n, tpl, tk, cl) : 'type_declaration )))));
        ([`Snterm
            (Gram.obj
               (type_ident_and_parameters : 'type_ident_and_parameters Gram.t ));
         `Slist0 (`Snterm (Gram.obj (constrain : 'constrain Gram.t )))],
          ("Gram.mk_action\n  (fun (cl : 'constrain list)  ((n,tpl) : 'type_ident_and_parameters) \n     (_loc : FanLoc.t)  ->\n     (`TyDcl (_loc, n, tpl, (`Nil _loc), cl) : 'type_declaration ))\n",
            (Gram.mk_action
               (fun (cl : 'constrain list) 
                  ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t) 
                  ->
                  (`TyDcl (_loc, n, tpl, (`Nil _loc), cl) : 'type_declaration )))))]));
  Gram.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Slist0
            (`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t )))],
           ("Gram.mk_action\n  (fun (tpl : 'type_parameter list)  (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i, tpl) : 'type_ident_and_parameters ))\n",
             (Gram.mk_action
                (fun (tpl : 'type_parameter list)  (i : 'a_lident) 
                   (_loc : FanLoc.t)  ->
                   ((i, tpl) : 'type_ident_and_parameters )))))]));
  Gram.extend_single (constrain : 'constrain Gram.t )
    (None,
      (None, None,
        [([`Skeyword "constraint";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     ((t1, t2) : 'constrain ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   ((t1, t2) : 'constrain )))))]));
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
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'typevars )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'typevars )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"forall\" n s)) : 'typevars )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"forall\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"forall" n s)) : 'typevars )
                  | _ ->
                      failwith "`Ant (_loc, (mk_anti ~c:\"forall\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'typevars )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'typevars )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Skeyword "'"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Normal _loc), (`Some i)) : 'typevars ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`Quote (_loc, (`Normal _loc), (`Some i)) : 'typevars )))))]));
  Gram.extend (ctyp : 'ctyp Gram.t )
    (None,
      [((Some "=="), (Some `NA),
         [([`Sself; `Skeyword "=="; `Sself],
            ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`TyMan (_loc, t1, t2) : 'ctyp ))\n",
              (Gram.mk_action
                 (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                    (`TyMan (_loc, t1, t2) : 'ctyp )))))]);
      ((Some "private"), (Some `NA),
        [([`Skeyword "private";
          `Snterml ((Gram.obj (ctyp : 'ctyp Gram.t )), "alias")],
           ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (`Priv (_loc, t) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (`Priv (_loc, t) : 'ctyp )))))]);
      ((Some "alias"), (Some `LA),
        [([`Sself; `Skeyword "as"; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, t1, t2) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (`Alias (_loc, t1, t2) : 'ctyp )))))]);
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
          ("Gram.mk_action\n  (fun (t : 'ctyp)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `OPTLABEL s -> (`TyOlb (_loc, (`Lid (_loc, s)), t) : 'ctyp )\n     | _ -> failwith \"`TyOlb (_loc, (`Lid (_loc, s)), t)\n\")\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `OPTLABEL s ->
                      (`TyOlb (_loc, (`Lid (_loc, s)), t) : 'ctyp )
                  | _ -> failwith "`TyOlb (_loc, (`Lid (_loc, s)), t)\n"))));
        ([`Skeyword "?";
         `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Sself],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`TyOlb (_loc, i, t) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                  (`TyOlb (_loc, i, t) : 'ctyp )))))]);
      ((Some "apply"), (Some `LA),
        [([`Sself; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (let t = `App (_loc, t1, t2) in\n      try `Id (_loc, (FanAst.ident_of_ctyp t)) with | Invalid_argument _ -> t : \n     'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (let t = `App (_loc, t1, t2) in
                    try `Id (_loc, (FanAst.ident_of_ctyp t))
                    with | Invalid_argument _ -> t : 'ctyp )))))]);
      ((Some "."), (Some `LA),
        [([`Sself; `Skeyword "."; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->\n     (try\n        `Id\n          (_loc,\n            (`Dot\n               (_loc, (FanAst.ident_of_ctyp t1), (FanAst.ident_of_ctyp t2))))\n      with | Invalid_argument s -> raise (XStream.Error s) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (try
                      `Id
                        (_loc,
                          (`Dot
                             (_loc, (FanAst.ident_of_ctyp t1),
                               (FanAst.ident_of_ctyp t2))))
                    with | Invalid_argument s -> raise (XStream.Error s) : 
                   'ctyp )))))]);
      ((Some "simple"), None,
        [([`Skeyword "'"; `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->\n     (`Quote (_loc, (`Normal _loc), (`Some i)) : 'ctyp ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  _  (_loc : FanLoc.t)  ->
                   (`Quote (_loc, (`Normal _loc), (`Some i)) : 'ctyp )))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'ctyp )))));
        ([`Stoken
            (((function
               | `Ant ((""|"typ"|"anti"|"tup"),_) -> true
               | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"typ\"|\"anti\"|\"tup\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\"|\"anti\"|\"tup\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'ctyp )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ((""|"typ"|"anti"|"tup" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'ctyp )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("id",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"id\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"id\" as n),s) ->\n         (`Id (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s)))) : 'ctyp )\n     | _ ->\n         failwith \"`Id (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("id" as n),s) ->
                      (`Id (_loc, (`Ant (_loc, (mk_anti ~c:"ident" n s)))) : 
                      'ctyp )
                  | _ ->
                      failwith
                        "`Id (_loc, (`Ant (_loc, (mk_anti ~c:\"ident\" n s))))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x -> (AstQuotation.expand _loc x DynAst.ctyp_tag : 'ctyp )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'ctyp )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (i :>ident)) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`Id (_loc, (i :>ident)) : 'ctyp )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (i :>ident)) : 'ctyp ))\n",
            (Gram.mk_action
               (fun (i : 'a_uident)  (_loc : FanLoc.t)  ->
                  (`Id (_loc, (i :>ident)) : 'ctyp )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword "*";
         `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->\n     (`Tup (_loc, (`Sta (_loc, t, tl))) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _ 
                  (_loc : FanLoc.t)  ->
                  (`Tup (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'ctyp )))));
        ([`Skeyword "["; `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  -> (`Sum (_loc, (`Nil _loc)) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (`Sum (_loc, (`Nil _loc)) : 'ctyp )))));
        ([`Skeyword "[";
         `Snterm
           (Gram.obj
              (constructor_declarations : 'constructor_declarations Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (t : 'constructor_declarations)  _  (_loc : FanLoc.t)  ->\n     (`Sum (_loc, t) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (t : 'constructor_declarations)  _  (_loc : FanLoc.t) 
                  -> (`Sum (_loc, t) : 'ctyp )))));
        ([`Skeyword "[";
         `Skeyword "=";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->\n     (`TyVrnEq (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                  (`TyVrnEq (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "["; `Skeyword ">"; `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  _  _  (_loc : FanLoc.t)  ->\n     (`TyVrnSup (_loc, (`Nil _loc)) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  _  _  (_loc : FanLoc.t)  ->
                  (`TyVrnSup (_loc, (`Nil _loc)) : 'ctyp )))));
        ([`Skeyword "[";
         `Skeyword ">";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->\n     (`TyVrnSup (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                  (`TyVrnSup (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[";
         `Skeyword "<";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->\n     (`TyVrnInf (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                  (`TyVrnInf (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[";
         `Skeyword "<";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword ">";
         `Snterm (Gram.obj (name_tags : 'name_tags Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t) \n     -> (`TyVrnInfSup (_loc, rfl, ntl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _  _ 
                  (_loc : FanLoc.t)  ->
                  (`TyVrnInfSup (_loc, rfl, ntl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->\n     (`TyVrnInf (_loc, rfl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->
                  (`TyVrnInf (_loc, rfl) : 'ctyp )))));
        ([`Skeyword "[<";
         `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
         `Skeyword ">";
         `Snterm (Gram.obj (name_tags : 'name_tags Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->\n     (`TyVrnInfSup (_loc, rfl, ntl) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                  (_loc : FanLoc.t)  ->
                  (`TyVrnInfSup (_loc, rfl, ntl) : 'ctyp )))));
        ([`Skeyword "{";
         `Snterm
           (Gram.obj
              (label_declaration_list : 'label_declaration_list Gram.t ));
         `Skeyword "}"],
          ("Gram.mk_action\n  (fun _  (t : 'label_declaration_list)  _  (_loc : FanLoc.t)  ->\n     (`TyRec (_loc, t) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (t : 'label_declaration_list)  _  (_loc : FanLoc.t) 
                  -> (`TyRec (_loc, t) : 'ctyp )))));
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
         `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (p : 'module_type)  _  _  (_loc : FanLoc.t)  ->\n     (`Package (_loc, p) : 'ctyp ))\n",
            (Gram.mk_action
               (fun _  (p : 'module_type)  _  _  (_loc : FanLoc.t)  ->
                  (`Package (_loc, p) : 'ctyp )))))])]);
  Gram.extend_single (star_ctyp : 'star_ctyp Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'star_ctyp )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'star_ctyp )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp*\" n s)) : 'star_ctyp )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp*\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp*" n s)) : 'star_ctyp )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp*\" n s))\n"))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'constructor_declarations )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'constructor_declarations )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp|\" n s)) : 'constructor_declarations )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp|\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp|" n s)) : 'constructor_declarations )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp|\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'constructor_declarations )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'constructor_declarations )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Sself; `Skeyword "|"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'constructor_declarations)  _  (t1 : 'constructor_declarations) \n     (_loc : FanLoc.t)  -> (`Or (_loc, t1, t2) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (t2 : 'constructor_declarations)  _ 
                  (t1 : 'constructor_declarations)  (_loc : FanLoc.t)  ->
                  (`Or (_loc, t1, t2) : 'constructor_declarations )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
         `Skeyword "of";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'constructor_arg_list)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Of (_loc, (`Id (_loc, (s :>ident))), t) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : FanLoc.t)  ->
                  (`Of (_loc, (`Id (_loc, (s :>ident))), t) : 'constructor_declarations )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (let (tl,rt) = Ctyp.to_generalized t in\n      `TyCol\n        (_loc, (`Id (_loc, (s :>ident))),\n          (`Arrow (_loc, (FanAst.and_of_list tl), rt))) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->
                  (let (tl,rt) = Ctyp.to_generalized t in
                   `TyCol
                     (_loc, (`Id (_loc, (s :>ident))),
                       (`Arrow (_loc, (FanAst.and_of_list tl), rt))) : 
                  'constructor_declarations )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
          ("Gram.mk_action\n  (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (s :>ident)) : 'constructor_declarations ))\n",
            (Gram.mk_action
               (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->
                  (`Id (_loc, (s :>ident)) : 'constructor_declarations )))))]));
  Gram.extend_single
    (constructor_declaration : 'constructor_declaration Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ((""|"typ"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"typ\"),_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'constructor_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'constructor_declaration )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'constructor_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'constructor_declaration )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ));
         `Skeyword "of";
         `Snterm
           (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'constructor_arg_list)  _  (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Of (_loc, (`Id (_loc, (s :>ident))), t) : 'constructor_declaration ))\n",
            (Gram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                  (_loc : FanLoc.t)  ->
                  (`Of (_loc, (`Id (_loc, (s :>ident))), t) : 'constructor_declaration )))));
        ([`Snterm (Gram.obj (a_uident : 'a_uident Gram.t ))],
          ("Gram.mk_action\n  (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (s :>ident)) : 'constructor_declaration ))\n",
            (Gram.mk_action
               (fun (s : 'a_uident)  (_loc : FanLoc.t)  ->
                  (`Id (_loc, (s :>ident)) : 'constructor_declaration )))))]));
  Gram.extend_single (constructor_arg_list : 'constructor_arg_list Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Ant ("list",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"list\",_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctypand\" n s)) : 'constructor_arg_list )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctypand\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant (("list" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctypand" n s)) : 'constructor_arg_list )
                   | _ ->
                       failwith "`Ant (_loc, (mk_anti ~c:\"ctypand\" n s))\n"))));
        ([`Sself; `Skeyword "and"; `Sself],
          ("Gram.mk_action\n  (fun (t2 : 'constructor_arg_list)  _  (t1 : 'constructor_arg_list) \n     (_loc : FanLoc.t)  -> (`And (_loc, t1, t2) : 'constructor_arg_list ))\n",
            (Gram.mk_action
               (fun (t2 : 'constructor_arg_list)  _ 
                  (t1 : 'constructor_arg_list)  (_loc : FanLoc.t)  ->
                  (`And (_loc, t1, t2) : 'constructor_arg_list )))));
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
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"\"|\"typ\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp\" n s)) : 'label_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Ant ((""|"typ" as n),s) ->
                       (`Ant (_loc, (mk_anti ~c:"ctyp" n s)) : 'label_declaration )
                   | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp\" n s))\n"))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp;\" n s)) : 'label_declaration )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp;\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp;" n s)) : 'label_declaration )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp;\" n s))\n"))));
        ([`Stoken
            (((function | `QUOTATION _ -> true | _ -> false)),
              (`Normal, "`QUOTATION _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `QUOTATION x ->\n         (AstQuotation.expand _loc x DynAst.ctyp_tag : 'label_declaration )\n     | _ -> failwith \"AstQuotation.expand _loc x DynAst.ctyp_tag\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'label_declaration )
                  | _ ->
                      failwith "AstQuotation.expand _loc x DynAst.ctyp_tag\n"))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`TyCol (_loc, (`Id (_loc, (s :>ident))), t) : 'label_declaration ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`TyCol (_loc, (`Id (_loc, (s :>ident))), t) : 'label_declaration )))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
         `Skeyword ":";
         `Skeyword "mutable";
         `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'ctyp)  _  _  (s : 'a_lident)  (_loc : FanLoc.t)  ->\n     (`TyCol (_loc, (`Id (_loc, (s :>ident))), (`Mut (_loc, t))) : 'label_declaration ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  _  _  (s : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`TyCol (_loc, (`Id (_loc, (s :>ident))), (`Mut (_loc, t))) : 
                  'label_declaration )))))]));
  Gram.extend_single (class_name_and_param : 'class_name_and_param Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ));
          `Skeyword "[";
          `Snterm
            (Gram.obj (comma_type_parameter : 'comma_type_parameter Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (x : 'comma_type_parameter)  _  (i : 'a_lident)  (_loc : FanLoc.t) \n     -> ((i, x) : 'class_name_and_param ))\n",
             (Gram.mk_action
                (fun _  (x : 'comma_type_parameter)  _  (i : 'a_lident) 
                   (_loc : FanLoc.t)  -> ((i, x) : 'class_name_and_param )))));
        ([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
          ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i, (`Nil _loc)) : 'class_name_and_param ))\n",
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  ((i, (`Nil _loc)) : 'class_name_and_param )))))]));
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
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp,\" n s)) : 'comma_type_parameter )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp,\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp," n s)) : 'comma_type_parameter )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp,\" n s))\n"))));
        ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->\n     (t : 'comma_type_parameter ))\n",
            (Gram.mk_action
               (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (t : 'comma_type_parameter )))))]));
  Gram.extend_single (opt_comma_ctyp : 'opt_comma_ctyp Gram.t )
    (None,
      (None, None,
        [([`Skeyword "[";
          `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (x : 'comma_ctyp)  _  (_loc : FanLoc.t)  -> (x : 'opt_comma_ctyp ))\n",
             (Gram.mk_action
                (fun _  (x : 'comma_ctyp)  _  (_loc : FanLoc.t)  ->
                   (x : 'opt_comma_ctyp )))));
        ([],
          ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_comma_ctyp ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (`Nil _loc : 'opt_comma_ctyp )))))]));
  Gram.extend_single (comma_ctyp : 'comma_ctyp Gram.t )
    (None,
      (None, None,
        [([`Sself; `Skeyword ","; `Sself],
           ("Gram.mk_action\n  (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp)  (_loc : FanLoc.t)  ->\n     (`Com (_loc, t1, t2) : 'comma_ctyp ))\n",
             (Gram.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : FanLoc.t)  -> (`Com (_loc, t1, t2) : 'comma_ctyp )))));
        ([`Stoken
            (((function | `Ant ("list",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"list\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"list\" as n),s) ->\n         (`Ant (_loc, (mk_anti ~c:\"ctyp,\" n s)) : 'comma_ctyp )\n     | _ -> failwith \"`Ant (_loc, (mk_anti ~c:\"ctyp,\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("list" as n),s) ->
                      (`Ant (_loc, (mk_anti ~c:"ctyp," n s)) : 'comma_ctyp )
                  | _ -> failwith "`Ant (_loc, (mk_anti ~c:\"ctyp,\" n s))\n"))));
        ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
          ("Gram.mk_action (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'comma_ctyp ))\n",
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'comma_ctyp )))))]))
let _ =
  AstParsers.register_parser ("revise", (fun ()  -> apply (); apply_ctyp ()))
