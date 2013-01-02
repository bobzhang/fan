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
  (Gram.clear a_CHAR;
   Gram.clear a_FLOAT;
   Gram.clear a_INT;
   Gram.clear a_INT32;
   Gram.clear a_INT64;
   Gram.clear a_LABEL;
   Gram.clear a_LIDENT;
   Gram.clear a_NATIVEINT;
   Gram.clear a_OPTLABEL;
   Gram.clear a_STRING;
   Gram.clear a_UIDENT;
   Gram.clear a_ident;
   Gram.clear amp_ctyp;
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
   Gram.clear label;
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
   Gram.clear opt_eq_ctyp;
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
   Gram.clear poly_type;
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
   Gram.clear type_constraint;
   Gram.clear type_declaration;
   Gram.clear type_ident_and_parameters;
   Gram.clear type_kind;
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
       | Some (`ANT (("list" as n),s),_loc) ->
           (XStream.junk __strm; Ast.ExAnt (_loc, (mk_anti ~c:"expr;" n s)))
       | _ -> symb1 __strm in
     let rec kont al (__strm : _ XStream.t) =
       match XStream.peek __strm with
       | Some (`KEYWORD ";",_) ->
           (XStream.junk __strm;
            (let a =
               try symb __strm
               with | XStream.Failure  -> raise (XStream.Error "") in
             let s = __strm in
             let _loc = FanLoc.merge (Ast.loc_of_expr al) (Ast.loc_of_expr a) in
             kont (Ast.ExSem (_loc, al, a)) s))
       | _ -> al in
     fun (__strm : _ XStream.t)  -> let a = symb __strm in kont a __strm);
  (Gram.extend (module_expr_quot : 'module_expr_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (x : 'module_expr)  (_loc : FanLoc.t)  ->
                   (x : 'module_expr_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.MeNil _loc : 'module_expr_quot ))))])]);
   Gram.extend (module_binding0 : 'module_binding0 Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword ")";
            `Sself],
             (Gram.mk_action
                (fun (mb : 'module_binding0)  _  (mt : 'module_type)  _ 
                   (m : 'a_UIDENT)  _  (_loc : FanLoc.t)  ->
                   (Ast.MeFun (_loc, m, mt, mb) : 'module_binding0 ))));
          ([`Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            (Gram.mk_action
               (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.MeTyc (_loc, me, mt) : 'module_binding0 ))));
          ([`Skeyword "=";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            (Gram.mk_action
               (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                  (me : 'module_binding0 ))))])]);
   Gram.extend (module_expr : 'module_expr Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  _  (t : 'module_type)  _ 
                   (i : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.MeFun (_loc, i, t, me) : 'module_expr ))));
          ([`Skeyword "struct";
           `Snterm (Gram.obj (str_items : 'str_items Gram.t ));
           `Skeyword "end"],
            (Gram.mk_action
               (fun _  (st : 'str_items)  _  (_loc : FanLoc.t)  ->
                  (Ast.MeStr (_loc, st) : 'module_expr ))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            (Gram.mk_action
               (fun (me2 : 'module_expr)  (me1 : 'module_expr) 
                  (_loc : FanLoc.t)  ->
                  (Ast.MeApp (_loc, me1, me2) : 'module_expr ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `ANT ((""|"mexp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"mexp"|"anti"|"list" as n),s) ->
                      (Ast.MeAnt (_loc, (mk_anti ~c:"module_expr" n s)) : 
                      'module_expr )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.module_expr_tag : 
                     'module_expr )
                 | _ -> assert false)));
         ([`Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                 (Ast.MeId (_loc, i) : 'module_expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (mt : 'module_type)  _  (me : 'module_expr)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.MeTyc (_loc, me, mt) : 'module_expr ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                 (me : 'module_expr ))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (e : 'expr)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.MePkg (_loc, e) : 'module_expr ))));
         ([`Skeyword "(";
          `Skeyword "val";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (p : 'package_type)  _  (e : 'expr)  _  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.MePkg
                    (_loc, (Ast.ExTyc (_loc, e, (Ast.TyPkg (_loc, p))))) : 
                 'module_expr ))))])]));
  (Gram.extend (module_binding_quot : 'module_binding_quot Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (b2 : 'module_binding_quot)  _ 
                   (b1 : 'module_binding_quot)  (_loc : FanLoc.t)  ->
                   (Ast.MbAnd (_loc, b1, b2) : 'module_binding_quot ))));
          ([`Stoken
              (((function
                 | `ANT (("module_binding"|"anti"|""),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"module_binding\"|\"anti\"|\"\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("module_binding"|"anti"|"" as n),s) ->
                      (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                      'module_binding_quot )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            (Gram.mk_action
               (fun (mt : 'module_type)  _  (m : 'a_UIDENT) 
                  (_loc : FanLoc.t)  ->
                  (Ast.MbCol (_loc, m, mt) : 'module_binding_quot ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            (Gram.mk_action
               (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                  (m : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.MbColEq (_loc, m, mt, me) : 'module_binding_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.MbNil _loc : 'module_binding_quot ))))])]);
   Gram.extend (module_binding : 'module_binding Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (b2 : 'module_binding)  _  (b1 : 'module_binding) 
                   (_loc : FanLoc.t)  ->
                   (Ast.MbAnd (_loc, b1, b2) : 'module_binding ))));
          ([`Stoken
              (((function
                 | `ANT (("module_binding"|"anti"|"list"|""),_) -> true
                 | _ -> false)),
                (`Normal,
                  "`ANT ((\"module_binding\"|\"anti\"|\"list\"|\"\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("module_binding"|"anti"|"list"|"" as n),s) ->
                      (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                      'module_binding )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.module_binding_tag : 
                      'module_binding )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            (Gram.mk_action
               (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                  (m : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.MbColEq (_loc, m, mt, me) : 'module_binding ))))])]);
   Gram.extend (module_rec_declaration : 'module_rec_declaration Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (m2 : 'module_rec_declaration)  _ 
                   (m1 : 'module_rec_declaration)  (_loc : FanLoc.t)  ->
                   (Ast.MbAnd (_loc, m1, m2) : 'module_rec_declaration ))));
          ([`Stoken
              (((function
                 | `ANT ((""|"module_binding"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal,
                  "`ANT ((\"\"|\"module_binding\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"module_binding"|"anti"|"list" as n),s) ->
                      (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                      'module_rec_declaration )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.module_binding_tag : 
                      'module_rec_declaration )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            (Gram.mk_action
               (fun (mt : 'module_type)  _  (m : 'a_UIDENT) 
                  (_loc : FanLoc.t)  ->
                  (Ast.MbCol (_loc, m, mt) : 'module_rec_declaration ))))])]));
  (Gram.extend (with_constr_quot : 'with_constr_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (with_constr : 'with_constr Gram.t ))],
             (Gram.mk_action
                (fun (x : 'with_constr)  (_loc : FanLoc.t)  ->
                   (x : 'with_constr_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.WcNil _loc : 'with_constr_quot ))))])]);
   Gram.extend (with_constr : 'with_constr Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (wc2 : 'with_constr)  _  (wc1 : 'with_constr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.WcAnd (_loc, wc1, wc2) : 'with_constr ))));
          ([`Stoken
              (((function
                 | `ANT ((""|"with_constr"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal,
                  "`ANT ((\"\"|\"with_constr\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"with_constr"|"anti"|"list" as n),s) ->
                      (Ast.WcAnt (_loc, (mk_anti ~c:"with_constr" n s)) : 
                      'with_constr )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.with_constr_tag : 
                      'with_constr )
                  | _ -> assert false)));
          ([`Skeyword "type";
           `Snterm
             (Gram.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters)  _
                   (_loc : FanLoc.t)  ->
                  (Ast.WcTyp (_loc, t1, t2) : 'with_constr ))));
          ([`Skeyword "type";
           `Snterm
             (Gram.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gram.t ));
           `Skeyword ":=";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters)  _
                   (_loc : FanLoc.t)  ->
                  (Ast.WcTyS (_loc, t1, t2) : 'with_constr ))));
          ([`Skeyword "module";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "=";
           `Snterm
             (Gram.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gram.t ))],
            (Gram.mk_action
               (fun (i2 : 'module_longident_with_app)  _ 
                  (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.WcMod (_loc, i1, i2) : 'with_constr ))));
          ([`Skeyword "module";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword ":=";
           `Snterm
             (Gram.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gram.t ))],
            (Gram.mk_action
               (fun (i2 : 'module_longident_with_app)  _ 
                  (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.WcMoS (_loc, i1, i2) : 'with_constr ))))])]));
  (Gram.extend (module_type : 'module_type Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "functor";
            `Skeyword "(";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ":";
            `Sself;
            `Skeyword ")";
            `Skeyword "->";
            `Sself],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  _  (t : 'module_type)  _ 
                   (i : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.MtFun (_loc, i, t, mt) : 'module_type ))))]);
       ((Some "with"), None,
         [([`Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (with_constr : 'with_constr Gram.t ))],
            (Gram.mk_action
               (fun (wc : 'with_constr)  _  (mt : 'module_type) 
                  (_loc : FanLoc.t)  ->
                  (Ast.MtWit (_loc, mt, wc) : 'module_type ))))]);
       ((Some "apply"), None,
         [([`Sself; `Sself],
            (Gram.mk_action
               (fun (mt2 : 'module_type)  (mt1 : 'module_type) 
                  (_loc : FanLoc.t)  ->
                  (ModuleType.app0 mt1 mt2 : 'module_type ))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            (Gram.mk_action
               (fun (mt2 : 'module_type)  _  (mt1 : 'module_type) 
                  (_loc : FanLoc.t)  ->
                  (ModuleType.acc0 mt1 mt2 : 'module_type ))))]);
       ((Some "sig"), None,
         [([`Skeyword "sig";
           `Snterm (Gram.obj (sig_items : 'sig_items Gram.t ));
           `Skeyword "end"],
            (Gram.mk_action
               (fun _  (sg : 'sig_items)  _  (_loc : FanLoc.t)  ->
                  (Ast.MtSig (_loc, sg) : 'module_type ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `ANT ((""|"mtyp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"mtyp\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"mtyp"|"anti"|"list" as n),s) ->
                      (Ast.MtAnt (_loc, (mk_anti ~c:"module_type" n s)) : 
                      'module_type )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.module_type_tag : 
                     'module_type )
                 | _ -> assert false)));
         ([`Snterm
             (Gram.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gram.t ))],
           (Gram.mk_action
              (fun (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                 (Ast.MtId (_loc, i) : 'module_type ))));
         ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                 (Ast.MtQuo (_loc, i) : 'module_type ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                 (mt : 'module_type ))));
         ([`Skeyword "module";
          `Skeyword "type";
          `Skeyword "of";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
           (Gram.mk_action
              (fun (me : 'module_expr)  _  _  _  (_loc : FanLoc.t)  ->
                 (Ast.MtOf (_loc, me) : 'module_type ))))])]);
   Gram.extend (module_declaration : 'module_declaration Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                   (mt : 'module_declaration ))));
          ([`Skeyword "(";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword ")";
           `Sself],
            (Gram.mk_action
               (fun (mt : 'module_declaration)  _  (t : 'module_type)  _ 
                  (i : 'a_UIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.MtFun (_loc, i, t, mt) : 'module_declaration ))))])]);
   Gram.extend (module_type_quot : 'module_type_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (x : 'module_type)  (_loc : FanLoc.t)  ->
                   (x : 'module_type_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.MtNil _loc : 'module_type_quot ))))])]));
  (Gram.extend (sig_item_quot : 'sig_item_quot Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ))],
             (Gram.mk_action
                (fun (dp : 'opt_expr)  (n : 'a_LIDENT)  _  (_loc : FanLoc.t) 
                   -> (Ast.SgDir (_loc, n, dp) : 'sig_item_quot ))));
          ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (sg2 : 'sig_item_quot)  _  (sg1 : 'sig_item) 
                  (_loc : FanLoc.t)  ->
                  (match sg2 with
                   | Ast.SgNil _loc -> sg1
                   | _ -> Ast.SgSem (_loc, sg1, sg2) : 'sig_item_quot ))));
          ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ))],
            (Gram.mk_action
               (fun (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                  (sg : 'sig_item_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.SgNil _loc : 'sig_item_quot ))))])]);
   Gram.extend (sig_item : 'sig_item Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"sigi"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"sigi"|"anti"|"list" as n),s) ->
                       (Ast.SgAnt (_loc, (mk_anti ~c:"sig_item" n s)) : 
                       'sig_item )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.sig_item_tag : 
                      'sig_item )
                  | _ -> assert false)));
          ([`Skeyword "exception";
           `Snterm
             (Gram.obj
                (constructor_declaration : 'constructor_declaration Gram.t ))],
            (Gram.mk_action
               (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->
                  (Ast.SgExc (_loc, t) : 'sig_item ))));
          ([`Skeyword "external";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (string_list : 'string_list Gram.t ))],
            (Gram.mk_action
               (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_LIDENT) 
                  _  (_loc : FanLoc.t)  ->
                  (Ast.SgExt (_loc, i, t, sl) : 'sig_item ))));
          ([`Skeyword "include";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            (Gram.mk_action
               (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                  (Ast.SgInc (_loc, mt) : 'sig_item ))));
          ([`Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Snterm
             (Gram.obj (module_declaration : 'module_declaration Gram.t ))],
            (Gram.mk_action
               (fun (mt : 'module_declaration)  (i : 'a_UIDENT)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.SgMod (_loc, i, mt) : 'sig_item ))));
          ([`Skeyword "module";
           `Skeyword "rec";
           `Snterm
             (Gram.obj
                (module_rec_declaration : 'module_rec_declaration Gram.t ))],
            (Gram.mk_action
               (fun (mb : 'module_rec_declaration)  _  _  (_loc : FanLoc.t) 
                  -> (Ast.SgRecMod (_loc, mb) : 'sig_item ))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            (Gram.mk_action
               (fun (mt : 'module_type)  _  (i : 'a_ident)  _  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.SgMty (_loc, i, mt) : 'sig_item ))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.SgMty (_loc, i, (Ast.MtNil _loc)) : 'sig_item ))));
          ([`Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.SgOpn (_loc, i) : 'sig_item ))));
          ([`Skeyword "type";
           `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
            (Gram.mk_action
               (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                  (Ast.SgTyp (_loc, t) : 'sig_item ))));
          ([`Skeyword "val";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.SgVal (_loc, i, t) : 'sig_item ))));
          ([`Skeyword "class";
           `Snterm
             (Gram.obj (class_description : 'class_description Gram.t ))],
            (Gram.mk_action
               (fun (cd : 'class_description)  _  (_loc : FanLoc.t)  ->
                  (Ast.SgCls (_loc, cd) : 'sig_item ))));
          ([`Skeyword "class";
           `Skeyword "type";
           `Snterm
             (Gram.obj
                (class_type_declaration : 'class_type_declaration Gram.t ))],
            (Gram.mk_action
               (fun (ctd : 'class_type_declaration)  _  _  (_loc : FanLoc.t) 
                  -> (Ast.SgClt (_loc, ctd) : 'sig_item ))))])]);
   Gram.extend (interf : 'interf Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Skeyword ";;"],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (([Ast.SgDir (_loc, n, dp)], (Some _loc)) : 'interf ))));
          ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun ((sil,stopped) : 'interf)  _  (si : 'sig_item) 
                  (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'interf ))));
          ([`Stoken
              (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `EOI -> (([], None) : 'interf )
                  | _ -> assert false)))])]);
   Gram.extend (sig_items : 'sig_items Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"sigi"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"sigi"|"anti"|"list" as n),s) ->
                       (Ast.SgAnt (_loc, (mk_anti n ~c:"sig_item" s)) : 
                       'sig_items )
                   | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANT ((""|"sigi"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (sg : 'sig_items)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"sigi"|"anti"|"list" as n),s) ->
                      (Ast.SgSem
                         (_loc,
                           (Ast.SgAnt (_loc, (mk_anti n ~c:"sig_item" s))),
                           sg) : 'sig_items )
                  | _ -> assert false)));
          ([`Slist0
              (Gram.srules sig_items
                 [([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                          (sg : 'e__1 ))))])],
            (Gram.mk_action
               (fun (l : 'e__1 list)  (_loc : FanLoc.t)  ->
                  (Ast.sgSem_of_list l : 'sig_items ))))])]));
  (let grammar_entry_create = Gram.mk in
   let fun_def_patt: 'fun_def_patt Gram.t =
     grammar_entry_create "fun_def_patt" in
   Gram.extend (expr_quot : 'expr_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword ",";
            `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ))],
             (Gram.mk_action
                (fun (e2 : 'comma_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                   -> (Ast.ExCom (_loc, e1, e2) : 'expr_quot ))));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword ";";
           `Snterm (Gram.obj (sem_expr : 'sem_expr Gram.t ))],
            (Gram.mk_action
               (fun (e2 : 'sem_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExSem (_loc, e1, e2) : 'expr_quot ))));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'expr_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.ExNil _loc : 'expr_quot ))))])]);
   Gram.extend (cvalue_binding : 'cvalue_binding Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (e : 'cvalue_binding ))));
          ([`Skeyword ":";
           `Skeyword "type";
           `Snterm
             (Gram.obj (unquoted_typevars : 'unquoted_typevars Gram.t ));
           `Skeyword ".";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (t2 : 'ctyp)  _ 
                  (t1 : 'unquoted_typevars)  _  _  (_loc : FanLoc.t)  ->
                  (let u = Ast.TyPol (_loc, t1, t2) in Ast.ExTyc (_loc, e, u) : 
                  'cvalue_binding ))));
          ([`Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (t : 'poly_type)  _  (_loc : FanLoc.t) 
                  -> (Ast.ExTyc (_loc, e, t) : 'cvalue_binding ))));
          ([`Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ));
           `Skeyword ":>";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (t2 : 'ctyp)  _  (t : 'poly_type)  _ 
                  (_loc : FanLoc.t)  ->
                  (match t with
                   | Ast.TyPol (_loc,_,_) ->
                       raise (XStream.Error "unexpected polytype here")
                   | _ -> Ast.ExCoe (_loc, e, t, t2) : 'cvalue_binding ))));
          ([`Skeyword ":>";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExCoe (_loc, e, (Ast.TyNil _loc), t) : 'cvalue_binding ))))])]);
   Gram.extend (fun_binding : 'fun_binding Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ")";
            `Sself],
             (Gram.mk_action
                (fun (e : 'fun_binding)  _  (i : 'a_LIDENT)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExFUN (_loc, i, e) : 'fun_binding ))));
          ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t )); `Sself],
            (Gram.mk_action
               (fun (e : 'fun_binding)  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                  (Ast.ExFun
                     (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e))) : 
                  'fun_binding ))));
          ([`Snterm (Gram.obj (cvalue_binding : 'cvalue_binding Gram.t ))],
            (Gram.mk_action
               (fun (bi : 'cvalue_binding)  (_loc : FanLoc.t)  ->
                  (bi : 'fun_binding ))))])]);
   Gram.extend (lang : 'lang Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) ->
                       (let old = AstQuotation.default.contents in
                        (AstQuotation.default := s; old) : 'lang )
                   | _ -> assert false)))])]);
   Gram.extend (pos_exprs : 'pos_exprs Gram.t )
     (None,
       [(None, None,
          [([`Slist1sep
               ((Gram.srules pos_exprs
                   [([`Stoken
                        (((function | `STR (_,_) -> true | _ -> false)),
                          (`Normal, "`STR (_,_)"));
                     `Skeyword ":";
                     `Stoken
                       (((function | `STR (_,_) -> true | _ -> false)),
                         (`Normal, "`STR (_,_)"))],
                      (Gram.mk_action
                         (fun (__fan_2 : [> FanToken.t])  _ 
                            (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                            match (__fan_2, __fan_0) with
                            | (`STR (_,y),`STR (_,x)) -> ((x, y) : 'e__2 )
                            | _ -> assert false)))]), (`Skeyword ";"))],
             (Gram.mk_action
                (fun (xys : 'e__2 list)  (_loc : FanLoc.t)  ->
                   (let old = AstQuotation.map.contents in
                    AstQuotation.map := (SMap.add_list xys old); old : 
                   'pos_exprs ))))])]);
   Gram.extend (fun_def_patt : 'fun_def_patt Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (i : 'a_LIDENT)  _  _  (_loc : FanLoc.t)  ->
                   (fun e  -> Ast.ExFUN (_loc, i, e) : 'fun_def_patt ))));
          ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'ipatt)  (_loc : FanLoc.t)  ->
                  (fun e  ->
                     Ast.ExFun
                       (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e))) : 
                  'fun_def_patt ))));
          ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
           `Skeyword "when";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (w : 'expr)  _  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                  (fun e  -> Ast.ExFun (_loc, (Ast.McArr (_loc, p, w, e))) : 
                  'fun_def_patt ))))])]);
   Gram.extend (fun_def : 'fun_def Gram.t )
     (None,
       [(None, (Some `RA),
          [([`Snterm (Gram.obj (fun_def_patt : 'fun_def_patt Gram.t ));
            `Skeyword "->";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (f : 'fun_def_patt)  (_loc : FanLoc.t) 
                   -> (f e : 'fun_def ))));
          ([`Snterm (Gram.obj (fun_def_patt : 'fun_def_patt Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (e : 'fun_def)  (f : 'fun_def_patt)  (_loc : FanLoc.t) 
                  -> (f e : 'fun_def ))))])]);
   Gram.extend (opt_expr : 'opt_expr Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'opt_expr ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.ExNil _loc : 'opt_expr ))))])]);
   Gram.extend (expr : 'expr Gram.t )
     (None,
       [((Some "top"), (Some `RA),
          [([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Sself],
             (Gram.mk_action
                (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExLet (_loc, r, bi, x) : 'expr ))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
           `Skeyword "in";
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (mb : 'module_binding0)  (m : 'a_UIDENT) 
                  _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExLmd (_loc, m, mb, e) : 'expr ))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (i : 'module_longident)  _  _ 
                  (_loc : FanLoc.t)  -> (Ast.ExOpI (_loc, i, e) : 'expr ))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            (Gram.mk_action
               (fun (a : 'match_case)  _  (x : 'expr)  _  (bi : 'binding) 
                  (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExTry
                          (_loc,
                            (Ast.ExLet
                               (_loc, r, bi,
                                 (Ast.ExFun
                                    (_loc,
                                      (Ast.McArr
                                         (_loc,
                                           (Ast.PaId
                                              (_loc,
                                                (Ast.IdUid (_loc, "()")))),
                                           (Ast.ExNil _loc), x)))))),
                            (Ast.match_pre#match_case a))),
                       (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))))) : 
                  'expr ))));
          ([`Skeyword "match";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            (Gram.mk_action
               (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t) 
                  -> (Ast.ExMat (_loc, e, a) : 'expr ))));
          ([`Skeyword "try";
           `Sself;
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            (Gram.mk_action
               (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t) 
                  -> (Ast.ExTry (_loc, e, a) : 'expr ))));
          ([`Skeyword "if";
           `Sself;
           `Skeyword "then";
           `Sself;
           `Skeyword "else";
           `Sself],
            (Gram.mk_action
               (fun (e3 : 'expr)  _  (e2 : 'expr)  _  (e1 : 'expr)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExIfe (_loc, e1, e2, e3) : 'expr ))));
          ([`Skeyword "if"; `Sself; `Skeyword "then"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExIfe
                     (_loc, e1, e2,
                       (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))))) : 
                  'expr ))));
          ([`Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            (Gram.mk_action
               (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                  (Expr.mksequence ~loc:_loc seq : 'expr ))));
          ([`Skeyword "with";
           `Snterm (Gram.obj (lang : 'lang Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (x : 'expr)  (old : 'lang)  _  (_loc : FanLoc.t)  ->
                  (AstQuotation.default := old; x : 'expr ))));
          ([`Skeyword "with";
           `Skeyword "{";
           `Snterm (Gram.obj (pos_exprs : 'pos_exprs Gram.t ));
           `Skeyword "}";
           `Sself],
            (Gram.mk_action
               (fun (x : 'expr)  _  (old : 'pos_exprs)  _  _ 
                  (_loc : FanLoc.t)  -> (AstQuotation.map := old; x : 
                  'expr ))));
          ([`Skeyword "for";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword "=";
           `Sself;
           `Snterm (Gram.obj (direction_flag : 'direction_flag Gram.t ));
           `Sself;
           `Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            (Gram.mk_action
               (fun _  (seq : 'sequence)  _  (e2 : 'expr) 
                  (df : 'direction_flag)  (e1 : 'expr)  _  (i : 'a_LIDENT)  _
                   (_loc : FanLoc.t)  ->
                  (Ast.ExFor (_loc, i, e1, e2, df, seq) : 'expr ))));
          ([`Skeyword "while";
           `Sself;
           `Skeyword "do";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "done"],
            (Gram.mk_action
               (fun _  (seq : 'sequence)  _  (e : 'expr)  _ 
                  (_loc : FanLoc.t)  -> (Ast.ExWhi (_loc, e, seq) : 'expr ))))]);
       ((Some ":="), (Some `NA),
         [([`Sself; `Skeyword ":="; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExAss
                     (_loc,
                       (Ast.ExAcc
                          (_loc, e1,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents")))))),
                       e2) : 'expr ))));
         ([`Sself; `Skeyword "<-"; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (match Expr.bigarray_set _loc e1 e2 with
                  | Some e -> e
                  | None  -> Ast.ExAss (_loc, e1, e2) : 'expr ))))]);
       ((Some "||"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop0 : 'infixop0 Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (op : 'infixop0)  (e1 : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                  'expr ))))]);
       ((Some "&&"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop1 : 'infixop1 Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (op : 'infixop1)  (e1 : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                  'expr ))))]);
       ((Some "<"), (Some `LA),
         [([`Sself;
           `Snterm (Gram.obj (infixop2 : 'infixop2 Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (op : 'infixop2)  (e1 : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                  'expr ))))]);
       ((Some "^"), (Some `RA),
         [([`Sself;
           `Snterm (Gram.obj (infixop3 : 'infixop3 Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (op : 'infixop3)  (e1 : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                  'expr ))))]);
       ((Some "+"), (Some `LA),
         [([`Sself;
           `Snterm (Gram.obj (infixop4 : 'infixop4 Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (op : 'infixop4)  (e1 : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                  'expr ))))]);
       ((Some "*"), (Some `LA),
         [([`Sself; `Skeyword "land"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "land")))),
                            e1)), e2) : 'expr ))));
         ([`Sself; `Skeyword "lor"; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExApp
                         (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "lor")))),
                           e1)), e2) : 'expr ))));
         ([`Sself; `Skeyword "lxor"; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExApp
                         (_loc,
                           (Ast.ExId (_loc, (Ast.IdLid (_loc, "lxor")))), e1)),
                      e2) : 'expr ))));
         ([`Sself; `Skeyword "mod"; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExApp
                         (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "mod")))),
                           e1)), e2) : 'expr ))));
         ([`Sself; `Snterm (Gram.obj (infixop5 : 'infixop5 Gram.t )); `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  (op : 'infixop5)  (e1 : 'expr) 
                 (_loc : FanLoc.t)  ->
                 (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                 'expr ))))]);
       ((Some "**"), (Some `RA),
         [([`Sself; `Skeyword "asr"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "asr")))), e1)),
                       e2) : 'expr ))));
         ([`Sself; `Skeyword "lsl"; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExApp
                         (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "lsl")))),
                           e1)), e2) : 'expr ))));
         ([`Sself; `Skeyword "lsr"; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExApp
                    (_loc,
                      (Ast.ExApp
                         (_loc, (Ast.ExId (_loc, (Ast.IdLid (_loc, "lsr")))),
                           e1)), e2) : 'expr ))));
         ([`Sself; `Snterm (Gram.obj (infixop6 : 'infixop6 Gram.t )); `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  (op : 'infixop6)  (e1 : 'expr) 
                 (_loc : FanLoc.t)  ->
                 (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                 'expr ))))]);
       ((Some "obj"), (Some `RA),
         [([`Skeyword "fun";
           `Skeyword "[";
           `Slist0sep
             ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
               (`Skeyword "|"));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExFun (_loc, (Ast.mcOr_of_list a)) : 'expr ))));
         ([`Skeyword "function";
          `Skeyword "[";
          `Slist0sep
            ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
              (`Skeyword "|"));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExFun (_loc, (Ast.mcOr_of_list a)) : 'expr ))));
         ([`Skeyword "fun"; `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
           (Gram.mk_action
              (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr ))));
         ([`Skeyword "function";
          `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
           (Gram.mk_action
              (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr ))));
         ([`Skeyword "object";
          `Snterm
            (Gram.obj (opt_class_self_patt : 'opt_class_self_patt Gram.t ));
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           (Gram.mk_action
              (fun _  (cst : 'class_structure)  (csp : 'opt_class_self_patt) 
                 _  (_loc : FanLoc.t)  ->
                 (Ast.ExObj (_loc, csp, cst) : 'expr ))))]);
       ((Some "unary minus"), (Some `NA),
         [([`Skeyword "-"; `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Expr.mkumin _loc "-" e : 'expr ))));
         ([`Skeyword "-."; `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Expr.mkumin _loc "-." e : 'expr ))))]);
       ((Some "apply"), (Some `LA),
         [([`Sself; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, e1, e2) : 'expr ))));
         ([`Skeyword "assert"; `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Expr.mkassert _loc e : 'expr ))));
         ([`Skeyword "new";
          `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExNew (_loc, i) : 'expr ))));
         ([`Skeyword "lazy"; `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExLaz (_loc, e) : 'expr ))))]);
       ((Some "label"), (Some `NA),
         [([`Skeyword "~";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExLab (_loc, i, e) : 'expr ))));
         ([`Skeyword "~"; `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
           (Gram.mk_action
              (fun (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExLab (_loc, i, (Ast.ExNil _loc)) : 'expr ))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)
                  ->
                 match __fan_0 with
                 | `LABEL i -> (Ast.ExLab (_loc, i, e) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)
                  ->
                 match __fan_0 with
                 | `OPTLABEL i -> (Ast.ExOlb (_loc, i, e) : 'expr )
                 | _ -> assert false)));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
          `Skeyword ":";
          `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExOlb (_loc, i, e) : 'expr ))));
         ([`Skeyword "?"; `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
           (Gram.mk_action
              (fun (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExOlb (_loc, i, (Ast.ExNil _loc)) : 'expr ))))]);
       ((Some "."), (Some `LA),
         [([`Sself; `Skeyword "."; `Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                  -> (Ast.ExAre (_loc, e1, e2) : 'expr ))));
         ([`Sself; `Skeyword "."; `Skeyword "["; `Sself; `Skeyword "]"],
           (Gram.mk_action
              (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExSte (_loc, e1, e2) : 'expr ))));
         ([`Sself;
          `Skeyword ".";
          `Skeyword "{";
          `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ));
          `Skeyword "}"],
           (Gram.mk_action
              (fun _  (e2 : 'comma_expr)  _  _  (e1 : 'expr) 
                 (_loc : FanLoc.t)  ->
                 (Expr.bigarray_get _loc e1 e2 : 'expr ))));
         ([`Sself; `Skeyword "."; `Sself],
           (Gram.mk_action
              (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExAcc (_loc, e1, e2) : 'expr ))));
         ([`Sself;
          `Skeyword "#";
          `Snterm (Gram.obj (label : 'label Gram.t ))],
           (Gram.mk_action
              (fun (lab : 'label)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                 (Ast.ExSnd (_loc, e, lab) : 'expr ))))]);
       ((Some "~-"), (Some `NA),
         [([`Skeyword "!"; `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExAcc
                     (_loc, e,
                       (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents"))))) : 
                  'expr ))));
         ([`Snterm (Gram.obj (prefixop : 'prefixop Gram.t )); `Sself],
           (Gram.mk_action
              (fun (e : 'expr)  (f : 'prefixop)  (_loc : FanLoc.t)  ->
                 (Ast.ExApp (_loc, f, e) : 'expr ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.expr_tag : 'expr )
                  | _ -> assert false)));
         ([`Stoken
             (((function
                | `ANT
                    (("exp"|""|"anti"|"`bool"|"tup"|"seq"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"),_)
                    -> true
                | _ -> false)),
               (`Normal,
                 "`ANT\n  ((\"exp\"|\"\"|\"anti\"|\"`bool\"|\"tup\"|\"seq\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"|\"`int64\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"|\"str\"|\"`str\"),_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT
                     (("exp"|""|"anti"|"`bool"|"tup"|"seq"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"
                         as n),s)
                     -> (Ast.ExAnt (_loc, (mk_anti ~c:"expr" n s)) : 
                     'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT (_,s) -> (Ast.ExInt (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `INT32 (_,_) -> true | _ -> false)),
               (`Normal, "`INT32 (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT32 (_,s) -> (Ast.ExInt32 (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `INT64 (_,_) -> true | _ -> false)),
               (`Normal, "`INT64 (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT64 (_,s) -> (Ast.ExInt64 (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `FLO (_,_) -> true | _ -> false)),
               (`Normal, "`FLO (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `FLO (_,s) -> (Ast.ExFlo (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `CHAR (_,s) -> (Ast.ExChr (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (_,s) -> (Ast.ExStr (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `NATIVEINT (_,_) -> true | _ -> false)),
               (`Normal, "`NATIVEINT (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `NATIVEINT (_,s) -> (Ast.ExNativeInt (_loc, s) : 'expr )
                 | _ -> assert false)));
         ([`Stry
             (`Snterm
                (Gram.obj
                   (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                    Gram.t )));
          `Sself;
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (e : 'expr)  (i : 'module_longident_dot_lparen) 
                 (_loc : FanLoc.t)  -> (Ast.ExOpI (_loc, i, e) : 'expr ))));
         ([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'ident)  (_loc : FanLoc.t)  ->
                 (Ast.ExId (_loc, i) : 'expr ))));
         ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
           (Gram.mk_action
              (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExVrn (_loc, s) : 'expr ))));
         ([`Skeyword "["; `Skeyword "]"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))) : 'expr ))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
          `Skeyword "::";
          `Sself;
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (last : 'expr)  _  (mk_list : 'sem_expr_for_list)  _ 
                 (_loc : FanLoc.t)  -> (mk_list last : 'expr ))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (mk_list : 'sem_expr_for_list)  _  (_loc : FanLoc.t) 
                 ->
                 (mk_list (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))) : 
                 'expr ))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExArr (_loc, (Ast.ExNil _loc)) : 'expr ))));
         ([`Skeyword "[|";
          `Snterm (Gram.obj (sem_expr : 'sem_expr Gram.t ));
          `Skeyword "|]"],
           (Gram.mk_action
              (fun _  (el : 'sem_expr)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExArr (_loc, el) : 'expr ))));
         ([`Skeyword "{";
          `Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"));
          `Skeyword "with";
          `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
          `Skeyword "}"],
           (Gram.mk_action
              (fun _  (el : 'label_expr_list)  _  (__fan_1 : [> FanToken.t]) 
                 _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `LID x ->
                     (Ast.ExRec
                        (_loc, el, (Ast.ExId (_loc, (Ast.IdLid (_loc, x))))) : 
                     'expr )
                 | _ -> assert false)));
         ([`Skeyword "{";
          `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
          `Skeyword "}"],
           (Gram.mk_action
              (fun _  (el : 'label_expr_list)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExRec (_loc, el, (Ast.ExNil _loc)) : 'expr ))));
         ([`Skeyword "{";
          `Skeyword "(";
          `Sself;
          `Skeyword ")";
          `Skeyword "with";
          `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
          `Skeyword "}"],
           (Gram.mk_action
              (fun _  (el : 'label_expr_list)  _  _  (e : 'expr)  _  _ 
                 (_loc : FanLoc.t)  -> (Ast.ExRec (_loc, el, e) : 'expr ))));
         ([`Skeyword "{<"; `Skeyword ">}"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExOvr (_loc, (Ast.RbNil _loc)) : 'expr ))));
         ([`Skeyword "{<";
          `Snterm (Gram.obj (field_expr_list : 'field_expr_list Gram.t ));
          `Skeyword ">}"],
           (Gram.mk_action
              (fun _  (fel : 'field_expr_list)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExOvr (_loc, fel) : 'expr ))));
         ([`Skeyword "("; `Skeyword ")"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) : 'expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExTyc (_loc, e, t) : 'expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (el : 'comma_expr)  _  (e : 'expr)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.ExTup (_loc, (Ast.ExCom (_loc, e, el))) : 'expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ";";
          `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (seq : 'sequence)  _  (e : 'expr)  _  (_loc : FanLoc.t)
                  ->
                 (Expr.mksequence ~loc:_loc (Ast.ExSem (_loc, e, seq)) : 
                 'expr ))));
         ([`Skeyword "("; `Sself; `Skeyword ";"; `Skeyword ")"],
           (Gram.mk_action
              (fun _  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Expr.mksequence ~loc:_loc e : 'expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'expr)  _ 
                 (_loc : FanLoc.t)  -> (Ast.ExCoe (_loc, e, t, t2) : 
                 'expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":>";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Ast.ExCoe (_loc, e, (Ast.TyNil _loc), t) : 'expr ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  -> (e : 'expr ))));
         ([`Skeyword "begin";
          `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
          `Skeyword "end"],
           (Gram.mk_action
              (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                 (Expr.mksequence ~loc:_loc seq : 'expr ))));
         ([`Skeyword "begin"; `Skeyword "end"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) : 'expr ))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (me : 'module_expr)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.ExPkg (_loc, me) : 'expr ))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (pt : 'package_type)  _  (me : 'module_expr)  _  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.ExPkg (_loc, (Ast.MeTyc (_loc, me, pt))) : 'expr ))))])]);
   Gram.extend (sequence : 'sequence Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'expr)  _  (bi : 'binding) 
                   (rf : 'opt_rec)  _  (_loc : FanLoc.t)  ->
                   (k (Ast.ExLet (_loc, rf, bi, e)) : 'sequence ))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword ";";
           `Sself],
            (Gram.mk_action
               (fun (el : 'sequence)  _  (bi : 'binding)  (rf : 'opt_rec)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.ExLet (_loc, rf, bi, (Expr.mksequence ~loc:_loc el)) : 
                  'sequence ))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
            (Gram.mk_action
               (fun (k : 'sequence')  (e : 'expr)  _  (mb : 'module_binding0)
                   (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                  (k (Ast.ExLmd (_loc, m, mb, e)) : 'sequence ))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
           `Skeyword ";";
           `Sself],
            (Gram.mk_action
               (fun (el : 'sequence)  _  (mb : 'module_binding0) 
                  (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExLmd (_loc, m, mb, (Expr.mksequence ~loc:_loc el)) : 
                  'sequence ))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Sself],
            (Gram.mk_action
               (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                  (_loc : FanLoc.t)  -> (Ast.ExOpI (_loc, i, e) : 'sequence ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.ExAnt (_loc, (mk_anti ~c:"expr;" n s)) : 'sequence )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
            (Gram.mk_action
               (fun (k : 'sequence')  (e : 'expr)  (_loc : FanLoc.t)  ->
                  (k e : 'sequence ))))])]);
   Gram.extend (sequence' : 'sequence' Gram.t )
     (None,
       [(None, None,
          [([],
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' ))));
          ([`Skeyword ";"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' ))));
          ([`Skeyword ";"; `Snterm (Gram.obj (sequence : 'sequence Gram.t ))],
            (Gram.mk_action
               (fun (el : 'sequence)  _  (_loc : FanLoc.t)  ->
                  (fun e  -> Ast.ExSem (_loc, e, el) : 'sequence' ))))])]);
   Gram.extend (infixop1 : 'infixop1 Gram.t )
     (None,
       [(None, None,
          [([Gram.srules infixop1
               [([`Skeyword "&"],
                  (Gram.mk_action
                     (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__3 ))));
               ([`Skeyword "&&"],
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__3 ))))]],
             (Gram.mk_action
                (fun (x : 'e__3)  (_loc : FanLoc.t)  ->
                   (Ast.ExId (_loc, (Ast.IdLid (_loc, x))) : 'infixop1 ))))])]);
   Gram.extend (infixop0 : 'infixop0 Gram.t )
     (None,
       [(None, None,
          [([Gram.srules infixop0
               [([`Skeyword "or"],
                  (Gram.mk_action
                     (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__4 ))));
               ([`Skeyword "||"],
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 ))))]],
             (Gram.mk_action
                (fun (x : 'e__4)  (_loc : FanLoc.t)  ->
                   (Ast.ExId (_loc, (Ast.IdLid (_loc, x))) : 'infixop0 ))))])]);
   Gram.extend (sem_expr_for_list : 'sem_expr_for_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (el : 'sem_expr_for_list)  _  (e : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      Ast.ExApp
                        (_loc,
                          (Ast.ExApp
                             (_loc,
                               (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                               e)), (el acc)) : 'sem_expr_for_list ))));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t )); `Skeyword ";"],
            (Gram.mk_action
               (fun _  (e : 'expr)  (_loc : FanLoc.t)  ->
                  (fun acc  ->
                     Ast.ExApp
                       (_loc,
                         (Ast.ExApp
                            (_loc,
                              (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e)),
                         acc) : 'sem_expr_for_list ))));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                  (fun acc  ->
                     Ast.ExApp
                       (_loc,
                         (Ast.ExApp
                            (_loc,
                              (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e)),
                         acc) : 'sem_expr_for_list ))))])]);
   Gram.extend (comma_expr : 'comma_expr Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (e2 : 'comma_expr)  _  (e1 : 'comma_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExCom (_loc, e1, e2) : 'comma_expr ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.ExAnt (_loc, (mk_anti ~c:"expr," n s)) : 'comma_expr )
                  | _ -> assert false)));
          ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
            (Gram.mk_action
               (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'comma_expr ))))])]);
   Gram.extend (dummy : 'dummy Gram.t )
     (None,
       [(None, None,
          [([], (Gram.mk_action (fun (_loc : FanLoc.t)  -> (() : 'dummy ))))])]));
  (Gram.extend (binding_quot : 'binding_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (binding : 'binding Gram.t ))],
             (Gram.mk_action
                (fun (x : 'binding)  (_loc : FanLoc.t)  ->
                   (x : 'binding_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.BiNil _loc : 'binding_quot ))))])]);
   Gram.extend (binding : 'binding Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT (("binding"|"list"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"binding\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT (("binding"|"list" as n),s) ->
                       (Ast.BiAnt (_loc, (mk_anti ~c:"binding" n s)) : 
                       'binding )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ((""|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"anti\"),_)"));
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"anti" as n),s) ->
                      (Ast.BiEq
                         (_loc, (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s))),
                           e) : 'binding )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ((""|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"anti" as n),s) ->
                      (Ast.BiAnt (_loc, (mk_anti ~c:"binding" n s)) : 
                      'binding )
                  | _ -> assert false)));
          ([`Sself; `Skeyword "and"; `Sself],
            (Gram.mk_action
               (fun (b2 : 'binding)  _  (b1 : 'binding)  (_loc : FanLoc.t) 
                  -> (Ast.BiAnd (_loc, b1, b2) : 'binding ))));
          ([`Snterm (Gram.obj (let_binding : 'let_binding Gram.t ))],
            (Gram.mk_action
               (fun (b : 'let_binding)  (_loc : FanLoc.t)  -> (b : 'binding ))))])]);
   Gram.extend (let_binding : 'let_binding Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
             (Gram.mk_action
                (fun (e : 'fun_binding)  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.BiEq (_loc, p, e) : 'let_binding ))))])]));
  (Gram.extend (match_case : 'match_case Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "[";
            `Slist0sep
              ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
                (`Skeyword "|"));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (l : 'match_case0 list)  _  (_loc : FanLoc.t)  ->
                   (Ast.mcOr_of_list l : 'match_case ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword "->";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (p : 'patt)  (_loc : FanLoc.t)  ->
                  (Ast.McArr (_loc, p, (Ast.ExNil _loc), e) : 'match_case ))))])]);
   Gram.extend (match_case0 : 'match_case0 Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT (("match_case"|"list"|"anti"|""),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANT ((\"match_case\"|\"list\"|\"anti\"|\"\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT (("match_case"|"list"|"anti"|"" as n),s) ->
                       (Ast.McAnt (_loc, (mk_anti ~c:"match_case" n s)) : 
                       'match_case0 )
                   | _ -> assert false)));
          ([`Snterm (Gram.obj (patt_as_patt_opt : 'patt_as_patt_opt Gram.t ));
           `Skeyword "when";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword "->";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (w : 'expr)  _  (p : 'patt_as_patt_opt) 
                  (_loc : FanLoc.t)  ->
                  (Ast.McArr (_loc, p, w, e) : 'match_case0 ))));
          ([`Snterm (Gram.obj (patt_as_patt_opt : 'patt_as_patt_opt Gram.t ));
           `Skeyword "->";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (p : 'patt_as_patt_opt) 
                  (_loc : FanLoc.t)  ->
                  (Ast.McArr (_loc, p, (Ast.ExNil _loc), e) : 'match_case0 ))))])]);
   Gram.extend (match_case_quot : 'match_case_quot Gram.t )
     (None,
       [(None, None,
          [([`Slist0sep
               ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
                 (`Skeyword "|"))],
             (Gram.mk_action
                (fun (x : 'match_case0 list)  (_loc : FanLoc.t)  ->
                   (Ast.mcOr_of_list x : 'match_case_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.McNil _loc : 'match_case_quot ))))])]));
  (Gram.extend (rec_binding_quot : 'rec_binding_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ))],
             (Gram.mk_action
                (fun (x : 'label_expr_list)  (_loc : FanLoc.t)  ->
                   (x : 'rec_binding_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.RbNil _loc : 'rec_binding_quot ))))])]);
   Gram.extend (label_expr : 'label_expr Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT (("rec_binding"|""|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANT ((\"rec_binding\"|\"\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT (("rec_binding"|""|"anti"|"list" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'label_expr )
                   | _ -> assert false)));
          ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
           `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
            (Gram.mk_action
               (fun (e : 'fun_binding)  (i : 'label_longident) 
                  (_loc : FanLoc.t)  ->
                  (Ast.RbEq (_loc, i, e) : 'label_expr ))));
          ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                  (Ast.RbEq
                     (_loc, i,
                       (Ast.ExId (_loc, (Ast.IdLid (_loc, (Ident.to_lid i)))))) : 
                  'label_expr ))))])]);
   Gram.extend (field_expr : 'field_expr Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"bi"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"bi\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"bi"|"anti"|"list" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'field_expr )
                   | _ -> assert false)));
          ([`Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword "=";
           `Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
            (Gram.mk_action
               (fun (e : 'expr)  _  (l : 'label)  (_loc : FanLoc.t)  ->
                  (Ast.RbEq (_loc, (Ast.IdLid (_loc, l)), e) : 'field_expr ))))])]);
   Gram.extend (label_expr_list : 'label_expr_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (b2 : 'label_expr_list)  _  (b1 : 'label_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.RbSem (_loc, b1, b2) : 'label_expr_list ))));
          ([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ));
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  (b1 : 'label_expr)  (_loc : FanLoc.t)  ->
                  (b1 : 'label_expr_list ))));
          ([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ))],
            (Gram.mk_action
               (fun (b1 : 'label_expr)  (_loc : FanLoc.t)  ->
                  (b1 : 'label_expr_list ))))])]);
   Gram.extend (field_expr_list : 'field_expr_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (b2 : 'field_expr_list)  _  (b1 : 'field_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.RbSem (_loc, b1, b2) : 'field_expr_list ))));
          ([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ));
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  (b1 : 'field_expr)  (_loc : FanLoc.t)  ->
                  (b1 : 'field_expr_list ))));
          ([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ))],
            (Gram.mk_action
               (fun (b1 : 'field_expr)  (_loc : FanLoc.t)  ->
                  (b1 : 'field_expr_list ))))])]));
  (let grammar_entry_create = Gram.mk in
   let patt_constr: 'patt_constr Gram.t = grammar_entry_create "patt_constr" in
   Gram.extend (patt_quot : 'patt_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ",";
            `Snterm (Gram.obj (comma_patt : 'comma_patt Gram.t ))],
             (Gram.mk_action
                (fun (y : 'comma_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaCom (_loc, x, y) : 'patt_quot ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword ";";
           `Snterm (Gram.obj (sem_patt : 'sem_patt Gram.t ))],
            (Gram.mk_action
               (fun (y : 'sem_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                  (Ast.PaSem (_loc, x, y) : 'patt_quot ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (y : 'patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                  (let i =
                     match x with
                     | Ast.PaAnt (loc,s) -> Ast.IdAnt (loc, s)
                     | p -> Ast.ident_of_patt p in
                   Ast.PaEq (_loc, i, y) : 'patt_quot ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (x : 'patt)  (_loc : FanLoc.t)  -> (x : 'patt_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.PaNil _loc : 'patt_quot ))))])]);
   Gram.extend (patt_as_patt_opt : 'patt_as_patt_opt Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword "as";
            `Snterm (Gram.obj (patt : 'patt Gram.t ))],
             (Gram.mk_action
                (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaAli (_loc, p1, p2) : 'patt_as_patt_opt ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'patt)  (_loc : FanLoc.t)  ->
                  (p : 'patt_as_patt_opt ))))])]);
   Gram.extend (opt_class_self_patt : 'opt_class_self_patt Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "(";
            `Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (p : 'opt_class_self_patt ))));
          ([`Skeyword "(";
           `Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaTyc (_loc, p, t) : 'opt_class_self_patt ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.PaNil _loc : 'opt_class_self_patt ))))])]);
   Gram.extend (patt_constr : 'patt_constr Gram.t )
     (None,
       [(None, None,
          [([`Snterm
               (Gram.obj (module_longident : 'module_longident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                   (Ast.PaId (_loc, i) : 'patt_constr ))));
          ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaVrn (_loc, s) : 'patt_constr ))));
          ([`Stoken
              (((function | `ANT ((""|"pat"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"pat\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"pat"|"anti" as n),s) ->
                      (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'patt_constr )
                  | _ -> assert false)))])]);
   Gram.extend (patt : 'patt Gram.t )
     (None,
       [((Some "|"), (Some `LA),
          [([`Sself; `Skeyword "|"; `Sself],
             (Gram.mk_action
                (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaOrp (_loc, p1, p2) : 'patt ))))]);
       ((Some ".."), (Some `NA),
         [([`Sself; `Skeyword ".."; `Sself],
            (Gram.mk_action
               (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                  (Ast.PaRng (_loc, p1, p2) : 'patt ))))]);
       ((Some "apply"), (Some `LA),
         [([`Snterm (Gram.obj (patt_constr : 'patt_constr Gram.t )); `Sself],
            (Gram.mk_action
               (fun (p2 : 'patt)  (p1 : 'patt_constr)  (_loc : FanLoc.t)  ->
                  (match p2 with
                   | Ast.PaTup (_loc,p) ->
                       List.fold_left
                         (fun p1  p2  -> Ast.PaApp (_loc, p1, p2)) p1
                         (Ast.list_of_patt p [])
                   | _ -> Ast.PaApp (_loc, p1, p2) : 'patt ))));
         ([`Snterm (Gram.obj (patt_constr : 'patt_constr Gram.t ))],
           (Gram.mk_action
              (fun (p1 : 'patt_constr)  (_loc : FanLoc.t)  -> (p1 : 'patt ))));
         ([`Skeyword "lazy"; `Sself],
           (Gram.mk_action
              (fun (p : 'patt)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaLaz (_loc, p) : 'patt ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `ANT
                     ((""|"pat"|"anti"|"tup"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"),_)
                     -> true
                 | _ -> false)),
                (`Normal,
                  "`ANT\n  ((\"\"|\"pat\"|\"anti\"|\"tup\"|\"int\"|\"`int\"|\"int32\"|\"`int32\"|\"int64\"|\"`int64\"|\"nativeint\"|\"`nativeint\"|\"flo\"|\"`flo\"|\"chr\"|\"`chr\"|\"str\"|\"`str\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT
                      ((""|"pat"|"anti"|"tup"|"int"|"`int"|"int32"|"`int32"|"int64"|"`int64"|"nativeint"|"`nativeint"|"flo"|"`flo"|"chr"|"`chr"|"str"|"`str"
                          as n),s)
                      -> (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 
                      'patt )
                  | _ -> assert false)));
         ([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'ident)  (_loc : FanLoc.t)  ->
                 (Ast.PaId (_loc, i) : 'patt ))));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT (_,s) -> (Ast.PaInt (_loc, s) : 'patt )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `INT32 (_,_) -> true | _ -> false)),
               (`Normal, "`INT32 (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT32 (_,s) -> (Ast.PaInt32 (_loc, s) : 'patt )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `INT64 (_,_) -> true | _ -> false)),
               (`Normal, "`INT64 (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT64 (_,s) -> (Ast.PaInt64 (_loc, s) : 'patt )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `FLO (_,_) -> true | _ -> false)),
               (`Normal, "`FLO (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `FLO (_,s) -> (Ast.PaFlo (_loc, s) : 'patt )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `CHAR (_,s) -> (Ast.PaChr (_loc, s) : 'patt )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (_,s) -> (Ast.PaStr (_loc, s) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT (_,_) -> true | _ -> false)),
              (`Normal, "`INT (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `INT (_,s) -> (Ast.PaInt (_loc, (String.neg s)) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT32 (_,_) -> true | _ -> false)),
              (`Normal, "`INT32 (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `INT32 (_,s) ->
                     (Ast.PaInt32 (_loc, (String.neg s)) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "-";
          `Stoken
            (((function | `INT64 (_,_) -> true | _ -> false)),
              (`Normal, "`INT64 (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `INT64 (_,s) ->
                     (Ast.PaInt64 (_loc, (String.neg s)) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "-";
          `Stoken
            (((function | `NATIVEINT (_,_) -> true | _ -> false)),
              (`Normal, "`NATIVEINT (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `NATIVEINT (_,s) ->
                     (Ast.PaInt64 (_loc, (String.neg s)) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "-";
          `Stoken
            (((function | `FLO (_,_) -> true | _ -> false)),
              (`Normal, "`FLO (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `FLO (_,s) -> (Ast.PaFlo (_loc, (String.neg s)) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "["; `Skeyword "]"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.PaId (_loc, (Ast.IdUid (_loc, "[]"))) : 'patt ))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_patt_for_list : 'sem_patt_for_list Gram.t ));
          `Skeyword "::";
          `Sself;
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (last : 'patt)  _  (mk_list : 'sem_patt_for_list)  _ 
                 (_loc : FanLoc.t)  -> (mk_list last : 'patt ))));
         ([`Skeyword "[";
          `Snterm (Gram.obj (sem_patt_for_list : 'sem_patt_for_list Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (mk_list : 'sem_patt_for_list)  _  (_loc : FanLoc.t) 
                 ->
                 (mk_list (Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))) : 
                 'patt ))));
         ([`Skeyword "[|"; `Skeyword "|]"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.PaArr (_loc, (Ast.PaNil _loc)) : 'patt ))));
         ([`Skeyword "[|";
          `Snterm (Gram.obj (sem_patt : 'sem_patt Gram.t ));
          `Skeyword "|]"],
           (Gram.mk_action
              (fun _  (pl : 'sem_patt)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaArr (_loc, pl) : 'patt ))));
         ([`Skeyword "{";
          `Snterm (Gram.obj (label_patt_list : 'label_patt_list Gram.t ));
          `Skeyword "}"],
           (Gram.mk_action
              (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaRec (_loc, pl) : 'patt ))));
         ([`Skeyword "("; `Skeyword ")"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.PaId (_loc, (Ast.IdUid (_loc, "()"))) : 'patt ))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.PaMod (_loc, m) : 'patt ))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (pt : 'package_type)  _  (m : 'a_UIDENT)  _  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.PaTyc
                    (_loc, (Ast.PaMod (_loc, m)), (Ast.TyPkg (_loc, pt))) : 
                 'patt ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  -> (p : 'patt ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaTyc (_loc, p, t) : 'patt ))));
         ([`Skeyword "("; `Sself; `Skeyword "as"; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (p2 : 'patt)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaAli (_loc, p, p2) : 'patt ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Snterm (Gram.obj (comma_patt : 'comma_patt Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (pl : 'comma_patt)  _  (p : 'patt)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.PaTup (_loc, (Ast.PaCom (_loc, p, pl))) : 'patt ))));
         ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
           (Gram.mk_action
              (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaVrn (_loc, s) : 'patt ))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaTyp (_loc, i) : 'patt ))));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.patt_tag : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "_"],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'patt ))));
         ([`Stoken
             (((function | `LABEL _ -> true | _ -> false)),
               (`Normal, "`LABEL _"));
          `Sself],
           (Gram.mk_action
              (fun (p : 'patt)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)
                  ->
                 match __fan_0 with
                 | `LABEL i -> (Ast.PaLab (_loc, i, p) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "~";
          `Stoken
            (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
              (`Normal, "`ANT ((\"\"|\"lid\"),_)"));
          `Skeyword ":";
          `Sself],
           (Gram.mk_action
              (fun (p : 'patt)  _  (__fan_1 : [> FanToken.t])  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `ANT ((""|"lid" as n),i) ->
                     (Ast.PaLab (_loc, (mk_anti n i), p) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "~";
          `Stoken
            (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
              (`Normal, "`ANT ((\"\"|\"lid\"),_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `ANT ((""|"lid" as n),i) ->
                     (Ast.PaLab (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                     'patt )
                 | _ -> assert false)));
         ([`Skeyword "~";
          `Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `LID i -> (Ast.PaLab (_loc, i, (Ast.PaNil _loc)) : 'patt )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `OPTLABEL _ -> true | _ -> false)),
               (`Normal, "`OPTLABEL _"));
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (f : 'eq_expr)  (p : 'patt_tcon)  _ 
                 (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `OPTLABEL i -> (f i p : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "?";
          `Stoken
            (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
              (`Normal, "`ANT ((\"\"|\"lid\"),_)"));
          `Skeyword ":";
          `Skeyword "(";
          `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
          `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (f : 'eq_expr)  (p : 'patt_tcon)  _  _ 
                 (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `ANT ((""|"lid" as n),i) -> (f (mk_anti n i) p : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "?";
          `Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `LID i -> (Ast.PaOlb (_loc, i, (Ast.PaNil _loc)) : 'patt )
                 | _ -> assert false)));
         ([`Skeyword "?";
          `Stoken
            (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
              (`Normal, "`ANT ((\"\"|\"lid\"),_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `ANT ((""|"lid" as n),i) ->
                     (Ast.PaOlb (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                     'patt )
                 | _ -> assert false)));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.PaOlb (_loc, "", p) : 'patt ))));
         ([`Skeyword "?";
          `Skeyword "(";
          `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _ 
                 (_loc : FanLoc.t)  -> (Ast.PaOlbi (_loc, "", p, e) : 
                 'patt ))))])]);
   Gram.extend (ipatt : 'ipatt Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "{";
            `Snterm (Gram.obj (label_patt_list : 'label_patt_list Gram.t ));
            `Skeyword "}"],
             (Gram.mk_action
                (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->
                   (Ast.PaRec (_loc, pl) : 'ipatt ))));
          ([`Stoken
              (((function
                 | `ANT ((""|"pat"|"anti"|"tup"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"pat\"|\"anti\"|\"tup\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"pat"|"anti"|"tup" as n),s) ->
                      (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "("; `Skeyword ")"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "()"))) : 'ipatt ))));
          ([`Skeyword "(";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaMod (_loc, m) : 'ipatt ))));
          ([`Skeyword "(";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (pt : 'package_type)  _  (m : 'a_UIDENT)  _  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.PaTyc
                     (_loc, (Ast.PaMod (_loc, m)), (Ast.TyPkg (_loc, pt))) : 
                  'ipatt ))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (p : 'ipatt)  _  (_loc : FanLoc.t)  -> (p : 'ipatt ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (p : 'ipatt)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaTyc (_loc, p, t) : 'ipatt ))));
          ([`Skeyword "("; `Sself; `Skeyword "as"; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (p2 : 'ipatt)  _  (p : 'ipatt)  _  (_loc : FanLoc.t) 
                  -> (Ast.PaAli (_loc, p, p2) : 'ipatt ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ",";
           `Snterm (Gram.obj (comma_ipatt : 'comma_ipatt Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (pl : 'comma_ipatt)  _  (p : 'ipatt)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.PaTup (_loc, (Ast.PaCom (_loc, p, pl))) : 'ipatt ))));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, (Ast.IdLid (_loc, s))) : 'ipatt ))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.patt_tag : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "_"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'ipatt ))));
          ([`Stoken
              (((function | `LABEL _ -> true | _ -> false)),
                (`Normal, "`LABEL _"));
           `Sself],
            (Gram.mk_action
               (fun (p : 'ipatt)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LABEL i -> (Ast.PaLab (_loc, i, p) : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "~";
           `Stoken
             (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"lid\"),_)"));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (p : 'ipatt)  _  (__fan_1 : [> FanToken.t])  _ 
                  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `ANT ((""|"lid" as n),i) ->
                      (Ast.PaLab (_loc, (mk_anti n i), p) : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "~";
           `Stoken
             (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"lid\"),_)"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `ANT ((""|"lid" as n),i) ->
                      (Ast.PaLab (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                      'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "~";
           `Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `LID i ->
                      (Ast.PaLab (_loc, i, (Ast.PaNil _loc)) : 'ipatt )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `OPTLABEL _ -> true | _ -> false)),
                (`Normal, "`OPTLABEL _"));
           `Skeyword "(";
           `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
           `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (f : 'eq_expr)  (p : 'patt_tcon)  _ 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `OPTLABEL i -> (f i p : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "?";
           `Stoken
             (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"lid\"),_)"));
           `Skeyword ":";
           `Skeyword "(";
           `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
           `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (f : 'eq_expr)  (p : 'patt_tcon)  _  _ 
                  (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `ANT ((""|"lid" as n),i) -> (f (mk_anti n i) p : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "?";
           `Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `LID i ->
                      (Ast.PaOlb (_loc, i, (Ast.PaNil _loc)) : 'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "?";
           `Stoken
             (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"lid\"),_)"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `ANT ((""|"lid" as n),i) ->
                      (Ast.PaOlb (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                      'ipatt )
                  | _ -> assert false)));
          ([`Skeyword "?";
           `Skeyword "(";
           `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaOlb (_loc, "", p) : 'ipatt ))));
          ([`Skeyword "?";
           `Skeyword "(";
           `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.PaOlbi (_loc, "", p, e) : 'ipatt ))))])]);
   Gram.extend (sem_patt : 'sem_patt Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANT (\"list\",_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT (("list" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt;" n s)) : 
                       'sem_patt )
                   | _ -> assert false)));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"; `Sself],
            (Gram.mk_action
               (fun (p2 : 'sem_patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                  (Ast.PaSem (_loc, p1, p2) : 'sem_patt ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"],
            (Gram.mk_action
               (fun _  (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt ))))])]);
   Gram.extend (sem_patt_for_list : 'sem_patt_for_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (pl : 'sem_patt_for_list)  _  (p : 'patt) 
                   (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      Ast.PaApp
                        (_loc,
                          (Ast.PaApp
                             (_loc,
                               (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))),
                               p)), (pl acc)) : 'sem_patt_for_list ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"],
            (Gram.mk_action
               (fun _  (p : 'patt)  (_loc : FanLoc.t)  ->
                  (fun acc  ->
                     Ast.PaApp
                       (_loc,
                         (Ast.PaApp
                            (_loc,
                              (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))), p)),
                         acc) : 'sem_patt_for_list ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'patt)  (_loc : FanLoc.t)  ->
                  (fun acc  ->
                     Ast.PaApp
                       (_loc,
                         (Ast.PaApp
                            (_loc,
                              (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))), p)),
                         acc) : 'sem_patt_for_list ))))])]);
   Gram.extend (patt_tcon : 'patt_tcon Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaTyc (_loc, p, t) : 'patt_tcon ))));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'patt_tcon ))))])]);
   Gram.extend (ipatt_tcon : 'ipatt_tcon Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"anti" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'ipatt_tcon )
                   | _ -> assert false)));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, (Ast.IdLid (_loc, i))) : 'ipatt_tcon ))));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.PaTyc
                     (_loc, (Ast.PaId (_loc, (Ast.IdLid (_loc, i)))), t) : 
                  'ipatt_tcon ))))])]);
   Gram.extend (eq_expr : 'eq_expr Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (fun i  p  -> Ast.PaOlbi (_loc, i, p, e) : 'eq_expr ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (fun i  p  -> Ast.PaOlb (_loc, i, p) : 'eq_expr ))))])]);
   Gram.extend (comma_ipatt : 'comma_ipatt Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (p2 : 'comma_ipatt)  _  (p1 : 'comma_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaCom (_loc, p1, p2) : 'comma_ipatt ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.PaAnt (_loc, (mk_anti ~c:"patt," n s)) : 'comma_ipatt )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'ipatt)  (_loc : FanLoc.t)  -> (p : 'comma_ipatt ))))])]);
   Gram.extend (comma_patt : 'comma_patt Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (p2 : 'comma_patt)  _  (p1 : 'comma_patt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaCom (_loc, p1, p2) : 'comma_patt ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.PaAnt (_loc, (mk_anti ~c:"patt," n s)) : 'comma_patt )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'comma_patt ))))])]);
   Gram.extend (label_patt_list : 'label_patt_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (p2 : 'label_patt_list)  _  (p1 : 'label_patt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, p2) : 'label_patt_list ))));
          ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
           `Skeyword ";";
           `Skeyword "_"],
            (Gram.mk_action
               (fun _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                  (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) : 'label_patt_list ))));
          ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
           `Skeyword ";";
           `Skeyword "_";
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                  (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) : 'label_patt_list ))));
          ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                  (p1 : 'label_patt_list ))));
          ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ))],
            (Gram.mk_action
               (fun (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                  (p1 : 'label_patt_list ))))])]);
   Gram.extend (label_patt : 'label_patt Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"pat"|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"pat\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"pat"|"anti" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'label_patt )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.patt_tag : 'label_patt )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.PaAnt (_loc, (mk_anti ~c:"patt;" n s)) : 'label_patt )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (patt : 'patt Gram.t ))],
            (Gram.mk_action
               (fun (p : 'patt)  _  (i : 'label_longident)  (_loc : FanLoc.t)
                   -> (Ast.PaEq (_loc, i, p) : 'label_patt ))));
          ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                  (Ast.PaEq
                     (_loc, i,
                       (Ast.PaId (_loc, (Ast.IdLid (_loc, (Ident.to_lid i)))))) : 
                  'label_patt ))))])]));
  (Gram.extend (ctyp_quot : 'ctyp_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
            `Skeyword ",";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (y : 'comma_ctyp)  _  (x : 'more_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyCom (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword ";";
           `Snterm
             (Gram.obj
                (label_declaration_list : 'label_declaration_list Gram.t ))],
            (Gram.mk_action
               (fun (y : 'label_declaration_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TySem (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "|";
           `Snterm
             (Gram.obj
                (constructor_declarations : 'constructor_declarations Gram.t ))],
            (Gram.mk_action
               (fun (y : 'constructor_declarations)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (Ast.TyOr (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "of";
           `Snterm
             (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
            (Gram.mk_action
               (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  -> (Ast.TyOf (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "of";
           `Snterm
             (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ));
           `Skeyword "|";
           `Snterm
             (Gram.obj
                (constructor_declarations : 'constructor_declarations Gram.t ))],
            (Gram.mk_action
               (fun (z : 'constructor_declarations)  _ 
                  (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyOr (_loc, (Ast.TyOf (_loc, x, y)), z) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "of";
           `Skeyword "&";
           `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (y : 'amp_ctyp)  _  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyOfAmp (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "of";
           `Skeyword "&";
           `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ));
           `Skeyword "|";
           `Snterm (Gram.obj (row_field : 'row_field Gram.t ))],
            (Gram.mk_action
               (fun (z : 'row_field)  _  (y : 'amp_ctyp)  _  _ 
                  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->
                  (Ast.TyOr (_loc, (Ast.TyOfAmp (_loc, x, y)), z) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (y : 'more_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                  -> (Ast.TyCol (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword ";";
           `Snterm
             (Gram.obj
                (label_declaration_list : 'label_declaration_list Gram.t ))],
            (Gram.mk_action
               (fun (z : 'label_declaration_list)  _  (y : 'more_ctyp)  _ 
                  (x : 'more_ctyp)  (_loc : FanLoc.t)  ->
                  (Ast.TySem (_loc, (Ast.TyCol (_loc, x, y)), z) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "*";
           `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (y : 'star_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                  -> (Ast.TySta (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "&";
           `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (y : 'amp_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                  -> (Ast.TyAmp (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
           `Skeyword "and";
           `Snterm
             (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
            (Gram.mk_action
               (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyAnd (_loc, x, y) : 'ctyp_quot ))));
          ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (x : 'more_ctyp)  (_loc : FanLoc.t)  -> (x : 'ctyp_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'ctyp_quot ))))])]);
   Gram.extend (more_ctyp : 'more_ctyp Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "mutable"; `Sself],
             (Gram.mk_action
                (fun (x : 'more_ctyp)  _  (_loc : FanLoc.t)  ->
                   (Ast.TyMut (_loc, x) : 'more_ctyp ))));
          ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (x : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrn (_loc, x) : 'more_ctyp ))));
          ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (x : 'ctyp)  (_loc : FanLoc.t)  -> (x : 'more_ctyp ))));
          ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
            (Gram.mk_action
               (fun (x : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (x : 'more_ctyp ))))])]);
   Gram.extend (type_parameter : 'type_parameter Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ"|"anti" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti n s)) : 'type_parameter )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'type_parameter )
                  | _ -> assert false)));
          ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuo (_loc, i) : 'type_parameter ))));
          ([`Skeyword "+";
           `Skeyword "'";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuP (_loc, i) : 'type_parameter ))));
          ([`Skeyword "-";
           `Skeyword "'";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuM (_loc, i) : 'type_parameter ))))])]);
   Gram.extend
     (type_ident_and_parameters : 'type_ident_and_parameters Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Slist0
              (`Snterm
                 (Gram.obj
                    (optional_type_parameter : 'optional_type_parameter
                                                 Gram.t )))],
             (Gram.mk_action
                (fun (tpl : 'optional_type_parameter list)  (i : 'a_LIDENT) 
                   (_loc : FanLoc.t)  ->
                   ((i, tpl) : 'type_ident_and_parameters ))))])]);
   Gram.extend (optional_type_parameter : 'optional_type_parameter Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ"|"anti" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti n s)) : 'optional_type_parameter )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'optional_type_parameter )
                  | _ -> assert false)));
          ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuo (_loc, i) : 'optional_type_parameter ))));
          ([`Skeyword "+";
           `Skeyword "'";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuP (_loc, i) : 'optional_type_parameter ))));
          ([`Skeyword "-";
           `Skeyword "'";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuM (_loc, i) : 'optional_type_parameter ))));
          ([`Skeyword "+"; `Skeyword "_"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyAnP _loc : 'optional_type_parameter ))));
          ([`Skeyword "-"; `Skeyword "_"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyAnM _loc : 'optional_type_parameter ))));
          ([`Skeyword "_"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (Ast.TyAny _loc : 'optional_type_parameter ))))])]);
   Gram.extend
     (type_longident_and_parameters : 'type_longident_and_parameters Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ));
            `Snterm (Gram.obj (type_parameters : 'type_parameters Gram.t ))],
             (Gram.mk_action
                (fun (tpl : 'type_parameters)  (i : 'type_longident) 
                   (_loc : FanLoc.t)  ->
                   (tpl (Ast.TyId (_loc, i)) : 'type_longident_and_parameters ))));
          ([`Stoken
              (((function | `ANT ((""|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"anti" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti n s ~c:"ctyp")) : 'type_longident_and_parameters )
                  | _ -> assert false)))])]);
   Gram.extend (type_parameters : 'type_parameters Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : FanLoc.t)  ->
                   (fun acc  -> t2 (Ast.TyApp (_loc, acc, t1)) : 'type_parameters ))));
          ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
            (Gram.mk_action
               (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (fun acc  -> Ast.TyApp (_loc, acc, t) : 'type_parameters ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (fun t  -> t : 'type_parameters ))))])]);
   Gram.extend (opt_class_self_type : 'opt_class_self_type Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "(";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (t : 'opt_class_self_type ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.TyNil _loc : 'opt_class_self_type ))))])]);
   Gram.extend (type_constraint : 'type_constraint Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "type"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (() : 'type_constraint ))));
          ([`Skeyword "constraint"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (() : 'type_constraint ))))])]);
   Gram.extend (meth_list : 'meth_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  _  (m : 'meth_decl) 
                   (_loc : FanLoc.t)  ->
                   (((Ast.TySem (_loc, m, ml)), v) : 'meth_list ))));
          ([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
           `Skeyword ";";
           `Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl) 
                  (_loc : FanLoc.t)  -> ((m, v) : 'meth_list ))));
          ([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
           `Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : FanLoc.t) 
                  -> ((m, v) : 'meth_list ))))])]);
   Gram.extend (meth_decl : 'meth_decl Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'meth_decl )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp;" n s)) : 'meth_decl )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'meth_decl )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (lab : 'a_LIDENT)  (_loc : FanLoc.t)
                   ->
                  (Ast.TyCol
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, lab)))), t) : 
                  'meth_decl ))))])]);
   Gram.extend (opt_meth_list : 'opt_meth_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (meth_list : 'meth_list Gram.t ))],
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  (_loc : FanLoc.t)  ->
                   (Ast.TyObj (_loc, ml, v) : 'opt_meth_list ))));
          ([`Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
            (Gram.mk_action
               (fun (v : 'opt_dot_dot)  (_loc : FanLoc.t)  ->
                  (Ast.TyObj (_loc, (Ast.TyNil _loc), v) : 'opt_meth_list ))))])]);
   Gram.extend (poly_type : 'poly_type Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'poly_type ))))])]);
   Gram.extend (package_type : 'package_type Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (p : 'module_type)  (_loc : FanLoc.t)  ->
                   (p : 'package_type ))))])]);
   Gram.extend (unquoted_typevars : 'unquoted_typevars Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Sself],
             (Gram.mk_action
                (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyApp (_loc, t1, t2) : 'unquoted_typevars ))));
          ([`Stoken
              (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"typ" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'unquoted_typevars )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'unquoted_typevars )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  (_loc : FanLoc.t)  ->
                  (Ast.TyId (_loc, (Ast.IdLid (_loc, i))) : 'unquoted_typevars ))))])]);
   Gram.extend (row_field : 'row_field Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'row_field )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp|" n s)) : 'row_field )
                  | _ -> assert false)));
          ([`Sself; `Skeyword "|"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'row_field)  _  (t1 : 'row_field) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyOr (_loc, t1, t2) : 'row_field ))));
          ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrn (_loc, i) : 'row_field ))));
          ([`Skeyword "`";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
           `Skeyword "of";
           `Skeyword "&";
           `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'amp_ctyp)  _  _  (i : 'a_ident)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyOfAmp (_loc, (Ast.TyVrn (_loc, i)), t) : 'row_field ))));
          ([`Skeyword "`";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
           `Skeyword "of";
           `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'amp_ctyp)  _  (i : 'a_ident)  _  (_loc : FanLoc.t) 
                  ->
                  (Ast.TyOf (_loc, (Ast.TyVrn (_loc, i)), t) : 'row_field ))));
          ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'row_field ))))])]);
   Gram.extend (amp_ctyp : 'amp_ctyp Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "&"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'amp_ctyp)  _  (t1 : 'amp_ctyp)  (_loc : FanLoc.t)
                    -> (Ast.TyAmp (_loc, t1, t2) : 'amp_ctyp ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp&" n s)) : 'amp_ctyp )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'amp_ctyp ))))])]);
   Gram.extend (name_tags : 'name_tags Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'name_tags )
                   | _ -> assert false)));
          ([`Sself; `Sself],
            (Gram.mk_action
               (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : FanLoc.t) 
                  -> (Ast.TyApp (_loc, t1, t2) : 'name_tags ))));
          ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrn (_loc, i) : 'name_tags ))))])]);
   Gram.extend (opt_polyt : 'opt_polyt Gram.t )
     (None,
       [(None, None,
          [([`Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (_loc : FanLoc.t)  ->
                   (t : 'opt_polyt ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'opt_polyt ))))])]);
   Gram.extend (type_declaration : 'type_declaration Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ"|"anti" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'type_declaration )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctypand" n s)) : 
                      'type_declaration )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'type_declaration )
                  | _ -> assert false)));
          ([`Sself; `Skeyword "and"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyAnd (_loc, t1, t2) : 'type_declaration ))));
          ([`Snterm
              (Gram.obj
                 (type_ident_and_parameters : 'type_ident_and_parameters
                                                Gram.t ));
           `Snterm (Gram.obj (opt_eq_ctyp : 'opt_eq_ctyp Gram.t ));
           `Slist0 (`Snterm (Gram.obj (constrain : 'constrain Gram.t )))],
            (Gram.mk_action
               (fun (cl : 'constrain list)  (tk : 'opt_eq_ctyp) 
                  ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t) 
                  -> (Ast.TyDcl (_loc, n, tpl, tk, cl) : 'type_declaration ))))])]);
   Gram.extend (constrain : 'constrain Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "constraint";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   ((t1, t2) : 'constrain ))))])]);
   Gram.extend (opt_eq_ctyp : 'opt_eq_ctyp Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "=";
            `Snterm (Gram.obj (type_kind : 'type_kind Gram.t ))],
             (Gram.mk_action
                (fun (tk : 'type_kind)  _  (_loc : FanLoc.t)  ->
                   (tk : 'opt_eq_ctyp ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'opt_eq_ctyp ))))])]);
   Gram.extend (type_kind : 'type_kind Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'type_kind ))))])]);
   Gram.extend (typevars : 'typevars Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Sself],
             (Gram.mk_action
                (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : FanLoc.t) 
                   -> (Ast.TyApp (_loc, t1, t2) : 'typevars ))));
          ([`Stoken
              (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"typ" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'typevars )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"forall" n s)) : 
                      'typevars )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'typevars )
                  | _ -> assert false)));
          ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuo (_loc, i) : 'typevars ))))])]);
   Gram.extend (ctyp : 'ctyp Gram.t )
     (None,
       [((Some "=="), (Some `LA),
          [([`Sself; `Skeyword "=="; `Sself],
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                   (Ast.TyMan (_loc, t1, t2) : 'ctyp ))))]);
       ((Some "private"), (Some `NA),
         [([`Skeyword "private";
           `Snterml ((Gram.obj (ctyp : 'ctyp Gram.t )), "alias")],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyPrv (_loc, t) : 'ctyp ))))]);
       ((Some "alias"), (Some `LA),
         [([`Sself; `Skeyword "as"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                  (Ast.TyAli (_loc, t1, t2) : 'ctyp ))))]);
       ((Some "forall"), (Some `LA),
         [([`Skeyword "!";
           `Snterm (Gram.obj (typevars : 'typevars Gram.t ));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _  (_loc : FanLoc.t) 
                  -> (Ast.TyPol (_loc, t1, t2) : 'ctyp ))))]);
       ((Some "arrow"), (Some `RA),
         [([`Sself; `Skeyword "->"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                  (Ast.TyArr (_loc, t1, t2) : 'ctyp ))))]);
       ((Some "label"), (Some `NA),
         [([`Skeyword "~";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyLab (_loc, i, t) : 'ctyp ))));
         ([`Snterm (Gram.obj (a_LABEL : 'a_LABEL Gram.t )); `Sself],
           (Gram.mk_action
              (fun (t : 'ctyp)  (i : 'a_LABEL)  (_loc : FanLoc.t)  ->
                 (Ast.TyLab (_loc, i, t) : 'ctyp ))));
         ([`Skeyword "?";
          `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
          `Skeyword ":";
          `Sself],
           (Gram.mk_action
              (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                 (Ast.TyOlb (_loc, i, t) : 'ctyp ))));
         ([`Snterm (Gram.obj (a_OPTLABEL : 'a_OPTLABEL Gram.t )); `Sself],
           (Gram.mk_action
              (fun (t : 'ctyp)  (i : 'a_OPTLABEL)  (_loc : FanLoc.t)  ->
                 (Ast.TyOlb (_loc, i, t) : 'ctyp ))))]);
       ((Some "apply"), (Some `LA),
         [([`Sself; `Sself],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                  (let t = Ast.TyApp (_loc, t1, t2) in
                   try Ast.TyId (_loc, (Ast.ident_of_ctyp t))
                   with | Invalid_argument _ -> t : 'ctyp ))))]);
       ((Some "."), (Some `LA),
         [([`Sself; `Skeyword "."; `Sself],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : FanLoc.t)  ->
                  (try
                     Ast.TyId
                       (_loc,
                         (Ast.IdAcc
                            (_loc, (Ast.ident_of_ctyp t1),
                              (Ast.ident_of_ctyp t2))))
                   with | Invalid_argument s -> raise (XStream.Error s) : 
                  'ctyp ))))]);
       ((Some "simple"), None,
         [([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuo (_loc, i) : 'ctyp ))));
         ([`Skeyword "_"],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> (Ast.TyAny _loc : 'ctyp ))));
         ([`Stoken
             (((function | `ANT ((""|"typ"|"anti"),_) -> true | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"typ\"|\"anti\"),_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT ((""|"typ"|"anti" as n),s) ->
                     (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'ctyp )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `ANT ("tup",_) -> true | _ -> false)),
               (`Normal, "`ANT (\"tup\",_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT (("tup" as n),s) ->
                     (Ast.TyTup
                        (_loc, (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)))) : 
                     'ctyp )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `ANT ("id",_) -> true | _ -> false)),
               (`Normal, "`ANT (\"id\",_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT (("id" as n),s) ->
                     (Ast.TyId
                        (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)))) : 
                     'ctyp )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.ctyp_tag : 'ctyp )
                 | _ -> assert false)));
         ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
           (Gram.mk_action
              (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                 (Ast.TyId (_loc, (Ast.IdLid (_loc, i))) : 'ctyp ))));
         ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
           (Gram.mk_action
              (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                 (Ast.TyId (_loc, (Ast.IdUid (_loc, i))) : 'ctyp ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword "*";
          `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)
                  -> (Ast.TyTup (_loc, (Ast.TySta (_loc, t, tl))) : 'ctyp ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'ctyp ))));
         ([`Skeyword "["; `Skeyword "]"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (Ast.TySum (_loc, (Ast.TyNil _loc)) : 'ctyp ))));
         ([`Skeyword "[";
          `Snterm
            (Gram.obj
               (constructor_declarations : 'constructor_declarations Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (t : 'constructor_declarations)  _  (_loc : FanLoc.t) 
                 -> (Ast.TySum (_loc, t) : 'ctyp ))));
         ([`Skeyword "[";
          `Skeyword "=";
          `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.TyVrnEq (_loc, rfl) : 'ctyp ))));
         ([`Skeyword "["; `Skeyword ">"; `Skeyword "]"],
           (Gram.mk_action
              (fun _  _  _  (_loc : FanLoc.t)  ->
                 (Ast.TyVrnSup (_loc, (Ast.TyNil _loc)) : 'ctyp ))));
         ([`Skeyword "[";
          `Skeyword ">";
          `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.TyVrnSup (_loc, rfl) : 'ctyp ))));
         ([`Skeyword "[";
          `Skeyword "<";
          `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.TyVrnInf (_loc, rfl) : 'ctyp ))));
         ([`Skeyword "[";
          `Skeyword "<";
          `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
          `Skeyword ">";
          `Snterm (Gram.obj (name_tags : 'name_tags Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.TyVrnInfSup (_loc, rfl, ntl) : 'ctyp ))));
         ([`Skeyword "[<";
          `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->
                 (Ast.TyVrnInf (_loc, rfl) : 'ctyp ))));
         ([`Skeyword "[<";
          `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
          `Skeyword ">";
          `Snterm (Gram.obj (name_tags : 'name_tags Gram.t ));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.TyVrnInfSup (_loc, rfl, ntl) : 'ctyp ))));
         ([`Skeyword "{";
          `Snterm
            (Gram.obj
               (label_declaration_list : 'label_declaration_list Gram.t ));
          `Skeyword "}"],
           (Gram.mk_action
              (fun _  (t : 'label_declaration_list)  _  (_loc : FanLoc.t)  ->
                 (Ast.TyRec (_loc, t) : 'ctyp ))));
         ([`Skeyword "#";
          `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                 (Ast.TyCls (_loc, i) : 'ctyp ))));
         ([`Skeyword "<";
          `Snterm (Gram.obj (opt_meth_list : 'opt_meth_list Gram.t ));
          `Skeyword ">"],
           (Gram.mk_action
              (fun _  (t : 'opt_meth_list)  _  (_loc : FanLoc.t)  ->
                 (t : 'ctyp ))));
         ([`Skeyword "(";
          `Skeyword "module";
          `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (p : 'package_type)  _  _  (_loc : FanLoc.t)  ->
                 (Ast.TyPkg (_loc, p) : 'ctyp ))))])]);
   Gram.extend (star_ctyp : 'star_ctyp Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'star_ctyp )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp*" n s)) : 'star_ctyp )
                  | _ -> assert false)));
          ([`Sself; `Skeyword "*"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TySta (_loc, t1, t2) : 'star_ctyp ))));
          ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'star_ctyp ))))])]);
   Gram.extend (constructor_declarations : 'constructor_declarations Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'constructor_declarations )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp|" n s)) : 'constructor_declarations )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'constructor_declarations )
                  | _ -> assert false)));
          ([`Sself; `Skeyword "|"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'constructor_declarations)  _ 
                  (t1 : 'constructor_declarations)  (_loc : FanLoc.t)  ->
                  (Ast.TyOr (_loc, t1, t2) : 'constructor_declarations ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword "of";
           `Snterm
             (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
            (Gram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_UIDENT) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyOf
                     (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))), t) : 
                  'constructor_declarations ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (s : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (let (tl,rt) = Ctyp.to_generalized t in
                   Ast.TyCol
                     (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))),
                       (Ast.TyArr (_loc, (Ast.tyAnd_of_list tl), rt))) : 
                  'constructor_declarations ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.TyId (_loc, (Ast.IdUid (_loc, s))) : 'constructor_declarations ))))])]);
   Gram.extend (constructor_declaration : 'constructor_declaration Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'constructor_declaration )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'constructor_declaration )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword "of";
           `Snterm
             (Gram.obj (constructor_arg_list : 'constructor_arg_list Gram.t ))],
            (Gram.mk_action
               (fun (t : 'constructor_arg_list)  _  (s : 'a_UIDENT) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyOf
                     (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))), t) : 
                  'constructor_declaration ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.TyId (_loc, (Ast.IdUid (_loc, s))) : 'constructor_declaration ))))])]);
   Gram.extend (constructor_arg_list : 'constructor_arg_list Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANT (\"list\",_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctypand" n s)) : 
                       'constructor_arg_list )
                   | _ -> assert false)));
          ([`Sself; `Skeyword "and"; `Sself],
            (Gram.mk_action
               (fun (t2 : 'constructor_arg_list)  _ 
                  (t1 : 'constructor_arg_list)  (_loc : FanLoc.t)  ->
                  (Ast.TyAnd (_loc, t1, t2) : 'constructor_arg_list ))));
          ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  ->
                  (t : 'constructor_arg_list ))))])]);
   Gram.extend (label_declaration_list : 'label_declaration_list Gram.t )
     (None,
       [(None, None,
          [([`Snterm
               (Gram.obj (label_declaration : 'label_declaration Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (t2 : 'label_declaration_list)  _ 
                   (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                   (Ast.TySem (_loc, t1, t2) : 'label_declaration_list ))));
          ([`Snterm
              (Gram.obj (label_declaration : 'label_declaration Gram.t ));
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                  (t1 : 'label_declaration_list ))));
          ([`Snterm
              (Gram.obj (label_declaration : 'label_declaration Gram.t ))],
            (Gram.mk_action
               (fun (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                  (t1 : 'label_declaration_list ))))])]);
   Gram.extend (label_declaration : 'label_declaration Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'label_declaration )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp;" n s)) : 'label_declaration )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.ctyp_tag : 'label_declaration )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (s : 'a_LIDENT)  (_loc : FanLoc.t) 
                  ->
                  (Ast.TyCol
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))), t) : 
                  'label_declaration ))));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Skeyword "mutable";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  _  (s : 'a_LIDENT) 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyCol
                     (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))),
                       (Ast.TyMut (_loc, t))) : 'label_declaration ))))])]);
   Gram.extend (class_name_and_param : 'class_name_and_param Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword "[";
            `Snterm
              (Gram.obj
                 (comma_type_parameter : 'comma_type_parameter Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (x : 'comma_type_parameter)  _  (i : 'a_LIDENT) 
                   (_loc : FanLoc.t)  -> ((i, x) : 'class_name_and_param ))));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  ((i, (Ast.TyNil _loc)) : 'class_name_and_param ))))])]);
   Gram.extend (comma_type_parameter : 'comma_type_parameter Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (t2 : 'comma_type_parameter)  _ 
                   (t1 : 'comma_type_parameter)  (_loc : FanLoc.t)  ->
                   (Ast.TyCom (_loc, t1, t2) : 'comma_type_parameter ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp," n s)) : 'comma_type_parameter )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
            (Gram.mk_action
               (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                  (t : 'comma_type_parameter ))))])]);
   Gram.extend (opt_comma_ctyp : 'opt_comma_ctyp Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "[";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (x : 'comma_ctyp)  _  (_loc : FanLoc.t)  ->
                   (x : 'opt_comma_ctyp ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'opt_comma_ctyp ))))])]);
   Gram.extend (comma_ctyp : 'comma_ctyp Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyCom (_loc, t1, t2) : 'comma_ctyp ))));
          ([`Stoken
              (((function | `ANT ("list",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"list\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("list" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp," n s)) : 'comma_ctyp )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'comma_ctyp ))))])]));
  (Gram.extend (a_ident : 'a_ident Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  -> (i : 'a_ident ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  -> (i : 'a_ident ))))])]);
   Gram.extend (ident_quot : 'ident_quot Gram.t )
     (None,
       [((Some "."), None,
          [([`Sself; `Skeyword "."; `Sself],
             (Gram.mk_action
                (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.IdAcc (_loc, i, j) : 'ident_quot ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `ANT ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'ident_quot )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `ANT ("lid",_) -> true | _ -> false)),
               (`Normal, "`ANT (\"lid\",_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT (("lid" as n),s) ->
                     (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'ident_quot )
                 | _ -> assert false)));
         ([`Stoken
             (((function
                | `ANT ((""|"id"|"anti"|"list"|"uid"),_) -> true
                | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
          `Skeyword ".";
          `Sself],
           (Gram.mk_action
              (fun (i : 'ident_quot)  _  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                     (Ast.IdAcc
                        (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))),
                          i) : 'ident_quot )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `LID i -> (Ast.IdLid (_loc, i) : 'ident_quot )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID i -> (Ast.IdUid (_loc, i) : 'ident_quot )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"));
          `Skeyword ".";
          `Sself],
           (Gram.mk_action
              (fun (j : 'ident_quot)  _  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID s ->
                     (Ast.IdAcc (_loc, (Ast.IdUid (_loc, s)), j) : 'ident_quot )
                 | _ -> assert false)));
         ([`Skeyword "("; `Sself; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.IdApp (_loc, i, j) : 'ident_quot ))))])]);
   Gram.extend (ident : 'ident Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"id"|"anti"|"list"|"uid"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'ident )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ("lid",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"lid\",_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("lid" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'ident )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANT ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (i : 'ident)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                      (Ast.IdAcc
                         (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))),
                           i) : 'ident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `LID _ -> true | _ -> false)),
                (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LID i -> (Ast.IdLid (_loc, i) : 'ident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID i -> (Ast.IdUid (_loc, i) : 'ident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (j : 'ident)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID s ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, s)), j) : 'ident )
                  | _ -> assert false)))])]);
   Gram.extend
     (module_longident_dot_lparen : 'module_longident_dot_lparen Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"id"|"anti"|"list"|"uid"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"));
            `Skeyword ".";
            `Skeyword "("],
             (Gram.mk_action
                (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'module_longident_dot_lparen )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (l : 'module_longident_dot_lparen)  _ 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID i ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)), l) : 'module_longident_dot_lparen )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Skeyword "("],
            (Gram.mk_action
               (fun _  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID i ->
                      (Ast.IdUid (_loc, i) : 'module_longident_dot_lparen )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT (("uid"|""),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"uid\"|\"\"),_)"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (l : 'module_longident_dot_lparen)  _ 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("uid"|"" as n),s) ->
                      (Ast.IdAcc
                         (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))),
                           l) : 'module_longident_dot_lparen )
                  | _ -> assert false)))])]);
   Gram.extend (module_longident : 'module_longident Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"id"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"id"|"anti"|"list" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'module_longident )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID i ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)), l) : 'module_longident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID i -> (Ast.IdUid (_loc, i) : 'module_longident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ((""|"uid"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"uid\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"uid" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ((""|"uid"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"uid\"),_)"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (l : 'module_longident)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"uid" as n),s) ->
                      (Ast.IdAcc
                         (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))),
                           l) : 'module_longident )
                  | _ -> assert false)))])]);
   Gram.extend
     (module_longident_with_app : 'module_longident_with_app Gram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             (Gram.mk_action
                (fun (j : 'module_longident_with_app) 
                   (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                   (Ast.IdApp (_loc, i, j) : 'module_longident_with_app ))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            (Gram.mk_action
               (fun (j : 'module_longident_with_app)  _ 
                  (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                  (Ast.IdAcc (_loc, i, j) : 'module_longident_with_app ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `ANT ((""|"id"|"anti"|"list"|"uid"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"id"|"anti"|"list"|"uid" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident_with_app )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID i ->
                     (Ast.IdUid (_loc, i) : 'module_longident_with_app )
                 | _ -> assert false)));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (i : 'module_longident_with_app)  _  (_loc : FanLoc.t) 
                 -> (i : 'module_longident_with_app ))))])]);
   Gram.extend (type_longident : 'type_longident Gram.t )
     (None,
       [((Some "apply"), None,
          [([`Sself; `Sself],
             (Gram.mk_action
                (fun (j : 'type_longident)  (i : 'type_longident) 
                   (_loc : FanLoc.t)  ->
                   (Ast.IdApp (_loc, i, j) : 'type_longident ))))]);
       ((Some "."), None,
         [([`Sself; `Skeyword "."; `Sself],
            (Gram.mk_action
               (fun (j : 'type_longident)  _  (i : 'type_longident) 
                  (_loc : FanLoc.t)  ->
                  (Ast.IdAcc (_loc, i, j) : 'type_longident ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function
                 | `ANT ((""|"id"|"anti"|"list"|"uid"|"lid"),_) -> true
                 | _ -> false)),
                (`Normal,
                  "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"uid\"|\"lid\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"id"|"anti"|"list"|"uid"|"lid" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'type_longident )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `LID i -> (Ast.IdLid (_loc, i) : 'type_longident )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID i -> (Ast.IdUid (_loc, i) : 'type_longident )
                 | _ -> assert false)));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                 (i : 'type_longident ))))])]);
   Gram.extend (label_longident : 'label_longident Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"id"|"anti"|"list"|"lid"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANT ((\"\"|\"id\"|\"anti\"|\"list\"|\"lid\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"id"|"anti"|"list"|"lid" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'label_longident )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `LID _ -> true | _ -> false)),
                (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LID i -> (Ast.IdLid (_loc, i) : 'label_longident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID i ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)), l) : 'label_longident )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANT ((""|"uid"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"uid\"),_)"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (l : 'label_longident)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"uid" as n),s) ->
                      (Ast.IdAcc
                         (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))),
                           l) : 'label_longident )
                  | _ -> assert false)))])]);
   Gram.extend (class_type_longident : 'class_type_longident Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
             (Gram.mk_action
                (fun (x : 'type_longident)  (_loc : FanLoc.t)  ->
                   (x : 'class_type_longident ))))])]);
   Gram.extend (val_longident : 'val_longident Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
             (Gram.mk_action
                (fun (x : 'ident)  (_loc : FanLoc.t)  ->
                   (x : 'val_longident ))))])]);
   Gram.extend (class_longident : 'class_longident Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
             (Gram.mk_action
                (fun (x : 'label_longident)  (_loc : FanLoc.t)  ->
                   (x : 'class_longident ))))])]);
   Gram.extend (method_opt_override : 'method_opt_override Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "method"; `Skeyword "!"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.OvOverride : 'method_opt_override ))));
          ([`Skeyword "method";
           `Stoken
             (((function
                | `ANT ((""|"override"|"anti"),_) -> true
                | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"override\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `ANT ((""|"override"|"anti" as n),s) ->
                      (Ast.OvAnt (mk_anti ~c:"override_flag" n s) : 'method_opt_override )
                  | _ -> assert false)));
          ([`Skeyword "method"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (Ast.OvNil : 'method_opt_override ))))])]);
   Gram.extend (opt_override : 'opt_override Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "!"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (Ast.OvOverride : 'opt_override ))));
          ([`Stoken
              (((function
                 | `ANT (("!"|"override"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"!\"|\"override\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("!"|"override"|"anti" as n),s) ->
                      (Ast.OvAnt (mk_anti ~c:"override_flag" n s) : 'opt_override )
                  | _ -> assert false)));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.OvNil : 'opt_override ))))])]);
   Gram.extend (value_val_opt_override : 'value_val_opt_override Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "val"; `Skeyword "!"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.OvOverride : 'value_val_opt_override ))));
          ([`Skeyword "val";
           `Stoken
             (((function
                | `ANT ((""|"override"|"anti"),_) -> true
                | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"override\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `ANT ((""|"override"|"anti" as n),s) ->
                      (Ast.OvAnt (mk_anti ~c:"override_flag" n s) : 'value_val_opt_override )
                  | _ -> assert false)));
          ([`Skeyword "val"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (Ast.OvNil : 'value_val_opt_override ))))])]);
   Gram.extend (opt_as_lident : 'opt_as_lident Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "as";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                   (i : 'opt_as_lident ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> ("" : 'opt_as_lident ))))])]);
   Gram.extend (label : 'label Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  -> (i : 'label ))))])]);
   Gram.extend (direction_flag : 'direction_flag Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "to"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.DiTo : 'direction_flag ))));
          ([`Skeyword "downto"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (Ast.DiDownto : 'direction_flag ))));
          ([`Stoken
              (((function | `ANT (("to"|"anti"|""),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"to\"|\"anti\"|\"\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("to"|"anti"|"" as n),s) ->
                      (Ast.DiAnt (mk_anti ~c:"direction_flag" n s) : 
                      'direction_flag )
                  | _ -> assert false)))])]);
   Gram.extend (opt_private : 'opt_private Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "private"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.PrPrivate : 'opt_private ))));
          ([`Stoken
              (((function | `ANT (("private"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"private\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("private"|"anti" as n),s) ->
                      (Ast.PrAnt (mk_anti ~c:"private_flag" n s) : 'opt_private )
                  | _ -> assert false)));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.PrNil : 'opt_private ))))])]);
   Gram.extend (opt_mutable : 'opt_mutable Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "mutable"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.MuMutable : 'opt_mutable ))));
          ([`Stoken
              (((function | `ANT (("mutable"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"mutable\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("mutable"|"anti" as n),s) ->
                      (Ast.MuAnt (mk_anti ~c:"mutable_flag" n s) : 'opt_mutable )
                  | _ -> assert false)));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.MuNil : 'opt_mutable ))))])]);
   Gram.extend (opt_virtual : 'opt_virtual Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "virtual"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.ViVirtual : 'opt_virtual ))));
          ([`Stoken
              (((function | `ANT (("virtual"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"virtual\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("virtual"|"anti" as n),s) ->
                      (Ast.ViAnt (mk_anti ~c:"virtual_flag" n s) : 'opt_virtual )
                  | _ -> assert false)));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.ViNil : 'opt_virtual ))))])]);
   Gram.extend (opt_dot_dot : 'opt_dot_dot Gram.t )
     (None,
       [(None, None,
          [([`Skeyword ".."],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.RvRowVar : 'opt_dot_dot ))));
          ([`Stoken
              (((function | `ANT ((".."|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"..\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((".."|"anti" as n),s) ->
                      (Ast.RvAnt (mk_anti ~c:"row_var_flag" n s) : 'opt_dot_dot )
                  | _ -> assert false)));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.RvNil : 'opt_dot_dot ))))])]);
   Gram.extend (opt_rec : 'opt_rec Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "rec"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.ReRecursive : 'opt_rec ))));
          ([`Stoken
              (((function | `ANT (("rec"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"rec\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("rec"|"anti" as n),s) ->
                      (Ast.ReAnt (mk_anti ~c:"rec_flag" n s) : 'opt_rec )
                  | _ -> assert false)));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.ReNil : 'opt_rec ))))])]);
   Gram.extend (a_UIDENT : 'a_UIDENT Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"uid"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"uid\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"uid" as n),s) -> (mk_anti n s : 'a_UIDENT )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID s -> (s : 'a_UIDENT )
                  | _ -> assert false)))])]);
   Gram.extend (a_LIDENT : 'a_LIDENT Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"lid"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"lid\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"lid" as n),s) -> (mk_anti n s : 'a_LIDENT )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `LID _ -> true | _ -> false)),
                (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LID s -> (s : 'a_LIDENT )
                  | _ -> assert false)))])]);
   Gram.extend (a_LABEL : 'a_LABEL Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "~";
            `Stoken
              (((function | `ANT ("",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"\",_)"));
            `Skeyword ":"],
             (Gram.mk_action
                (fun _  (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `ANT (("" as n),s) -> (mk_anti n s : 'a_LABEL )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `LABEL _ -> true | _ -> false)),
                (`Normal, "`LABEL _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LABEL s -> (s : 'a_LABEL )
                  | _ -> assert false)))])]);
   Gram.extend (a_OPTLABEL : 'a_OPTLABEL Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "?";
            `Stoken
              (((function | `ANT ("",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"\",_)"));
            `Skeyword ":"],
             (Gram.mk_action
                (fun _  (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `ANT (("" as n),s) -> (mk_anti n s : 'a_OPTLABEL )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `OPTLABEL _ -> true | _ -> false)),
                (`Normal, "`OPTLABEL _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `OPTLABEL s -> (s : 'a_OPTLABEL )
                  | _ -> assert false)))])]);
   Gram.extend (string_list : 'string_list Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function | `ANT ((""|"str_list"),_) -> true | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"str_list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"str_list"),s) ->
                       (Ast.LAnt (mk_anti "str_list" s) : 'string_list )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `STR (_,_) -> true | _ -> false)),
                (`Normal, "`STR (_,_)"));
           `Sself],
            (Gram.mk_action
               (fun (xs : 'string_list)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (_,x) -> (Ast.LCons (x, xs) : 'string_list )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `STR (_,_) -> true | _ -> false)),
                (`Normal, "`STR (_,_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (_,x) -> (Ast.LCons (x, Ast.LNil) : 'string_list )
                  | _ -> assert false)))])]);
   Gram.extend (semi : 'semi Gram.t )
     (None,
       [(None, None,
          [([`Skeyword ";"],
             (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'semi ))))])]);
   Gram.extend (rec_flag_quot : 'rec_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ))],
             (Gram.mk_action
                (fun (x : 'opt_rec)  (_loc : FanLoc.t)  ->
                   (x : 'rec_flag_quot ))))])]);
   Gram.extend (direction_flag_quot : 'direction_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (direction_flag : 'direction_flag Gram.t ))],
             (Gram.mk_action
                (fun (x : 'direction_flag)  (_loc : FanLoc.t)  ->
                   (x : 'direction_flag_quot ))))])]);
   Gram.extend (mutable_flag_quot : 'mutable_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ))],
             (Gram.mk_action
                (fun (x : 'opt_mutable)  (_loc : FanLoc.t)  ->
                   (x : 'mutable_flag_quot ))))])]);
   Gram.extend (private_flag_quot : 'private_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_private : 'opt_private Gram.t ))],
             (Gram.mk_action
                (fun (x : 'opt_private)  (_loc : FanLoc.t)  ->
                   (x : 'private_flag_quot ))))])]);
   Gram.extend (virtual_flag_quot : 'virtual_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ))],
             (Gram.mk_action
                (fun (x : 'opt_virtual)  (_loc : FanLoc.t)  ->
                   (x : 'virtual_flag_quot ))))])]);
   Gram.extend (row_var_flag_quot : 'row_var_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
             (Gram.mk_action
                (fun (x : 'opt_dot_dot)  (_loc : FanLoc.t)  ->
                   (x : 'row_var_flag_quot ))))])]);
   Gram.extend (override_flag_quot : 'override_flag_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_override : 'opt_override Gram.t ))],
             (Gram.mk_action
                (fun (x : 'opt_override)  (_loc : FanLoc.t)  ->
                   (x : 'override_flag_quot ))))])]);
   Gram.extend (patt_eoi : 'patt_eoi Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Stoken
              (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  (x : 'patt) 
                   (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `EOI -> (x : 'patt_eoi )
                   | _ -> assert false)))])]);
   Gram.extend (expr_eoi : 'expr_eoi Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Stoken
              (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  (x : 'expr) 
                   (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `EOI -> (x : 'expr_eoi )
                   | _ -> assert false)))])]));
  (Gram.extend (implem : 'implem Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Skeyword ";;"],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (([Ast.StDir (_loc, n, dp)], (Some _loc)) : 'implem ))));
          ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun ((sil,stopped) : 'implem)  _  (si : 'str_item) 
                  (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'implem ))));
          ([`Stoken
              (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `EOI -> (([], None) : 'implem )
                  | _ -> assert false)))])]);
   Gram.extend (str_items : 'str_items Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"stri"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"stri"|"anti"|"list" as n),s) ->
                       (Ast.StAnt (_loc, (mk_anti n ~c:"str_item" s)) : 
                       'str_items )
                   | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANT ((""|"stri"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (st : 'str_items)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"stri"|"anti"|"list" as n),s) ->
                      (Ast.StSem
                         (_loc,
                           (Ast.StAnt (_loc, (mk_anti n ~c:"str_item" s))),
                           st) : 'str_items )
                  | _ -> assert false)));
          ([`Slist0
              (Gram.srules str_items
                 [([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (st : 'str_item)  (_loc : FanLoc.t)  ->
                          (st : 'e__5 ))))])],
            (Gram.mk_action
               (fun (l : 'e__5 list)  (_loc : FanLoc.t)  ->
                  (Ast.stSem_of_list l : 'str_items ))))])]);
   Gram.extend (top_phrase : 'top_phrase Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Skeyword ";;"],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (Some (Ast.StDir (_loc, n, dp)) : 'top_phrase ))));
          ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ))],
            (Gram.mk_action
               (fun _  (st : 'str_item)  (_loc : FanLoc.t)  ->
                  (Some st : 'top_phrase ))));
          ([`Stoken
              (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `EOI -> (None : 'top_phrase )
                  | _ -> assert false)))])]);
   Gram.extend (str_item_quot : 'str_item_quot Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ))],
             (Gram.mk_action
                (fun (dp : 'opt_expr)  (n : 'a_LIDENT)  _  (_loc : FanLoc.t) 
                   -> (Ast.StDir (_loc, n, dp) : 'str_item_quot ))));
          ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (st2 : 'str_item_quot)  _  (st1 : 'str_item) 
                  (_loc : FanLoc.t)  ->
                  (match st2 with
                   | Ast.StNil _loc -> st1
                   | _ -> Ast.StSem (_loc, st1, st2) : 'str_item_quot ))));
          ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ))],
            (Gram.mk_action
               (fun (st : 'str_item)  (_loc : FanLoc.t)  ->
                  (st : 'str_item_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> (Ast.StNil _loc : 'str_item_quot ))))])]);
   Gram.extend (str_item : 'str_item Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "exception";
            `Snterm
              (Gram.obj
                 (constructor_declaration : 'constructor_declaration Gram.t ))],
             (Gram.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->
                   (Ast.StExc (_loc, t, Ast.ONone) : 'str_item ))));
          ([`Skeyword "exception";
           `Snterm
             (Gram.obj
                (constructor_declaration : 'constructor_declaration Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'type_longident)  _  (t : 'constructor_declaration) 
                  _  (_loc : FanLoc.t)  ->
                  (Ast.StExc (_loc, t, (Ast.OSome i)) : 'str_item ))));
          ([`Skeyword "external";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (string_list : 'string_list Gram.t ))],
            (Gram.mk_action
               (fun (sl : 'string_list)  _  (t : 'ctyp)  _  (i : 'a_LIDENT) 
                  _  (_loc : FanLoc.t)  ->
                  (Ast.StExt (_loc, i, t, sl) : 'str_item ))));
          ([`Skeyword "include";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
            (Gram.mk_action
               (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.StInc (_loc, me) : 'str_item ))));
          ([`Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ))],
            (Gram.mk_action
               (fun (mb : 'module_binding0)  (i : 'a_UIDENT)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.StMod (_loc, i, mb) : 'str_item ))));
          ([`Skeyword "module";
           `Skeyword "rec";
           `Snterm (Gram.obj (module_binding : 'module_binding Gram.t ))],
            (Gram.mk_action
               (fun (mb : 'module_binding)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.StRecMod (_loc, mb) : 'str_item ))));
          ([`Skeyword "module";
           `Skeyword "type";
           `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
            (Gram.mk_action
               (fun (mt : 'module_type)  _  (i : 'a_ident)  _  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.StMty (_loc, i, mt) : 'str_item ))));
          ([`Skeyword "open";
           `Stoken
             (((function | `LID "lang" -> true | _ -> false)),
               (`Normal, "`LID \"lang\""));
           `Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
            (Gram.mk_action
               (fun (__fan_2 : [> FanToken.t])  (__fan_1 : [> FanToken.t])  _
                   (_loc : FanLoc.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`STR (_,s),`LID "lang") ->
                      ((AstQuotation.default := s; Ast.StNil _loc) : 
                      'str_item )
                  | _ -> assert false)));
          ([`Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.StOpn (_loc, i) : 'str_item ))));
          ([`Skeyword "type";
           `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
            (Gram.mk_action
               (fun (td : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                  (Ast.StTyp (_loc, td) : 'str_item ))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.StExp (_loc, (Ast.ExLet (_loc, r, bi, x))) : 'str_item ))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ))],
            (Gram.mk_action
               (fun (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t)  ->
                  (match bi with
                   | Ast.BiEq (_loc,Ast.PaAny _,e) -> Ast.StExp (_loc, e)
                   | _ -> Ast.StVal (_loc, r, bi) : 'str_item ))));
          ([`Skeyword "let";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (mb : 'module_binding0)  (m : 'a_UIDENT) 
                  _  _  (_loc : FanLoc.t)  ->
                  (Ast.StExp (_loc, (Ast.ExLmd (_loc, m, mb, e))) : 'str_item ))));
          ([`Skeyword "let";
           `Skeyword "open";
           `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (i : 'module_longident)  _  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.StExp (_loc, (Ast.ExOpI (_loc, i, e))) : 'str_item ))));
          ([`Skeyword "let";
           `Skeyword "try";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword "with";
           `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
            (Gram.mk_action
               (fun (a : 'match_case)  _  (x : 'expr)  _  (bi : 'binding) 
                  (r : 'opt_rec)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.StExp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExTry
                               (_loc,
                                 (Ast.ExLet
                                    (_loc, r, bi,
                                      (Ast.ExFun
                                         (_loc,
                                           (Ast.McArr
                                              (_loc,
                                                (Ast.PaId
                                                   (_loc,
                                                     (Ast.IdUid (_loc, "()")))),
                                                (Ast.ExNil _loc), x)))))),
                                 (Ast.match_pre#match_case a))),
                            (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))))))) : 
                  'str_item ))));
          ([`Skeyword "class";
           `Snterm
             (Gram.obj (class_declaration : 'class_declaration Gram.t ))],
            (Gram.mk_action
               (fun (cd : 'class_declaration)  _  (_loc : FanLoc.t)  ->
                  (Ast.StCls (_loc, cd) : 'str_item ))));
          ([`Skeyword "class";
           `Skeyword "type";
           `Snterm
             (Gram.obj
                (class_type_declaration : 'class_type_declaration Gram.t ))],
            (Gram.mk_action
               (fun (ctd : 'class_type_declaration)  _  _  (_loc : FanLoc.t) 
                  -> (Ast.StClt (_loc, ctd) : 'str_item ))));
          ([`Stoken
              (((function
                 | `ANT ((""|"stri"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"stri"|"anti"|"list" as n),s) ->
                      (Ast.StAnt (_loc, (mk_anti ~c:"str_item" n s)) : 
                      'str_item )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.str_item_tag : 
                      'str_item )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.StExp (_loc, e) : 'str_item ))))])]));
  (Gram.extend (class_sig_item_quot : 'class_sig_item_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (class_sig_item : 'class_sig_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (x2 : 'class_sig_item_quot)  _  (x1 : 'class_sig_item) 
                   (_loc : FanLoc.t)  ->
                   (match x2 with
                    | Ast.CgNil _loc -> x1
                    | _ -> Ast.CgSem (_loc, x1, x2) : 'class_sig_item_quot ))));
          ([`Snterm (Gram.obj (class_sig_item : 'class_sig_item Gram.t ))],
            (Gram.mk_action
               (fun (x : 'class_sig_item)  (_loc : FanLoc.t)  ->
                  (x : 'class_sig_item_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.CgNil _loc : 'class_sig_item_quot ))))])]);
   Gram.extend (class_signature : 'class_signature Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"csg"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"csg"|"anti"|"list" as n),s) ->
                       (Ast.CgAnt (_loc, (mk_anti ~c:"class_sig_item" n s)) : 
                       'class_signature )
                   | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANT ((""|"csg"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (csg : 'class_signature)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"csg"|"anti"|"list" as n),s) ->
                      (Ast.CgSem
                         (_loc,
                           (Ast.CgAnt
                              (_loc, (mk_anti ~c:"class_sig_item" n s))),
                           csg) : 'class_signature )
                  | _ -> assert false)));
          ([`Slist0
              (Gram.srules class_signature
                 [([`Snterm
                      (Gram.obj (class_sig_item : 'class_sig_item Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (csg : 'class_sig_item)  (_loc : FanLoc.t)  ->
                          (csg : 'e__6 ))))])],
            (Gram.mk_action
               (fun (l : 'e__6 list)  (_loc : FanLoc.t)  ->
                  (Ast.cgSem_of_list l : 'class_signature ))))])]);
   Gram.extend (class_sig_item : 'class_sig_item Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"csg"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"csg"|"anti"|"list" as n),s) ->
                       (Ast.CgAnt (_loc, (mk_anti ~c:"class_sig_item" n s)) : 
                       'class_sig_item )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.class_sig_item_tag : 
                      'class_sig_item )
                  | _ -> assert false)));
          ([`Skeyword "inherit";
           `Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
            (Gram.mk_action
               (fun (cs : 'class_type)  _  (_loc : FanLoc.t)  ->
                  (Ast.CgInh (_loc, cs) : 'class_sig_item ))));
          ([`Skeyword "val";
           `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
           `Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (l : 'label)  (mv : 'opt_virtual) 
                  (mf : 'opt_mutable)  _  (_loc : FanLoc.t)  ->
                  (Ast.CgVal (_loc, l, mf, mv, t) : 'class_sig_item ))));
          ([`Skeyword "method";
           `Skeyword "virtual";
           `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  (pf : 'opt_private)  _
                   _  (_loc : FanLoc.t)  ->
                  (Ast.CgVir (_loc, l, pf, t) : 'class_sig_item ))));
          ([`Skeyword "method";
           `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  (pf : 'opt_private)  _
                   (_loc : FanLoc.t)  ->
                  (Ast.CgMth (_loc, l, pf, t) : 'class_sig_item ))));
          ([`Skeyword "method";
           `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
           `Skeyword "virtual";
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  _  (pf : 'opt_private)
                   _  (_loc : FanLoc.t)  ->
                  (Ast.CgVir (_loc, l, pf, t) : 'class_sig_item ))));
          ([`Snterm (Gram.obj (type_constraint : 'type_constraint Gram.t ));
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                  (Ast.CgCtr (_loc, t1, t2) : 'class_sig_item ))))])]));
  (Gram.extend (class_structure : 'class_structure Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"cst"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"cst"|"anti"|"list" as n),s) ->
                       (Ast.CrAnt (_loc, (mk_anti ~c:"class_str_item" n s)) : 
                       'class_structure )
                   | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANT ((""|"cst"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"));
           `Snterm (Gram.obj (semi : 'semi Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (cst : 'class_structure)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"cst"|"anti"|"list" as n),s) ->
                      (Ast.CrSem
                         (_loc,
                           (Ast.CrAnt
                              (_loc, (mk_anti ~c:"class_str_item" n s))),
                           cst) : 'class_structure )
                  | _ -> assert false)));
          ([`Slist0
              (Gram.srules class_structure
                 [([`Snterm
                      (Gram.obj (class_str_item : 'class_str_item Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (cst : 'class_str_item)  (_loc : FanLoc.t)  ->
                          (cst : 'e__7 ))))])],
            (Gram.mk_action
               (fun (l : 'e__7 list)  (_loc : FanLoc.t)  ->
                  (Ast.crSem_of_list l : 'class_structure ))))])]);
   Gram.extend (class_str_item : 'class_str_item Gram.t )
     (None,
       [(None, None,
          [([`Stoken
               (((function
                  | `ANT ((""|"cst"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANT ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `ANT ((""|"cst"|"anti"|"list" as n),s) ->
                       (Ast.CrAnt (_loc, (mk_anti ~c:"class_str_item" n s)) : 
                       'class_str_item )
                   | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.class_str_item_tag : 
                      'class_str_item )
                  | _ -> assert false)));
          ([`Skeyword "inherit";
           `Snterm (Gram.obj (opt_override : 'opt_override Gram.t ));
           `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ));
           `Snterm (Gram.obj (opt_as_lident : 'opt_as_lident Gram.t ))],
            (Gram.mk_action
               (fun (pb : 'opt_as_lident)  (ce : 'class_expr) 
                  (o : 'opt_override)  _  (_loc : FanLoc.t)  ->
                  (Ast.CrInh (_loc, o, ce, pb) : 'class_str_item ))));
          ([`Snterm
              (Gram.obj
                 (value_val_opt_override : 'value_val_opt_override Gram.t ));
           `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Snterm (Gram.obj (cvalue_binding : 'cvalue_binding Gram.t ))],
            (Gram.mk_action
               (fun (e : 'cvalue_binding)  (lab : 'label) 
                  (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CrVal (_loc, lab, o, mf, e) : 'class_str_item ))));
          ([`Snterm
              (Gram.obj
                 (value_val_opt_override : 'value_val_opt_override Gram.t ));
           `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
           `Skeyword "virtual";
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  _  (mf : 'opt_mutable)
                   (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->
                  (if o <> Ast.OvNil
                   then
                     raise
                       (XStream.Error
                          "override (!) is incompatible with virtual")
                   else Ast.CrVvr (_loc, l, mf, t) : 'class_str_item ))));
          ([`Snterm
              (Gram.obj
                 (value_val_opt_override : 'value_val_opt_override Gram.t ));
           `Skeyword "virtual";
           `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  (mf : 'opt_mutable)  _
                   (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->
                  (if o <> Ast.OvNil
                   then
                     raise
                       (XStream.Error
                          "override (!) is incompatible with virtual")
                   else Ast.CrVvr (_loc, l, mf, t) : 'class_str_item ))));
          ([`Snterm
              (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
           `Skeyword "virtual";
           `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  (pf : 'opt_private)  _
                   (o : 'method_opt_override)  (_loc : FanLoc.t)  ->
                  (if o <> Ast.OvNil
                   then
                     raise
                       (XStream.Error
                          "override (!) is incompatible with virtual")
                   else Ast.CrVir (_loc, l, pf, t) : 'class_str_item ))));
          ([`Snterm
              (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
           `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Snterm (Gram.obj (opt_polyt : 'opt_polyt Gram.t ));
           `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
            (Gram.mk_action
               (fun (e : 'fun_binding)  (topt : 'opt_polyt)  (l : 'label) 
                  (pf : 'opt_private)  (o : 'method_opt_override) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CrMth (_loc, l, o, pf, e, topt) : 'class_str_item ))));
          ([`Snterm
              (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
           `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
           `Skeyword "virtual";
           `Snterm (Gram.obj (label : 'label Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
            (Gram.mk_action
               (fun (t : 'poly_type)  _  (l : 'label)  _  (pf : 'opt_private)
                   (o : 'method_opt_override)  (_loc : FanLoc.t)  ->
                  (if o <> Ast.OvNil
                   then
                     raise
                       (XStream.Error
                          "override (!) is incompatible with virtual")
                   else Ast.CrVir (_loc, l, pf, t) : 'class_str_item ))));
          ([`Snterm (Gram.obj (type_constraint : 'type_constraint Gram.t ));
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
            (Gram.mk_action
               (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                  (Ast.CrCtr (_loc, t1, t2) : 'class_str_item ))));
          ([`Skeyword "initializer";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (se : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.CrIni (_loc, se) : 'class_str_item ))))])]);
   Gram.extend (class_str_item_quot : 'class_str_item_quot Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (class_str_item : 'class_str_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (x2 : 'class_str_item_quot)  _  (x1 : 'class_str_item) 
                   (_loc : FanLoc.t)  ->
                   (match x2 with
                    | Ast.CrNil _loc -> x1
                    | _ -> Ast.CrSem (_loc, x1, x2) : 'class_str_item_quot ))));
          ([`Snterm (Gram.obj (class_str_item : 'class_str_item Gram.t ))],
            (Gram.mk_action
               (fun (x : 'class_str_item)  (_loc : FanLoc.t)  ->
                  (x : 'class_str_item_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.CrNil _loc : 'class_str_item_quot ))))])]));
  (Gram.extend (class_expr_quot : 'class_expr_quot Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeAnd (_loc, ce1, ce2) : 'class_expr_quot ))));
          ([`Sself; `Skeyword "="; `Sself],
            (Gram.mk_action
               (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CeEq (_loc, ce1, ce2) : 'class_expr_quot ))));
          ([`Skeyword "virtual";
           `Snterm
             (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
            (Gram.mk_action
               (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t) 
                  ->
                  (Ast.CeCon (_loc, Ast.ViVirtual, (Ast.IdLid (_loc, i)), ot) : 
                  'class_expr_quot ))));
          ([`Stoken
              (((function | `ANT ("virtual",_) -> true | _ -> false)),
                (`Normal, "`ANT (\"virtual\",_)"));
           `Snterm (Gram.obj (ident : 'ident Gram.t ));
           `Snterm (Gram.obj (opt_comma_ctyp : 'opt_comma_ctyp Gram.t ))],
            (Gram.mk_action
               (fun (ot : 'opt_comma_ctyp)  (i : 'ident) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT (("virtual" as n),s) ->
                      (let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
                       Ast.CeCon (_loc, anti, i, ot) : 'class_expr_quot )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
            (Gram.mk_action
               (fun (x : 'class_expr)  (_loc : FanLoc.t)  ->
                  (x : 'class_expr_quot ))));
          ([],
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  ->
                  (Ast.CeNil _loc : 'class_expr_quot ))))])]);
   Gram.extend (class_declaration : 'class_declaration Gram.t )
     (None,
       [(None, None,
          [([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeAnd (_loc, c1, c2) : 'class_declaration ))));
          ([`Stoken
              (((function
                 | `ANT ((""|"cdcl"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"cdcl\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"cdcl"|"anti"|"list" as n),s) ->
                      (Ast.CeAnt (_loc, (mk_anti ~c:"class_expr" n s)) : 
                      'class_declaration )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `QUOTATION x ->
                      (AstQuotation.expand _loc x DynAst.class_expr_tag : 
                      'class_declaration )
                  | _ -> assert false)));
          ([`Snterm
              (Gram.obj
                 (class_info_for_class_expr : 'class_info_for_class_expr
                                                Gram.t ));
           `Snterm
             (Gram.obj (class_fun_binding : 'class_fun_binding Gram.t ))],
            (Gram.mk_action
               (fun (ce : 'class_fun_binding) 
                  (ci : 'class_info_for_class_expr)  (_loc : FanLoc.t)  ->
                  (Ast.CeEq (_loc, ci, ce) : 'class_declaration ))))])]);
   Gram.extend (class_fun_binding : 'class_fun_binding Gram.t )
     (None,
       [(None, None,
          [([`Skeyword "=";
            `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
             (Gram.mk_action
                (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                   (ce : 'class_fun_binding ))));
          ([`Skeyword ":";
           `Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ));
           `Skeyword "=";
           `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
            (Gram.mk_action
               (fun (ce : 'class_expr)  _  (ct : 'class_type_plus)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.CeTyc (_loc, ce, ct) : 'class_fun_binding ))));
          ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t )); `Sself],
            (Gram.mk_action
               (fun (cfb : 'class_fun_binding)  (p : 'ipatt) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CeFun (_loc, p, cfb) : 'class_fun_binding ))))])]);
   Gram.extend
     (class_info_for_class_expr : 'class_info_for_class_expr Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
            `Snterm
              (Gram.obj
                 (class_name_and_param : 'class_name_and_param Gram.t ))],
             (Gram.mk_action
                (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeCon (_loc, mv, (Ast.IdLid (_loc, i)), ot) : 
                   'class_info_for_class_expr ))))])]);
   Gram.extend (class_fun_def : 'class_fun_def Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t )); `Sself],
             (Gram.mk_action
                (fun (ce : 'class_fun_def)  (p : 'ipatt)  (_loc : FanLoc.t) 
                   -> (Ast.CeFun (_loc, p, ce) : 'class_fun_def ))));
          ([`Skeyword "->";
           `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
            (Gram.mk_action
               (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                  (ce : 'class_fun_def ))))])]);
   Gram.extend (class_expr : 'class_expr Gram.t )
     (None,
       [((Some "top"), None,
          [([`Skeyword "fun";
            `Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
            `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
             (Gram.mk_action
                (fun (ce : 'class_fun_def)  (p : 'ipatt)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeFun (_loc, p, ce) : 'class_expr ))));
          ([`Skeyword "function";
           `Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
           `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
            (Gram.mk_action
               (fun (ce : 'class_fun_def)  (p : 'ipatt)  _  (_loc : FanLoc.t)
                   -> (Ast.CeFun (_loc, p, ce) : 'class_expr ))));
          ([`Skeyword "let";
           `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
           `Snterm (Gram.obj (binding : 'binding Gram.t ));
           `Skeyword "in";
           `Sself],
            (Gram.mk_action
               (fun (ce : 'class_expr)  _  (bi : 'binding)  (rf : 'opt_rec) 
                  _  (_loc : FanLoc.t)  ->
                  (Ast.CeLet (_loc, rf, bi, ce) : 'class_expr ))))]);
       ((Some "apply"), (Some `NA),
         [([`Sself; `Snterml ((Gram.obj (expr : 'expr Gram.t )), "label")],
            (Gram.mk_action
               (fun (e : 'expr)  (ce : 'class_expr)  (_loc : FanLoc.t)  ->
                  (Ast.CeApp (_loc, ce, e) : 'class_expr ))))]);
       ((Some "simple"), None,
         [([`Stoken
              (((function | `ANT ((""|"cexp"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"cexp\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"cexp"|"anti" as n),s) ->
                      (Ast.CeAnt (_loc, (mk_anti ~c:"class_expr" n s)) : 
                      'class_expr )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.class_expr_tag : 
                     'class_expr )
                 | _ -> assert false)));
         ([`Snterm
             (Gram.obj
                (class_longident_and_param : 'class_longident_and_param
                                               Gram.t ))],
           (Gram.mk_action
              (fun (ce : 'class_longident_and_param)  (_loc : FanLoc.t)  ->
                 (ce : 'class_expr ))));
         ([`Skeyword "object";
          `Snterm
            (Gram.obj (opt_class_self_patt : 'opt_class_self_patt Gram.t ));
          `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
          `Skeyword "end"],
           (Gram.mk_action
              (fun _  (cst : 'class_structure)  (csp : 'opt_class_self_patt) 
                 _  (_loc : FanLoc.t)  ->
                 (Ast.CeStr (_loc, csp, cst) : 'class_expr ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ":";
          `Snterm (Gram.obj (class_type : 'class_type Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (ct : 'class_type)  _  (ce : 'class_expr)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.CeTyc (_loc, ce, ct) : 'class_expr ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                 (ce : 'class_expr ))))])]);
   Gram.extend
     (class_longident_and_param : 'class_longident_and_param Gram.t )
     (None,
       [(None, None,
          [([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ));
            `Skeyword "[";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (t : 'comma_ctyp)  _  (ci : 'class_longident) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeCon (_loc, Ast.ViNil, ci, t) : 'class_longident_and_param ))));
          ([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
            (Gram.mk_action
               (fun (ci : 'class_longident)  (_loc : FanLoc.t)  ->
                  (Ast.CeCon (_loc, Ast.ViNil, ci, (Ast.TyNil _loc)) : 
                  'class_longident_and_param ))))])]));
  Gram.extend (class_description : 'class_description Gram.t )
    (None,
      [(None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            (Gram.mk_action
               (fun (cd2 : 'class_description)  _  (cd1 : 'class_description)
                   (_loc : FanLoc.t)  ->
                  (Ast.CtAnd (_loc, cd1, cd2) : 'class_description ))));
         ([`Stoken
             (((function
                | `ANT ((""|"typ"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT ((""|"typ"|"anti"|"list" as n),s) ->
                     (Ast.CtAnt (_loc, (mk_anti ~c:"class_type" n s)) : 
                     'class_description )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.class_type_tag : 
                     'class_description )
                 | _ -> assert false)));
         ([`Snterm
             (Gram.obj
                (class_info_for_class_type : 'class_info_for_class_type
                                               Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ))],
           (Gram.mk_action
              (fun (ct : 'class_type_plus)  _ 
                 (ci : 'class_info_for_class_type)  (_loc : FanLoc.t)  ->
                 (Ast.CtCol (_loc, ci, ct) : 'class_description ))))])]);
  Gram.extend (class_type_declaration : 'class_type_declaration Gram.t )
    (None,
      [(None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            (Gram.mk_action
               (fun (cd2 : 'class_type_declaration)  _ 
                  (cd1 : 'class_type_declaration)  (_loc : FanLoc.t)  ->
                  (Ast.CtAnd (_loc, cd1, cd2) : 'class_type_declaration ))));
         ([`Stoken
             (((function
                | `ANT ((""|"typ"|"anti"|"list"),_) -> true
                | _ -> false)),
               (`Normal, "`ANT ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT ((""|"typ"|"anti"|"list" as n),s) ->
                     (Ast.CtAnt (_loc, (mk_anti ~c:"class_type" n s)) : 
                     'class_type_declaration )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.class_type_tag : 
                     'class_type_declaration )
                 | _ -> assert false)));
         ([`Snterm
             (Gram.obj
                (class_info_for_class_type : 'class_info_for_class_type
                                               Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
           (Gram.mk_action
              (fun (ct : 'class_type)  _  (ci : 'class_info_for_class_type) 
                 (_loc : FanLoc.t)  ->
                 (Ast.CtEq (_loc, ci, ct) : 'class_type_declaration ))))])]);
  Gram.extend
    (class_info_for_class_type : 'class_info_for_class_type Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
           `Snterm
             (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
            (Gram.mk_action
               (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CtCon (_loc, mv, (Ast.IdLid (_loc, i)), ot) : 
                  'class_info_for_class_type ))))])]);
  Gram.extend (class_type_quot : 'class_type_quot Gram.t )
    (None,
      [(None, None,
         [([`Sself; `Skeyword "and"; `Sself],
            (Gram.mk_action
               (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CtAnd (_loc, ct1, ct2) : 'class_type_quot ))));
         ([`Sself; `Skeyword "="; `Sself],
           (Gram.mk_action
              (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                 (_loc : FanLoc.t)  ->
                 (Ast.CtEq (_loc, ct1, ct2) : 'class_type_quot ))));
         ([`Sself; `Skeyword ":"; `Sself],
           (Gram.mk_action
              (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                 (_loc : FanLoc.t)  ->
                 (Ast.CtCol (_loc, ct1, ct2) : 'class_type_quot ))));
         ([`Skeyword "virtual";
          `Snterm
            (Gram.obj (class_name_and_param : 'class_name_and_param Gram.t ))],
           (Gram.mk_action
              (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t)  ->
                 (Ast.CtCon (_loc, Ast.ViVirtual, (Ast.IdLid (_loc, i)), ot) : 
                 'class_type_quot ))));
         ([`Stoken
             (((function | `ANT ("virtual",_) -> true | _ -> false)),
               (`Normal, "`ANT (\"virtual\",_)"));
          `Snterm (Gram.obj (ident : 'ident Gram.t ));
          `Snterm (Gram.obj (opt_comma_ctyp : 'opt_comma_ctyp Gram.t ))],
           (Gram.mk_action
              (fun (ot : 'opt_comma_ctyp)  (i : 'ident) 
                 (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT (("virtual" as n),s) ->
                     (let anti = Ast.ViAnt (mk_anti ~c:"class_type" n s) in
                      Ast.CtCon (_loc, anti, i, ot) : 'class_type_quot )
                 | _ -> assert false)));
         ([`Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ))],
           (Gram.mk_action
              (fun (x : 'class_type_plus)  (_loc : FanLoc.t)  ->
                 (x : 'class_type_quot ))));
         ([],
           (Gram.mk_action
              (fun (_loc : FanLoc.t)  -> (Ast.CtNil _loc : 'class_type_quot ))))])]);
  Gram.extend (class_type_plus : 'class_type_plus Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "[";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword "]";
           `Skeyword "->";
           `Sself],
            (Gram.mk_action
               (fun (ct : 'class_type_plus)  _  _  (t : 'ctyp)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.CtFun (_loc, t, ct) : 'class_type_plus ))));
         ([`Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
           (Gram.mk_action
              (fun (ct : 'class_type)  (_loc : FanLoc.t)  ->
                 (ct : 'class_type_plus ))))])]);
  Gram.extend (class_type : 'class_type Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `ANT ((""|"ctyp"|"anti"),_) -> true | _ -> false)),
                (`Normal, "`ANT ((\"\"|\"ctyp\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `ANT ((""|"ctyp"|"anti" as n),s) ->
                      (Ast.CtAnt (_loc, (mk_anti ~c:"class_type" n s)) : 
                      'class_type )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `QUOTATION _ -> true | _ -> false)),
               (`Normal, "`QUOTATION _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `QUOTATION x ->
                     (AstQuotation.expand _loc x DynAst.class_type_tag : 
                     'class_type )
                 | _ -> assert false)));
         ([`Snterm
             (Gram.obj
                (class_type_longident_and_param : 'class_type_longident_and_param
                                                    Gram.t ))],
           (Gram.mk_action
              (fun (ct : 'class_type_longident_and_param)  (_loc : FanLoc.t) 
                 -> (ct : 'class_type ))));
         ([`Skeyword "object";
          `Snterm
            (Gram.obj (opt_class_self_type : 'opt_class_self_type Gram.t ));
          `Snterm (Gram.obj (class_signature : 'class_signature Gram.t ));
          `Skeyword "end"],
           (Gram.mk_action
              (fun _  (csg : 'class_signature)  (cst : 'opt_class_self_type) 
                 _  (_loc : FanLoc.t)  ->
                 (Ast.CtSig (_loc, cst, csg) : 'class_type ))))])]);
  Gram.extend
    (class_type_longident_and_param : 'class_type_longident_and_param Gram.t )
    (None,
      [(None, None,
         [([`Snterm
              (Gram.obj
                 (class_type_longident : 'class_type_longident Gram.t ));
           `Skeyword "[";
           `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (t : 'comma_ctyp)  _  (i : 'class_type_longident) 
                  (_loc : FanLoc.t)  ->
                  (Ast.CtCon (_loc, Ast.ViNil, i, t) : 'class_type_longident_and_param ))));
         ([`Snterm
             (Gram.obj (class_type_longident : 'class_type_longident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'class_type_longident)  (_loc : FanLoc.t)  ->
                 (Ast.CtCon (_loc, Ast.ViNil, i, (Ast.TyNil _loc)) : 
                 'class_type_longident_and_param ))))])])
let _ = AstParsers.register_parser ("revise", apply)