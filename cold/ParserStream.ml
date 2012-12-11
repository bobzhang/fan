open Fan.Syntax
open LibUtil
open FanStreamTools
let apply () =
  let grammar_entry_create = Gram.mk in
  let parser_ipatt: 'parser_ipatt Gram.t =
    grammar_entry_create "parser_ipatt"
  and stream_quot: 'stream_quot Gram.t = grammar_entry_create "stream_quot"
  and stream_expr: 'stream_expr Gram.t = grammar_entry_create "stream_expr"
  and parser_case_list: 'parser_case_list Gram.t =
    grammar_entry_create "parser_case_list"
  and parser_case: 'parser_case Gram.t = grammar_entry_create "parser_case"
  and stream_patt: 'stream_patt Gram.t = grammar_entry_create "stream_patt"
  and stream_end: 'stream_end Gram.t = grammar_entry_create "stream_end"
  and stream_begin: 'stream_begin Gram.t =
    grammar_entry_create "stream_begin"
  and stream_patt_comp_err_list: 'stream_patt_comp_err_list Gram.t =
    grammar_entry_create "stream_patt_comp_err_list"
  and stream_patt_comp_err: 'stream_patt_comp_err Gram.t =
    grammar_entry_create "stream_patt_comp_err"
  and stream_patt_comp: 'stream_patt_comp Gram.t =
    grammar_entry_create "stream_patt_comp"
  and stream_expr_comp_list: 'stream_expr_comp_list Gram.t =
    grammar_entry_create "stream_expr_comp_list"
  and stream_expr_comp: 'stream_expr_comp Gram.t =
    grammar_entry_create "stream_expr_comp" in
  Gram.extend (expr : 'expr Gram.t )
    ((Some (`Level "top")),
      [(None, None,
         [([`Skeyword "parser";
           `Sopt
             (Gram.srules expr
                [([`Stoken
                     (((function | `UID _ -> true | _ -> false)),
                       (`Normal, "`UID _"))],
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `UID n -> (n : 'e__1 )
                         | _ -> assert false)))]);
           `Sopt (`Snterm (Gram.obj (parser_ipatt : 'parser_ipatt Gram.t )));
           `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
            (Gram.mk_action
               (fun (pcl : 'parser_case_list)  (po : 'parser_ipatt option) 
                  (name : 'e__1 option)  _  (_loc : FanLoc.t)  ->
                  (match name with
                   | Some o ->
                       Ref.protect FanStreamTools.grammar_module_name o
                         (fun _  -> cparser _loc po pcl)
                   | None  -> cparser _loc po pcl : 'expr ))));
         ([`Skeyword "match";
          `Sself;
          `Skeyword "with";
          `Skeyword "parser";
          `Sopt
            (Gram.srules expr
               [([`Stoken
                    (((function | `UID _ -> true | _ -> false)),
                      (`Normal, "`UID _"))],
                  (Gram.mk_action
                     (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match __fan_0 with
                        | `UID n -> (n : 'e__2 )
                        | _ -> assert false)))]);
          `Sopt (`Snterm (Gram.obj (parser_ipatt : 'parser_ipatt Gram.t )));
          `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
           (Gram.mk_action
              (fun (pcl : 'parser_case_list)  (po : 'parser_ipatt option) 
                 (name : 'e__2 option)  _  _  (e : 'expr)  _ 
                 (_loc : FanLoc.t)  ->
                 (match name with
                  | Some o ->
                      Ref.protect FanStreamTools.grammar_module_name o
                        (fun _  -> cparser_match _loc e po pcl)
                  | None  -> cparser_match _loc e po pcl : 'expr ))))])]);
  Gram.extend (expr : 'expr Gram.t )
    ((Some (`Level "simple")),
      [(None, None,
         [([`Snterm (Gram.obj (stream_begin : 'stream_begin Gram.t ));
           `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ))],
            (Gram.mk_action
               (fun _  (name : 'stream_begin)  (_loc : FanLoc.t)  ->
                  (match name with
                   | Some o ->
                       Ref.protect FanStreamTools.grammar_module_name o
                         (fun _  -> FanStreamTools.empty _loc)
                   | None  -> FanStreamTools.empty _loc : 'expr ))));
         ([`Snterm (Gram.obj (stream_begin : 'stream_begin Gram.t ));
          `Snterm
            (Gram.obj
               (stream_expr_comp_list : 'stream_expr_comp_list Gram.t ));
          `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ))],
           (Gram.mk_action
              (fun _  (sel : 'stream_expr_comp_list)  (name : 'stream_begin) 
                 (_loc : FanLoc.t)  ->
                 (match name with
                  | Some o ->
                      Ref.protect FanStreamTools.grammar_module_name o
                        (fun _  -> cstream _loc sel)
                  | None  -> cstream _loc sel : 'expr ))))])]);
  Gram.extend (parser_ipatt : 'parser_ipatt Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, (Ast.IdLid (_loc, i))) : 'parser_ipatt ))));
         ([`Skeyword "_"],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'parser_ipatt ))))])]);
  Gram.extend (parser_case_list : 'parser_case_list Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "[";
           `Slist0sep
             ((`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))),
               (`Skeyword "|"));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (pcl : 'parser_case list)  _  (_loc : FanLoc.t)  ->
                  (pcl : 'parser_case_list ))));
         ([`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))],
           (Gram.mk_action
              (fun (pc : 'parser_case)  (_loc : FanLoc.t)  ->
                 ([pc] : 'parser_case_list ))))])]);
  Gram.extend (parser_case : 'parser_case Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "[<";
           `Snterm (Gram.obj (stream_patt : 'stream_patt Gram.t ));
           `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ));
           `Sopt (`Snterm (Gram.obj (parser_ipatt : 'parser_ipatt Gram.t )));
           `Skeyword "->";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  (po : 'parser_ipatt option)  _ 
                  (sp : 'stream_patt)  _  (_loc : FanLoc.t)  ->
                  ((sp, po, e) : 'parser_case ))))])]);
  Gram.extend (stream_begin : 'stream_begin Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "[<";
           `Sopt
             (Gram.srules stream_begin
                [([`Skeyword "!";
                  `Stoken
                    (((function | `UID _ -> true | _ -> false)),
                      (`Normal, "`UID _"))],
                   (Gram.mk_action
                      (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t) 
                         ->
                         match __fan_1 with
                         | `UID n -> (n : 'e__3 )
                         | _ -> assert false)))])],
            (Gram.mk_action
               (fun (name : 'e__3 option)  _  (_loc : FanLoc.t)  ->
                  (name : 'stream_begin ))))])]);
  Gram.extend (stream_end : 'stream_end Gram.t )
    (None,
      [(None, None,
         [([`Skeyword ">]"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (() : 'stream_end ))))])]);
  Gram.extend (stream_quot : 'stream_quot Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "'"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (() : 'stream_quot ))))])]);
  Gram.extend (stream_expr : 'stream_expr Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'stream_expr ))))])]);
  Gram.extend (stream_patt : 'stream_patt Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (stream_patt_comp : 'stream_patt_comp Gram.t ))],
            (Gram.mk_action
               (fun (spc : 'stream_patt_comp)  (_loc : FanLoc.t)  ->
                  ([(spc, None)] : 'stream_patt ))));
         ([`Snterm (Gram.obj (stream_patt_comp : 'stream_patt_comp Gram.t ));
          `Skeyword ";";
          `Snterm
            (Gram.obj
               (stream_patt_comp_err_list : 'stream_patt_comp_err_list Gram.t ))],
           (Gram.mk_action
              (fun (sp : 'stream_patt_comp_err_list)  _ 
                 (spc : 'stream_patt_comp)  (_loc : FanLoc.t)  ->
                 ((spc, None) :: sp : 'stream_patt ))));
         ([],
           (Gram.mk_action (fun (_loc : FanLoc.t)  -> ([] : 'stream_patt ))))])]);
  Gram.extend (stream_patt_comp : 'stream_patt_comp Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
           `Sopt
             (Gram.srules stream_patt_comp
                [([`Skeyword "when";
                  `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
                   (Gram.mk_action
                      (fun (e : 'stream_expr)  _  (_loc : FanLoc.t)  ->
                         (e : 'e__4 ))))])],
            (Gram.mk_action
               (fun (eo : 'e__4 option)  (p : 'patt)  (_loc : FanLoc.t)  ->
                  (SpTrm (_loc, p, eo) : 'stream_patt_comp ))));
         ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
          `Skeyword "=";
          `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
           (Gram.mk_action
              (fun (e : 'stream_expr)  _  (p : 'patt)  (_loc : FanLoc.t)  ->
                 (SpNtr (_loc, p, e) : 'stream_patt_comp ))));
         ([`Snterm (Gram.obj (stream_quot : 'stream_quot Gram.t ));
          `Snterm (Gram.obj (patt : 'patt Gram.t ))],
           (Gram.mk_action
              (fun (p : 'patt)  _  (_loc : FanLoc.t)  ->
                 (SpStr (_loc, p) : 'stream_patt_comp ))))])]);
  Gram.extend (stream_patt_comp_err : 'stream_patt_comp_err Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (stream_patt_comp : 'stream_patt_comp Gram.t ));
           `Sopt
             (Gram.srules stream_patt_comp_err
                [([`Skeyword "??";
                  `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
                   (Gram.mk_action
                      (fun (e : 'stream_expr)  _  (_loc : FanLoc.t)  ->
                         (e : 'e__5 ))))])],
            (Gram.mk_action
               (fun (eo : 'e__5 option)  (spc : 'stream_patt_comp) 
                  (_loc : FanLoc.t)  -> ((spc, eo) : 'stream_patt_comp_err ))))])]);
  Gram.extend
    (stream_patt_comp_err_list : 'stream_patt_comp_err_list Gram.t )
    (None,
      [(None, None,
         [([`Snterm
              (Gram.obj
                 (stream_patt_comp_err : 'stream_patt_comp_err Gram.t ))],
            (Gram.mk_action
               (fun (spc : 'stream_patt_comp_err)  (_loc : FanLoc.t)  ->
                  ([spc] : 'stream_patt_comp_err_list ))));
         ([`Snterm
             (Gram.obj (stream_patt_comp_err : 'stream_patt_comp_err Gram.t ));
          `Skeyword ";"],
           (Gram.mk_action
              (fun _  (spc : 'stream_patt_comp_err)  (_loc : FanLoc.t)  ->
                 ([spc] : 'stream_patt_comp_err_list ))));
         ([`Snterm
             (Gram.obj (stream_patt_comp_err : 'stream_patt_comp_err Gram.t ));
          `Skeyword ";";
          `Sself],
           (Gram.mk_action
              (fun (sp : 'stream_patt_comp_err_list)  _ 
                 (spc : 'stream_patt_comp_err)  (_loc : FanLoc.t)  -> (spc ::
                 sp : 'stream_patt_comp_err_list ))))])]);
  Gram.extend (stream_expr_comp_list : 'stream_expr_comp_list Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (stream_expr_comp : 'stream_expr_comp Gram.t ));
           `Skeyword ";";
           `Sself],
            (Gram.mk_action
               (fun (sel : 'stream_expr_comp_list)  _ 
                  (se : 'stream_expr_comp)  (_loc : FanLoc.t)  -> (se ::
                  sel : 'stream_expr_comp_list ))));
         ([`Snterm (Gram.obj (stream_expr_comp : 'stream_expr_comp Gram.t ));
          `Skeyword ";"],
           (Gram.mk_action
              (fun _  (se : 'stream_expr_comp)  (_loc : FanLoc.t)  ->
                 ([se] : 'stream_expr_comp_list ))));
         ([`Snterm (Gram.obj (stream_expr_comp : 'stream_expr_comp Gram.t ))],
           (Gram.mk_action
              (fun (se : 'stream_expr_comp)  (_loc : FanLoc.t)  ->
                 ([se] : 'stream_expr_comp_list ))))])]);
  Gram.extend (stream_expr_comp : 'stream_expr_comp Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'stream_expr)  (_loc : FanLoc.t)  ->
                  (SeTrm (_loc, e) : 'stream_expr_comp ))));
         ([`Snterm (Gram.obj (stream_quot : 'stream_quot Gram.t ));
          `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
           (Gram.mk_action
              (fun (e : 'stream_expr)  _  (_loc : FanLoc.t)  ->
                 (SeNtr (_loc, e) : 'stream_expr_comp ))))])])
let _ = AstParsers.register_parser ("stream", apply)