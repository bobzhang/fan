open FanGrammar
open FanGrammarTools
open Fan.Syntax
open LibUtil
open Lib
open FanUtil
module Ast = Camlp4Ast
let _ = FanConfig.antiquotations := true
let nonterminals = Gram.mk "nonterminals"
let nonterminalsclear = Gram.mk "nonterminalsclear"
let delete_rule_header = Gram.mk "delete_rule_header"
let extend_header = Gram.mk "extend_header"
let qualuid = Gram.mk "qualuid"
let qualid = Gram.mk "qualid"
let t_qualid = Gram.mk "t_qualid"
let entry_name: ([ `name of string | `non]* FanGrammar.name) Gram.t =
  Gram.mk "entry_name"
let locals = Gram.mk "locals"
let entry = Gram.mk "entry"
let position = Gram.mk "position"
let assoc = Gram.mk "assoc"
let name = Gram.mk "name"
let string = Gram.mk "string"
let pattern = Gram.mk "pattern"
let simple_expr = Gram.mk "simple_expr"
let delete_rules = Gram.mk "delete_rules"
let simple_patt = Gram.mk "simple_patt"
let internal_patt = Gram.mk "internal_patt"
let _ =
  Gram.extend (extend_header : 'extend_header Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "(";
           `Snterm (Gram.obj (qualid : 'qualid Gram.t ));
           `Skeyword ":";
           `Snterm (Gram.obj (t_qualid : 't_qualid Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 't_qualid)  _  (i : 'qualid)  _ 
                  (_loc : FanLoc.t)  ->
                  (let old = gm () in
                   let () = grammar_module_name := t in ((Some i), old) : 
                  'extend_header ))));
         ([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
           (Gram.mk_action
              (fun (t : 'qualuid)  (_loc : FanLoc.t)  ->
                 (let old = gm () in
                  let () = grammar_module_name := t in (None, old) : 
                 'extend_header ))));
         ([],
           (Gram.mk_action
              (fun (_loc : FanLoc.t)  -> ((None, (gm ())) : 'extend_header ))))])]);
  Gram.extend (nonterminals : 'nonterminals Gram.t )
    (None,
      [(None, None,
         [([Gram.srules nonterminals
              [([`Skeyword "(";
                `Snterm (Gram.obj (qualid : 'qualid Gram.t ));
                `Skeyword ":";
                `Snterm (Gram.obj (t_qualid : 't_qualid Gram.t ));
                `Skeyword ")"],
                 (Gram.mk_action
                    (fun _  (t : 't_qualid)  _  (x : 'qualid)  _ 
                       (_loc : FanLoc.t)  -> (`dynamic (x, t) : 'e__1 ))));
              ([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
                (Gram.mk_action
                   (fun (t : 'qualuid)  (_loc : FanLoc.t)  ->
                      (`static t : 'e__1 ))))];
           `Slist0
             (Gram.srules nonterminals
                [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
                   (Gram.mk_action
                      (fun (x : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                         ((_loc, x, None, None) : 'e__3 ))));
                ([`Skeyword "(";
                 `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"));
                 `Skeyword ")"],
                  (Gram.mk_action
                     (fun _  (__fan_2 : [> FanToken.t])  (x : 'a_LIDENT)  _ 
                        (_loc : FanLoc.t)  ->
                        match __fan_2 with
                        | `STR (_,y) -> ((_loc, x, (Some y), None) : 'e__3 )
                        | _ -> assert false)));
                ([`Skeyword "(";
                 `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"));
                 `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
                 `Skeyword ")"],
                  (Gram.mk_action
                     (fun _  (t : 'ctyp)  (__fan_2 : [> FanToken.t]) 
                        (x : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                        match __fan_2 with
                        | `STR (_,y) ->
                            ((_loc, x, (Some y), (Some t)) : 'e__3 )
                        | _ -> assert false)));
                ([`Skeyword "(";
                 `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
                 `Skeyword ":";
                 `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
                 `Sopt
                   (Gram.srules nonterminals
                      [([`Stoken
                           (((function | `STR (_,_) -> true | _ -> false)),
                             (`Normal, "`STR (_,_)"))],
                         (Gram.mk_action
                            (fun (__fan_0 : [> FanToken.t]) 
                               (_loc : FanLoc.t)  ->
                               match __fan_0 with
                               | `STR (_,y) -> (y : 'e__2 )
                               | _ -> assert false)))]);
                 `Skeyword ")"],
                  (Gram.mk_action
                     (fun _  (y : 'e__2 option)  (t : 'ctyp)  _ 
                        (x : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                        ((_loc, x, y, (Some t)) : 'e__3 ))))])],
            (Gram.mk_action
               (fun (ls : 'e__3 list)  (t : 'e__1)  (_loc : FanLoc.t)  ->
                  (let mk =
                     match t with
                     | `static t ->
                         Ast.ExId
                           (_loc,
                             (Ast.IdAcc (_loc, t, (Ast.IdLid (_loc, "mk")))))
                     | `dynamic (x,t) ->
                         Ast.ExApp
                           (_loc,
                             (Ast.ExId
                                (_loc,
                                  (Ast.IdAcc
                                     (_loc, t,
                                       (Ast.IdLid (_loc, "mk_dynamic")))))),
                             (Ast.ExId (_loc, x))) in
                   let rest =
                     List.map
                       (fun (_loc,x,descr,ty)  ->
                          match (descr, ty) with
                          | (Some d,None ) ->
                              Ast.StVal
                                (_loc, Ast.ReNil,
                                  (Ast.BiEq
                                     (_loc,
                                       (Ast.PaId
                                          (_loc, (Ast.IdLid (_loc, x)))),
                                       (Ast.ExApp
                                          (_loc, mk, (Ast.ExStr (_loc, d)))))))
                          | (Some d,Some typ) ->
                              Ast.StVal
                                (_loc, Ast.ReNil,
                                  (Ast.BiEq
                                     (_loc,
                                       (Ast.PaId
                                          (_loc, (Ast.IdLid (_loc, x)))),
                                       (Ast.ExTyc
                                          (_loc,
                                            (Ast.ExApp
                                               (_loc, mk,
                                                 (Ast.ExStr (_loc, d)))),
                                            typ)))))
                          | (None ,None ) ->
                              Ast.StVal
                                (_loc, Ast.ReNil,
                                  (Ast.BiEq
                                     (_loc,
                                       (Ast.PaId
                                          (_loc, (Ast.IdLid (_loc, x)))),
                                       (Ast.ExApp
                                          (_loc, mk, (Ast.ExStr (_loc, x)))))))
                          | (None ,Some typ) ->
                              Ast.StVal
                                (_loc, Ast.ReNil,
                                  (Ast.BiEq
                                     (_loc,
                                       (Ast.PaId
                                          (_loc, (Ast.IdLid (_loc, x)))),
                                       (Ast.ExTyc
                                          (_loc,
                                            (Ast.ExApp
                                               (_loc, mk,
                                                 (Ast.ExStr (_loc, x)))),
                                            typ)))))) ls in
                   Ast.stSem_of_list rest : 'nonterminals ))))])]);
  Gram.extend (nonterminalsclear : 'nonterminalsclear Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ));
           `Slist0 (`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t )))],
            (Gram.mk_action
               (fun (ls : 'a_LIDENT list)  (t : 'qualuid)  (_loc : FanLoc.t) 
                  ->
                  (let rest =
                     List.map
                       (fun x  ->
                          Ast.ExApp
                            (_loc,
                              (Ast.ExId
                                 (_loc,
                                   (Ast.IdAcc
                                      (_loc, t, (Ast.IdLid (_loc, "clear")))))),
                              (Ast.ExId (_loc, (Ast.IdLid (_loc, x)))))) ls in
                   Ast.ExSeq (_loc, (Ast.exSem_of_list rest)) : 'nonterminalsclear ))))])]);
  Gram.extend (extend_body : 'extend_body Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (extend_header : 'extend_header Gram.t ));
           `Sopt (`Snterm (Gram.obj (locals : 'locals Gram.t )));
           `Slist1 (`Snterm (Gram.obj (entry : 'entry Gram.t )))],
            (Gram.mk_action
               (fun (el : 'entry list)  (locals : 'locals option) 
                  ((gram,old) : 'extend_header)  (_loc : FanLoc.t)  ->
                  (let res = text_of_functorial_extend _loc gram locals el in
                   let () = grammar_module_name := old in res : 'extend_body ))))])]);
  Gram.extend (delete_rule_header : 'delete_rule_header Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
            (Gram.mk_action
               (fun (g : 'qualuid)  (_loc : FanLoc.t)  ->
                  (let old = gm () in
                   let () = grammar_module_name := g in old : 'delete_rule_header ))))])]);
  Gram.extend (delete_rule_body : 'delete_rule_body Gram.t )
    (None,
      [(None, None,
         [([`Snterm
              (Gram.obj (delete_rule_header : 'delete_rule_header Gram.t ));
           `Slist0
             (`Snterm (Gram.obj (delete_rules : 'delete_rules Gram.t )))],
            (Gram.mk_action
               (fun (es : 'delete_rules list)  (old : 'delete_rule_header) 
                  (_loc : FanLoc.t)  ->
                  (let () = grammar_module_name := old in
                   Ast.ExSeq (_loc, (Ast.exSem_of_list es)) : 'delete_rule_body ))))])]);
  Gram.extend (delete_rules : 'delete_rules Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (name : 'name Gram.t ));
           `Skeyword ":";
           `Skeyword "[";
           `Slist1sep
             ((Gram.srules delete_rules
                 [([`Slist0sep
                      ((`Snterm (Gram.obj (psymbol : 'psymbol Gram.t ))),
                        (`Skeyword ";"))],
                    (Gram.mk_action
                       (fun (sl : 'psymbol list)  (_loc : FanLoc.t)  ->
                          (sl : 'e__4 ))))]), (`Skeyword "|"));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (sls : 'e__4 list)  _  _  (n : 'name) 
                  (_loc : FanLoc.t)  ->
                  (let rest =
                     List.map
                       (fun sl  ->
                          let (e,b) = expr_of_delete_rule _loc n sl in
                          Ast.ExApp
                            (_loc,
                              (Ast.ExApp
                                 (_loc,
                                   (Ast.ExId
                                      (_loc,
                                        (Ast.IdAcc
                                           (_loc, (gm ()),
                                             (Ast.IdLid (_loc, "delete_rule")))))),
                                   e)), b)) sls in
                   Ast.ExSeq (_loc, (Ast.exSem_of_list rest)) : 'delete_rules ))))])]);
  Gram.extend (qualuid : 'qualuid Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (xs : 'qualuid)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID x ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)), xs) : 
                      'qualuid )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID x -> (Ast.IdUid (_loc, x) : 'qualuid )
                 | _ -> assert false)))])]);
  Gram.extend (qualid : 'qualid Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (xs : 'qualid)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID x ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)), xs) : 
                      'qualid )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID i -> (Ast.IdUid (_loc, i) : 'qualid )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `LID i -> (Ast.IdLid (_loc, i) : 'qualid )
                 | _ -> assert false)))])]);
  Gram.extend (t_qualid : 't_qualid Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID _ -> true | _ -> false)),
                (`Normal, "`UID _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (xs : 't_qualid)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID x ->
                      (Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)), xs) : 
                      't_qualid )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"));
          `Skeyword ".";
          `Stoken
            (((function | `LID "t" -> true | _ -> false)),
              (`Normal, "`LID \"t\""))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  _  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match (__fan_2, __fan_0) with
                 | (`LID "t",`UID x) -> (Ast.IdUid (_loc, x) : 't_qualid )
                 | _ -> assert false)))])]);
  Gram.extend (locals : 'locals Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `LID "local" -> true | _ -> false)),
                (`Normal, "`LID \"local\""));
           `Skeyword ":";
           `Slist1 (`Snterm (Gram.obj (name : 'name Gram.t )));
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  (sl : 'name list)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LID "local" -> (sl : 'locals )
                  | _ -> assert false)))])]);
  Gram.extend (name : 'name Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (qualid : 'qualid Gram.t ))],
            (Gram.mk_action
               (fun (il : 'qualid)  (_loc : FanLoc.t)  ->
                  (mk_name _loc il : 'name ))))])]);
  Gram.extend (entry_name : 'entry_name Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (qualid : 'qualid Gram.t ));
           `Sopt
             (Gram.srules entry_name
                [([`Stoken
                     (((function | `STR (_,_) -> true | _ -> false)),
                       (`Normal, "`STR (_,_)"))],
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `STR (_,x) -> (x : 'e__5 )
                         | _ -> assert false)))])],
            (Gram.mk_action
               (fun (name : 'e__5 option)  (il : 'qualid)  (_loc : FanLoc.t) 
                  ->
                  (((match name with
                     | Some x ->
                         let old = AstQuotation.default.contents in
                         (AstQuotation.default := x; `name old)
                     | None  -> `non), (mk_name _loc il)) : 'entry_name ))))])]);
  Gram.extend (entry : 'entry Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (entry_name : 'entry_name Gram.t ));
           `Skeyword ":";
           `Sopt (`Snterm (Gram.obj (position : 'position Gram.t )));
           `Snterm (Gram.obj (level_list : 'level_list Gram.t ))],
            (Gram.mk_action
               (fun (levels : 'level_list)  (pos : 'position option)  _ 
                  ((n,p) : 'entry_name)  (_loc : FanLoc.t)  ->
                  ((match n with
                    | `name old -> AstQuotation.default := old
                    | _ -> ());
                   mk_entry ~name:p ~pos ~levels : 'entry ))))])]);
  Gram.extend (position : 'position Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID ("First"|"Last") -> true | _ -> false)),
                (`Normal, "`UID (\"First\"|\"Last\")"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID ("First"|"Last" as x) ->
                      (Ast.ExVrn (_loc, x) : 'position )
                  | _ -> assert false)));
         ([`Stoken
             (((function
                | `UID ("Before"|"After"|"Level") -> true
                | _ -> false)),
               (`Normal, "`UID (\"Before\"|\"After\"|\"Level\")"));
          `Snterm (Gram.obj (string : 'string Gram.t ))],
           (Gram.mk_action
              (fun (n : 'string)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID ("Before"|"After"|"Level" as x) ->
                     (Ast.ExApp (_loc, (Ast.ExVrn (_loc, x)), n) : 'position )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID x ->
                     (failwithf
                        "%s is not the right position:(First|Last) or (Before|After|Level)"
                        x : 'position )
                 | _ -> assert false)))])]);
  Gram.extend (level_list : 'level_list Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "{";
           `Slist0 (`Snterm (Gram.obj (level : 'level Gram.t )));
           `Skeyword "}"],
            (Gram.mk_action
               (fun _  (ll : 'level list)  _  (_loc : FanLoc.t)  ->
                  (ll : 'level_list ))));
         ([`Snterm (Gram.obj (level : 'level Gram.t ))],
           (Gram.mk_action
              (fun (l : 'level)  (_loc : FanLoc.t)  -> ([l] : 'level_list ))))])]);
  Gram.extend (level : 'level Gram.t )
    (None,
      [(None, None,
         [([`Sopt
              (Gram.srules level
                 [([`Stoken
                      (((function | `STR (_,_) -> true | _ -> false)),
                        (`Normal, "`STR (_,_)"))],
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `STR (_,x) -> (x : 'e__6 )
                          | _ -> assert false)))]);
           `Sopt (`Snterm (Gram.obj (assoc : 'assoc Gram.t )));
           `Snterm (Gram.obj (rule_list : 'rule_list Gram.t ))],
            (Gram.mk_action
               (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                  (label : 'e__6 option)  (_loc : FanLoc.t)  ->
                  (mk_level ~label ~assoc ~rules : 'level ))))])]);
  Gram.extend (assoc : 'assoc Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID ("LA"|"RA"|"NA") -> true | _ -> false)),
                (`Normal, "`UID (\"LA\"|\"RA\"|\"NA\")"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID ("LA"|"RA"|"NA" as x) ->
                      (Ast.ExVrn (_loc, x) : 'assoc )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID _ -> true | _ -> false)),
               (`Normal, "`UID _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID x ->
                     (failwithf
                        "%s is not a correct associativity:(LA|RA|NA)" x : 
                     'assoc )
                 | _ -> assert false)))])]);
  Gram.extend (rule_list : 'rule_list Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "["; `Skeyword "]"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  -> ([] : 'rule_list ))));
         ([`Skeyword "[";
          `Slist1sep
            ((`Snterm (Gram.obj (rule : 'rule Gram.t ))), (`Skeyword "|"));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (rules : 'rule list)  _  (_loc : FanLoc.t)  ->
                 (retype_rule_list_without_patterns _loc rules : 'rule_list ))))])]);
  Gram.extend (rule : 'rule Gram.t )
    (None,
      [(None, None,
         [([`Slist0sep
              ((`Snterm (Gram.obj (psymbol : 'psymbol Gram.t ))),
                (`Skeyword ";"));
           `Sopt
             (Gram.srules rule
                [([`Skeyword "->"; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
                   (Gram.mk_action
                      (fun (act : 'expr)  _  (_loc : FanLoc.t)  ->
                         (act : 'e__7 ))))])],
            (Gram.mk_action
               (fun (action : 'e__7 option)  (psl : 'psymbol list) 
                  (_loc : FanLoc.t)  -> (mk_rule ~prod:psl ~action : 
                  'rule ))))])]);
  Gram.extend (psymbol : 'psymbol Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (symbol : 'symbol Gram.t ));
           `Sopt
             (Gram.srules psymbol
                [([`Skeyword "{";
                  `Snterm (Gram.obj (pattern : 'pattern Gram.t ));
                  `Skeyword "}"],
                   (Gram.mk_action
                      (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  ->
                         (p : 'e__8 ))))])],
            (Gram.mk_action
               (fun (p : 'e__8 option)  (s : 'symbol)  (_loc : FanLoc.t)  ->
                  (match p with
                   | Some _ -> { s with pattern = p }
                   | None  -> s : 'psymbol ))))])]);
  Gram.extend (symbol : 'symbol Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID ("L0"|"L1") -> true | _ -> false)),
                (`Normal, "`UID (\"L0\"|\"L1\")"));
           `Sself;
           `Sopt
             (Gram.srules symbol
                [([`Stoken
                     (((function | `UID "SEP" -> true | _ -> false)),
                       (`Normal, "`UID \"SEP\""));
                  `Snterm (Gram.obj (symbol : 'symbol Gram.t ))],
                   (Gram.mk_action
                      (fun (t : 'symbol)  (__fan_0 : [> FanToken.t]) 
                         (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `UID "SEP" -> (t : 'e__9 )
                         | _ -> assert false)))])],
            (Gram.mk_action
               (fun (sep : 'e__9 option)  (s : 'symbol) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID ("L0"|"L1" as x) ->
                      (let () = check_not_tok s in
                       let styp =
                         `STapp (_loc, (`STlid (_loc, "list")), (s.styp)) in
                       let text =
                         mk_slist _loc
                           (match x with
                            | "L0" -> false
                            | "L1" -> true
                            | _ -> failwithf "only (L0|L1) allowed here") sep
                           s in
                       mk_symbol ~text ~styp ~pattern:None : 'symbol )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID "OPT" -> true | _ -> false)),
               (`Normal, "`UID \"OPT\""));
          `Sself],
           (Gram.mk_action
              (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID "OPT" ->
                     (let () = check_not_tok s in
                      let styp =
                        `STapp (_loc, (`STlid (_loc, "option")), (s.styp)) in
                      let text = `TXopt (_loc, (s.text)) in
                      mk_symbol ~text ~styp ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID "TRY" -> true | _ -> false)),
               (`Normal, "`UID \"TRY\""));
          `Sself],
           (Gram.mk_action
              (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID "TRY" ->
                     (let text = `TXtry (_loc, (s.text)) in
                      mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                     'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID "PEEK" -> true | _ -> false)),
               (`Normal, "`UID \"PEEK\""));
          `Sself],
           (Gram.mk_action
              (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID "PEEK" ->
                     (let text = `TXpeek (_loc, (s.text)) in
                      mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                     'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID "S" -> true | _ -> false)),
               (`Normal, "`UID \"S\""))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID "S" ->
                     (mk_symbol ~text:(`TXself _loc)
                        ~styp:(`STself (_loc, "S")) ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `UID "N" -> true | _ -> false)),
               (`Normal, "`UID \"N\""))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `UID "N" ->
                     (mk_symbol ~text:(`TXnext _loc)
                        ~styp:(`STself (_loc, "N")) ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Skeyword "[";
          `Slist1sep
            ((`Snterm (Gram.obj (rule : 'rule Gram.t ))), (`Skeyword "|"));
          `Skeyword "]"],
           (Gram.mk_action
              (fun _  (rl : 'rule list)  _  (_loc : FanLoc.t)  ->
                 (let rl = retype_rule_list_without_patterns _loc rl in
                  let t = new_type_var () in
                  mk_symbol ~text:(`TXrules (_loc, (mk_srules _loc t rl "")))
                    ~styp:(`STquo (_loc, t)) ~pattern:None : 'symbol ))));
         ([`Snterm (Gram.obj (simple_patt : 'simple_patt Gram.t ))],
           (Gram.mk_action
              (fun (p : 'simple_patt)  (_loc : FanLoc.t)  ->
                 (let (p,ls) = Expr.filter_patt_with_captured_variables p in
                  match ls with
                  | [] -> mk_tok _loc ~pattern:p (`STtok _loc)
                  | (x,y)::ys ->
                      let restrict =
                        List.fold_left
                          (fun acc  (x,y)  ->
                             Ast.ExApp
                               (_loc,
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExId
                                         (_loc, (Ast.IdLid (_loc, "&&")))),
                                      acc)),
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExApp
                                         (_loc,
                                           (Ast.ExId
                                              (_loc, (Ast.IdLid (_loc, "=")))),
                                           x)), y))))
                          (Ast.ExApp
                             (_loc,
                               (Ast.ExApp
                                  (_loc,
                                    (Ast.ExId (_loc, (Ast.IdLid (_loc, "=")))),
                                    x)), y)) ys in
                      mk_tok _loc ~restrict ~pattern:p (`STtok _loc) : 
                 'symbol ))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (_,s) ->
                     (mk_symbol ~text:(`TXkwd (_loc, s)) ~styp:(`STtok _loc)
                        ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Snterm (Gram.obj (name : 'name Gram.t ));
          `Sopt
            (Gram.srules symbol
               [([`Stoken
                    (((function | `UID "Level" -> true | _ -> false)),
                      (`Normal, "`UID \"Level\""));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"))],
                  (Gram.mk_action
                     (fun (__fan_1 : [> FanToken.t]) 
                        (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match (__fan_1, __fan_0) with
                        | (`STR (_,s),`UID "Level") -> (s : 'e__10 )
                        | _ -> assert false)))])],
           (Gram.mk_action
              (fun (lev : 'e__10 option)  (n : 'name)  (_loc : FanLoc.t)  ->
                 (mk_symbol ~text:(`TXnterm (_loc, n, lev))
                    ~styp:(`STquo (_loc, (n.tvar))) ~pattern:None : 'symbol ))));
         ([`Stoken
             (((function | `ANT (("nt"|""),_) -> true | _ -> false)),
               (`Normal, "`ANT ((\"nt\"|\"\"),_)"));
          `Sopt
            (Gram.srules symbol
               [([`Stoken
                    (((function | `UID "Level" -> true | _ -> false)),
                      (`Normal, "`UID \"Level\""));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"))],
                  (Gram.mk_action
                     (fun (__fan_1 : [> FanToken.t]) 
                        (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match (__fan_1, __fan_0) with
                        | (`STR (_,s),`UID "Level") -> (s : 'e__11 )
                        | _ -> assert false)))])],
           (Gram.mk_action
              (fun (lev : 'e__11 option)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT (("nt"|""),s) ->
                     (let i = parse_ident _loc s in
                      let n = mk_name _loc i in
                      mk_symbol ~text:(`TXnterm (_loc, n, lev))
                        ~styp:(`STquo (_loc, (n.tvar))) ~pattern:None : 
                     'symbol )
                 | _ -> assert false)));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (s : 'symbol)  _  (_loc : FanLoc.t)  -> (s : 'symbol ))))])]);
  Gram.extend (simple_patt : 'simple_patt Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaVrn (_loc, s) : 'simple_patt ))));
         ([`Skeyword "`";
          `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
          `Stoken
            (((function | `ANT ((""|"anti"),_) -> true | _ -> false)),
              (`Normal, "`ANT ((\"\"|\"anti\"),_)"))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  (v : 'a_ident)  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_2 with
                 | `ANT ((""|"anti" as n),s) ->
                     (Ast.PaApp
                        (_loc, (Ast.PaVrn (_loc, v)),
                          (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)))) : 
                     'simple_patt )
                 | _ -> assert false)));
         ([`Skeyword "`";
          `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
          `Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  (s : 'a_ident)  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_2 with
                 | `STR (_,v) ->
                     (Ast.PaApp
                        (_loc, (Ast.PaVrn (_loc, s)), (Ast.PaStr (_loc, v))) : 
                     'simple_patt )
                 | _ -> assert false)));
         ([`Skeyword "`";
          `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
          `Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  (s : 'a_ident)  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_2 with
                 | `LID x ->
                     (Ast.PaApp
                        (_loc, (Ast.PaVrn (_loc, s)),
                          (Ast.PaId (_loc, (Ast.IdLid (_loc, x))))) : 
                     'simple_patt )
                 | _ -> assert false)));
         ([`Skeyword "`";
          `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
          `Skeyword "_"],
           (Gram.mk_action
              (fun _  (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                 (Ast.PaApp (_loc, (Ast.PaVrn (_loc, s)), (Ast.PaAny _loc)) : 
                 'simple_patt ))));
         ([`Skeyword "`";
          `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
          `Skeyword "(";
          `Slist1sep
            ((`Snterm (Gram.obj (internal_patt : 'internal_patt Gram.t ))),
              (`Skeyword ","));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (v : 'internal_patt list)  _  (s : 'a_ident)  _ 
                 (_loc : FanLoc.t)  ->
                 (match v with
                  | x::[] -> Ast.PaApp (_loc, (Ast.PaVrn (_loc, s)), x)
                  | x::xs ->
                      Ast.PaApp
                        (_loc, (Ast.PaApp (_loc, (Ast.PaVrn (_loc, s)), x)),
                          (Ast.paCom_of_list xs))
                  | _ -> assert false : 'simple_patt ))))])]);
  Gram.extend (internal_patt : 'internal_patt Gram.t )
    (None,
      [((Some "as"), None,
         [([`Sself; `Skeyword "as"; `Sself],
            (Gram.mk_action
               (fun (p2 : 'internal_patt)  _  (p1 : 'internal_patt) 
                  (_loc : FanLoc.t)  ->
                  (Ast.PaAli (_loc, p1, p2) : 'internal_patt ))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           (Gram.mk_action
              (fun (p2 : 'internal_patt)  _  (p1 : 'internal_patt) 
                 (_loc : FanLoc.t)  ->
                 (Ast.PaOrp (_loc, p1, p2) : 'internal_patt ))))]);
      ((Some "simple"), None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (_,s) -> (Ast.PaStr (_loc, s) : 'internal_patt )
                 | _ -> assert false)));
        ([`Skeyword "_"],
          (Gram.mk_action
             (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'internal_patt ))));
        ([`Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"))],
          (Gram.mk_action
             (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                match __fan_0 with
                | `LID x ->
                    (Ast.PaId (_loc, (Ast.IdLid (_loc, x))) : 'internal_patt )
                | _ -> assert false)));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          (Gram.mk_action
             (fun _  (p : 'internal_patt)  _  (_loc : FanLoc.t)  ->
                (p : 'internal_patt ))))])]);
  Gram.extend (pattern : 'pattern Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `LID _ -> true | _ -> false)),
                (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `LID i ->
                      (Ast.PaId (_loc, (Ast.IdLid (_loc, i))) : 'pattern )
                  | _ -> assert false)));
         ([`Skeyword "_"],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'pattern ))));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  ->
                 (p : 'pattern ))));
         ([`Skeyword "(";
          `Sself;
          `Skeyword ",";
          `Slist1sep (`Sself, (`Skeyword ","));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _ 
                 (_loc : FanLoc.t)  ->
                 (Ast.PaTup
                    (_loc, (Ast.PaCom (_loc, p1, (Ast.paCom_of_list ps)))) : 
                 'pattern ))))])]);
  Gram.extend (string : 'string Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `STR (_,_) -> true | _ -> false)),
                (`Normal, "`STR (_,_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (_,s) -> (Ast.ExStr (_loc, s) : 'string )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `ANT ("",_) -> true | _ -> false)),
               (`Normal, "`ANT (\"\",_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `ANT ("",s) -> (parse_expr _loc s : 'string )
                 | _ -> assert false)))])]);
  Gram.extend (symbol : 'symbol Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `UID ("FOLD0"|"FOLD1") -> true | _ -> false)),
                (`Normal, "`UID (\"FOLD0\"|\"FOLD1\")"));
           `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
           `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (s : 'symbol)  (e : 'simple_expr)  (f : 'simple_expr) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `UID ("FOLD0"|"FOLD1" as x) ->
                      (sfold _loc [x] f e s : 'symbol )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `UID ("FOLD0"|"FOLD1") -> true | _ -> false)),
               (`Normal, "`UID (\"FOLD0\"|\"FOLD1\")"));
          `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
          `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
          `Sself;
          `Stoken
            (((function | `UID "SEP" -> true | _ -> false)),
              (`Normal, "`UID \"SEP\""));
          `Sself],
           (Gram.mk_action
              (fun (sep : 'symbol)  (__fan_4 : [> FanToken.t])  (s : 'symbol)
                  (e : 'simple_expr)  (f : 'simple_expr) 
                 (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match (__fan_4, __fan_0) with
                 | (`UID ("SEP" as y),`UID ("FOLD0"|"FOLD1" as x)) ->
                     (sfold ~sep _loc [x; y] f e s : 'symbol )
                 | _ -> assert false)))])]);
  Gram.extend (simple_expr : 'simple_expr Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.ExId (_loc, (Ast.IdLid (_loc, i))) : 'simple_expr ))));
         ([`Skeyword "(";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (e : 'simple_expr ))))])])
let _ = AstQuotation.add_quotation_of_expr ~name:"extend" ~entry:extend_body
let _ =
  AstQuotation.add_quotation_of_expr ~name:"delete" ~entry:delete_rule_body
let _ =
  AstQuotation.add_quotation_of_expr ~name:"extend.clear"
    ~entry:nonterminalsclear
let _ =
  AstQuotation.add_quotation_of_str_item ~name:"extend.create"
    ~entry:nonterminals
let _ =
  Options.add ("-meta_action", (FanArg.Set meta_action), "Undocumented")