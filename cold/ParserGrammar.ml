open FanGrammar
open FanGrammarTools
open PreCast.Syntax
open LibUtil
open Lib
open FanUtil
open FanAst
let _ = FanConfig.antiquotations := true
let nonterminals = Gram.mk "nonterminals"
let nonterminalsclear = Gram.mk "nonterminalsclear"
let delete_rule_header = Gram.mk "delete_rule_header"
let extend_header = Gram.mk "extend_header"
let qualuid = Gram.mk "qualuid"
let qualid = Gram.mk "qualid"
let t_qualid = Gram.mk "t_qualid"
let entry_name: ([ `name of FanToken.name | `non]* FanGrammar.name) Gram.t =
  Gram.mk "entry_name"
let locals = Gram.mk "locals"
let entry = Gram.mk "entry"
let position = Gram.mk "position"
let assoc = Gram.mk "assoc"
let name = Gram.mk "name"
let string = Gram.mk "string"
let pattern: action_pattern Gram.t = Gram.mk "pattern"
let simple_expr = Gram.mk "simple_expr"
let delete_rules = Gram.mk "delete_rules"
let simple_patt: simple_patt Gram.t = Gram.mk "simple_patt"
let internal_patt = Gram.mk "internal_patt"
let _ =
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
                [([`Stoken
                     (((function | `Lid _ -> true | _ -> false)),
                       (`Normal, "`Lid _"))],
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Lid x -> ((_loc, x, None, None) : 'e__3 )
                         | _ -> assert false)));
                ([`Skeyword "(";
                 `Stoken
                   (((function | `Lid _ -> true | _ -> false)),
                     (`Normal, "`Lid _"));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"));
                 `Skeyword ")"],
                  (Gram.mk_action
                     (fun _  (__fan_2 : [> FanToken.t]) 
                        (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                        match (__fan_2, __fan_1) with
                        | (`STR (_,y),`Lid x) ->
                            ((_loc, x, (Some y), None) : 'e__3 )
                        | _ -> assert false)));
                ([`Skeyword "(";
                 `Stoken
                   (((function | `Lid _ -> true | _ -> false)),
                     (`Normal, "`Lid _"));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"));
                 `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
                 `Skeyword ")"],
                  (Gram.mk_action
                     (fun _  (t : 'ctyp)  (__fan_2 : [> FanToken.t]) 
                        (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                        match (__fan_2, __fan_1) with
                        | (`STR (_,y),`Lid x) ->
                            ((_loc, x, (Some y), (Some t)) : 'e__3 )
                        | _ -> assert false)));
                ([`Skeyword "(";
                 `Stoken
                   (((function | `Lid _ -> true | _ -> false)),
                     (`Normal, "`Lid _"));
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
                        (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                        match __fan_1 with
                        | `Lid x -> ((_loc, x, y, (Some t)) : 'e__3 )
                        | _ -> assert false)))])],
            (Gram.mk_action
               (fun (ls : 'e__3 list)  (t : 'e__1)  (_loc : FanLoc.t)  ->
                  (let mk =
                     match t with
                     | `static t ->
                         `Id (_loc, (`Dot (_loc, t, (`Lid (_loc, "mk")))))
                     | `dynamic (x,t) ->
                         `ExApp
                           (_loc,
                             (`Id
                                (_loc,
                                  (`Dot
                                     (_loc, t, (`Lid (_loc, "mk_dynamic")))))),
                             (`Id (_loc, x))) in
                   let rest =
                     List.map
                       (fun (_loc,x,descr,ty)  ->
                          match (descr, ty) with
                          | (Some d,None ) ->
                              `Value
                                (_loc, (`ReNil _loc),
                                  (`Bind
                                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                                       (`ExApp (_loc, mk, (`Str (_loc, d)))))))
                          | (Some d,Some typ) ->
                              `Value
                                (_loc, (`ReNil _loc),
                                  (`Bind
                                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                                       (`Constraint
                                          (_loc,
                                            (`ExApp
                                               (_loc, mk, (`Str (_loc, d)))),
                                            typ)))))
                          | (None ,None ) ->
                              `Value
                                (_loc, (`ReNil _loc),
                                  (`Bind
                                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                                       (`ExApp (_loc, mk, (`Str (_loc, x)))))))
                          | (None ,Some typ) ->
                              `Value
                                (_loc, (`ReNil _loc),
                                  (`Bind
                                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                                       (`Constraint
                                          (_loc,
                                            (`ExApp
                                               (_loc, mk, (`Str (_loc, x)))),
                                            typ)))))) ls in
                   FanAst.sem_of_list rest : 'nonterminals ))))])]);
  Gram.extend (nonterminalsclear : 'nonterminalsclear Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ));
           `Slist0
             (Gram.srules nonterminalsclear
                [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
                   (Gram.mk_action
                      (fun (x : 'a_lident)  (_loc : FanLoc.t)  ->
                         (x : 'e__4 ))))])],
            (Gram.mk_action
               (fun (ls : 'e__4 list)  (t : 'qualuid)  (_loc : FanLoc.t)  ->
                  (let rest =
                     List.map
                       (fun x  ->
                          let _loc = loc_of x in
                          `ExApp
                            (_loc,
                              (`Id
                                 (_loc,
                                   (`Dot (_loc, t, (`Lid (_loc, "clear")))))),
                              (`Id (_loc, (x :>ident))))) ls in
                   `Seq (_loc, (FanAst.sem_of_list rest)) : 'nonterminalsclear ))))])])
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
                   `Seq (_loc, (FanAst.sem_of_list es)) : 'delete_rule_body ))))])]);
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
                          (sl : 'e__5 ))))]), (`Skeyword "|"));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (sls : 'e__5 list)  _  _  (n : 'name) 
                  (_loc : FanLoc.t)  ->
                  (let rest =
                     List.map
                       (fun sl  ->
                          let (e,b) = expr_of_delete_rule _loc n sl in
                          `ExApp
                            (_loc,
                              (`ExApp
                                 (_loc,
                                   (`Id
                                      (_loc,
                                        (`Dot
                                           (_loc, (gm ()),
                                             (`Lid (_loc, "delete_rule")))))),
                                   e)), b)) sls in
                   `Seq (_loc, (FanAst.sem_of_list rest)) : 'delete_rules ))))])]);
  Gram.extend (qualuid : 'qualuid Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (xs : 'qualuid)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid x -> (`Uid (_loc, x) : 'qualuid )
                 | _ -> assert false)))])]);
  Gram.extend (qualid : 'qualid Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (xs : 'qualid)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Lid i -> (`Lid (_loc, i) : 'qualid )
                 | _ -> assert false)))])]);
  Gram.extend (t_qualid : 't_qualid Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (xs : 't_qualid)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid x ->
                      (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Stoken
            (((function | `Lid "t" -> true | _ -> false)),
              (`Normal, "`Lid \"t\""))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  _  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match (__fan_2, __fan_0) with
                 | (`Lid "t",`Uid x) -> (`Uid (_loc, x) : 't_qualid )
                 | _ -> assert false)))])]);
  Gram.extend (locals : 'locals Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Lid "local" -> true | _ -> false)),
                (`Normal, "`Lid \"local\""));
           `Skeyword ":";
           `Slist1 (`Snterm (Gram.obj (name : 'name Gram.t )));
           `Skeyword ";"],
            (Gram.mk_action
               (fun _  (sl : 'name list)  _  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid "local" -> (sl : 'locals )
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
                         | `STR (_,x) -> (x : 'e__6 )
                         | _ -> assert false)))])],
            (Gram.mk_action
               (fun (name : 'e__6 option)  (il : 'qualid)  (_loc : FanLoc.t) 
                  ->
                  (((match name with
                     | Some x ->
                         let old = AstQuotation.default.contents in
                         (AstQuotation.default :=
                            (FanToken.resolve_name ((`Sub []), x));
                          `name old)
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
              (((function | `Uid ("First"|"Last") -> true | _ -> false)),
                (`Normal, "`Uid (\"First\"|\"Last\")"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid ("First"|"Last" as x) ->
                      (`ExVrn (_loc, x) : 'position )
                  | _ -> assert false)));
         ([`Stoken
             (((function
                | `Uid ("Before"|"After"|"Level") -> true
                | _ -> false)),
               (`Normal, "`Uid (\"Before\"|\"After\"|\"Level\")"));
          `Snterm (Gram.obj (string : 'string Gram.t ))],
           (Gram.mk_action
              (fun (n : 'string)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid ("Before"|"After"|"Level" as x) ->
                     (`ExApp (_loc, (`ExVrn (_loc, x)), n) : 'position )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid x ->
                     (failwithf
                        "%s is not the right position:(First|Last) or (Before|After|Level)"
                        x : 'position )
                 | _ -> assert false)))])]);
  Gram.extend (level_list : 'level_list Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "{";
           `Slist1 (`Snterm (Gram.obj (level : 'level Gram.t )));
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
                          | `STR (_,x) -> (x : 'e__7 )
                          | _ -> assert false)))]);
           `Sopt (`Snterm (Gram.obj (assoc : 'assoc Gram.t )));
           `Snterm (Gram.obj (rule_list : 'rule_list Gram.t ))],
            (Gram.mk_action
               (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                  (label : 'e__7 option)  (_loc : FanLoc.t)  ->
                  (mk_level ~label ~assoc ~rules : 'level ))))])]);
  Gram.extend (assoc : 'assoc Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Uid ("LA"|"RA"|"NA") -> true | _ -> false)),
                (`Normal, "`Uid (\"LA\"|\"RA\"|\"NA\")"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid ("LA"|"RA"|"NA" as x) ->
                      (`ExVrn (_loc, x) : 'assoc )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid x ->
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
                         (act : 'e__8 ))))])],
            (Gram.mk_action
               (fun (action : 'e__8 option)  (psl : 'psymbol list) 
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
                         (p : 'e__9 ))))])],
            (Gram.mk_action
               (fun (p : 'e__9 option)  (s : 'symbol)  (_loc : FanLoc.t)  ->
                  (match p with
                   | Some _ ->
                       {
                         s with
                         pattern = (p : action_pattern option  :>patt option)
                       }
                   | None  -> s : 'psymbol ))))])]);
  Gram.extend (symbol : 'symbol Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Uid ("L0"|"L1") -> true | _ -> false)),
                (`Normal, "`Uid (\"L0\"|\"L1\")"));
           `Sself;
           `Sopt
             (Gram.srules symbol
                [([`Stoken
                     (((function | `Uid "SEP" -> true | _ -> false)),
                       (`Normal, "`Uid \"SEP\""));
                  `Snterm (Gram.obj (symbol : 'symbol Gram.t ))],
                   (Gram.mk_action
                      (fun (t : 'symbol)  (__fan_0 : [> FanToken.t]) 
                         (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid "SEP" -> (t : 'e__10 )
                         | _ -> assert false)))])],
            (Gram.mk_action
               (fun (sep : 'e__10 option)  (s : 'symbol) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid ("L0"|"L1" as x) ->
                      (let () = check_not_tok s in
                       let styp =
                         `TyApp
                           (_loc, (`Id (_loc, (`Lid (_loc, "list")))),
                             (s.styp)) in
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
             (((function | `Uid "OPT" -> true | _ -> false)),
               (`Normal, "`Uid \"OPT\""));
          `Sself],
           (Gram.mk_action
              (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid "OPT" ->
                     (let () = check_not_tok s in
                      let styp =
                        `TyApp
                          (_loc, (`Id (_loc, (`Lid (_loc, "option")))),
                            (s.styp)) in
                      let text = `TXopt (_loc, (s.text)) in
                      mk_symbol ~text ~styp ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `Uid "TRY" -> true | _ -> false)),
               (`Normal, "`Uid \"TRY\""));
          `Sself],
           (Gram.mk_action
              (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid "TRY" ->
                     (let text = `TXtry (_loc, (s.text)) in
                      mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                     'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `Uid "PEEK" -> true | _ -> false)),
               (`Normal, "`Uid \"PEEK\""));
          `Sself],
           (Gram.mk_action
              (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid "PEEK" ->
                     (let text = `TXpeek (_loc, (s.text)) in
                      mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                     'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `Uid "S" -> true | _ -> false)),
               (`Normal, "`Uid \"S\""))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid "S" ->
                     (mk_symbol ~text:(`TXself _loc)
                        ~styp:(`Self (_loc, "S")) ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Stoken
             (((function | `Uid "N" -> true | _ -> false)),
               (`Normal, "`Uid \"N\""))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Uid "N" ->
                     (mk_symbol ~text:(`TXnext _loc)
                        ~styp:(`Self (_loc, "N")) ~pattern:None : 'symbol )
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
                    ~styp:(`Quote
                             (_loc, (`Normal _loc), (`Some (`Lid (_loc, t)))))
                    ~pattern:None : 'symbol ))));
         ([`Snterm (Gram.obj (simple_patt : 'simple_patt Gram.t ))],
           (Gram.mk_action
              (fun (p : 'simple_patt)  (_loc : FanLoc.t)  ->
                 (let (p,ls) =
                    Expr.filter_patt_with_captured_variables
                      (p : simple_patt  :>patt) in
                  match ls with
                  | [] -> mk_tok _loc ~pattern:p (`Tok _loc)
                  | (x,y)::ys ->
                      let restrict =
                        List.fold_left
                          (fun acc  (x,y)  ->
                             `ExApp
                               (_loc,
                                 (`ExApp
                                    (_loc, (`Id (_loc, (`Lid (_loc, "&&")))),
                                      acc)),
                                 (`ExApp
                                    (_loc,
                                      (`ExApp
                                         (_loc,
                                           (`Id (_loc, (`Lid (_loc, "=")))),
                                           x)), y))))
                          (`ExApp
                             (_loc,
                               (`ExApp
                                  (_loc, (`Id (_loc, (`Lid (_loc, "=")))), x)),
                               y)) ys in
                      mk_tok _loc ~restrict ~pattern:p (`Tok _loc) : 
                 'symbol ))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (_,s) ->
                     (mk_symbol ~text:(`TXkwd (_loc, s)) ~styp:(`Tok _loc)
                        ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Snterm (Gram.obj (name : 'name Gram.t ));
          `Sopt
            (Gram.srules symbol
               [([`Stoken
                    (((function | `Uid "Level" -> true | _ -> false)),
                      (`Normal, "`Uid \"Level\""));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"))],
                  (Gram.mk_action
                     (fun (__fan_1 : [> FanToken.t]) 
                        (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match (__fan_1, __fan_0) with
                        | (`STR (_,s),`Uid "Level") -> (s : 'e__11 )
                        | _ -> assert false)))])],
           (Gram.mk_action
              (fun (lev : 'e__11 option)  (n : 'name)  (_loc : FanLoc.t)  ->
                 (mk_symbol ~text:(`TXnterm (_loc, n, lev))
                    ~styp:(`Quote
                             (_loc, (`Normal _loc),
                               (`Some (`Lid (_loc, (n.tvar))))))
                    ~pattern:None : 'symbol ))));
         ([`Stoken
             (((function | `Ant (("nt"|""),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"nt\"|\"\"),_)"));
          `Sopt
            (Gram.srules symbol
               [([`Stoken
                    (((function | `Uid "Level" -> true | _ -> false)),
                      (`Normal, "`Uid \"Level\""));
                 `Stoken
                   (((function | `STR (_,_) -> true | _ -> false)),
                     (`Normal, "`STR (_,_)"))],
                  (Gram.mk_action
                     (fun (__fan_1 : [> FanToken.t]) 
                        (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        match (__fan_1, __fan_0) with
                        | (`STR (_,s),`Uid "Level") -> (s : 'e__12 )
                        | _ -> assert false)))])],
           (Gram.mk_action
              (fun (lev : 'e__12 option)  (__fan_0 : [> FanToken.t]) 
                 (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Ant (("nt"|""),s) ->
                     (let i = parse_ident _loc s in
                      let n = mk_name _loc i in
                      mk_symbol ~text:(`TXnterm (_loc, n, lev))
                        ~styp:(`Quote
                                 (_loc, (`Normal _loc),
                                   (`Some (`Lid (_loc, (n.tvar))))))
                        ~pattern:None : 'symbol )
                 | _ -> assert false)));
         ([`Skeyword "("; `Sself; `Skeyword ")"],
           (Gram.mk_action
              (fun _  (s : 'symbol)  _  (_loc : FanLoc.t)  -> (s : 'symbol ))))])]);
  Gram.extend (simple_patt : 'simple_patt Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
            (Gram.mk_action
               (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                  (`PaVrn (_loc, s) : 'simple_patt ))));
         ([`Skeyword "`";
          `Snterm (Gram.obj (luident : 'luident Gram.t ));
          `Stoken
            (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  (v : 'luident)  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_2 with
                 | `Ant ((""|"anti" as n),s) ->
                     (`PaApp
                        (_loc, (`PaVrn (_loc, v)),
                          (`Ant (_loc, (mk_anti ~c:"patt" n s)))) : 'simple_patt )
                 | _ -> assert false)));
         ([`Skeyword "`";
          `Snterm (Gram.obj (luident : 'luident Gram.t ));
          `Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  (s : 'luident)  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_2 with
                 | `STR (_,v) ->
                     (`PaApp (_loc, (`PaVrn (_loc, s)), (`Str (_loc, v))) : 
                     'simple_patt )
                 | _ -> assert false)));
         ([`Skeyword "`";
          `Snterm (Gram.obj (luident : 'luident Gram.t ));
          `Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
           (Gram.mk_action
              (fun (__fan_2 : [> FanToken.t])  (s : 'luident)  _ 
                 (_loc : FanLoc.t)  ->
                 match __fan_2 with
                 | `Lid x ->
                     (`PaApp
                        (_loc, (`PaVrn (_loc, s)),
                          (`Id (_loc, (`Lid (_loc, x))))) : 'simple_patt )
                 | _ -> assert false)));
         ([`Skeyword "`";
          `Snterm (Gram.obj (luident : 'luident Gram.t ));
          `Skeyword "_"],
           (Gram.mk_action
              (fun _  (s : 'luident)  _  (_loc : FanLoc.t)  ->
                 (`PaApp (_loc, (`PaVrn (_loc, s)), (`Any _loc)) : 'simple_patt ))));
         ([`Skeyword "`";
          `Snterm (Gram.obj (luident : 'luident Gram.t ));
          `Skeyword "(";
          `Slist1sep
            ((`Snterm (Gram.obj (internal_patt : 'internal_patt Gram.t ))),
              (`Skeyword ","));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (v : 'internal_patt list)  _  (s : 'luident)  _ 
                 (_loc : FanLoc.t)  ->
                 (match v with
                  | x::[] -> `PaApp (_loc, (`PaVrn (_loc, s)), x)
                  | x::xs ->
                      `PaApp
                        (_loc, (`PaApp (_loc, (`PaVrn (_loc, s)), x)),
                          (FanAst.com_of_list xs))
                  | _ -> assert false : 'simple_patt ))))])]);
  Gram.extend (internal_patt : 'internal_patt Gram.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_lident)  _  (p1 : 'internal_patt) 
                  (_loc : FanLoc.t)  ->
                  (`Alias (_loc, p1, s) : 'internal_patt ))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           (Gram.mk_action
              (fun (p2 : 'internal_patt)  _  (p1 : 'internal_patt) 
                 (_loc : FanLoc.t)  -> (`Or (_loc, p1, p2) : 'internal_patt ))))]);
      ((Some "simple"), None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (_,s) -> (`Str (_loc, s) : 'internal_patt )
                 | _ -> assert false)));
        ([`Skeyword "_"],
          (Gram.mk_action
             (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'internal_patt ))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
          (Gram.mk_action
             (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                match __fan_0 with
                | `Lid x -> (`Id (_loc, (`Lid (_loc, x))) : 'internal_patt )
                | _ -> assert false)));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          (Gram.mk_action
             (fun _  (p : 'internal_patt)  _  (_loc : FanLoc.t)  ->
                (p : 'internal_patt ))))])]);
  Gram.extend (pattern : 'pattern Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid i -> (`Id (_loc, (`Lid (_loc, i))) : 'pattern )
                  | _ -> assert false)));
         ([`Skeyword "_"],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'pattern ))));
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
                 (`Tup (_loc, (`Com (_loc, p1, (FanAst.com_of_list ps)))) : 
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
                  | `STR (_,s) -> (`Str (_loc, s) : 'string )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `Ant ("",_) -> true | _ -> false)),
               (`Normal, "`Ant (\"\",_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `Ant ("",s) -> (parse_expr _loc s : 'string )
                 | _ -> assert false)))])]);
  Gram.extend (symbol : 'symbol Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `Uid ("FOLD0"|"FOLD1") -> true | _ -> false)),
                (`Normal, "`Uid (\"FOLD0\"|\"FOLD1\")"));
           `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
           `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
           `Sself],
            (Gram.mk_action
               (fun (s : 'symbol)  (e : 'simple_expr)  (f : 'simple_expr) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid ("FOLD0"|"FOLD1" as x) ->
                      (sfold _loc [x] f e s : 'symbol )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `Uid ("FOLD0"|"FOLD1") -> true | _ -> false)),
               (`Normal, "`Uid (\"FOLD0\"|\"FOLD1\")"));
          `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
          `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
          `Sself;
          `Stoken
            (((function | `Uid "SEP" -> true | _ -> false)),
              (`Normal, "`Uid \"SEP\""));
          `Sself],
           (Gram.mk_action
              (fun (sep : 'symbol)  (__fan_4 : [> FanToken.t])  (s : 'symbol)
                  (e : 'simple_expr)  (f : 'simple_expr) 
                 (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match (__fan_4, __fan_0) with
                 | (`Uid ("SEP" as y),`Uid ("FOLD0"|"FOLD1" as x)) ->
                     (sfold ~sep _loc [x; y] f e s : 'symbol )
                 | _ -> assert false)))])]);
  Gram.extend (simple_expr : 'simple_expr Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                  (`Id (_loc, (i :>ident)) : 'simple_expr ))));
         ([`Skeyword "(";
          `Snterm (Gram.obj (expr : 'expr Gram.t ));
          `Skeyword ")"],
           (Gram.mk_action
              (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (e : 'simple_expr ))))])])
let d = `Absolute ["Fan"; "Lang"]
let _ = AstQuotation.of_expr ~name:(d, "extend") ~entry:extend_body
let _ = AstQuotation.of_expr ~name:(d, "delete") ~entry:delete_rule_body
let _ = AstQuotation.of_expr ~name:(d, "clear") ~entry:nonterminalsclear
let _ = AstQuotation.of_str_item ~name:(d, "create") ~entry:nonterminals
let _ =
  Options.add ("-meta_action", (FanArg.Set meta_action), "Undocumented")