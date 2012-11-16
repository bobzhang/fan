open LibUtil
open FanUtil
open Lib
open GramLib
module IdDebugParser = struct
  let name = "Camlp4DebugParser" let version = Sys.ocaml_version
  end
module MakeDebugParser(Syntax:Sig.Camlp4Syntax) = struct
  include Syntax module Ast = Camlp4Ast
  let debug_mode =
    try
      let str = Sys.getenv "STATIC_CAMLP4_DEBUG" in
      let rec loop acc i =
        try
          let pos = String.index_from str i ':' in
          loop (SSet.add (String.sub str i (pos - i)) acc) (pos + 1)
        with
        | Not_found  ->
            SSet.add (String.sub str i ((String.length str) - i)) acc in
      let sections = loop SSet.empty 0 in
      if SSet.mem "*" sections
      then fun _  -> true
      else (fun x  -> SSet.mem x sections)
    with | Not_found  -> (fun _  -> false)
  let mk_debug_mode _loc =
    function
    | None  ->
        Ast.ExId
          (_loc,
            (Ast.IdAcc
               (_loc, (Ast.IdUid (_loc, "Debug")),
                 (Ast.IdLid (_loc, "mode")))))
    | Some m ->
        Ast.ExId
          (_loc,
            (Ast.IdAcc
               (_loc, (Ast.IdUid (_loc, m)),
                 (Ast.IdAcc
                    (_loc, (Ast.IdUid (_loc, "Debug")),
                      (Ast.IdLid (_loc, "mode")))))))
  let mk_debug _loc m fmt section args =
    let call =
      Expr.apply
        (Ast.ExApp
           (_loc,
             (Ast.ExApp
                (_loc,
                  (Ast.ExId
                     (_loc,
                       (Ast.IdAcc
                          (_loc, (Ast.IdUid (_loc, "Debug")),
                            (Ast.IdLid (_loc, "printf")))))),
                  (Ast.ExStr (_loc, section)))), (Ast.ExStr (_loc, fmt))))
        args in
    Ast.ExIfe
      (_loc,
        (Ast.ExApp
           (_loc, (mk_debug_mode _loc m), (Ast.ExStr (_loc, section)))),
        call, (Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))))
  let _ =
    let grammar_entry_create = Gram.mk in
    let start_debug: 'start_debug Gram.t = grammar_entry_create "start_debug"
    and end_or_in: 'end_or_in Gram.t = grammar_entry_create "end_or_in" in
    Gram.extend (expr : 'expr Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (start_debug : 'start_debug Gram.t ));
             `Stoken
               (((function | `LID _ -> true | _ -> false)),
                 (`Normal, "`LID _"));
             `Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"));
             `Slist0 (`Snterml ((Gram.obj (expr : 'expr Gram.t )), "."));
             `Snterm (Gram.obj (end_or_in : 'end_or_in Gram.t ))],
              (Gram.mk_action
                 (fun (x : 'end_or_in)  (args : 'expr list)  __camlp4_1 
                    __camlp4_0  (m : 'start_debug)  (_loc : FanLoc.t)  ->
                    match (__camlp4_1, __camlp4_0) with
                    | (`STR (_,fmt),`LID section) ->
                        ((match (x, (debug_mode section)) with
                          | (None ,false ) ->
                              Ast.ExId (_loc, (Ast.IdUid (_loc, "()")))
                          | (Some e,false ) -> e
                          | (None ,_) -> mk_debug _loc m fmt section args
                          | (Some e,_) ->
                              Ast.ExLet
                                (_loc, Ast.ReNil,
                                  (Ast.BiEq
                                     (_loc,
                                       (Ast.PaId
                                          (_loc, (Ast.IdUid (_loc, "()")))),
                                       (mk_debug _loc m fmt section args))),
                                  e)) : 'expr )
                    | _ -> assert false)))])]);
    Gram.extend (end_or_in : 'end_or_in Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "in"; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (Some e : 'end_or_in ))));
           ([`Skeyword "end"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (None : 'end_or_in ))))])]);
    Gram.extend (start_debug : 'start_debug Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `LID "camlp4_debug" -> true | _ -> false)),
                  (`Normal, "`LID \"camlp4_debug\""))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `LID "camlp4_debug" -> (Some "Camlp4" : 'start_debug )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `LID "debug" -> true | _ -> false)),
                 (`Normal, "`LID \"debug\""))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `LID "debug" -> (None : 'start_debug )
                   | _ -> assert false)))])])
  end
module IdGrammarParser = struct
  let name = "Camlp4GrammarParser" let version = Sys.ocaml_version
  end
module MakeGrammarParser(Syntax:Sig.Camlp4Syntax) = struct
  include Syntax module Ast = Camlp4Ast open FanGrammar open FanGrammarTools
  let _ = FanConfig.antiquotations := true
  let _ =
    let grammar_entry_create = Gram.mk in
    let delete_rule_header: 'delete_rule_header Gram.t =
      grammar_entry_create "delete_rule_header"
    and delete_rules: 'delete_rules Gram.t =
      grammar_entry_create "delete_rules"
    and simple_expr: 'simple_expr Gram.t = grammar_entry_create "simple_expr"
    and pattern: 'pattern Gram.t = grammar_entry_create "pattern"
    and string: 'string Gram.t = grammar_entry_create "string"
    and name: 'name Gram.t = grammar_entry_create "name"
    and assoc: 'assoc Gram.t = grammar_entry_create "assoc"
    and position: 'position Gram.t = grammar_entry_create "position"
    and entry: 'entry Gram.t = grammar_entry_create "entry"
    and locals: 'locals Gram.t = grammar_entry_create "locals"
    and entry_name: 'entry_name Gram.t = grammar_entry_create "entry_name"
    and t_qualid: 't_qualid Gram.t = grammar_entry_create "t_qualid"
    and qualid: 'qualid Gram.t = grammar_entry_create "qualid"
    and qualuid: 'qualuid Gram.t = grammar_entry_create "qualuid"
    and extend_header: 'extend_header Gram.t =
      grammar_entry_create "extend_header" in
    Gram.extend (extend_header : 'extend_header Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    ((None, (gm ())) : 'extend_header ))));
           ([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
             (Gram.mk_action
                (fun (t : 'qualuid)  (_loc : FanLoc.t)  ->
                   (let old = gm () in
                    let () = grammar_module_name := t in (None, old) : 
                   'extend_header ))));
           ([`Skeyword "(";
            `Snterm (Gram.obj (qualid : 'qualid Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (t_qualid : 't_qualid Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (t : 't_qualid)  _  (i : 'qualid)  _ 
                   (_loc : FanLoc.t)  ->
                   (let old = gm () in
                    let () = grammar_module_name := t in ((Some i), old) : 
                   'extend_header ))))])]);
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
                        ((`Snterm (Gram.obj (symbol : 'symbol Gram.t ))),
                          (`Skeyword ";"))],
                      (Gram.mk_action
                         (fun (sl : 'symbol list)  (_loc : FanLoc.t)  ->
                            (sl : 'e__1 ))))]), (`Skeyword "|"));
             `Skeyword "]"],
              (Gram.mk_action
                 (fun _  (sls : 'e__1 list)  _  _  (n : 'name) 
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
                                               (Ast.IdLid
                                                  (_loc, "delete_rule")))))),
                                     e)), b)) sls in
                     Ast.ExSeq (_loc, (Ast.exSem_of_list rest)) : 'delete_rules ))))])]);
    Gram.extend (qualuid : 'qualuid Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `UID _ -> true | _ -> false)),
                  (`Normal, "`UID _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `UID x -> (Ast.IdUid (_loc, x) : 'qualuid )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `UID _ -> true | _ -> false)),
                 (`Normal, "`UID _"));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (xs : 'qualuid)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID x ->
                       (Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)), xs) : 
                       'qualuid )
                   | _ -> assert false)))])]);
    Gram.extend (qualid : 'qualid Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `LID _ -> true | _ -> false)),
                  (`Normal, "`LID _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `LID i -> (Ast.IdLid (_loc, i) : 'qualid )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `UID _ -> true | _ -> false)),
                 (`Normal, "`UID _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID i -> (Ast.IdUid (_loc, i) : 'qualid )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `UID _ -> true | _ -> false)),
                 (`Normal, "`UID _"));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (xs : 'qualid)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID x ->
                       (Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)), xs) : 
                       'qualid )
                   | _ -> assert false)))])]);
    Gram.extend (t_qualid : 't_qualid Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `UID _ -> true | _ -> false)),
                  (`Normal, "`UID _"));
             `Skeyword ".";
             `Stoken
               (((function | `LID "t" -> true | _ -> false)),
                 (`Normal, "`LID \"t\""))],
              (Gram.mk_action
                 (fun __camlp4_1  _  __camlp4_0  (_loc : FanLoc.t)  ->
                    match (__camlp4_1, __camlp4_0) with
                    | (`LID "t",`UID x) -> (Ast.IdUid (_loc, x) : 't_qualid )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `UID _ -> true | _ -> false)),
                 (`Normal, "`UID _"));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (xs : 't_qualid)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID x ->
                       (Ast.IdAcc (_loc, (Ast.IdUid (_loc, x)), xs) : 
                       't_qualid )
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
                 (fun _  (sl : 'name list)  _  __camlp4_0  (_loc : FanLoc.t) 
                    ->
                    match __camlp4_0 with
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
                        (fun __camlp4_0  (_loc : FanLoc.t)  ->
                           match __camlp4_0 with
                           | `STR (_,x) -> (x : 'e__2 )
                           | _ -> assert false)))])],
              (Gram.mk_action
                 (fun (name : 'e__2 option)  (il : 'qualid) 
                    (_loc : FanLoc.t)  ->
                    (((match name with
                       | Some x ->
                           let old = Quotation.default.contents in
                           (Quotation.default := x; `name old)
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
                      | `name old -> Quotation.default := old
                      | _ -> ());
                     mk_entry ~name:p ~pos ~levels : 'entry ))))])]);
    Gram.extend (position : 'position Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function
                   | `UID ("Before"|"After"|"Level") -> true
                   | _ -> false)),
                  (`Normal, "`UID (\"Before\"|\"After\"|\"Level\")"));
             `Snterm (Gram.obj (string : 'string Gram.t ))],
              (Gram.mk_action
                 (fun (n : 'string)  __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `UID ("Before"|"After"|"Level" as x) ->
                        (Ast.ExApp (_loc, (Ast.ExVrn (_loc, x)), n) : 
                        'position )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `UID ("First"|"Last") -> true | _ -> false)),
                 (`Normal, "`UID (\"First\"|\"Last\")"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID ("First"|"Last" as x) ->
                       (Ast.ExVrn (_loc, x) : 'position )
                   | _ -> assert false)))])]);
    Gram.extend (level_list : 'level_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (level : 'level Gram.t ))],
              (Gram.mk_action
                 (fun (l : 'level)  (_loc : FanLoc.t)  ->
                    ([l] : 'level_list ))));
           ([`Skeyword "{";
            `Slist0 (`Snterm (Gram.obj (level : 'level Gram.t )));
            `Skeyword "}"],
             (Gram.mk_action
                (fun _  (ll : 'level list)  _  (_loc : FanLoc.t)  ->
                   (ll : 'level_list ))))])]);
    Gram.extend (level : 'level Gram.t )
      (None,
        [(None, None,
           [([`Sopt
                (Gram.srules level
                   [([`Stoken
                        (((function | `STR (_,_) -> true | _ -> false)),
                          (`Normal, "`STR (_,_)"))],
                      (Gram.mk_action
                         (fun __camlp4_0  (_loc : FanLoc.t)  ->
                            match __camlp4_0 with
                            | `STR (_,x) -> (x : 'e__3 )
                            | _ -> assert false)))]);
             `Sopt (`Snterm (Gram.obj (assoc : 'assoc Gram.t )));
             `Snterm (Gram.obj (rule_list : 'rule_list Gram.t ))],
              (Gram.mk_action
                 (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                    (label : 'e__3 option)  (_loc : FanLoc.t)  ->
                    (mk_level ~label ~assoc ~rules : 'level ))))])]);
    Gram.extend (assoc : 'assoc Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `UID _ -> true | _ -> false)),
                  (`Normal, "`UID _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `UID x ->
                        (failwithf
                           "%s is not a correct associativity:(LA|RA|NA)" x : 
                        'assoc )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `UID ("LA"|"RA"|"NA") -> true | _ -> false)),
                 (`Normal, "`UID (\"LA\"|\"RA\"|\"NA\")"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID ("LA"|"RA"|"NA" as x) ->
                       (Ast.ExVrn (_loc, x) : 'assoc )
                   | _ -> assert false)))])]);
    Gram.extend (rule_list : 'rule_list Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "[";
             `Slist1sep
               ((`Snterm (Gram.obj (rule : 'rule Gram.t ))), (`Skeyword "|"));
             `Skeyword "]"],
              (Gram.mk_action
                 (fun _  (rules : 'rule list)  _  (_loc : FanLoc.t)  ->
                    (retype_rule_list_without_patterns _loc rules : 'rule_list ))));
           ([`Skeyword "["; `Skeyword "]"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> ([] : 'rule_list ))))])]);
    Gram.extend (rule : 'rule Gram.t )
      (None,
        [(None, None,
           [([`Slist0sep
                ((`Snterm (Gram.obj (psymbol : 'psymbol Gram.t ))),
                  (`Skeyword ";"));
             `Sopt
               (Gram.srules rule
                  [([`Skeyword "->";
                    `Snterm (Gram.obj (expr : 'expr Gram.t ))],
                     (Gram.mk_action
                        (fun (act : 'expr)  _  (_loc : FanLoc.t)  ->
                           (act : 'e__4 ))))])],
              (Gram.mk_action
                 (fun (action : 'e__4 option)  (psl : 'psymbol list) 
                    (_loc : FanLoc.t)  ->
                    (mk_rule ~prod:psl ~action : 'rule ))))])]);
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
                           (p : 'e__5 ))))])],
              (Gram.mk_action
                 (fun (p : 'e__5 option)  (s : 'symbol)  (_loc : FanLoc.t) 
                    ->
                    (match p with
                     | Some _ -> { s with pattern = p }
                     | None  -> s : 'psymbol ))))])]);
    Gram.extend (symbol : 'symbol Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "("; `Sself; `Skeyword ")"],
              (Gram.mk_action
                 (fun _  (s : 'symbol)  _  (_loc : FanLoc.t)  ->
                    (s : 'symbol ))));
           ([`Stoken
               (((function | `ANTIQUOT (("nt"|""),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"nt\"|\"\"),_)"));
            `Sopt
              (Gram.srules symbol
                 [([`Stoken
                      (((function | `UID "Level" -> true | _ -> false)),
                        (`Normal, "`UID \"Level\""));
                   `Stoken
                     (((function | `STR (_,_) -> true | _ -> false)),
                       (`Normal, "`STR (_,_)"))],
                    (Gram.mk_action
                       (fun __camlp4_1  __camlp4_0  (_loc : FanLoc.t)  ->
                          match (__camlp4_1, __camlp4_0) with
                          | (`STR (_,s),`UID "Level") -> (s : 'e__8 )
                          | _ -> assert false)))])],
             (Gram.mk_action
                (fun (lev : 'e__8 option)  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("nt"|""),s) ->
                       (let i = AntiquotSyntax.parse_ident _loc s in
                        let n = mk_name _loc i in
                        mk_symbol ~text:(`TXnterm (_loc, n, lev))
                          ~styp:(`STquo (_loc, (n.tvar))) ~pattern:None : 
                       'symbol )
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
                       (fun __camlp4_1  __camlp4_0  (_loc : FanLoc.t)  ->
                          match (__camlp4_1, __camlp4_0) with
                          | (`STR (_,s),`UID "Level") -> (s : 'e__7 )
                          | _ -> assert false)))])],
             (Gram.mk_action
                (fun (lev : 'e__7 option)  (n : 'name)  (_loc : FanLoc.t)  ->
                   (mk_symbol ~text:(`TXnterm (_loc, n, lev))
                      ~styp:(`STquo (_loc, (n.tvar))) ~pattern:None : 
                   'symbol ))));
           ([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `STR (_,s) ->
                       (mk_symbol ~text:(`TXkwd (_loc, s))
                          ~styp:(`STtok _loc) ~pattern:None : 'symbol )
                   | _ -> assert false)));
           ([`Skeyword "`";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
            `Sopt (`Snterm (Gram.obj (patt : 'patt Gram.t )))],
             (Gram.mk_action
                (fun (p : 'patt option)  (i : 'a_ident)  _  (_loc : FanLoc.t)
                    ->
                   (let p =
                      match p with
                      | None  -> Ast.PaVrn (_loc, i)
                      | Some p -> Ast.PaApp (_loc, (Ast.PaVrn (_loc, i)), p) in
                    let (p,ls) = Expr.filter_patt_with_captured_variables p in
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
                                                (_loc,
                                                  (Ast.IdLid (_loc, "=")))),
                                             x)), y))))
                            (Ast.ExApp
                               (_loc,
                                 (Ast.ExApp
                                    (_loc,
                                      (Ast.ExId
                                         (_loc, (Ast.IdLid (_loc, "=")))), x)),
                                 y)) ys in
                        mk_tok _loc ~restrict ~pattern:p (`STtok _loc) : 
                   'symbol ))));
           ([`Skeyword "[";
            `Slist0sep
              ((`Snterm (Gram.obj (rule : 'rule Gram.t ))), (`Skeyword "|"));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (rl : 'rule list)  _  (_loc : FanLoc.t)  ->
                   (let rl = retype_rule_list_without_patterns _loc rl in
                    let t = new_type_var () in
                    mk_symbol ~text:(`TXrules (_loc, (srules _loc t rl "")))
                      ~styp:(`STquo (_loc, t)) ~pattern:None : 'symbol ))));
           ([`Stoken
               (((function | `UID "N" -> true | _ -> false)),
                 (`Normal, "`UID \"N\""))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID "N" ->
                       (mk_symbol ~text:(`TXnext _loc)
                          ~styp:(`STself (_loc, "N")) ~pattern:None : 
                       'symbol )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `UID "S" -> true | _ -> false)),
                 (`Normal, "`UID \"S\""))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID "S" ->
                       (mk_symbol ~text:(`TXself _loc)
                          ~styp:(`STself (_loc, "S")) ~pattern:None : 
                       'symbol )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `UID "TRY" -> true | _ -> false)),
                 (`Normal, "`UID \"TRY\""));
            `Sself],
             (Gram.mk_action
                (fun (s : 'symbol)  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID "TRY" ->
                       (let text = `TXtry (_loc, (s.text)) in
                        mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                       'symbol )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `UID "OPT" -> true | _ -> false)),
                 (`Normal, "`UID \"OPT\""));
            `Sself],
             (Gram.mk_action
                (fun (s : 'symbol)  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID "OPT" ->
                       (let () = check_not_tok s in
                        let styp =
                          `STapp (_loc, (`STlid (_loc, "option")), (s.styp)) in
                        let text = `TXopt (_loc, (s.text)) in
                        mk_symbol ~text ~styp ~pattern:None : 'symbol )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `UID ("LIST0"|"LIST1") -> true | _ -> false)),
                 (`Normal, "`UID (\"LIST0\"|\"LIST1\")"));
            `Sself;
            `Sopt
              (Gram.srules symbol
                 [([`Stoken
                      (((function | `UID "SEP" -> true | _ -> false)),
                        (`Normal, "`UID \"SEP\""));
                   `Snterm (Gram.obj (symbol : 'symbol Gram.t ))],
                    (Gram.mk_action
                       (fun (t : 'symbol)  __camlp4_0  (_loc : FanLoc.t)  ->
                          match __camlp4_0 with
                          | `UID "SEP" -> (t : 'e__6 )
                          | _ -> assert false)))])],
             (Gram.mk_action
                (fun (sep : 'e__6 option)  (s : 'symbol)  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID ("LIST0"|"LIST1" as x) ->
                       (let () = check_not_tok s in
                        let styp =
                          `STapp (_loc, (`STlid (_loc, "list")), (s.styp)) in
                        let text =
                          slist _loc
                            (match x with
                             | "LIST0" -> false
                             | "LIST1" -> true
                             | _ ->
                                 failwithf "only (LIST0|LIST1) allowed here")
                            sep s in
                        mk_symbol ~text ~styp ~pattern:None : 'symbol )
                   | _ -> assert false)))])]);
    Gram.extend (pattern : 'pattern Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "(";
             `Sself;
             `Skeyword ",";
             `Slist1sep (`Sself, (`Skeyword ","));
             `Skeyword ")"],
              (Gram.mk_action
                 (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _ 
                    (_loc : FanLoc.t)  ->
                    (Ast.PaTup
                       (_loc, (Ast.PaCom (_loc, p1, (Ast.paCom_of_list ps)))) : 
                    'pattern ))));
           ([`Skeyword "("; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  ->
                   (p : 'pattern ))));
           ([`Skeyword "_"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'pattern ))));
           ([`Stoken
               (((function | `LID _ -> true | _ -> false)),
                 (`Normal, "`LID _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `LID i ->
                       (Ast.PaId (_loc, (Ast.IdLid (_loc, i))) : 'pattern )
                   | _ -> assert false)))])]);
    Gram.extend (string : 'string Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                  (`Normal, "`ANTIQUOT (\"\",_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `ANTIQUOT ("",s) ->
                        (AntiquotSyntax.parse_expr _loc s : 'string )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `STR (_,s) -> (Ast.ExStr (_loc, s) : 'string )
                   | _ -> assert false)))])]);
    Gram.extend (symbol : 'symbol Gram.t )
      (None,
        [(None, None,
           [([`Stoken
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
                 (fun (sep : 'symbol)  __camlp4_1  (s : 'symbol) 
                    (e : 'simple_expr)  (f : 'simple_expr)  __camlp4_0 
                    (_loc : FanLoc.t)  ->
                    match (__camlp4_1, __camlp4_0) with
                    | (`UID ("SEP" as y),`UID ("FOLD0"|"FOLD1" as x)) ->
                        (sfold ~sep _loc [x; y] f e s : 'symbol )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `UID ("FOLD0"|"FOLD1") -> true | _ -> false)),
                 (`Normal, "`UID (\"FOLD0\"|\"FOLD1\")"));
            `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
            `Snterm (Gram.obj (simple_expr : 'simple_expr Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (s : 'symbol)  (e : 'simple_expr)  (f : 'simple_expr) 
                   __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `UID ("FOLD0"|"FOLD1" as x) ->
                       (sfold _loc [x] f e s : 'symbol )
                   | _ -> assert false)))])]);
    Gram.extend (simple_expr : 'simple_expr Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "(";
             `Snterm (Gram.obj (expr : 'expr Gram.t ));
             `Skeyword ")"],
              (Gram.mk_action
                 (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (e : 'simple_expr ))));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                   (Ast.ExId (_loc, (Ast.IdLid (_loc, i))) : 'simple_expr ))))])])
  let _ =
    Options.add
      ("-split_ext", (FanArg.Set split_ext),
        "Split EXTEND by functions to turn around a PowerPC problem.")
  let _ =
    Options.add
      ("-split_gext", (FanArg.Set split_ext),
        "Old name for the option -split_ext.")
  let _ =
    Options.add ("-meta_action", (FanArg.Set meta_action), "Undocumented")
  end
module IdListComprehension = struct
  let name = "Camlp4ListComprehension" let version = Sys.ocaml_version
  end
module MakeListComprehension(Syntax:Sig.Camlp4Syntax) = struct
  include Syntax module Ast = Camlp4Ast
  let _ =
    Gram.delete_rule expr
      [`Skeyword "[";
      `Snterm (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
      `Skeyword "]"]
  let comprehension_or_sem_expr_for_list =
    Gram.mk "comprehension_or_sem_expr_for_list"
  let _ =
    let grammar_entry_create = Gram.mk in
    let item: 'item Gram.t = grammar_entry_create "item" in
    Gram.extend (expr : 'expr Gram.t )
      ((Some (`Level "simple")),
        [(None, None,
           [([`Skeyword "[";
             `Snterm
               (Gram.obj
                  (comprehension_or_sem_expr_for_list : 'comprehension_or_sem_expr_for_list
                                                          Gram.t ));
             `Skeyword "]"],
              (Gram.mk_action
                 (fun _  (e : 'comprehension_or_sem_expr_for_list)  _ 
                    (_loc : FanLoc.t)  -> (e : 'expr ))))])]);
    Gram.extend
      (comprehension_or_sem_expr_for_list : 'comprehension_or_sem_expr_for_list
                                              Gram.t )
      (None,
        [(None, None,
           [([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                    (Ast.ExApp
                       (_loc,
                         (Ast.ExApp
                            (_loc,
                              (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e)),
                         (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))) : 
                    'comprehension_or_sem_expr_for_list ))));
           ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
            `Skeyword "|";
            `Slist1sep
              ((`Snterm (Gram.obj (item : 'item Gram.t ))), (`Skeyword ";"))],
             (Gram.mk_action
                (fun (l : 'item list)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                   (Expr.compr _loc e l : 'comprehension_or_sem_expr_for_list ))));
           ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (e : 'expr)  (_loc : FanLoc.t)  ->
                   (Ast.ExApp
                      (_loc,
                        (Ast.ExApp
                           (_loc,
                             (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e)),
                        (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))))) : 
                   'comprehension_or_sem_expr_for_list ))));
           ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
            `Skeyword ";";
            `Snterm
              (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ))],
             (Gram.mk_action
                (fun (mk : 'sem_expr_for_list)  _  (e : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp
                      (_loc,
                        (Ast.ExApp
                           (_loc,
                             (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))), e)),
                        (mk (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))))) : 
                   'comprehension_or_sem_expr_for_list ))))])]);
    Gram.extend (item : 'item Gram.t )
      (None,
        [(None, None,
           [([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  -> (`cond e : 'item ))));
           ([`Stry
               (Gram.srules item
                  [([`Snterm (Gram.obj (patt : 'patt Gram.t ));
                    `Skeyword "<-"],
                     (Gram.mk_action
                        (fun _  (p : 'patt)  (_loc : FanLoc.t)  ->
                           (p : 'e__9 ))))]);
            `Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
             (Gram.mk_action
                (fun (e : 'expr)  (p : 'e__9)  (_loc : FanLoc.t)  ->
                   (`gen (p, e) : 'item ))))])])
  let _ =
    if is_revised ~expr ~sem_expr_for_list
    then
      Gram.extend
        (comprehension_or_sem_expr_for_list : 'comprehension_or_sem_expr_for_list
                                                Gram.t )
        (None,
          [(None, None,
             [([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
               `Skeyword "::";
               `Snterm (Gram.obj (expr : 'expr Gram.t ))],
                (Gram.mk_action
                   (fun (last : 'expr)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                      (Ast.ExApp
                         (_loc,
                           (Ast.ExApp
                              (_loc,
                                (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                                e)), last) : 'comprehension_or_sem_expr_for_list ))));
             ([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top");
              `Skeyword ";";
              `Snterm
                (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
              `Skeyword "::";
              `Snterm (Gram.obj (expr : 'expr Gram.t ))],
               (Gram.mk_action
                  (fun (last : 'expr)  _  (mk : 'sem_expr_for_list)  _ 
                     (e : 'expr)  (_loc : FanLoc.t)  ->
                     (Ast.ExApp
                        (_loc,
                          (Ast.ExApp
                             (_loc,
                               (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                               e)), (mk last)) : 'comprehension_or_sem_expr_for_list ))))])])
    else ()
  end
module IdMacroParser = struct
  let name = "Camlp4MacroParser" let version = Sys.ocaml_version
  end
module MakeMacroParser(Syntax:Sig.Camlp4Syntax) = struct
  include Syntax module Ast = Camlp4Ast
  type 'a item_or_def =  
    | SdStr of 'a
    | SdDef of string* (string list* Ast.expr) option
    | SdUnd of string
    | SdITE of bool* 'a item_or_def list* 'a item_or_def list
    | SdLazy of 'a Lazy.t  let defined = ref []
  let is_defined i = List.mem_assoc i defined.contents
  let incorrect_number loc l1 l2 =
    FanLoc.raise loc
      (Failure
         (Printf.sprintf "expected %d parameters; found %d" (List.length l2)
            (List.length l1)))
  let define eo x =
    (match eo with
     | Some ([],e) ->
         (Gram.extend (expr : 'expr Gram.t )
            ((Some (`Level "simple")),
              [(None, None,
                 [([`Stoken
                      (((function
                         | `UID __x when x = __x -> true
                         | _ -> false)), (`Antiquot, "`UID __x"))],
                    (Gram.mk_action
                       (fun __camlp4_0  (_loc : FanLoc.t)  ->
                          match __camlp4_0 with
                          | `UID _ ->
                              (((new Ast.reloc) _loc)#expr e : 'expr )
                          | _ -> assert false)))])]);
          Gram.extend (patt : 'patt Gram.t )
            ((Some (`Level "simple")),
              [(None, None,
                 [([`Stoken
                      (((function
                         | `UID __x when x = __x -> true
                         | _ -> false)), (`Antiquot, "`UID __x"))],
                    (Gram.mk_action
                       (fun __camlp4_0  (_loc : FanLoc.t)  ->
                          match __camlp4_0 with
                          | `UID _ ->
                              (let p = Expr.substp _loc [] e in
                               ((new Ast.reloc) _loc)#patt p : 'patt )
                          | _ -> assert false)))])]))
     | Some (sl,e) ->
         (Gram.extend (expr : 'expr Gram.t )
            ((Some (`Level "apply")),
              [(None, None,
                 [([`Stoken
                      (((function
                         | `UID __x when x = __x -> true
                         | _ -> false)), (`Antiquot, "`UID __x"));
                   `Sself],
                    (Gram.mk_action
                       (fun (param : 'expr)  __camlp4_0  (_loc : FanLoc.t) 
                          ->
                          match __camlp4_0 with
                          | `UID _ ->
                              (let el =
                                 match param with
                                 | Ast.ExTup (_,e) -> Ast.list_of_expr e []
                                 | e -> [e] in
                               if (List.length el) = (List.length sl)
                               then
                                 let env = List.combine sl el in
                                 ((new Expr.subst) _loc env)#expr e
                               else incorrect_number _loc el sl : 'expr )
                          | _ -> assert false)))])]);
          Gram.extend (patt : 'patt Gram.t )
            ((Some (`Level "simple")),
              [(None, None,
                 [([`Stoken
                      (((function
                         | `UID __x when x = __x -> true
                         | _ -> false)), (`Antiquot, "`UID __x"));
                   `Sself],
                    (Gram.mk_action
                       (fun (param : 'patt)  __camlp4_0  (_loc : FanLoc.t) 
                          ->
                          match __camlp4_0 with
                          | `UID _ ->
                              (let pl =
                                 match param with
                                 | Ast.PaTup (_,p) -> Ast.list_of_patt p []
                                 | p -> [p] in
                               if (List.length pl) = (List.length sl)
                               then
                                 let env = List.combine sl pl in
                                 let p = Expr.substp _loc env e in
                                 ((new Ast.reloc) _loc)#patt p
                               else incorrect_number _loc pl sl : 'patt )
                          | _ -> assert false)))])]))
     | None  -> ());
    defined := ((x, eo) :: (defined.contents))
  let undef x =
    try
      (let eo = List.assoc x defined.contents in
       match eo with
       | Some ([],_) ->
           (Gram.delete_rule expr
              [`Stoken
                 (((function | `UID __x when x = __x -> true | _ -> false)),
                   (`Antiquot, "`UID __x"))];
            Gram.delete_rule patt
              [`Stoken
                 (((function | `UID __x when x = __x -> true | _ -> false)),
                   (`Antiquot, "`UID __x"))])
       | Some (_,_) ->
           (Gram.delete_rule expr
              [`Stoken
                 (((function | `UID __x when x = __x -> true | _ -> false)),
                   (`Antiquot, "`UID __x"));
              `Sself];
            Gram.delete_rule patt
              [`Stoken
                 (((function | `UID __x when x = __x -> true | _ -> false)),
                   (`Antiquot, "`UID __x"));
              `Sself])
       | None  -> ());
      defined := (list_remove x defined.contents)
    with | Not_found  -> ()
  let parse_def s =
    match Gram.parse_string expr (FanLoc.mk "<command line>") s with
    | Ast.ExId (_,Ast.IdUid (_,n)) -> define None n
    | Ast.ExApp
        (_,Ast.ExApp
         (_,Ast.ExId (_,Ast.IdLid (_,"=")),Ast.ExId (_,Ast.IdUid (_,n))),e)
        -> define (Some ([], e)) n
    | _ -> invalid_arg s let include_dirs = ref []
  let add_include_dir str =
    if str <> ""
    then
      let str =
        if (str.[(String.length str) - 1]) = '/' then str else str ^ "/" in
      include_dirs := (include_dirs.contents @ [str])
    else ()
  let parse_include_file rule =
    let dir_ok file dir = Sys.file_exists (dir ^ file) in
    fun file  ->
      let file =
        try (List.find (dir_ok file) (include_dirs.contents @ ["./"])) ^ file
        with | Not_found  -> file in
      let ch = open_in file in
      let st = Stream.of_channel ch in Gram.parse rule (FanLoc.mk file) st
  let rec execute_macro nil cons =
    function
    | SdStr i -> i
    | SdDef (x,eo) -> (define eo x; nil)
    | SdUnd x -> (undef x; nil)
    | SdITE (b,l1,l2) -> execute_macro_list nil cons (if b then l1 else l2)
    | SdLazy l -> Lazy.force l
  and execute_macro_list nil cons =
    function
    | [] -> nil
    | hd::tl ->
        let il1 = execute_macro nil cons hd in
        let il2 = execute_macro_list nil cons tl in cons il1 il2
  let stack = Stack.create ()
  let make_SdITE_result st1 st2 =
    let test = Stack.pop stack in SdITE (test, st1, st2)
  type branch =  
    | Then
    | Else 
  let execute_macro_if_active_branch _loc nil cons branch macro_def =
    let test = Stack.top stack in
    let item =
      if (test && (branch = Then)) || ((not test) && (branch = Else))
      then execute_macro nil cons macro_def
      else nil in
    SdStr item
  let _ =
    let grammar_entry_create = Gram.mk in
    let macro_def: 'macro_def Gram.t = grammar_entry_create "macro_def"
    and uident: 'uident Gram.t = grammar_entry_create "uident"
    and opt_macro_value: 'opt_macro_value Gram.t =
      grammar_entry_create "opt_macro_value"
    and endif: 'endif Gram.t = grammar_entry_create "endif"
    and sglist_else: 'sglist_else Gram.t = grammar_entry_create "sglist_else"
    and sglist_then: 'sglist_then Gram.t = grammar_entry_create "sglist_then"
    and smlist_else: 'smlist_else Gram.t = grammar_entry_create "smlist_else"
    and smlist_then: 'smlist_then Gram.t = grammar_entry_create "smlist_then"
    and else_expr: 'else_expr Gram.t = grammar_entry_create "else_expr"
    and else_macro_def_sig: 'else_macro_def_sig Gram.t =
      grammar_entry_create "else_macro_def_sig"
    and else_macro_def: 'else_macro_def Gram.t =
      grammar_entry_create "else_macro_def"
    and uident_eval_ifndef: 'uident_eval_ifndef Gram.t =
      grammar_entry_create "uident_eval_ifndef"
    and uident_eval_ifdef: 'uident_eval_ifdef Gram.t =
      grammar_entry_create "uident_eval_ifdef"
    and macro_def_sig: 'macro_def_sig Gram.t =
      grammar_entry_create "macro_def_sig" in
    Gram.extend (str_item : 'str_item Gram.t )
      ((Some `First),
        [(None, None,
           [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ))],
              (Gram.mk_action
                 (fun (x : 'macro_def)  (_loc : FanLoc.t)  ->
                    (execute_macro (Ast.StNil _loc)
                       (fun a  b  -> Ast.StSem (_loc, a, b)) x : 'str_item ))))])]);
    Gram.extend (sig_item : 'sig_item Gram.t )
      ((Some `First),
        [(None, None,
           [([`Snterm (Gram.obj (macro_def_sig : 'macro_def_sig Gram.t ))],
              (Gram.mk_action
                 (fun (x : 'macro_def_sig)  (_loc : FanLoc.t)  ->
                    (execute_macro (Ast.SgNil _loc)
                       (fun a  b  -> Ast.SgSem (_loc, a, b)) x : 'sig_item ))))])]);
    Gram.extend (macro_def : 'macro_def Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "INCLUDE";
             `Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `STR (_,fname) ->
                        (SdLazy (lazy (parse_include_file str_items fname)) : 
                        'macro_def )
                    | _ -> assert false)));
           ([`Skeyword "IFNDEF";
            `Snterm
              (Gram.obj (uident_eval_ifndef : 'uident_eval_ifndef Gram.t ));
            `Skeyword "THEN";
            `Snterm (Gram.obj (smlist_then : 'smlist_then Gram.t ));
            `Snterm (Gram.obj (else_macro_def : 'else_macro_def Gram.t ))],
             (Gram.mk_action
                (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ 
                   (_loc : FanLoc.t)  ->
                   (make_SdITE_result st1 st2 : 'macro_def ))));
           ([`Skeyword "IFDEF";
            `Snterm
              (Gram.obj (uident_eval_ifdef : 'uident_eval_ifdef Gram.t ));
            `Skeyword "THEN";
            `Snterm (Gram.obj (smlist_then : 'smlist_then Gram.t ));
            `Snterm (Gram.obj (else_macro_def : 'else_macro_def Gram.t ))],
             (Gram.mk_action
                (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ 
                   (_loc : FanLoc.t)  ->
                   (make_SdITE_result st1 st2 : 'macro_def ))));
           ([`Skeyword "UNDEF";
            `Snterm (Gram.obj (uident : 'uident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                   (SdUnd i : 'macro_def ))));
           ([`Skeyword "DEFINE";
            `Snterm (Gram.obj (uident : 'uident Gram.t ));
            `Snterm (Gram.obj (opt_macro_value : 'opt_macro_value Gram.t ))],
             (Gram.mk_action
                (fun (def : 'opt_macro_value)  (i : 'uident)  _ 
                   (_loc : FanLoc.t)  -> (SdDef (i, def) : 'macro_def ))))])]);
    Gram.extend (macro_def_sig : 'macro_def_sig Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "INCLUDE";
             `Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `STR (_,fname) ->
                        (SdLazy (lazy (parse_include_file sig_items fname)) : 
                        'macro_def_sig )
                    | _ -> assert false)));
           ([`Skeyword "IFNDEF";
            `Snterm
              (Gram.obj (uident_eval_ifndef : 'uident_eval_ifndef Gram.t ));
            `Skeyword "THEN";
            `Snterm (Gram.obj (sglist_then : 'sglist_then Gram.t ));
            `Snterm
              (Gram.obj (else_macro_def_sig : 'else_macro_def_sig Gram.t ))],
             (Gram.mk_action
                (fun (sg2 : 'else_macro_def_sig)  (sg1 : 'sglist_then)  _  _ 
                   _  (_loc : FanLoc.t)  ->
                   (make_SdITE_result sg1 sg2 : 'macro_def_sig ))));
           ([`Skeyword "IFDEF";
            `Snterm
              (Gram.obj (uident_eval_ifdef : 'uident_eval_ifdef Gram.t ));
            `Skeyword "THEN";
            `Snterm (Gram.obj (sglist_then : 'sglist_then Gram.t ));
            `Snterm
              (Gram.obj (else_macro_def_sig : 'else_macro_def_sig Gram.t ))],
             (Gram.mk_action
                (fun (sg2 : 'else_macro_def_sig)  (sg1 : 'sglist_then)  _  _ 
                   _  (_loc : FanLoc.t)  ->
                   (make_SdITE_result sg1 sg2 : 'macro_def_sig ))));
           ([`Skeyword "UNDEF";
            `Snterm (Gram.obj (uident : 'uident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                   (SdUnd i : 'macro_def_sig ))));
           ([`Skeyword "DEFINE";
            `Snterm (Gram.obj (uident : 'uident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                   (SdDef (i, None) : 'macro_def_sig ))))])]);
    Gram.extend (uident_eval_ifdef : 'uident_eval_ifdef Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (uident : 'uident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'uident)  (_loc : FanLoc.t)  ->
                    (Stack.push (is_defined i) stack : 'uident_eval_ifdef ))))])]);
    Gram.extend (uident_eval_ifndef : 'uident_eval_ifndef Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (uident : 'uident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'uident)  (_loc : FanLoc.t)  ->
                    (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef ))))])]);
    Gram.extend (else_macro_def : 'else_macro_def Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  -> ([] : 'else_macro_def ))));
           ([`Skeyword "ELSE";
            `Snterm (Gram.obj (smlist_else : 'smlist_else Gram.t ));
            `Snterm (Gram.obj (endif : 'endif Gram.t ))],
             (Gram.mk_action
                (fun _  (st : 'smlist_else)  _  (_loc : FanLoc.t)  ->
                   (st : 'else_macro_def ))))])]);
    Gram.extend (else_macro_def_sig : 'else_macro_def_sig Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  -> ([] : 'else_macro_def_sig ))));
           ([`Skeyword "ELSE";
            `Snterm (Gram.obj (sglist_else : 'sglist_else Gram.t ));
            `Snterm (Gram.obj (endif : 'endif Gram.t ))],
             (Gram.mk_action
                (fun _  (st : 'sglist_else)  _  (_loc : FanLoc.t)  ->
                   (st : 'else_macro_def_sig ))))])]);
    Gram.extend (else_expr : 'else_expr Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) : 'else_expr ))));
           ([`Skeyword "ELSE";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Snterm (Gram.obj (endif : 'endif Gram.t ))],
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (e : 'else_expr ))))])]);
    Gram.extend (smlist_then : 'smlist_then Gram.t )
      (None,
        [(None, None,
           [([`Slist1
                (Gram.srules smlist_then
                   [([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (si : 'str_item)  (_loc : FanLoc.t)  ->
                            (SdStr si : 'e__10 ))));
                   ([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ));
                    `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                     (Gram.mk_action
                        (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->
                           (execute_macro_if_active_branch _loc
                              (Ast.StNil _loc)
                              (fun a  b  -> Ast.StSem (_loc, a, b)) Then d : 
                           'e__10 ))))])],
              (Gram.mk_action
                 (fun (sml : 'e__10 list)  (_loc : FanLoc.t)  ->
                    (sml : 'smlist_then ))))])]);
    Gram.extend (smlist_else : 'smlist_else Gram.t )
      (None,
        [(None, None,
           [([`Slist1
                (Gram.srules smlist_else
                   [([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (si : 'str_item)  (_loc : FanLoc.t)  ->
                            (SdStr si : 'e__11 ))));
                   ([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ));
                    `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                     (Gram.mk_action
                        (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->
                           (execute_macro_if_active_branch _loc
                              (Ast.StNil _loc)
                              (fun a  b  -> Ast.StSem (_loc, a, b)) Else d : 
                           'e__11 ))))])],
              (Gram.mk_action
                 (fun (sml : 'e__11 list)  (_loc : FanLoc.t)  ->
                    (sml : 'smlist_else ))))])]);
    Gram.extend (sglist_then : 'sglist_then Gram.t )
      (None,
        [(None, None,
           [([`Slist1
                (Gram.srules sglist_then
                   [([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (si : 'sig_item)  (_loc : FanLoc.t)  ->
                            (SdStr si : 'e__12 ))));
                   ([`Snterm
                       (Gram.obj (macro_def_sig : 'macro_def_sig Gram.t ));
                    `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                     (Gram.mk_action
                        (fun _  (d : 'macro_def_sig)  (_loc : FanLoc.t)  ->
                           (execute_macro_if_active_branch _loc
                              (Ast.SgNil _loc)
                              (fun a  b  -> Ast.SgSem (_loc, a, b)) Then d : 
                           'e__12 ))))])],
              (Gram.mk_action
                 (fun (sgl : 'e__12 list)  (_loc : FanLoc.t)  ->
                    (sgl : 'sglist_then ))))])]);
    Gram.extend (sglist_else : 'sglist_else Gram.t )
      (None,
        [(None, None,
           [([`Slist1
                (Gram.srules sglist_else
                   [([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (si : 'sig_item)  (_loc : FanLoc.t)  ->
                            (SdStr si : 'e__13 ))));
                   ([`Snterm
                       (Gram.obj (macro_def_sig : 'macro_def_sig Gram.t ));
                    `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                     (Gram.mk_action
                        (fun _  (d : 'macro_def_sig)  (_loc : FanLoc.t)  ->
                           (execute_macro_if_active_branch _loc
                              (Ast.SgNil _loc)
                              (fun a  b  -> Ast.SgSem (_loc, a, b)) Else d : 
                           'e__13 ))))])],
              (Gram.mk_action
                 (fun (sgl : 'e__13 list)  (_loc : FanLoc.t)  ->
                    (sgl : 'sglist_else ))))])]);
    Gram.extend (endif : 'endif Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "ENDIF"],
              (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif ))));
           ([`Skeyword "END"],
             (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif ))))])]);
    Gram.extend (opt_macro_value : 'opt_macro_value Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (None : 'opt_macro_value ))));
           ([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (Some ([], e) : 'opt_macro_value ))));
           ([`Skeyword "(";
            `Slist1sep
              ((Gram.srules opt_macro_value
                  [([`Stoken
                       (((function | `LID _ -> true | _ -> false)),
                         (`Normal, "`LID _"))],
                     (Gram.mk_action
                        (fun __camlp4_0  (_loc : FanLoc.t)  ->
                           match __camlp4_0 with
                           | `LID x -> (x : 'e__14 )
                           | _ -> assert false)))]), (`Skeyword ","));
            `Skeyword ")";
            `Skeyword "=";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  _  (pl : 'e__14 list)  _ 
                   (_loc : FanLoc.t)  -> (Some (pl, e) : 'opt_macro_value ))))])]);
    Gram.extend (expr : 'expr Gram.t )
      ((Some (`Level "top")),
        [(None, None,
           [([`Skeyword "DEFINE";
             `Stoken
               (((function | `LID _ -> true | _ -> false)),
                 (`Normal, "`LID _"));
             `Skeyword "=";
             `Sself;
             `Skeyword "IN";
             `Sself],
              (Gram.mk_action
                 (fun (body : 'expr)  _  (def : 'expr)  _  __camlp4_0  _ 
                    (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `LID i ->
                        (((new Expr.subst) _loc [(i, def)])#expr body : 
                        'expr )
                    | _ -> assert false)));
           ([`Skeyword "IFNDEF";
            `Snterm (Gram.obj (uident : 'uident Gram.t ));
            `Skeyword "THEN";
            `Sself;
            `Snterm (Gram.obj (else_expr : 'else_expr Gram.t ))],
             (Gram.mk_action
                (fun (e2 : 'else_expr)  (e1 : 'expr)  _  (i : 'uident)  _ 
                   (_loc : FanLoc.t)  ->
                   (if is_defined i then e2 else e1 : 'expr ))));
           ([`Skeyword "IFDEF";
            `Snterm (Gram.obj (uident : 'uident Gram.t ));
            `Skeyword "THEN";
            `Sself;
            `Snterm (Gram.obj (else_expr : 'else_expr Gram.t ))],
             (Gram.mk_action
                (fun (e2 : 'else_expr)  (e1 : 'expr)  _  (i : 'uident)  _ 
                   (_loc : FanLoc.t)  ->
                   (if is_defined i then e1 else e2 : 'expr ))))])]);
    Gram.extend (patt : 'patt Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "IFNDEF";
             `Snterm (Gram.obj (uident : 'uident Gram.t ));
             `Skeyword "THEN";
             `Sself;
             `Skeyword "ELSE";
             `Sself;
             `Snterm (Gram.obj (endif : 'endif Gram.t ))],
              (Gram.mk_action
                 (fun _  (p2 : 'patt)  _  (p1 : 'patt)  _  (i : 'uident)  _ 
                    (_loc : FanLoc.t)  ->
                    (if is_defined i then p2 else p1 : 'patt ))));
           ([`Skeyword "IFDEF";
            `Snterm (Gram.obj (uident : 'uident Gram.t ));
            `Skeyword "THEN";
            `Sself;
            `Skeyword "ELSE";
            `Sself;
            `Snterm (Gram.obj (endif : 'endif Gram.t ))],
             (Gram.mk_action
                (fun _  (p2 : 'patt)  _  (p1 : 'patt)  _  (i : 'uident)  _ 
                   (_loc : FanLoc.t)  ->
                   (if is_defined i then p1 else p2 : 'patt ))))])]);
    Gram.extend (uident : 'uident Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `UID _ -> true | _ -> false)),
                  (`Normal, "`UID _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `UID i -> (i : 'uident )
                    | _ -> assert false)))])]);
    Gram.extend (expr : 'expr Gram.t )
      ((Some (`Before "simple")),
        [(None, None,
           [([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
              (Gram.mk_action
                 (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                    (Ast.ExVrn (_loc, s) : 'expr ))));
           ([`Skeyword "`";
            Gram.srules expr
              [([`Skeyword "IN"],
                 (Gram.mk_action
                    (fun x  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "DEFINE"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "ENDIF"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "END"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "ELSE"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "THEN"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "IFNDEF"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))));
              ([`Skeyword "IFDEF"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__15 ))))]],
             (Gram.mk_action
                (fun (kwd : 'e__15)  _  (_loc : FanLoc.t)  ->
                   (Ast.ExVrn (_loc, kwd) : 'expr ))))])]);
    Gram.extend (patt : 'patt Gram.t )
      ((Some (`Before "simple")),
        [(None, None,
           [([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
              (Gram.mk_action
                 (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                    (Ast.PaVrn (_loc, s) : 'patt ))));
           ([`Skeyword "`";
            Gram.srules patt
              [([`Skeyword "ENDIF"],
                 (Gram.mk_action
                    (fun x  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__16 ))));
              ([`Skeyword "END"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__16 ))));
              ([`Skeyword "ELSE"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__16 ))));
              ([`Skeyword "THEN"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__16 ))));
              ([`Skeyword "IFNDEF"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__16 ))));
              ([`Skeyword "IFDEF"],
                (Gram.mk_action
                   (fun x  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__16 ))))]],
             (Gram.mk_action
                (fun (kwd : 'e__16)  _  (_loc : FanLoc.t)  ->
                   (Ast.PaVrn (_loc, kwd) : 'patt ))))])])
  let _ =
    Options.add
      ("-D", (FanArg.String parse_def),
        "<string> Define for IFDEF instruction.")
  let _ =
    Options.add
      ("-U", (FanArg.String undef),
        "<string> Undefine for IFDEF instruction.")
  let _ =
    Options.add
      ("-I", (FanArg.String add_include_dir),
        "<string> Add a directory to INCLUDE search path.")
  end
module MakeNothing(Syn:Sig.Camlp4Syntax) = struct
  module Ast = Camlp4Ast
  let _ =
    Syn.AstFilters.register_str_item_filter
      (Ast.map_expr Expr.map_expr)#str_item
  end
module IdRevisedParser = struct
  let name = "Camlp4OCamlRevisedParser" let version = Sys.ocaml_version
  end
module MakeRevisedParser(Syntax:Sig.Camlp4Syntax) = struct
  include Syntax module Ast = Camlp4Ast
  let _ = FanConfig.constructors_arity := false
  let help_sequences () =
    Printf.eprintf
      "New syntax:\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\n    while e do e1; e2; ... ; en done\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\nOld syntax (still supported):\n    begin e1; e2; ... ; en end\n    while e begin e1; e2; ... ; en end\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\nVery old (no more supported) syntax:\n    do e1; e2; ... ; en-1; return en\n    while e do e1; e2; ... ; en; done\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\n";
    flush stderr;
    exit 1
  let _ =
    Options.add
      ("-help_seq", (FanArg.Unit help_sequences),
        "Print explanations about new sequences and exit.")
  let _ = Gram.clear a_CHAR let _ = Gram.clear a_FLOAT
  let _ = Gram.clear a_INT let _ = Gram.clear a_INT32
  let _ = Gram.clear a_INT64 let _ = Gram.clear a_LABEL
  let _ = Gram.clear a_LIDENT let _ = Gram.clear a_NATIVEINT
  let _ = Gram.clear a_OPTLABEL let _ = Gram.clear a_STRING
  let _ = Gram.clear a_UIDENT let _ = Gram.clear a_ident
  let _ = Gram.clear amp_ctyp let _ = Gram.clear and_ctyp
  let _ = Gram.clear match_case let _ = Gram.clear match_case0
  let _ = Gram.clear match_case_quot let _ = Gram.clear binding
  let _ = Gram.clear binding_quot let _ = Gram.clear rec_binding_quot
  let _ = Gram.clear class_declaration let _ = Gram.clear class_description
  let _ = Gram.clear class_expr let _ = Gram.clear class_expr_quot
  let _ = Gram.clear class_fun_binding let _ = Gram.clear class_fun_def
  let _ = Gram.clear class_info_for_class_expr
  let _ = Gram.clear class_info_for_class_type
  let _ = Gram.clear class_longident
  let _ = Gram.clear class_longident_and_param
  let _ = Gram.clear class_name_and_param let _ = Gram.clear class_sig_item
  let _ = Gram.clear class_sig_item_quot let _ = Gram.clear class_signature
  let _ = Gram.clear class_str_item let _ = Gram.clear class_str_item_quot
  let _ = Gram.clear class_structure let _ = Gram.clear class_type
  let _ = Gram.clear class_type_declaration
  let _ = Gram.clear class_type_longident
  let _ = Gram.clear class_type_longident_and_param
  let _ = Gram.clear class_type_plus let _ = Gram.clear class_type_quot
  let _ = Gram.clear comma_ctyp let _ = Gram.clear comma_expr
  let _ = Gram.clear comma_ipatt let _ = Gram.clear comma_patt
  let _ = Gram.clear comma_type_parameter let _ = Gram.clear constrain
  let _ = Gram.clear constructor_arg_list
  let _ = Gram.clear constructor_declaration
  let _ = Gram.clear constructor_declarations let _ = Gram.clear ctyp
  let _ = Gram.clear ctyp_quot let _ = Gram.clear cvalue_binding
  let _ = Gram.clear direction_flag let _ = Gram.clear dummy
  let _ = Gram.clear eq_expr let _ = Gram.clear expr
  let _ = Gram.clear expr_eoi let _ = Gram.clear expr_quot
  let _ = Gram.clear field_expr let _ = Gram.clear field_expr_list
  let _ = Gram.clear fun_binding let _ = Gram.clear fun_def
  let _ = Gram.clear ident let _ = Gram.clear ident_quot
  let _ = Gram.clear implem let _ = Gram.clear interf
  let _ = Gram.clear ipatt let _ = Gram.clear ipatt_tcon
  let _ = Gram.clear label let _ = Gram.clear label_declaration
  let _ = Gram.clear label_declaration_list
  let _ = Gram.clear label_expr_list let _ = Gram.clear label_expr
  let _ = Gram.clear label_ipatt let _ = Gram.clear label_ipatt_list
  let _ = Gram.clear label_longident let _ = Gram.clear label_patt
  let _ = Gram.clear label_patt_list let _ = Gram.clear labeled_ipatt
  let _ = Gram.clear let_binding let _ = Gram.clear meth_list
  let _ = Gram.clear meth_decl let _ = Gram.clear module_binding
  let _ = Gram.clear module_binding0 let _ = Gram.clear module_binding_quot
  let _ = Gram.clear module_declaration let _ = Gram.clear module_expr
  let _ = Gram.clear module_expr_quot let _ = Gram.clear module_longident
  let _ = Gram.clear module_longident_with_app
  let _ = Gram.clear module_rec_declaration let _ = Gram.clear module_type
  let _ = Gram.clear module_type_quot let _ = Gram.clear more_ctyp
  let _ = Gram.clear name_tags let _ = Gram.clear opt_as_lident
  let _ = Gram.clear opt_class_self_patt
  let _ = Gram.clear opt_class_self_type let _ = Gram.clear opt_comma_ctyp
  let _ = Gram.clear opt_dot_dot let _ = Gram.clear opt_eq_ctyp
  let _ = Gram.clear opt_expr let _ = Gram.clear opt_meth_list
  let _ = Gram.clear opt_mutable let _ = Gram.clear opt_polyt
  let _ = Gram.clear opt_private let _ = Gram.clear opt_rec
  let _ = Gram.clear opt_virtual let _ = Gram.clear opt_when_expr
  let _ = Gram.clear patt let _ = Gram.clear patt_as_patt_opt
  let _ = Gram.clear patt_eoi let _ = Gram.clear patt_quot
  let _ = Gram.clear patt_tcon let _ = Gram.clear phrase
  let _ = Gram.clear poly_type let _ = Gram.clear row_field
  let _ = Gram.clear sem_expr let _ = Gram.clear sem_expr_for_list
  let _ = Gram.clear sem_patt let _ = Gram.clear sem_patt_for_list
  let _ = Gram.clear semi let _ = Gram.clear sequence
  let _ = Gram.clear sig_item let _ = Gram.clear sig_item_quot
  let _ = Gram.clear sig_items let _ = Gram.clear star_ctyp
  let _ = Gram.clear str_item let _ = Gram.clear str_item_quot
  let _ = Gram.clear str_items let _ = Gram.clear top_phrase
  let _ = Gram.clear type_constraint let _ = Gram.clear type_declaration
  let _ = Gram.clear type_ident_and_parameters let _ = Gram.clear type_kind
  let _ = Gram.clear type_longident
  let _ = Gram.clear type_longident_and_parameters
  let _ = Gram.clear type_parameter let _ = Gram.clear type_parameters
  let _ = Gram.clear typevars let _ = Gram.clear use_file
  let _ = Gram.clear val_longident let _ = Gram.clear with_constr
  let _ = Gram.clear with_constr_quot
  let _ =
    let list = ['!'; '?'; '~'] in
    let excl = ["!="; "??"] in
    setup_op_parser prefixop
      (fun x  ->
         (not (List.mem x excl)) &&
           (((String.length x) >= 2) &&
              ((List.mem (x.[0]) list) && (symbolchar x 1))))
  let _ =
    let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
    let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
    let excl = ["<-"; "||"; "&&"] in
    setup_op_parser infixop0
      (fun x  ->
         (List.mem x list_ok) ||
           ((not (List.mem x excl)) &&
              (((String.length x) >= 2) &&
                 ((List.mem (x.[0]) list_first_char_ok) && (symbolchar x 1)))))
  let _ =
    let list = ['@'; '^'] in
    setup_op_parser infixop1
      (fun x  ->
         ((String.length x) >= 1) &&
           ((List.mem (x.[0]) list) && (symbolchar x 1)))
  let _ =
    let list = ['+'; '-'] in
    setup_op_parser infixop2
      (fun x  ->
         (x <> "->") &&
           (((String.length x) >= 1) &&
              ((List.mem (x.[0]) list) && (symbolchar x 1))))
  let _ =
    let list = ['*'; '/'; '%'; '\\'] in
    setup_op_parser infixop3
      (fun x  ->
         ((String.length x) >= 1) &&
           ((List.mem (x.[0]) list) &&
              ((((x.[0]) <> '*') ||
                  (((String.length x) < 2) || ((x.[1]) <> '*')))
                 && (symbolchar x 1))))
  let _ =
    setup_op_parser infixop4
      (fun x  ->
         ((String.length x) >= 2) &&
           (((x.[0]) == '*') && (((x.[1]) == '*') && (symbolchar x 2))))
  let _ =
    FanToken.Filter.define_filter (Gram.get_filter ())
      (fun f  strm  -> infix_kwds_filter (f strm))
  let _ =
    Gram.setup_parser sem_expr
      (let symb1 = Gram.parse_origin_tokens expr in
       let symb (__strm : _ Stream.t) =
         match Stream.peek __strm with
         | Some (`ANTIQUOT (("list" as n),s),ti) ->
             (Stream.junk __strm;
              (let _loc = Gram.token_location ti in
               Ast.ExAnt (_loc, (mk_anti ~c:"expr;" n s))))
         | _ -> symb1 __strm in
       let rec kont al (__strm : _ Stream.t) =
         match Stream.peek __strm with
         | Some (`KEYWORD ";",_) ->
             (Stream.junk __strm;
              (let a =
                 try symb __strm
                 with | Stream.Failure  -> raise (Stream.Error "") in
               let s = __strm in
               let _loc =
                 FanLoc.merge (Ast.loc_of_expr al) (Ast.loc_of_expr a) in
               kont (Ast.ExSem (_loc, al, a)) s))
         | _ -> al in
       fun (__strm : _ Stream.t)  -> let a = symb __strm in kont a __strm)
  let _ =
    Gram.extend (module_expr : 'module_expr Gram.t )
      (None,
        [((Some "top"), None,
           [([`Skeyword "struct";
             `Snterm (Gram.obj (str_items : 'str_items Gram.t ));
             `Skeyword "end"],
              (Gram.mk_action
                 (fun _  (st : 'str_items)  _  (_loc : FanLoc.t)  ->
                    (Ast.MeStr (_loc, st) : 'module_expr ))));
           ([`Skeyword "functor";
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
                   (Ast.MeFun (_loc, i, t, me) : 'module_expr ))))]);
        ((Some "apply"), None,
          [([`Sself; `Sself],
             (Gram.mk_action
                (fun (me2 : 'module_expr)  (me1 : 'module_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.MeApp (_loc, me1, me2) : 'module_expr ))))]);
        ((Some "simple"), None,
          [([`Skeyword "(";
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
                   'module_expr ))));
          ([`Skeyword "(";
           `Skeyword "val";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (e : 'expr)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.MePkg (_loc, e) : 'module_expr ))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                  (me : 'module_expr ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ":";
           `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (mt : 'module_type)  _  (me : 'module_expr)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.MeTyc (_loc, me, mt) : 'module_expr ))));
          ([`Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'module_longident)  (_loc : FanLoc.t)  ->
                  (Ast.MeId (_loc, i) : 'module_expr ))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `QUOTATION x ->
                      (Quotation.expand _loc x DynAst.module_expr_tag : 
                      'module_expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"mexp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"mexp\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"mexp"|"anti"|"list" as n),s) ->
                      (Ast.MeAnt (_loc, (mk_anti ~c:"module_expr" n s)) : 
                      'module_expr )
                  | _ -> assert false)))])])
  let _ =
    Gram.extend (str_item : 'str_item Gram.t )
      (None,
        [((Some "top"), None,
           [([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                    (Ast.StExp (_loc, e) : 'str_item ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.str_item_tag : 
                       'str_item )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"stri"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"stri"|"anti"|"list" as n),s) ->
                       (Ast.StAnt (_loc, (mk_anti ~c:"str_item" n s)) : 
                       'str_item )
                   | _ -> assert false)));
           ([`Skeyword "class";
            `Skeyword "type";
            `Snterm
              (Gram.obj
                 (class_type_declaration : 'class_type_declaration Gram.t ))],
             (Gram.mk_action
                (fun (ctd : 'class_type_declaration)  _  _  (_loc : FanLoc.t)
                    -> (Ast.StClt (_loc, ctd) : 'str_item ))));
           ([`Skeyword "class";
            `Snterm
              (Gram.obj (class_declaration : 'class_declaration Gram.t ))],
             (Gram.mk_action
                (fun (cd : 'class_declaration)  _  (_loc : FanLoc.t)  ->
                   (Ast.StCls (_loc, cd) : 'str_item ))));
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
            `Skeyword "module";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
            `Skeyword "in";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (mb : 'module_binding0)  (m : 'a_UIDENT)
                    _  _  (_loc : FanLoc.t)  ->
                   (Ast.StExp (_loc, (Ast.ExLmd (_loc, m, mb, e))) : 
                   'str_item ))));
           ([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ))],
             (Gram.mk_action
                (fun (bi : 'binding)  (r : 'opt_rec)  _  (_loc : FanLoc.t) 
                   ->
                   (match bi with
                    | Ast.BiEq (_,Ast.PaAny _,e) -> Ast.StExp (_loc, e)
                    | _ -> Ast.StVal (_loc, r, bi) : 'str_item ))));
           ([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.StExp (_loc, (Ast.ExLet (_loc, r, bi, x))) : 
                   'str_item ))));
           ([`Skeyword "type";
            `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
             (Gram.mk_action
                (fun (td : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                   (Ast.StTyp (_loc, td) : 'str_item ))));
           ([`Skeyword "open";
            `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (Ast.StOpn (_loc, i) : 'str_item ))));
           ([`Skeyword "module";
            `Skeyword "type";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (i : 'a_ident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.StMty (_loc, i, mt) : 'str_item ))));
           ([`Skeyword "module";
            `Skeyword "rec";
            `Snterm (Gram.obj (module_binding : 'module_binding Gram.t ))],
             (Gram.mk_action
                (fun (mb : 'module_binding)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.StRecMod (_loc, mb) : 'str_item ))));
           ([`Skeyword "module";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ))],
             (Gram.mk_action
                (fun (mb : 'module_binding0)  (i : 'a_UIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.StMod (_loc, i, mb) : 'str_item ))));
           ([`Skeyword "include";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                   (Ast.StInc (_loc, me) : 'str_item ))));
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
           ([`Skeyword "exception";
            `Snterm
              (Gram.obj
                 (constructor_declaration : 'constructor_declaration Gram.t ))],
             (Gram.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->
                   (Ast.StExc (_loc, t, Ast.ONone) : 'str_item ))))])])
  let _ =
    Gram.extend (module_binding0 : 'module_binding0 Gram.t )
      (None,
        [(None, (Some `RA),
           [([`Skeyword "=";
             `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
              (Gram.mk_action
                 (fun (me : 'module_expr)  _  (_loc : FanLoc.t)  ->
                    (me : 'module_binding0 ))));
           ([`Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.MeTyc (_loc, me, mt) : 'module_binding0 ))));
           ([`Skeyword "(";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword ")";
            `Sself],
             (Gram.mk_action
                (fun (mb : 'module_binding0)  _  (mt : 'module_type)  _ 
                   (m : 'a_UIDENT)  _  (_loc : FanLoc.t)  ->
                   (Ast.MeFun (_loc, m, mt, mb) : 'module_binding0 ))))])]);
    Gram.extend (module_binding : 'module_binding Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
             `Skeyword ":";
             `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
             `Skeyword "=";
             `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
              (Gram.mk_action
                 (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                    (m : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.MbColEq (_loc, m, mt, me) : 'module_binding ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.module_binding_tag : 
                       'module_binding )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"\",_)"));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                   __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),m) ->
                       (Ast.MbColEq (_loc, (mk_anti n m), mt, me) : 'module_binding )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),s) ->
                       (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_binding )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("module_binding"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANTIQUOT ((\"module_binding\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("module_binding"|"anti"|"list" as n),s) ->
                       (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_binding )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (b2 : 'module_binding)  _  (b1 : 'module_binding) 
                   (_loc : FanLoc.t)  ->
                   (Ast.MbAnd (_loc, b1, b2) : 'module_binding ))))])])
  let _ =
    Gram.extend (module_rec_declaration : 'module_rec_declaration Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
             `Skeyword ":";
             `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
              (Gram.mk_action
                 (fun (mt : 'module_type)  _  (m : 'a_UIDENT) 
                    (_loc : FanLoc.t)  ->
                    (Ast.MbCol (_loc, m, mt) : 'module_rec_declaration ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.module_binding_tag : 
                       'module_rec_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"module_binding"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANTIQUOT ((\"\"|\"module_binding\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"module_binding"|"anti"|"list" as n),s)
                       ->
                       (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_rec_declaration )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (m2 : 'module_rec_declaration)  _ 
                   (m1 : 'module_rec_declaration)  (_loc : FanLoc.t)  ->
                   (Ast.MbAnd (_loc, m1, m2) : 'module_rec_declaration ))))])]);
    Gram.extend (with_constr : 'with_constr Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "module";
             `Snterm
               (Gram.obj (module_longident : 'module_longident Gram.t ));
             `Skeyword ":=";
             `Snterm
               (Gram.obj
                  (module_longident_with_app : 'module_longident_with_app
                                                 Gram.t ))],
              (Gram.mk_action
                 (fun (i2 : 'module_longident_with_app)  _ 
                    (i1 : 'module_longident)  _  (_loc : FanLoc.t)  ->
                    (Ast.WcMoS (_loc, i1, i2) : 'with_constr ))));
           ([`Skeyword "type";
            `Snterm
              (Gram.obj
                 (type_longident_and_parameters : 'type_longident_and_parameters
                                                    Gram.t ));
            `Skeyword ":=";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : FanLoc.t)  ->
                   (Ast.WcTyS (_loc, t1, t2) : 'with_constr ))));
           ([`Skeyword "type";
            `Stoken
              (((function
                 | `ANTIQUOT ((""|"typ"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"),_)"));
            `Skeyword ":=";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  _  __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
                       (Ast.WcTyS
                          (_loc, (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s))),
                            t) : 'with_constr )
                   | _ -> assert false)));
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
           ([`Skeyword "type";
            `Snterm
              (Gram.obj
                 (type_longident_and_parameters : 'type_longident_and_parameters
                                                    Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'type_longident_and_parameters) 
                   _  (_loc : FanLoc.t)  ->
                   (Ast.WcTyp (_loc, t1, t2) : 'with_constr ))));
           ([`Skeyword "type";
            `Stoken
              (((function
                 | `ANTIQUOT ((""|"typ"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"),_)"));
            `Skeyword "=";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  _  __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
                       (Ast.WcTyp
                          (_loc, (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s))),
                            t) : 'with_constr )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.with_constr_tag : 
                       'with_constr )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"with_constr"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANTIQUOT ((\"\"|\"with_constr\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"with_constr"|"anti"|"list" as n),s) ->
                       (Ast.WcAnt (_loc, (mk_anti ~c:"with_constr" n s)) : 
                       'with_constr )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (wc2 : 'with_constr)  _  (wc1 : 'with_constr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.WcAnd (_loc, wc1, wc2) : 'with_constr ))))])])
  let _ =
    Gram.extend (module_type : 'module_type Gram.t )
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
          [([`Sself; `Sself; `Snterm (Gram.obj (dummy : 'dummy Gram.t ))],
             (Gram.mk_action
                (fun _  (mt2 : 'module_type)  (mt1 : 'module_type) 
                   (_loc : FanLoc.t)  ->
                   (ModuleType.app mt1 mt2 : 'module_type ))))]);
        ((Some "."), None,
          [([`Sself; `Skeyword "."; `Sself],
             (Gram.mk_action
                (fun (mt2 : 'module_type)  _  (mt1 : 'module_type) 
                   (_loc : FanLoc.t)  ->
                   (ModuleType.acc mt1 mt2 : 'module_type ))))]);
        ((Some "sig"), None,
          [([`Skeyword "sig";
            `Snterm (Gram.obj (sig_items : 'sig_items Gram.t ));
            `Skeyword "end"],
             (Gram.mk_action
                (fun _  (sg : 'sig_items)  _  (_loc : FanLoc.t)  ->
                   (Ast.MtSig (_loc, sg) : 'module_type ))))]);
        ((Some "simple"), None,
          [([`Skeyword "module";
            `Skeyword "type";
            `Skeyword "of";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  _  _  (_loc : FanLoc.t)  ->
                   (Ast.MtOf (_loc, me) : 'module_type ))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                  (mt : 'module_type ))));
          ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.MtQuo (_loc, i) : 'module_type ))));
          ([`Snterm
              (Gram.obj
                 (module_longident_with_app : 'module_longident_with_app
                                                Gram.t ))],
            (Gram.mk_action
               (fun (i : 'module_longident_with_app)  (_loc : FanLoc.t)  ->
                  (Ast.MtId (_loc, i) : 'module_type ))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `QUOTATION x ->
                      (Quotation.expand _loc x DynAst.module_type_tag : 
                      'module_type )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"mtyp"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"mtyp\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"mtyp"|"anti"|"list" as n),s) ->
                      (Ast.MtAnt (_loc, (mk_anti ~c:"module_type" n s)) : 
                      'module_type )
                  | _ -> assert false)))])]);
    Gram.extend (module_declaration : 'module_declaration Gram.t )
      (None,
        [(None, (Some `RA),
           [([`Skeyword "(";
             `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
             `Skeyword ":";
             `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
             `Skeyword ")";
             `Sself],
              (Gram.mk_action
                 (fun (mt : 'module_declaration)  _  (t : 'module_type)  _ 
                    (i : 'a_UIDENT)  _  (_loc : FanLoc.t)  ->
                    (Ast.MtFun (_loc, i, t, mt) : 'module_declaration ))));
           ([`Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                   (mt : 'module_declaration ))))])])
  let _ =
    Gram.extend (sig_item : 'sig_item Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "class";
             `Skeyword "type";
             `Snterm
               (Gram.obj
                  (class_type_declaration : 'class_type_declaration Gram.t ))],
              (Gram.mk_action
                 (fun (ctd : 'class_type_declaration)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (Ast.SgClt (_loc, ctd) : 'sig_item ))));
           ([`Skeyword "class";
            `Snterm
              (Gram.obj (class_description : 'class_description Gram.t ))],
             (Gram.mk_action
                (fun (cd : 'class_description)  _  (_loc : FanLoc.t)  ->
                   (Ast.SgCls (_loc, cd) : 'sig_item ))));
           ([`Skeyword "val";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t) 
                   -> (Ast.SgVal (_loc, i, t) : 'sig_item ))));
           ([`Skeyword "type";
            `Snterm (Gram.obj (type_declaration : 'type_declaration Gram.t ))],
             (Gram.mk_action
                (fun (t : 'type_declaration)  _  (_loc : FanLoc.t)  ->
                   (Ast.SgTyp (_loc, t) : 'sig_item ))));
           ([`Skeyword "open";
            `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'module_longident)  _  (_loc : FanLoc.t)  ->
                   (Ast.SgOpn (_loc, i) : 'sig_item ))));
           ([`Skeyword "module";
            `Skeyword "type";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.SgMty (_loc, i, (Ast.MtNil _loc)) : 'sig_item ))));
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
            `Skeyword "rec";
            `Snterm
              (Gram.obj
                 (module_rec_declaration : 'module_rec_declaration Gram.t ))],
             (Gram.mk_action
                (fun (mb : 'module_rec_declaration)  _  _  (_loc : FanLoc.t) 
                   -> (Ast.SgRecMod (_loc, mb) : 'sig_item ))));
           ([`Skeyword "module";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Snterm
              (Gram.obj (module_declaration : 'module_declaration Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_declaration)  (i : 'a_UIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.SgMod (_loc, i, mt) : 'sig_item ))));
           ([`Skeyword "include";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (_loc : FanLoc.t)  ->
                   (Ast.SgInc (_loc, mt) : 'sig_item ))));
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
           ([`Skeyword "exception";
            `Snterm
              (Gram.obj
                 (constructor_declaration : 'constructor_declaration Gram.t ))],
             (Gram.mk_action
                (fun (t : 'constructor_declaration)  _  (_loc : FanLoc.t)  ->
                   (Ast.SgExc (_loc, t) : 'sig_item ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.sig_item_tag : 
                       'sig_item )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"sigi"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"sigi"|"anti"|"list" as n),s) ->
                       (Ast.SgAnt (_loc, (mk_anti ~c:"sig_item" n s)) : 
                       'sig_item )
                   | _ -> assert false)))])])
  let _ =
    Gram.extend (lang : 'lang Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `STR (_,_) -> true | _ -> false)),
                  (`Normal, "`STR (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `STR (_,s) ->
                        (let old = Quotation.default.contents in
                         (Quotation.default := s; old) : 'lang )
                    | _ -> assert false)))])]);
    Gram.extend (expr : 'expr Gram.t )
      (None,
        [((Some "top"), (Some `RA),
           [([`Skeyword "object";
             `Snterm
               (Gram.obj (opt_class_self_patt : 'opt_class_self_patt Gram.t ));
             `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
             `Skeyword "end"],
              (Gram.mk_action
                 (fun _  (cst : 'class_structure) 
                    (csp : 'opt_class_self_patt)  _  (_loc : FanLoc.t)  ->
                    (Ast.ExObj (_loc, csp, cst) : 'expr ))));
           ([`Skeyword "while";
            `Sself;
            `Skeyword "do";
            `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
            `Skeyword "done"],
             (Gram.mk_action
                (fun _  (seq : 'sequence)  _  (e : 'expr)  _ 
                   (_loc : FanLoc.t)  -> (Ast.ExWhi (_loc, e, seq) : 
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
                   (df : 'direction_flag)  (e1 : 'expr)  _  (i : 'a_LIDENT) 
                   _  (_loc : FanLoc.t)  ->
                   (Ast.ExFor (_loc, i, e1, e2, df, seq) : 'expr ))));
           ([`Skeyword "with";
            `Snterm (Gram.obj (lang : 'lang Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (x : 'expr)  (old : 'lang)  _  (_loc : FanLoc.t)  ->
                   (Quotation.default := old; x : 'expr ))));
           ([`Skeyword "do";
            `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
            `Skeyword "done"],
             (Gram.mk_action
                (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                   (Expr.mksequence _loc seq : 'expr ))));
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
           ([`Skeyword "try";
            `Sself;
            `Skeyword "with";
            `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
             (Gram.mk_action
                (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t) 
                   ->
                   (Ast.ExTry (_loc, (Expr.mksequence' _loc e), a) : 
                   'expr ))));
           ([`Skeyword "match";
            `Sself;
            `Skeyword "with";
            `Snterm (Gram.obj (match_case : 'match_case Gram.t ))],
             (Gram.mk_action
                (fun (a : 'match_case)  _  (e : 'expr)  _  (_loc : FanLoc.t) 
                   ->
                   (Ast.ExMat (_loc, (Expr.mksequence' _loc e), a) : 
                   'expr ))));
           ([`Skeyword "fun";
            `Snterm (Gram.obj (fun_def : 'fun_def Gram.t ))],
             (Gram.mk_action
                (fun (e : 'fun_def)  _  (_loc : FanLoc.t)  -> (e : 'expr ))));
           ([`Skeyword "fun";
            `Skeyword "[";
            `Slist0sep
              ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
                (`Skeyword "|"));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (a : 'match_case0 list)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.ExFun (_loc, (Ast.mcOr_of_list a)) : 'expr ))));
           ([`Skeyword "let";
            `Skeyword "open";
            `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
            `Skeyword "in";
            `Sself],
             (Gram.mk_action
                (fun (e : 'expr)  _  (i : 'module_longident)  _  _ 
                   (_loc : FanLoc.t)  -> (Ast.ExOpI (_loc, i, e) : 'expr ))));
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
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Sself],
             (Gram.mk_action
                (fun (x : 'expr)  _  (bi : 'binding)  (r : 'opt_rec)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExLet (_loc, r, bi, x) : 'expr ))))]);
        ((Some "where"), None,
          [([`Sself;
            `Skeyword "where";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (let_binding : 'let_binding Gram.t ))],
             (Gram.mk_action
                (fun (lb : 'let_binding)  (rf : 'opt_rec)  _  (e : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExLet (_loc, rf, lb, e) : 'expr ))))]);
        ((Some ":="), (Some `NA),
          [([`Sself;
            `Skeyword "<-";
            `Sself;
            `Snterm (Gram.obj (dummy : 'dummy Gram.t ))],
             (Gram.mk_action
                (fun _  (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (match Expr.bigarray_set _loc e1 e2 with
                    | Some e -> e
                    | None  -> Ast.ExAss (_loc, e1, e2) : 'expr ))));
          ([`Sself;
           `Skeyword ":=";
           `Sself;
           `Snterm (Gram.obj (dummy : 'dummy Gram.t ))],
            (Gram.mk_action
               (fun _  (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExAss
                     (_loc,
                       (Ast.ExAcc
                          (_loc, e1,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents")))))),
                       e2) : 'expr ))))]);
        ((Some "||"), (Some `RA),
          [([`Sself;
            `Snterm (Gram.obj (infixop6 : 'infixop6 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop6)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))))]);
        ((Some "&&"), (Some `RA),
          [([`Sself;
            `Snterm (Gram.obj (infixop5 : 'infixop5 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop5)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))))]);
        ((Some "<"), (Some `LA),
          [([`Sself;
            `Snterm (Gram.obj (infixop0 : 'infixop0 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop0)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))))]);
        ((Some "^"), (Some `RA),
          [([`Sself;
            `Snterm (Gram.obj (infixop1 : 'infixop1 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop1)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))))]);
        ((Some "+"), (Some `LA),
          [([`Sself;
            `Snterm (Gram.obj (infixop2 : 'infixop2 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop2)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))))]);
        ((Some "*"), (Some `LA),
          [([`Sself;
            `Snterm (Gram.obj (infixop3 : 'infixop3 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop3)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))));
          ([`Sself; `Skeyword "mod"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "mod")))), e1)),
                       e2) : 'expr ))));
          ([`Sself; `Skeyword "lxor"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "lxor")))),
                            e1)), e2) : 'expr ))));
          ([`Sself; `Skeyword "lor"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "lor")))), e1)),
                       e2) : 'expr ))));
          ([`Sself; `Skeyword "land"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "land")))),
                            e1)), e2) : 'expr ))))]);
        ((Some "**"), (Some `RA),
          [([`Sself;
            `Snterm (Gram.obj (infixop4 : 'infixop4 Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e2 : 'expr)  (op : 'infixop4)  (e1 : 'expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, (Ast.ExApp (_loc, op, e1)), e2) : 
                   'expr ))));
          ([`Sself; `Skeyword "lsr"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "lsr")))), e1)),
                       e2) : 'expr ))));
          ([`Sself; `Skeyword "lsl"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "lsl")))), e1)),
                       e2) : 'expr ))));
          ([`Sself; `Skeyword "asr"; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp
                     (_loc,
                       (Ast.ExApp
                          (_loc,
                            (Ast.ExId (_loc, (Ast.IdLid (_loc, "asr")))), e1)),
                       e2) : 'expr ))))]);
        ((Some "unary minus"), (Some `NA),
          [([`Skeyword "-."; `Sself],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (Expr.mkumin _loc "-." e : 'expr ))));
          ([`Skeyword "-"; `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Expr.mkumin _loc "-" e : 'expr ))))]);
        ((Some "apply"), (Some `LA),
          [([`Skeyword "lazy"; `Sself],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (Ast.ExLaz (_loc, e) : 'expr ))));
          ([`Skeyword "new";
           `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExNew (_loc, i) : 'expr ))));
          ([`Skeyword "assert"; `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Expr.mkassert _loc e : 'expr ))));
          ([`Sself; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExApp (_loc, e1, e2) : 'expr ))))]);
        ((Some "label"), (Some `NA),
          [([`Skeyword "?";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                   (Ast.ExOlb (_loc, i, (Ast.ExNil _loc)) : 'expr ))));
          ([`Skeyword "?";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExOlb (_loc, i, e) : 'expr ))));
          ([`Stoken
              (((function | `OPTLABEL _ -> true | _ -> false)),
                (`Normal, "`OPTLABEL _"));
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `OPTLABEL i -> (Ast.ExOlb (_loc, i, e) : 'expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `LABEL _ -> true | _ -> false)),
                (`Normal, "`LABEL _"));
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `LABEL i -> (Ast.ExLab (_loc, i, e) : 'expr )
                  | _ -> assert false)));
          ([`Skeyword "~"; `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExLab (_loc, i, (Ast.ExNil _loc)) : 'expr ))));
          ([`Skeyword "~";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExLab (_loc, i, e) : 'expr ))))]);
        ((Some "."), (Some `LA),
          [([`Sself;
            `Skeyword "#";
            `Snterm (Gram.obj (label : 'label Gram.t ))],
             (Gram.mk_action
                (fun (lab : 'label)  _  (e : 'expr)  (_loc : FanLoc.t)  ->
                   (Ast.ExSnd (_loc, e, lab) : 'expr ))));
          ([`Sself; `Skeyword "."; `Sself],
            (Gram.mk_action
               (fun (e2 : 'expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                  (Ast.ExAcc (_loc, e1, e2) : 'expr ))));
          ([`Sself;
           `Skeyword ".";
           `Skeyword "{";
           `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ));
           `Skeyword "}"],
            (Gram.mk_action
               (fun _  (e2 : 'comma_expr)  _  _  (e1 : 'expr) 
                  (_loc : FanLoc.t)  ->
                  (Expr.bigarray_get _loc e1 e2 : 'expr ))));
          ([`Sself; `Skeyword "."; `Skeyword "["; `Sself; `Skeyword "]"],
            (Gram.mk_action
               (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                  -> (Ast.ExSte (_loc, e1, e2) : 'expr ))));
          ([`Sself; `Skeyword "."; `Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (e2 : 'expr)  _  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                  -> (Ast.ExAre (_loc, e1, e2) : 'expr ))))]);
        ((Some "~-"), (Some `NA),
          [([`Snterm (Gram.obj (prefixop : 'prefixop Gram.t )); `Sself],
             (Gram.mk_action
                (fun (e : 'expr)  (f : 'prefixop)  (_loc : FanLoc.t)  ->
                   (Ast.ExApp (_loc, f, e) : 'expr ))));
          ([`Skeyword "!"; `Sself],
            (Gram.mk_action
               (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExAcc
                     (_loc, e,
                       (Ast.ExId (_loc, (Ast.IdLid (_loc, "contents"))))) : 
                  'expr ))))]);
        ((Some "simple"), None,
          [([`Skeyword "(";
            `Skeyword "module";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (pt : 'package_type)  _  (me : 'module_expr)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExPkg (_loc, (Ast.MeTyc (_loc, me, pt))) : 'expr ))));
          ([`Skeyword "(";
           `Skeyword "module";
           `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (me : 'module_expr)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExPkg (_loc, me) : 'expr ))));
          ([`Skeyword "begin"; `Skeyword "end"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) : 'expr ))));
          ([`Skeyword "begin";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword "end"],
            (Gram.mk_action
               (fun _  (seq : 'sequence)  _  (_loc : FanLoc.t)  ->
                  (Expr.mksequence _loc seq : 'expr ))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  -> (e : 'expr ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ":>";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExCoe (_loc, e, (Ast.TyNil _loc), t) : 'expr ))));
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
          ([`Skeyword "("; `Sself; `Skeyword ";"; `Skeyword ")"],
            (Gram.mk_action
               (fun _  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Expr.mksequence _loc e : 'expr ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ";";
           `Snterm (Gram.obj (sequence : 'sequence Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (seq : 'sequence)  _  (e : 'expr)  _ 
                  (_loc : FanLoc.t)  ->
                  (Expr.mksequence _loc (Ast.ExSem (_loc, e, seq)) : 
                  'expr ))));
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
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExTyc (_loc, e, t) : 'expr ))));
          ([`Skeyword "("; `Skeyword ")"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "()"))) : 'expr ))));
          ([`Skeyword "{<";
           `Snterm (Gram.obj (field_expr_list : 'field_expr_list Gram.t ));
           `Skeyword ">}"],
            (Gram.mk_action
               (fun _  (fel : 'field_expr_list)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExOvr (_loc, fel) : 'expr ))));
          ([`Skeyword "{<"; `Skeyword ">}"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExOvr (_loc, (Ast.RbNil _loc)) : 'expr ))));
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
          ([`Skeyword "{";
           `Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ));
           `Skeyword "}"],
            (Gram.mk_action
               (fun _  (el : 'label_expr_list)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExRec (_loc, el, (Ast.ExNil _loc)) : 'expr ))));
          ([`Skeyword "[|";
           `Snterm (Gram.obj (sem_expr : 'sem_expr Gram.t ));
           `Skeyword "|]"],
            (Gram.mk_action
               (fun _  (el : 'sem_expr)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExArr (_loc, el) : 'expr ))));
          ([`Skeyword "[|"; `Skeyword "|]"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExArr (_loc, (Ast.ExNil _loc)) : 'expr ))));
          ([`Skeyword "[";
           `Snterm
             (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (mk_list : 'sem_expr_for_list)  _  (_loc : FanLoc.t) 
                  ->
                  (mk_list (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))) : 
                  'expr ))));
          ([`Skeyword "[";
           `Snterm
             (Gram.obj (sem_expr_for_list : 'sem_expr_for_list Gram.t ));
           `Skeyword "::";
           `Sself;
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (last : 'expr)  _  (mk_list : 'sem_expr_for_list)  _ 
                  (_loc : FanLoc.t)  -> (mk_list last : 'expr ))));
          ([`Skeyword "["; `Skeyword "]"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.ExId (_loc, (Ast.IdUid (_loc, "[]"))) : 'expr ))));
          ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.ExVrn (_loc, s) : 'expr ))));
          ([`Stry
              (`Snterm (Gram.obj (val_longident : 'val_longident Gram.t )))],
            (Gram.mk_action
               (fun (i : 'val_longident)  (_loc : FanLoc.t)  ->
                  (Ast.ExId (_loc, i) : 'expr ))));
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
          ([`Snterm (Gram.obj (a_CHAR : 'a_CHAR Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_CHAR)  (_loc : FanLoc.t)  ->
                  (Ast.ExChr (_loc, s) : 'expr ))));
          ([`Snterm (Gram.obj (a_STRING : 'a_STRING Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_STRING)  (_loc : FanLoc.t)  ->
                  (Ast.ExStr (_loc, s) : 'expr ))));
          ([`Snterm (Gram.obj (a_FLOAT : 'a_FLOAT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_FLOAT)  (_loc : FanLoc.t)  ->
                  (Ast.ExFlo (_loc, s) : 'expr ))));
          ([`Snterm (Gram.obj (a_NATIVEINT : 'a_NATIVEINT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_NATIVEINT)  (_loc : FanLoc.t)  ->
                  (Ast.ExNativeInt (_loc, s) : 'expr ))));
          ([`Snterm (Gram.obj (a_INT64 : 'a_INT64 Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT64)  (_loc : FanLoc.t)  ->
                  (Ast.ExInt64 (_loc, s) : 'expr ))));
          ([`Snterm (Gram.obj (a_INT32 : 'a_INT32 Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT32)  (_loc : FanLoc.t)  ->
                  (Ast.ExInt32 (_loc, s) : 'expr ))));
          ([`Snterm (Gram.obj (a_INT : 'a_INT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT)  (_loc : FanLoc.t)  ->
                  (Ast.ExInt (_loc, s) : 'expr ))));
          ([`Stoken
              (((function | `ANTIQUOT ("seq",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"seq\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("seq" as n),s) ->
                      (Ast.ExSeq
                         (_loc, (Ast.ExAnt (_loc, (mk_anti ~c:"expr" n s)))) : 
                      'expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANTIQUOT ("tup",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"tup\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("tup" as n),s) ->
                      (Ast.ExTup
                         (_loc, (Ast.ExAnt (_loc, (mk_anti ~c:"expr" n s)))) : 
                      'expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANTIQUOT ("`bool",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"`bool\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("`bool" as n),s) ->
                      (Ast.ExId (_loc, (Ast.IdAnt (_loc, (mk_anti n s)))) : 
                      'expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANTIQUOT (("exp"|""|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"exp\"|\"\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("exp"|""|"anti" as n),s) ->
                      (Ast.ExAnt (_loc, (mk_anti ~c:"expr" n s)) : 'expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `QUOTATION x ->
                      (Quotation.expand _loc x DynAst.expr_tag : 'expr )
                  | _ -> assert false)))])]);
    Gram.extend (sequence : 'sequence Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
             `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
              (Gram.mk_action
                 (fun (k : 'sequence')  (e : 'expr)  (_loc : FanLoc.t)  ->
                    (k e : 'sequence ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.ExAnt (_loc, (mk_anti ~c:"expr;" n s)) : 
                       'sequence )
                   | _ -> assert false)));
           ([`Skeyword "let";
            `Skeyword "open";
            `Snterm (Gram.obj (module_longident : 'module_longident Gram.t ));
            `Skeyword "in";
            `Sself],
             (Gram.mk_action
                (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExOpI (_loc, i, e) : 'sequence ))));
           ([`Skeyword "let";
            `Skeyword "module";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (el : 'sequence)  _  (mb : 'module_binding0) 
                   (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.ExLmd (_loc, m, mb, (Expr.mksequence _loc el)) : 
                   'sequence ))));
           ([`Skeyword "let";
            `Skeyword "module";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Snterm (Gram.obj (module_binding0 : 'module_binding0 Gram.t ));
            `Skeyword "in";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'expr)  _ 
                   (mb : 'module_binding0)  (m : 'a_UIDENT)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (k (Ast.ExLmd (_loc, m, mb, e)) : 'sequence ))));
           ([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (el : 'sequence)  _  (bi : 'binding)  (rf : 'opt_rec)  _
                    (_loc : FanLoc.t)  ->
                   (Ast.ExLet (_loc, rf, bi, (Expr.mksequence _loc el)) : 
                   'sequence ))));
           ([`Skeyword "let";
            `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
            `Snterm (Gram.obj (binding : 'binding Gram.t ));
            `Skeyword "in";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Snterm (Gram.obj (sequence' : 'sequence' Gram.t ))],
             (Gram.mk_action
                (fun (k : 'sequence')  (e : 'expr)  _  (bi : 'binding) 
                   (rf : 'opt_rec)  _  (_loc : FanLoc.t)  ->
                   (k (Ast.ExLet (_loc, rf, bi, e)) : 'sequence ))))])]);
    Gram.extend (infixop5 : 'infixop5 Gram.t )
      (None,
        [(None, None,
           [([Gram.srules infixop5
                [([`Skeyword "&&"],
                   (Gram.mk_action
                      (fun x  (_loc : FanLoc.t)  ->
                         (Gram.string_of_token x : 'e__17 ))));
                ([`Skeyword "&"],
                  (Gram.mk_action
                     (fun x  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__17 ))))]],
              (Gram.mk_action
                 (fun (x : 'e__17)  (_loc : FanLoc.t)  ->
                    (Ast.ExId (_loc, (Ast.IdLid (_loc, x))) : 'infixop5 ))))])]);
    Gram.extend (infixop6 : 'infixop6 Gram.t )
      (None,
        [(None, None,
           [([Gram.srules infixop6
                [([`Skeyword "||"],
                   (Gram.mk_action
                      (fun x  (_loc : FanLoc.t)  ->
                         (Gram.string_of_token x : 'e__18 ))));
                ([`Skeyword "or"],
                  (Gram.mk_action
                     (fun x  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__18 ))))]],
              (Gram.mk_action
                 (fun (x : 'e__18)  (_loc : FanLoc.t)  ->
                    (Ast.ExId (_loc, (Ast.IdLid (_loc, x))) : 'infixop6 ))))])]);
    Gram.extend (sem_expr_for_list : 'sem_expr_for_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  ->
                    (fun acc  ->
                       Ast.ExApp
                         (_loc,
                           (Ast.ExApp
                              (_loc,
                                (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                                e)), acc) : 'sem_expr_for_list ))));
           ([`Snterm (Gram.obj (expr : 'expr Gram.t )); `Skeyword ";"],
             (Gram.mk_action
                (fun _  (e : 'expr)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      Ast.ExApp
                        (_loc,
                          (Ast.ExApp
                             (_loc,
                               (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                               e)), acc) : 'sem_expr_for_list ))));
           ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
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
                               e)), (el acc)) : 'sem_expr_for_list ))))])]);
    Gram.extend (comma_expr : 'comma_expr Gram.t )
      (None,
        [(None, None,
           [([`Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
              (Gram.mk_action
                 (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'comma_expr ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.ExAnt (_loc, (mk_anti ~c:"expr," n s)) : 
                       'comma_expr )
                   | _ -> assert false)));
           ([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (e2 : 'comma_expr)  _  (e1 : 'comma_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExCom (_loc, e1, e2) : 'comma_expr ))))])]);
    Gram.extend (dummy : 'dummy Gram.t )
      (None,
        [(None, None,
           [([], (Gram.mk_action (fun (_loc : FanLoc.t)  -> (() : 'dummy ))))])]);
    Gram.extend (sequence' : 'sequence' Gram.t )
      (None,
        [(None, None,
           [([`Skeyword ";";
             `Snterm (Gram.obj (sequence : 'sequence Gram.t ))],
              (Gram.mk_action
                 (fun (el : 'sequence)  _  (_loc : FanLoc.t)  ->
                    (fun e  -> Ast.ExSem (_loc, e, el) : 'sequence' ))));
           ([`Skeyword ";"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' ))));
           ([],
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (fun e  -> e : 'sequence' ))))])]);
    Gram.extend (fun_def : 'fun_def Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
             `Snterm (Gram.obj (fun_def_cont : 'fun_def_cont Gram.t ))],
              (Gram.mk_action
                 (fun ((w,e) : 'fun_def_cont)  (p : 'labeled_ipatt) 
                    (_loc : FanLoc.t)  ->
                    (Ast.ExFun (_loc, (Ast.McArr (_loc, p, w, e))) : 
                    'fun_def ))));
           ([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ")";
            `Snterm
              (Gram.obj
                 (fun_def_cont_no_when : 'fun_def_cont_no_when Gram.t ))],
             (Gram.mk_action
                (fun (e : 'fun_def_cont_no_when)  _  (i : 'a_LIDENT)  _  _ 
                   (_loc : FanLoc.t)  -> (Ast.ExFUN (_loc, i, e) : 'fun_def ))))])]);
    Gram.extend (fun_def_cont : 'fun_def_cont Gram.t )
      (None,
        [(None, (Some `RA),
           [([`Skeyword "->"; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (((Ast.ExNil _loc), e) : 'fun_def_cont ))));
           ([`Skeyword "when";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword "->";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (w : 'expr)  _  (_loc : FanLoc.t)  ->
                   ((w, e) : 'fun_def_cont ))));
           ([`Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun ((w,e) : 'fun_def_cont)  (p : 'labeled_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (((Ast.ExNil _loc),
                      (Ast.ExFun (_loc, (Ast.McArr (_loc, p, w, e))))) : 
                   'fun_def_cont ))));
           ([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ")";
            `Snterm
              (Gram.obj
                 (fun_def_cont_no_when : 'fun_def_cont_no_when Gram.t ))],
             (Gram.mk_action
                (fun (e : 'fun_def_cont_no_when)  _  (i : 'a_LIDENT)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (((Ast.ExNil _loc), (Ast.ExFUN (_loc, i, e))) : 'fun_def_cont ))))])]);
    Gram.extend (fun_def_cont_no_when : 'fun_def_cont_no_when Gram.t )
      (None,
        [(None, (Some `RA),
           [([`Skeyword "->"; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                    (e : 'fun_def_cont_no_when ))));
           ([`Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
            `Snterm (Gram.obj (fun_def_cont : 'fun_def_cont Gram.t ))],
             (Gram.mk_action
                (fun ((w,e) : 'fun_def_cont)  (p : 'labeled_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExFun (_loc, (Ast.McArr (_loc, p, w, e))) : 'fun_def_cont_no_when ))));
           ([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ")";
            `Sself],
             (Gram.mk_action
                (fun (e : 'fun_def_cont_no_when)  _  (i : 'a_LIDENT)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExFUN (_loc, i, e) : 'fun_def_cont_no_when ))))])])
  let _ =
    Gram.extend (binding : 'binding Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (let_binding : 'let_binding Gram.t ))],
              (Gram.mk_action
                 (fun (b : 'let_binding)  (_loc : FanLoc.t)  ->
                    (b : 'binding ))));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (b2 : 'binding)  _  (b1 : 'binding)  (_loc : FanLoc.t) 
                   -> (Ast.BiAnd (_loc, b1, b2) : 'binding ))));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.BiAnt (_loc, (mk_anti ~c:"binding" n s)) : 
                       'binding )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"));
            `Skeyword "=";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.BiEq
                          (_loc, (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s))),
                            e) : 'binding )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("binding"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"binding\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("binding"|"list" as n),s) ->
                       (Ast.BiAnt (_loc, (mk_anti ~c:"binding" n s)) : 
                       'binding )
                   | _ -> assert false)))])]);
    Gram.extend (let_binding : 'let_binding Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
             `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'fun_binding)  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                    (Ast.BiEq (_loc, p, e) : 'let_binding ))))])]);
    Gram.extend (fun_binding : 'fun_binding Gram.t )
      (None,
        [(None, (Some `RA),
           [([`Snterm (Gram.obj (cvalue_binding : 'cvalue_binding Gram.t ))],
              (Gram.mk_action
                 (fun (bi : 'cvalue_binding)  (_loc : FanLoc.t)  ->
                    (bi : 'fun_binding ))));
           ([`Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (e : 'fun_binding)  (p : 'labeled_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExFun
                      (_loc, (Ast.McArr (_loc, p, (Ast.ExNil _loc), e))) : 
                   'fun_binding ))));
           ([`Skeyword "(";
            `Skeyword "type";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ")";
            `Sself],
             (Gram.mk_action
                (fun (e : 'fun_binding)  _  (i : 'a_LIDENT)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.ExFUN (_loc, i, e) : 'fun_binding ))))])])
  let _ =
    Gram.extend (match_case : 'match_case Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
             `Skeyword "->";
             `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                    (Ast.McArr (_loc, p, (Ast.ExNil _loc), e) : 'match_case ))));
           ([`Skeyword "[";
            `Slist0sep
              ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
                (`Skeyword "|"));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (l : 'match_case0 list)  _  (_loc : FanLoc.t)  ->
                   (Ast.mcOr_of_list l : 'match_case ))))])]);
    Gram.extend (match_case0 : 'match_case0 Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj (patt_as_patt_opt : 'patt_as_patt_opt Gram.t ));
             `Snterm (Gram.obj (opt_when_expr : 'opt_when_expr Gram.t ));
             `Skeyword "->";
             `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (w : 'opt_when_expr) 
                    (p : 'patt_as_patt_opt)  (_loc : FanLoc.t)  ->
                    (Ast.McArr (_loc, p, w, e) : 'match_case0 ))));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"));
            `Skeyword "when";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword "->";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (w : 'expr)  _  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.McArr
                          (_loc, (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s))),
                            w, e) : 'match_case0 )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"));
            `Skeyword "->";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.McArr
                          (_loc, (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s))),
                            (Ast.ExNil _loc), e) : 'match_case0 )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.McAnt (_loc, (mk_anti ~c:"match_case" n s)) : 
                       'match_case0 )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("match_case"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"match_case\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("match_case"|"list" as n),s) ->
                       (Ast.McAnt (_loc, (mk_anti ~c:"match_case" n s)) : 
                       'match_case0 )
                   | _ -> assert false)))])])
  let _ =
    Gram.extend (opt_when_expr : 'opt_when_expr Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.ExNil _loc : 'opt_when_expr ))));
           ([`Skeyword "when"; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (w : 'expr)  _  (_loc : FanLoc.t)  ->
                   (w : 'opt_when_expr ))))])]);
    Gram.extend (patt_as_patt_opt : 'patt_as_patt_opt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'patt)  (_loc : FanLoc.t)  ->
                    (p : 'patt_as_patt_opt ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword "as";
            `Snterm (Gram.obj (patt : 'patt Gram.t ))],
             (Gram.mk_action
                (fun (p2 : 'patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaAli (_loc, p1, p2) : 'patt_as_patt_opt ))))])]);
    Gram.extend (label_expr_list : 'label_expr_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ))],
              (Gram.mk_action
                 (fun (b1 : 'label_expr)  (_loc : FanLoc.t)  ->
                    (b1 : 'label_expr_list ))));
           ([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (b1 : 'label_expr)  (_loc : FanLoc.t)  ->
                   (b1 : 'label_expr_list ))));
           ([`Snterm (Gram.obj (label_expr : 'label_expr Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (b2 : 'label_expr_list)  _  (b1 : 'label_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.RbSem (_loc, b1, b2) : 'label_expr_list ))))])]);
    Gram.extend (label_expr : 'label_expr Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                    (Ast.RbEq
                       (_loc, i,
                         (Ast.ExId
                            (_loc, (Ast.IdLid (_loc, (Ident.to_lid i)))))) : 
                    'label_expr ))));
           ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
            `Snterm (Gram.obj (fun_binding : 'fun_binding Gram.t ))],
             (Gram.mk_action
                (fun (e : 'fun_binding)  (i : 'label_longident) 
                   (_loc : FanLoc.t)  ->
                   (Ast.RbEq (_loc, i, e) : 'label_expr ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'label_expr )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"));
            `Skeyword "=";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.RbEq
                          (_loc,
                            (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))), e) : 
                       'label_expr )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"anti" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'label_expr )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("rec_binding",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"rec_binding\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("rec_binding" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'label_expr )
                   | _ -> assert false)))])])
  let _ =
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
          [([`Skeyword "lazy"; `Sself],
             (Gram.mk_action
                (fun (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (Ast.PaLaz (_loc, p) : 'patt ))));
          ([`Sself; `Sself],
            (Gram.mk_action
               (fun (p2 : 'patt)  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                  (Ast.PaApp (_loc, p1, p2) : 'patt ))))]);
        ((Some "simple"), None,
          [([`Skeyword "?";
            `Skeyword "(";
            `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (e : 'expr)  _  (p : 'patt_tcon)  _  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaOlbi (_loc, "", p, e) : 'patt ))));
          ([`Skeyword "?";
           `Skeyword "(";
           `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (p : 'patt_tcon)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaOlb (_loc, "", p) : 'patt ))));
          ([`Skeyword "?";
           `Stoken
             (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"lid" as n),i) ->
                      (Ast.PaOlb (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                      'patt )
                  | _ -> assert false)));
          ([`Skeyword "?";
           `Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `LID i ->
                      (Ast.PaOlb (_loc, i, (Ast.PaNil _loc)) : 'patt )
                  | _ -> assert false)));
          ([`Skeyword "?";
           `Stoken
             (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"));
           `Skeyword ":";
           `Skeyword "(";
           `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
           `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (f : 'eq_expr)  (p : 'patt_tcon)  _  _  __camlp4_0  _ 
                  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"lid" as n),i) ->
                      (f (mk_anti n i) p : 'patt )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `OPTLABEL _ -> true | _ -> false)),
                (`Normal, "`OPTLABEL _"));
           `Skeyword "(";
           `Snterm (Gram.obj (patt_tcon : 'patt_tcon Gram.t ));
           `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (f : 'eq_expr)  (p : 'patt_tcon)  _  __camlp4_0 
                  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `OPTLABEL i -> (f i p : 'patt )
                  | _ -> assert false)));
          ([`Skeyword "~";
           `Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"))],
            (Gram.mk_action
               (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `LID i ->
                      (Ast.PaLab (_loc, i, (Ast.PaNil _loc)) : 'patt )
                  | _ -> assert false)));
          ([`Skeyword "~";
           `Stoken
             (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"lid" as n),i) ->
                      (Ast.PaLab (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                      'patt )
                  | _ -> assert false)));
          ([`Skeyword "~";
           `Stoken
             (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
               (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (p : 'patt)  _  __camlp4_0  _  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"lid" as n),i) ->
                      (Ast.PaLab (_loc, (mk_anti n i), p) : 'patt )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `LABEL _ -> true | _ -> false)),
                (`Normal, "`LABEL _"));
           `Sself],
            (Gram.mk_action
               (fun (p : 'patt)  __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `LABEL i -> (Ast.PaLab (_loc, i, p) : 'patt )
                  | _ -> assert false)));
          ([`Skeyword "#";
           `Snterm (Gram.obj (type_longident : 'type_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaTyp (_loc, i) : 'patt ))));
          ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaVrn (_loc, s) : 'patt ))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `QUOTATION x ->
                      (Quotation.expand _loc x DynAst.patt_tag : 'patt )
                  | _ -> assert false)));
          ([`Skeyword "_"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'patt ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ",";
           `Snterm (Gram.obj (comma_patt : 'comma_patt Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (pl : 'comma_patt)  _  (p : 'patt)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.PaTup (_loc, (Ast.PaCom (_loc, p, pl))) : 'patt ))));
          ([`Skeyword "("; `Sself; `Skeyword "as"; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (p2 : 'patt)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaAli (_loc, p, p2) : 'patt ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ":";
           `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaTyc (_loc, p, t) : 'patt ))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  -> (p : 'patt ))));
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
          ([`Skeyword "(";
           `Skeyword "module";
           `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaMod (_loc, m) : 'patt ))));
          ([`Skeyword "("; `Skeyword ")"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "()"))) : 'patt ))));
          ([`Skeyword "{";
           `Snterm (Gram.obj (label_patt_list : 'label_patt_list Gram.t ));
           `Skeyword "}"],
            (Gram.mk_action
               (fun _  (pl : 'label_patt_list)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaRec (_loc, pl) : 'patt ))));
          ([`Skeyword "[|";
           `Snterm (Gram.obj (sem_patt : 'sem_patt Gram.t ));
           `Skeyword "|]"],
            (Gram.mk_action
               (fun _  (pl : 'sem_patt)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaArr (_loc, pl) : 'patt ))));
          ([`Skeyword "[|"; `Skeyword "|]"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaArr (_loc, (Ast.PaNil _loc)) : 'patt ))));
          ([`Skeyword "[";
           `Snterm
             (Gram.obj (sem_patt_for_list : 'sem_patt_for_list Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (mk_list : 'sem_patt_for_list)  _  (_loc : FanLoc.t) 
                  ->
                  (mk_list (Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))) : 
                  'patt ))));
          ([`Skeyword "[";
           `Snterm
             (Gram.obj (sem_patt_for_list : 'sem_patt_for_list Gram.t ));
           `Skeyword "::";
           `Sself;
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (last : 'patt)  _  (mk_list : 'sem_patt_for_list)  _ 
                  (_loc : FanLoc.t)  -> (mk_list last : 'patt ))));
          ([`Skeyword "["; `Skeyword "]"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, (Ast.IdUid (_loc, "[]"))) : 'patt ))));
          ([`Skeyword "-"; `Snterm (Gram.obj (a_FLOAT : 'a_FLOAT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_FLOAT)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaFlo (_loc, (neg_string s)) : 'patt ))));
          ([`Skeyword "-";
           `Snterm (Gram.obj (a_NATIVEINT : 'a_NATIVEINT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_NATIVEINT)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaNativeInt (_loc, (neg_string s)) : 'patt ))));
          ([`Skeyword "-"; `Snterm (Gram.obj (a_INT64 : 'a_INT64 Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT64)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaInt64 (_loc, (neg_string s)) : 'patt ))));
          ([`Skeyword "-"; `Snterm (Gram.obj (a_INT32 : 'a_INT32 Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT32)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaInt32 (_loc, (neg_string s)) : 'patt ))));
          ([`Skeyword "-"; `Snterm (Gram.obj (a_INT : 'a_INT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT)  _  (_loc : FanLoc.t)  ->
                  (Ast.PaInt (_loc, (neg_string s)) : 'patt ))));
          ([`Snterm (Gram.obj (a_CHAR : 'a_CHAR Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_CHAR)  (_loc : FanLoc.t)  ->
                  (Ast.PaChr (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (a_STRING : 'a_STRING Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_STRING)  (_loc : FanLoc.t)  ->
                  (Ast.PaStr (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (a_FLOAT : 'a_FLOAT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_FLOAT)  (_loc : FanLoc.t)  ->
                  (Ast.PaFlo (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (a_NATIVEINT : 'a_NATIVEINT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_NATIVEINT)  (_loc : FanLoc.t)  ->
                  (Ast.PaNativeInt (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (a_INT64 : 'a_INT64 Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT64)  (_loc : FanLoc.t)  ->
                  (Ast.PaInt64 (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (a_INT32 : 'a_INT32 Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT32)  (_loc : FanLoc.t)  ->
                  (Ast.PaInt32 (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (a_INT : 'a_INT Gram.t ))],
            (Gram.mk_action
               (fun (s : 'a_INT)  (_loc : FanLoc.t)  ->
                  (Ast.PaInt (_loc, s) : 'patt ))));
          ([`Snterm (Gram.obj (ident : 'ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'ident)  (_loc : FanLoc.t)  ->
                  (Ast.PaId (_loc, i) : 'patt ))));
          ([`Stoken
              (((function | `ANTIQUOT ("`bool",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"`bool\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("`bool" as n),s) ->
                      (Ast.PaId (_loc, (Ast.IdAnt (_loc, (mk_anti n s)))) : 
                      'patt )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANTIQUOT ("tup",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"tup\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("tup" as n),s) ->
                      (Ast.PaTup
                         (_loc, (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)))) : 
                      'patt )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"pat"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"pat\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
                      (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'patt )
                  | _ -> assert false)))])]);
    Gram.extend (comma_patt : 'comma_patt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'comma_patt ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt," n s)) : 
                       'comma_patt )
                   | _ -> assert false)));
           ([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (p2 : 'comma_patt)  _  (p1 : 'comma_patt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaCom (_loc, p1, p2) : 'comma_patt ))))])]);
    Gram.extend (sem_patt : 'sem_patt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"],
             (Gram.mk_action
                (fun _  (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'sem_patt ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt;" n s)) : 
                       'sem_patt )
                   | _ -> assert false)));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (p2 : 'sem_patt)  _  (p1 : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, p2) : 'sem_patt ))))])]);
    Gram.extend (sem_patt_for_list : 'sem_patt_for_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'patt)  (_loc : FanLoc.t)  ->
                    (fun acc  ->
                       Ast.PaApp
                         (_loc,
                           (Ast.PaApp
                              (_loc,
                                (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))),
                                p)), acc) : 'sem_patt_for_list ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t )); `Skeyword ";"],
             (Gram.mk_action
                (fun _  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (fun acc  ->
                      Ast.PaApp
                        (_loc,
                          (Ast.PaApp
                             (_loc,
                               (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))),
                               p)), acc) : 'sem_patt_for_list ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
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
                               p)), (pl acc)) : 'sem_patt_for_list ))))])]);
    Gram.extend (label_patt_list : 'label_patt_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ))],
              (Gram.mk_action
                 (fun (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                    (p1 : 'label_patt_list ))));
           ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (p1 : 'label_patt_list ))));
           ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
            `Skeyword ";";
            `Skeyword "_";
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) : 'label_patt_list ))));
           ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
            `Skeyword ";";
            `Skeyword "_"],
             (Gram.mk_action
                (fun _  _  (p1 : 'label_patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) : 'label_patt_list ))));
           ([`Snterm (Gram.obj (label_patt : 'label_patt Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (p2 : 'label_patt_list)  _  (p1 : 'label_patt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, p2) : 'label_patt_list ))))])]);
    Gram.extend (label_patt : 'label_patt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'label_longident)  (_loc : FanLoc.t)  ->
                    (Ast.PaEq
                       (_loc, i,
                         (Ast.PaId
                            (_loc, (Ast.IdLid (_loc, (Ident.to_lid i)))))) : 
                    'label_patt ))));
           ([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (patt : 'patt Gram.t ))],
             (Gram.mk_action
                (fun (p : 'patt)  _  (i : 'label_longident) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaEq (_loc, i, p) : 'label_patt ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt;" n s)) : 
                       'label_patt )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.patt_tag : 'label_patt )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"pat"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"pat\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'label_patt )
                   | _ -> assert false)))])]);
    Gram.extend (ipatt : 'ipatt Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "?";
             `Skeyword "(";
             `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
             `Skeyword "=";
             `Snterm (Gram.obj (expr : 'expr Gram.t ));
             `Skeyword ")"],
              (Gram.mk_action
                 (fun _  (e : 'expr)  _  (p : 'ipatt_tcon)  _  _ 
                    (_loc : FanLoc.t)  ->
                    (Ast.PaOlbi (_loc, "", p, e) : 'ipatt ))));
           ([`Skeyword "?";
            `Skeyword "(";
            `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p : 'ipatt_tcon)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.PaOlb (_loc, "", p) : 'ipatt ))));
           ([`Skeyword "?";
            `Stoken
              (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"lid" as n),i) ->
                       (Ast.PaOlb (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                       'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "?";
            `Stoken
              (((function | `LID _ -> true | _ -> false)),
                (`Normal, "`LID _"))],
             (Gram.mk_action
                (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `LID i ->
                       (Ast.PaOlb (_loc, i, (Ast.PaNil _loc)) : 'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "?";
            `Stoken
              (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"));
            `Skeyword ":";
            `Skeyword "(";
            `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
            `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (f : 'eq_expr)  (p : 'ipatt_tcon)  _  _  __camlp4_0 
                   _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"lid" as n),i) ->
                       (f (mk_anti n i) p : 'ipatt )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `OPTLABEL _ -> true | _ -> false)),
                 (`Normal, "`OPTLABEL _"));
            `Skeyword "(";
            `Snterm (Gram.obj (ipatt_tcon : 'ipatt_tcon Gram.t ));
            `Snterm (Gram.obj (eq_expr : 'eq_expr Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (f : 'eq_expr)  (p : 'ipatt_tcon)  _  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `OPTLABEL i -> (f i p : 'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "~";
            `Stoken
              (((function | `LID _ -> true | _ -> false)),
                (`Normal, "`LID _"))],
             (Gram.mk_action
                (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `LID i ->
                       (Ast.PaLab (_loc, i, (Ast.PaNil _loc)) : 'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "~";
            `Stoken
              (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"lid" as n),i) ->
                       (Ast.PaLab (_loc, (mk_anti n i), (Ast.PaNil _loc)) : 
                       'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "~";
            `Stoken
              (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"));
            `Skeyword ":";
            `Sself],
             (Gram.mk_action
                (fun (p : 'ipatt)  _  __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"lid" as n),i) ->
                       (Ast.PaLab (_loc, (mk_anti n i), p) : 'ipatt )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `LABEL _ -> true | _ -> false)),
                 (`Normal, "`LABEL _"));
            `Sself],
             (Gram.mk_action
                (fun (p : 'ipatt)  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `LABEL i -> (Ast.PaLab (_loc, i, p) : 'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "_"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.PaAny _loc : 'ipatt ))));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (s : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                   (Ast.PaId (_loc, (Ast.IdLid (_loc, s))) : 'ipatt ))));
           ([`Skeyword "(";
            `Sself;
            `Skeyword ",";
            `Snterm (Gram.obj (comma_ipatt : 'comma_ipatt Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (pl : 'comma_ipatt)  _  (p : 'ipatt)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaTup (_loc, (Ast.PaCom (_loc, p, pl))) : 'ipatt ))));
           ([`Skeyword "("; `Sself; `Skeyword "as"; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p2 : 'ipatt)  _  (p : 'ipatt)  _  (_loc : FanLoc.t) 
                   -> (Ast.PaAli (_loc, p, p2) : 'ipatt ))));
           ([`Skeyword "(";
            `Sself;
            `Skeyword ":";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'ipatt)  _  (_loc : FanLoc.t) 
                   -> (Ast.PaTyc (_loc, p, t) : 'ipatt ))));
           ([`Skeyword "("; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p : 'ipatt)  _  (_loc : FanLoc.t)  -> (p : 'ipatt ))));
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
           ([`Skeyword "(";
            `Skeyword "module";
            `Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (m : 'a_UIDENT)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.PaMod (_loc, m) : 'ipatt ))));
           ([`Skeyword "("; `Skeyword ")"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.PaId (_loc, (Ast.IdUid (_loc, "()"))) : 'ipatt ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.patt_tag : 'ipatt )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("tup",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"tup\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("tup" as n),s) ->
                       (Ast.PaTup
                          (_loc, (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)))) : 
                       'ipatt )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"pat"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"pat\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'ipatt )
                   | _ -> assert false)));
           ([`Skeyword "{";
            `Snterm (Gram.obj (label_ipatt_list : 'label_ipatt_list Gram.t ));
            `Skeyword "}"],
             (Gram.mk_action
                (fun _  (pl : 'label_ipatt_list)  _  (_loc : FanLoc.t)  ->
                   (Ast.PaRec (_loc, pl) : 'ipatt ))))])]);
    Gram.extend (labeled_ipatt : 'labeled_ipatt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'ipatt)  (_loc : FanLoc.t)  ->
                    (p : 'labeled_ipatt ))))])]);
    Gram.extend (comma_ipatt : 'comma_ipatt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'ipatt)  (_loc : FanLoc.t)  -> (p : 'comma_ipatt ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt," n s)) : 
                       'comma_ipatt )
                   | _ -> assert false)));
           ([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (p2 : 'comma_ipatt)  _  (p1 : 'comma_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaCom (_loc, p1, p2) : 'comma_ipatt ))))])]);
    Gram.extend (label_ipatt_list : 'label_ipatt_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label_ipatt : 'label_ipatt Gram.t ))],
              (Gram.mk_action
                 (fun (p1 : 'label_ipatt)  (_loc : FanLoc.t)  ->
                    (p1 : 'label_ipatt_list ))));
           ([`Snterm (Gram.obj (label_ipatt : 'label_ipatt Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (p1 : 'label_ipatt)  (_loc : FanLoc.t)  ->
                   (p1 : 'label_ipatt_list ))));
           ([`Snterm (Gram.obj (label_ipatt : 'label_ipatt Gram.t ));
            `Skeyword ";";
            `Skeyword "_";
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  _  _  (p1 : 'label_ipatt)  (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) : 'label_ipatt_list ))));
           ([`Snterm (Gram.obj (label_ipatt : 'label_ipatt Gram.t ));
            `Skeyword ";";
            `Skeyword "_"],
             (Gram.mk_action
                (fun _  _  (p1 : 'label_ipatt)  (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, (Ast.PaAny _loc)) : 'label_ipatt_list ))));
           ([`Snterm (Gram.obj (label_ipatt : 'label_ipatt Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (p2 : 'label_ipatt_list)  _  (p1 : 'label_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, p1, p2) : 'label_ipatt_list ))))])]);
    Gram.extend (label_ipatt : 'label_ipatt Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label_longident : 'label_longident Gram.t ));
             `Skeyword "=";
             `Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'ipatt)  _  (i : 'label_longident) 
                    (_loc : FanLoc.t)  ->
                    (Ast.PaEq (_loc, i, p) : 'label_ipatt ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.patt_tag : 'label_ipatt )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt;" n s)) : 
                       'label_ipatt )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"pat"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"pat\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"pat"|"anti" as n),s) ->
                       (Ast.PaAnt (_loc, (mk_anti ~c:"patt" n s)) : 'label_ipatt )
                   | _ -> assert false)))])])
  let _ =
    Gram.extend (type_declaration : 'type_declaration Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj
                   (type_ident_and_parameters : 'type_ident_and_parameters
                                                  Gram.t ));
             `Snterm (Gram.obj (opt_eq_ctyp : 'opt_eq_ctyp Gram.t ));
             `Slist0 (`Snterm (Gram.obj (constrain : 'constrain Gram.t )))],
              (Gram.mk_action
                 (fun (cl : 'constrain list)  (tk : 'opt_eq_ctyp) 
                    ((n,tpl) : 'type_ident_and_parameters)  (_loc : FanLoc.t)
                     ->
                    (Ast.TyDcl (_loc, n, tpl, tk, cl) : 'type_declaration ))));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'type_declaration)  _  (t1 : 'type_declaration) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyAnd (_loc, t1, t2) : 'type_declaration ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'type_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctypand" n s)) : 
                       'type_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"typ"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'type_declaration )
                   | _ -> assert false)))])]);
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
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'opt_eq_ctyp ))));
           ([`Skeyword "=";
            `Snterm (Gram.obj (type_kind : 'type_kind Gram.t ))],
             (Gram.mk_action
                (fun (tk : 'type_kind)  _  (_loc : FanLoc.t)  ->
                   (tk : 'opt_eq_ctyp ))))])]);
    Gram.extend (type_kind : 'type_kind Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'type_kind ))))])]);
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
    Gram.extend
      (type_longident_and_parameters : 'type_longident_and_parameters Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (type_longident : 'type_longident Gram.t ));
             `Snterm (Gram.obj (type_parameters : 'type_parameters Gram.t ))],
              (Gram.mk_action
                 (fun (tpl : 'type_parameters)  (i : 'type_longident) 
                    (_loc : FanLoc.t)  ->
                    (tpl (Ast.TyId (_loc, i)) : 'type_longident_and_parameters ))))])]);
    Gram.extend (type_parameters : 'type_parameters Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (fun t  -> t : 'type_parameters ))));
           ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
             (Gram.mk_action
                (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                   (fun acc  -> Ast.TyApp (_loc, acc, t) : 'type_parameters ))));
           ([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                   (_loc : FanLoc.t)  ->
                   (fun acc  -> t2 (Ast.TyApp (_loc, acc, t1)) : 'type_parameters ))))])]);
    Gram.extend (type_parameter : 'type_parameter Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "-";
             `Skeyword "'";
             `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                    (Ast.TyQuM (_loc, i) : 'type_parameter ))));
           ([`Skeyword "+";
            `Skeyword "'";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.TyQuP (_loc, i) : 'type_parameter ))));
           ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                   (Ast.TyQuo (_loc, i) : 'type_parameter ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'type_parameter )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"typ"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti n s)) : 'type_parameter )
                   | _ -> assert false)))])]);
    Gram.extend (optional_type_parameter : 'optional_type_parameter Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "_"],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (Ast.TyAny _loc : 'optional_type_parameter ))));
           ([`Skeyword "-"; `Skeyword "_"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.TyAnM _loc : 'optional_type_parameter ))));
           ([`Skeyword "+"; `Skeyword "_"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.TyAnP _loc : 'optional_type_parameter ))));
           ([`Skeyword "-";
            `Skeyword "'";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.TyQuM (_loc, i) : 'optional_type_parameter ))));
           ([`Skeyword "+";
            `Skeyword "'";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.TyQuP (_loc, i) : 'optional_type_parameter ))));
           ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                   (Ast.TyQuo (_loc, i) : 'optional_type_parameter ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'optional_type_parameter )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"typ"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti n s)) : 'optional_type_parameter )
                   | _ -> assert false)))])]);
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
          [([`Snterm (Gram.obj (a_OPTLABEL : 'a_OPTLABEL Gram.t )); `Sself],
             (Gram.mk_action
                (fun (t : 'ctyp)  (i : 'a_OPTLABEL)  (_loc : FanLoc.t)  ->
                   (Ast.TyOlb (_loc, i, t) : 'ctyp ))));
          ([`Skeyword "?";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyOlb (_loc, i, t) : 'ctyp ))));
          ([`Snterm (Gram.obj (a_LABEL : 'a_LABEL Gram.t )); `Sself],
            (Gram.mk_action
               (fun (t : 'ctyp)  (i : 'a_LABEL)  (_loc : FanLoc.t)  ->
                  (Ast.TyLab (_loc, i, t) : 'ctyp ))));
          ([`Skeyword "~";
           `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
           `Skeyword ":";
           `Sself],
            (Gram.mk_action
               (fun (t : 'ctyp)  _  (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyLab (_loc, i, t) : 'ctyp ))))]);
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
                    with | Invalid_argument s -> raise (Stream.Error s) : 
                   'ctyp ))))]);
        ((Some "simple"), None,
          [([`Skeyword "(";
            `Skeyword "module";
            `Snterm (Gram.obj (package_type : 'package_type Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p : 'package_type)  _  _  (_loc : FanLoc.t)  ->
                   (Ast.TyPkg (_loc, p) : 'ctyp ))));
          ([`Skeyword "<";
           `Snterm (Gram.obj (opt_meth_list : 'opt_meth_list Gram.t ));
           `Skeyword ">"],
            (Gram.mk_action
               (fun _  (t : 'opt_meth_list)  _  (_loc : FanLoc.t)  ->
                  (t : 'ctyp ))));
          ([`Skeyword "#";
           `Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'class_longident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyCls (_loc, i) : 'ctyp ))));
          ([`Skeyword "{";
           `Snterm
             (Gram.obj
                (label_declaration_list : 'label_declaration_list Gram.t ));
           `Skeyword "}"],
            (Gram.mk_action
               (fun _  (t : 'label_declaration_list)  _  (_loc : FanLoc.t) 
                  -> (Ast.TyRec (_loc, t) : 'ctyp ))));
          ([`Skeyword "[<";
           `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
           `Skeyword ">";
           `Snterm (Gram.obj (name_tags : 'name_tags Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyVrnInfSup (_loc, rfl, ntl) : 'ctyp ))));
          ([`Skeyword "[<";
           `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  (_loc : FanLoc.t)  ->
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
          ([`Skeyword "[";
           `Skeyword "<";
           `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrnInf (_loc, rfl) : 'ctyp ))));
          ([`Skeyword "[";
           `Skeyword ">";
           `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrnSup (_loc, rfl) : 'ctyp ))));
          ([`Skeyword "["; `Skeyword ">"; `Skeyword "]"],
            (Gram.mk_action
               (fun _  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrnSup (_loc, (Ast.TyNil _loc)) : 'ctyp ))));
          ([`Skeyword "[";
           `Skeyword "=";
           `Snterm (Gram.obj (row_field : 'row_field Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (rfl : 'row_field)  _  _  (_loc : FanLoc.t)  ->
                  (Ast.TyVrnEq (_loc, rfl) : 'ctyp ))));
          ([`Skeyword "[";
           `Snterm
             (Gram.obj
                (constructor_declarations : 'constructor_declarations Gram.t ));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (t : 'constructor_declarations)  _  (_loc : FanLoc.t) 
                  -> (Ast.TySum (_loc, t) : 'ctyp ))));
          ([`Skeyword "["; `Skeyword "]"],
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (Ast.TySum (_loc, (Ast.TyNil _loc)) : 'ctyp ))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            (Gram.mk_action
               (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  -> (t : 'ctyp ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword "*";
           `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.TyTup (_loc, (Ast.TySta (_loc, t, tl))) : 'ctyp ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.TyId (_loc, (Ast.IdUid (_loc, i))) : 'ctyp ))));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.TyId (_loc, (Ast.IdLid (_loc, i))) : 'ctyp ))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `QUOTATION x ->
                      (Quotation.expand _loc x DynAst.ctyp_tag : 'ctyp )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANTIQUOT ("id",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"id\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("id" as n),s) ->
                      (Ast.TyId
                         (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)))) : 
                      'ctyp )
                  | _ -> assert false)));
          ([`Stoken
              (((function | `ANTIQUOT ("tup",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"tup\",_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT (("tup" as n),s) ->
                      (Ast.TyTup
                         (_loc, (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)))) : 
                      'ctyp )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"typ"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"typ"|"anti" as n),s) ->
                      (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'ctyp )
                  | _ -> assert false)));
          ([`Skeyword "_"],
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (Ast.TyAny _loc : 'ctyp ))));
          ([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                  (Ast.TyQuo (_loc, i) : 'ctyp ))))])]);
    Gram.extend (star_ctyp : 'star_ctyp Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'star_ctyp ))));
           ([`Sself; `Skeyword "*"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TySta (_loc, t1, t2) : 'star_ctyp ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp*" n s)) : 
                       'star_ctyp )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'star_ctyp )
                   | _ -> assert false)))])]);
    Gram.extend
      (constructor_declarations : 'constructor_declarations Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (s : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.TyId (_loc, (Ast.IdUid (_loc, s))) : 'constructor_declarations ))));
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
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword "of";
            `Snterm
              (Gram.obj
                 (constructor_arg_list : 'constructor_arg_list Gram.t ))],
             (Gram.mk_action
                (fun (t : 'constructor_arg_list)  _  (s : 'a_UIDENT) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyOf
                      (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))), t) : 
                   'constructor_declarations ))));
           ([`Sself; `Skeyword "|"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'constructor_declarations)  _ 
                   (t1 : 'constructor_declarations)  (_loc : FanLoc.t)  ->
                   (Ast.TyOr (_loc, t1, t2) : 'constructor_declarations ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'constructor_declarations )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp|" n s)) : 
                       'constructor_declarations )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'constructor_declarations )
                   | _ -> assert false)))])]);
    Gram.extend (constructor_declaration : 'constructor_declaration Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (s : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.TyId (_loc, (Ast.IdUid (_loc, s))) : 'constructor_declaration ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword "of";
            `Snterm
              (Gram.obj
                 (constructor_arg_list : 'constructor_arg_list Gram.t ))],
             (Gram.mk_action
                (fun (t : 'constructor_arg_list)  _  (s : 'a_UIDENT) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyOf
                      (_loc, (Ast.TyId (_loc, (Ast.IdUid (_loc, s)))), t) : 
                   'constructor_declaration ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'constructor_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'constructor_declaration )
                   | _ -> assert false)))])]);
    Gram.extend (constructor_arg_list : 'constructor_arg_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'ctyp)  (_loc : FanLoc.t)  ->
                    (t : 'constructor_arg_list ))));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'constructor_arg_list)  _ 
                   (t1 : 'constructor_arg_list)  (_loc : FanLoc.t)  ->
                   (Ast.TyAnd (_loc, t1, t2) : 'constructor_arg_list ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctypand" n s)) : 
                       'constructor_arg_list )
                   | _ -> assert false)))])]);
    Gram.extend (label_declaration_list : 'label_declaration_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj (label_declaration : 'label_declaration Gram.t ))],
              (Gram.mk_action
                 (fun (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                    (t1 : 'label_declaration_list ))));
           ([`Snterm
               (Gram.obj (label_declaration : 'label_declaration Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                   (t1 : 'label_declaration_list ))));
           ([`Snterm
               (Gram.obj (label_declaration : 'label_declaration Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (t2 : 'label_declaration_list)  _ 
                   (t1 : 'label_declaration)  (_loc : FanLoc.t)  ->
                   (Ast.TySem (_loc, t1, t2) : 'label_declaration_list ))))])]);
    Gram.extend (label_declaration : 'label_declaration Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
             `Skeyword ":";
             `Skeyword "mutable";
             `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'poly_type)  _  _  (s : 'a_LIDENT) 
                    (_loc : FanLoc.t)  ->
                    (Ast.TyCol
                       (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))),
                         (Ast.TyMut (_loc, t))) : 'label_declaration ))));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (s : 'a_LIDENT)  (_loc : FanLoc.t) 
                   ->
                   (Ast.TyCol
                      (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, s)))), t) : 
                   'label_declaration ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'label_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp;" n s)) : 
                       'label_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'label_declaration )
                   | _ -> assert false)))])]);
    Gram.extend (class_name_and_param : 'class_name_and_param Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                    ((i, (Ast.TyNil _loc)) : 'class_name_and_param ))));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Skeyword "[";
            `Snterm
              (Gram.obj
                 (comma_type_parameter : 'comma_type_parameter Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (x : 'comma_type_parameter)  _  (i : 'a_LIDENT) 
                   (_loc : FanLoc.t)  -> ((i, x) : 'class_name_and_param ))))])]);
    Gram.extend (comma_type_parameter : 'comma_type_parameter Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'type_parameter)  (_loc : FanLoc.t)  ->
                    (t : 'comma_type_parameter ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp," n s)) : 
                       'comma_type_parameter )
                   | _ -> assert false)));
           ([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (t2 : 'comma_type_parameter)  _ 
                   (t1 : 'comma_type_parameter)  (_loc : FanLoc.t)  ->
                   (Ast.TyCom (_loc, t1, t2) : 'comma_type_parameter ))))])]);
    Gram.extend (opt_comma_ctyp : 'opt_comma_ctyp Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.TyNil _loc : 'opt_comma_ctyp ))));
           ([`Skeyword "[";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (x : 'comma_ctyp)  _  (_loc : FanLoc.t)  ->
                   (x : 'opt_comma_ctyp ))))])]);
    Gram.extend (comma_ctyp : 'comma_ctyp Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'comma_ctyp ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp," n s)) : 
                       'comma_ctyp )
                   | _ -> assert false)));
           ([`Sself; `Skeyword ","; `Sself],
             (Gram.mk_action
                (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyCom (_loc, t1, t2) : 'comma_ctyp ))))])]);
    Gram.extend (ctyp_quot : 'ctyp_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'ctyp_quot ))));
           ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (x : 'more_ctyp)  (_loc : FanLoc.t)  ->
                   (x : 'ctyp_quot ))));
           ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
            `Skeyword "and";
            `Snterm
              (Gram.obj
                 (constructor_arg_list : 'constructor_arg_list Gram.t ))],
             (Gram.mk_action
                (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyAnd (_loc, x, y) : 'ctyp_quot ))));
           ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
            `Skeyword "&";
            `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (y : 'amp_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t) 
                   -> (Ast.TyAmp (_loc, x, y) : 'ctyp_quot ))));
           ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
            `Skeyword "*";
            `Snterm (Gram.obj (star_ctyp : 'star_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (y : 'star_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t)
                    -> (Ast.TySta (_loc, x, y) : 'ctyp_quot ))));
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
            `Skeyword ":";
            `Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (y : 'more_ctyp)  _  (x : 'more_ctyp)  (_loc : FanLoc.t)
                    -> (Ast.TyCol (_loc, x, y) : 'ctyp_quot ))));
           ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
            `Skeyword "of";
            `Skeyword "&";
            `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ));
            `Skeyword "|";
            `Snterm (Gram.obj (row_field : 'row_field Gram.t ))],
             (Gram.mk_action
                (fun (z : 'row_field)  _  (y : 'amp_ctyp)  _  _ 
                   (x : 'more_ctyp)  (_loc : FanLoc.t)  ->
                   (Ast.TyOr (_loc, (Ast.TyOfAmp (_loc, x, y)), z) : 
                   'ctyp_quot ))));
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
            `Snterm
              (Gram.obj
                 (constructor_arg_list : 'constructor_arg_list Gram.t ));
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
            `Snterm
              (Gram.obj
                 (constructor_arg_list : 'constructor_arg_list Gram.t ))],
             (Gram.mk_action
                (fun (y : 'constructor_arg_list)  _  (x : 'more_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyOf (_loc, x, y) : 'ctyp_quot ))));
           ([`Snterm (Gram.obj (more_ctyp : 'more_ctyp Gram.t ));
            `Skeyword "|";
            `Snterm
              (Gram.obj
                 (constructor_declarations : 'constructor_declarations Gram.t ))],
             (Gram.mk_action
                (fun (y : 'constructor_declarations)  _  (x : 'more_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyOr (_loc, x, y) : 'ctyp_quot ))));
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
            `Skeyword ",";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (y : 'comma_ctyp)  _  (x : 'more_ctyp) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyCom (_loc, x, y) : 'ctyp_quot ))))])]);
    Gram.extend (more_ctyp : 'more_ctyp Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (type_parameter : 'type_parameter Gram.t ))],
              (Gram.mk_action
                 (fun (x : 'type_parameter)  (_loc : FanLoc.t)  ->
                    (x : 'more_ctyp ))));
           ([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (x : 'ctyp)  (_loc : FanLoc.t)  -> (x : 'more_ctyp ))));
           ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (x : 'a_ident)  _  (_loc : FanLoc.t)  ->
                   (Ast.TyVrn (_loc, x) : 'more_ctyp ))));
           ([`Skeyword "mutable"; `Sself],
             (Gram.mk_action
                (fun (x : 'more_ctyp)  _  (_loc : FanLoc.t)  ->
                   (Ast.TyMut (_loc, x) : 'more_ctyp ))))])])
  let _ =
    Gram.extend (a_ident : 'a_ident Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  -> (i : 'a_ident ))));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  -> (i : 'a_ident ))))])]);
    Gram.extend (ident : 'ident Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
             `Skeyword ".";
             `Sself],
              (Gram.mk_action
                 (fun (j : 'ident)  _  (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.IdAcc (_loc, (Ast.IdUid (_loc, i)), j) : 'ident ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (i : 'ident)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                       (Ast.IdAcc
                          (_loc,
                            (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))), i) : 
                       'ident )
                   | _ -> assert false)));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                   (Ast.IdLid (_loc, i) : 'ident ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                   (Ast.IdUid (_loc, i) : 'ident ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'ident )
                   | _ -> assert false)))])]);
    Gram.extend
      (module_longident_dot_lparen : 'module_longident_dot_lparen Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
             `Skeyword ".";
             `Skeyword "("],
              (Gram.mk_action
                 (fun _  _  (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.IdUid (_loc, i) : 'module_longident_dot_lparen ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (l : 'module_longident_dot_lparen)  _  (m : 'a_UIDENT) 
                   (_loc : FanLoc.t)  ->
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)), l) : 'module_longident_dot_lparen ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"));
            `Skeyword ".";
            `Skeyword "("],
             (Gram.mk_action
                (fun _  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'module_longident_dot_lparen )
                   | _ -> assert false)))])]);
    Gram.extend (module_longident : 'module_longident Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.IdUid (_loc, i) : 'module_longident ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (l : 'module_longident)  _  (m : 'a_UIDENT) 
                   (_loc : FanLoc.t)  ->
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)), l) : 'module_longident ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'module_longident )
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
          [([`Skeyword "("; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (i : 'module_longident_with_app)  _ 
                   (_loc : FanLoc.t)  -> (i : 'module_longident_with_app ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.IdUid (_loc, i) : 'module_longident_with_app ))));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'module_longident_with_app )
                  | _ -> assert false)))])]);
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
          [([`Skeyword "("; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (i : 'type_longident)  _  (_loc : FanLoc.t)  ->
                   (i : 'type_longident ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.IdUid (_loc, i) : 'type_longident ))));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.IdLid (_loc, i) : 'type_longident ))));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'type_longident )
                  | _ -> assert false)))])]);
    Gram.extend (label_longident : 'label_longident Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                    (Ast.IdLid (_loc, i) : 'label_longident ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ".";
            `Sself],
             (Gram.mk_action
                (fun (l : 'label_longident)  _  (m : 'a_UIDENT) 
                   (_loc : FanLoc.t)  ->
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, m)), l) : 'label_longident ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                       (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 
                       'label_longident )
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
    Gram.extend (class_declaration : 'class_declaration Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj
                   (class_info_for_class_expr : 'class_info_for_class_expr
                                                  Gram.t ));
             `Snterm
               (Gram.obj (class_fun_binding : 'class_fun_binding Gram.t ))],
              (Gram.mk_action
                 (fun (ce : 'class_fun_binding) 
                    (ci : 'class_info_for_class_expr)  (_loc : FanLoc.t)  ->
                    (Ast.CeEq (_loc, ci, ce) : 'class_declaration ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.class_expr_tag : 
                       'class_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"cdcl"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"cdcl\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"cdcl"|"anti"|"list" as n),s) ->
                       (Ast.CeAnt (_loc, (mk_anti ~c:"class_expr" n s)) : 
                       'class_declaration )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (c2 : 'class_declaration)  _  (c1 : 'class_declaration) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeAnd (_loc, c1, c2) : 'class_declaration ))))])]);
    Gram.extend (class_fun_binding : 'class_fun_binding Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
             `Sself],
              (Gram.mk_action
                 (fun (cfb : 'class_fun_binding)  (p : 'labeled_ipatt) 
                    (_loc : FanLoc.t)  ->
                    (Ast.CeFun (_loc, p, cfb) : 'class_fun_binding ))));
           ([`Skeyword ":";
            `Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
             (Gram.mk_action
                (fun (ce : 'class_expr)  _  (ct : 'class_type_plus)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeTyc (_loc, ce, ct) : 'class_fun_binding ))));
           ([`Skeyword "=";
            `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
             (Gram.mk_action
                (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                   (ce : 'class_fun_binding ))))])]);
    Gram.extend
      (class_info_for_class_type : 'class_info_for_class_type Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (opt_virtual : 'opt_virtual Gram.t ));
             `Snterm
               (Gram.obj
                  (class_name_and_param : 'class_name_and_param Gram.t ))],
              (Gram.mk_action
                 (fun ((i,ot) : 'class_name_and_param)  (mv : 'opt_virtual) 
                    (_loc : FanLoc.t)  ->
                    (Ast.CtCon (_loc, mv, (Ast.IdLid (_loc, i)), ot) : 
                    'class_info_for_class_type ))))])]);
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
           [([`Skeyword "->";
             `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
              (Gram.mk_action
                 (fun (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                    (ce : 'class_fun_def ))));
           ([`Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (ce : 'class_fun_def)  (p : 'labeled_ipatt) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeFun (_loc, p, ce) : 'class_fun_def ))))])]);
    Gram.extend (class_expr : 'class_expr Gram.t )
      (None,
        [((Some "top"), None,
           [([`Skeyword "let";
             `Snterm (Gram.obj (opt_rec : 'opt_rec Gram.t ));
             `Snterm (Gram.obj (binding : 'binding Gram.t ));
             `Skeyword "in";
             `Sself],
              (Gram.mk_action
                 (fun (ce : 'class_expr)  _  (bi : 'binding)  (rf : 'opt_rec)
                     _  (_loc : FanLoc.t)  ->
                    (Ast.CeLet (_loc, rf, bi, ce) : 'class_expr ))));
           ([`Skeyword "fun";
            `Snterm (Gram.obj (labeled_ipatt : 'labeled_ipatt Gram.t ));
            `Snterm (Gram.obj (class_fun_def : 'class_fun_def Gram.t ))],
             (Gram.mk_action
                (fun (ce : 'class_fun_def)  (p : 'labeled_ipatt)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeFun (_loc, p, ce) : 'class_expr ))))]);
        ((Some "apply"), (Some `NA),
          [([`Sself; `Snterml ((Gram.obj (expr : 'expr Gram.t )), "label")],
             (Gram.mk_action
                (fun (e : 'expr)  (ce : 'class_expr)  (_loc : FanLoc.t)  ->
                   (Ast.CeApp (_loc, ce, e) : 'class_expr ))))]);
        ((Some "simple"), None,
          [([`Skeyword "("; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (ce : 'class_expr)  _  (_loc : FanLoc.t)  ->
                   (ce : 'class_expr ))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ":";
           `Snterm (Gram.obj (class_type : 'class_type Gram.t ));
           `Skeyword ")"],
            (Gram.mk_action
               (fun _  (ct : 'class_type)  _  (ce : 'class_expr)  _ 
                  (_loc : FanLoc.t)  ->
                  (Ast.CeTyc (_loc, ce, ct) : 'class_expr ))));
          ([`Skeyword "object";
           `Snterm
             (Gram.obj (opt_class_self_patt : 'opt_class_self_patt Gram.t ));
           `Snterm (Gram.obj (class_structure : 'class_structure Gram.t ));
           `Skeyword "end"],
            (Gram.mk_action
               (fun _  (cst : 'class_structure)  (csp : 'opt_class_self_patt)
                   _  (_loc : FanLoc.t)  ->
                  (Ast.CeStr (_loc, csp, cst) : 'class_expr ))));
          ([`Snterm
              (Gram.obj
                 (class_longident_and_param : 'class_longident_and_param
                                                Gram.t ))],
            (Gram.mk_action
               (fun (ce : 'class_longident_and_param)  (_loc : FanLoc.t)  ->
                  (ce : 'class_expr ))));
          ([`Stoken
              (((function | `QUOTATION _ -> true | _ -> false)),
                (`Normal, "`QUOTATION _"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `QUOTATION x ->
                      (Quotation.expand _loc x DynAst.class_expr_tag : 
                      'class_expr )
                  | _ -> assert false)));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"cexp"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"cexp\"|\"anti\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"cexp"|"anti" as n),s) ->
                      (Ast.CeAnt (_loc, (mk_anti ~c:"class_expr" n s)) : 
                      'class_expr )
                  | _ -> assert false)))])]);
    Gram.extend
      (class_longident_and_param : 'class_longident_and_param Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ))],
              (Gram.mk_action
                 (fun (ci : 'class_longident)  (_loc : FanLoc.t)  ->
                    (Ast.CeCon (_loc, Ast.ViNil, ci, (Ast.TyNil _loc)) : 
                    'class_longident_and_param ))));
           ([`Snterm (Gram.obj (class_longident : 'class_longident Gram.t ));
            `Skeyword "[";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (t : 'comma_ctyp)  _  (ci : 'class_longident) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeCon (_loc, Ast.ViNil, ci, t) : 'class_longident_and_param ))))])]);
    Gram.extend (class_structure : 'class_structure Gram.t )
      (None,
        [(None, None,
           [([`Slist0
                (Gram.srules class_structure
                   [([`Snterm
                        (Gram.obj (class_str_item : 'class_str_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (cst : 'class_str_item)  (_loc : FanLoc.t) 
                            -> (cst : 'e__19 ))))])],
              (Gram.mk_action
                 (fun (l : 'e__19 list)  (_loc : FanLoc.t)  ->
                    (Ast.crSem_of_list l : 'class_structure ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"cst"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (cst : 'class_structure)  _  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"cst"|"anti"|"list" as n),s) ->
                       (Ast.CrSem
                          (_loc,
                            (Ast.CrAnt
                               (_loc, (mk_anti ~c:"class_str_item" n s))),
                            cst) : 'class_structure )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"cst"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"cst"|"anti"|"list" as n),s) ->
                       (Ast.CrAnt (_loc, (mk_anti ~c:"class_str_item" n s)) : 
                       'class_structure )
                   | _ -> assert false)))])]);
    Gram.extend (opt_class_self_patt : 'opt_class_self_patt Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.PaNil _loc : 'opt_class_self_patt ))));
           ([`Skeyword "(";
            `Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (Ast.PaTyc (_loc, p, t) : 'opt_class_self_patt ))));
           ([`Skeyword "(";
            `Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (p : 'patt)  _  (_loc : FanLoc.t)  ->
                   (p : 'opt_class_self_patt ))))])]);
    Gram.extend (class_str_item : 'class_str_item Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "initializer";
             `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (se : 'expr)  _  (_loc : FanLoc.t)  ->
                    (Ast.CrIni (_loc, se) : 'class_str_item ))));
           ([`Snterm (Gram.obj (type_constraint : 'type_constraint Gram.t ));
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (Ast.CrCtr (_loc, t1, t2) : 'class_str_item ))));
           ([`Snterm
               (Gram.obj (method_opt_override : 'method_opt_override Gram.t ));
            `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
            `Skeyword "virtual";
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  _ 
                   (pf : 'opt_private)  (o : 'method_opt_override) 
                   (_loc : FanLoc.t)  ->
                   (if o <> Ast.OvNil
                    then
                      raise
                        (Stream.Error
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
            `Skeyword "virtual";
            `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  (pf : 'opt_private) 
                   _  (o : 'method_opt_override)  (_loc : FanLoc.t)  ->
                   (if o <> Ast.OvNil
                    then
                      raise
                        (Stream.Error
                           "override (!) is incompatible with virtual")
                    else Ast.CrVir (_loc, l, pf, t) : 'class_str_item ))));
           ([`Snterm
               (Gram.obj
                  (value_val_opt_override : 'value_val_opt_override Gram.t ));
            `Skeyword "virtual";
            `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  (mf : 'opt_mutable) 
                   _  (o : 'value_val_opt_override)  (_loc : FanLoc.t)  ->
                   (if o <> Ast.OvNil
                    then
                      raise
                        (Stream.Error
                           "override (!) is incompatible with virtual")
                    else Ast.CrVvr (_loc, l, mf, t) : 'class_str_item ))));
           ([`Snterm
               (Gram.obj
                  (value_val_opt_override : 'value_val_opt_override Gram.t ));
            `Snterm (Gram.obj (opt_mutable : 'opt_mutable Gram.t ));
            `Skeyword "virtual";
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  _ 
                   (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                   (_loc : FanLoc.t)  ->
                   (if o <> Ast.OvNil
                    then
                      raise
                        (Stream.Error
                           "override (!) is incompatible with virtual")
                    else Ast.CrVvr (_loc, l, mf, t) : 'class_str_item ))));
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
           ([`Skeyword "inherit";
            `Snterm (Gram.obj (opt_override : 'opt_override Gram.t ));
            `Snterm (Gram.obj (class_expr : 'class_expr Gram.t ));
            `Snterm (Gram.obj (opt_as_lident : 'opt_as_lident Gram.t ))],
             (Gram.mk_action
                (fun (pb : 'opt_as_lident)  (ce : 'class_expr) 
                   (o : 'opt_override)  _  (_loc : FanLoc.t)  ->
                   (Ast.CrInh (_loc, o, ce, pb) : 'class_str_item ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.class_str_item_tag : 
                       'class_str_item )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"cst"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"cst\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"cst"|"anti"|"list" as n),s) ->
                       (Ast.CrAnt (_loc, (mk_anti ~c:"class_str_item" n s)) : 
                       'class_str_item )
                   | _ -> assert false)))])]);
    Gram.extend (method_opt_override : 'method_opt_override Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "method"],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (Ast.OvNil : 'method_opt_override ))));
           ([`Skeyword "method";
            `Stoken
              (((function
                 | `ANTIQUOT (("!"|"override"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"!\"|\"override\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("!"|"override"|"anti" as n),s) ->
                       (Ast.OvAnt (mk_anti n s) : 'method_opt_override )
                   | _ -> assert false)));
           ([`Skeyword "method"; `Skeyword "!"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.OvOverride : 'method_opt_override ))))])]);
    Gram.extend (value_val_opt_override : 'value_val_opt_override Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "val"],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (Ast.OvNil : 'value_val_opt_override ))));
           ([`Skeyword "val";
            `Stoken
              (((function
                 | `ANTIQUOT (("!"|"override"|"anti"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"!\"|\"override\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("!"|"override"|"anti" as n),s) ->
                       (Ast.OvAnt (mk_anti n s) : 'value_val_opt_override )
                   | _ -> assert false)));
           ([`Skeyword "val"; `Skeyword "!"],
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  ->
                   (Ast.OvOverride : 'value_val_opt_override ))))])]);
    Gram.extend (opt_as_lident : 'opt_as_lident Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> ("" : 'opt_as_lident ))));
           ([`Skeyword "as";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  _  (_loc : FanLoc.t)  ->
                   (i : 'opt_as_lident ))))])]);
    Gram.extend (opt_polyt : 'opt_polyt Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.TyNil _loc : 'opt_polyt ))));
           ([`Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (_loc : FanLoc.t)  ->
                   (t : 'opt_polyt ))))])]);
    Gram.extend (cvalue_binding : 'cvalue_binding Gram.t )
      (None,
        [(None, None,
           [([`Skeyword ":>";
             `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
             `Skeyword "=";
             `Snterm (Gram.obj (expr : 'expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                    (Ast.ExCoe (_loc, e, (Ast.TyNil _loc), t) : 'cvalue_binding ))));
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
                    | Ast.TyPol (_,_,_) ->
                        raise (Stream.Error "unexpected polytype here")
                    | _ -> Ast.ExCoe (_loc, e, t, t2) : 'cvalue_binding ))));
           ([`Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (t : 'poly_type)  _  (_loc : FanLoc.t) 
                   -> (Ast.ExTyc (_loc, e, t) : 'cvalue_binding ))));
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
                   (let u = Ast.TyPol (_loc, t1, t2) in
                    Ast.ExTyc (_loc, e, u) : 'cvalue_binding ))));
           ([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (e : 'cvalue_binding ))))])]);
    Gram.extend (label : 'label Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  -> (i : 'label ))))])]);
    Gram.extend (class_type : 'class_type Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "object";
             `Snterm
               (Gram.obj (opt_class_self_type : 'opt_class_self_type Gram.t ));
             `Snterm (Gram.obj (class_signature : 'class_signature Gram.t ));
             `Skeyword "end"],
              (Gram.mk_action
                 (fun _  (csg : 'class_signature) 
                    (cst : 'opt_class_self_type)  _  (_loc : FanLoc.t)  ->
                    (Ast.CtSig (_loc, cst, csg) : 'class_type ))));
           ([`Snterm
               (Gram.obj
                  (class_type_longident_and_param : 'class_type_longident_and_param
                                                      Gram.t ))],
             (Gram.mk_action
                (fun (ct : 'class_type_longident_and_param) 
                   (_loc : FanLoc.t)  -> (ct : 'class_type ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.class_type_tag : 
                       'class_type )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"ctyp"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"ctyp\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"ctyp"|"anti" as n),s) ->
                       (Ast.CtAnt (_loc, (mk_anti ~c:"class_type" n s)) : 
                       'class_type )
                   | _ -> assert false)))])]);
    Gram.extend
      (class_type_longident_and_param : 'class_type_longident_and_param
                                          Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj
                   (class_type_longident : 'class_type_longident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'class_type_longident)  (_loc : FanLoc.t)  ->
                    (Ast.CtCon (_loc, Ast.ViNil, i, (Ast.TyNil _loc)) : 
                    'class_type_longident_and_param ))));
           ([`Snterm
               (Gram.obj
                  (class_type_longident : 'class_type_longident Gram.t ));
            `Skeyword "[";
            `Snterm (Gram.obj (comma_ctyp : 'comma_ctyp Gram.t ));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (t : 'comma_ctyp)  _  (i : 'class_type_longident) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CtCon (_loc, Ast.ViNil, i, t) : 'class_type_longident_and_param ))))])]);
    Gram.extend (class_type_plus : 'class_type_plus Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
              (Gram.mk_action
                 (fun (ct : 'class_type)  (_loc : FanLoc.t)  ->
                    (ct : 'class_type_plus ))));
           ([`Skeyword "[";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword "]";
            `Skeyword "->";
            `Sself],
             (Gram.mk_action
                (fun (ct : 'class_type_plus)  _  _  (t : 'ctyp)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.CtFun (_loc, t, ct) : 'class_type_plus ))))])]);
    Gram.extend (opt_class_self_type : 'opt_class_self_type Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.TyNil _loc : 'opt_class_self_type ))));
           ([`Skeyword "(";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
            `Skeyword ")"],
             (Gram.mk_action
                (fun _  (t : 'ctyp)  _  (_loc : FanLoc.t)  ->
                   (t : 'opt_class_self_type ))))])]);
    Gram.extend (class_signature : 'class_signature Gram.t )
      (None,
        [(None, None,
           [([`Slist0
                (Gram.srules class_signature
                   [([`Snterm
                        (Gram.obj (class_sig_item : 'class_sig_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (csg : 'class_sig_item)  (_loc : FanLoc.t) 
                            -> (csg : 'e__20 ))))])],
              (Gram.mk_action
                 (fun (l : 'e__20 list)  (_loc : FanLoc.t)  ->
                    (Ast.cgSem_of_list l : 'class_signature ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"csg"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (csg : 'class_signature)  _  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"csg"|"anti"|"list" as n),s) ->
                       (Ast.CgSem
                          (_loc,
                            (Ast.CgAnt
                               (_loc, (mk_anti ~c:"class_sig_item" n s))),
                            csg) : 'class_signature )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"csg"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"csg"|"anti"|"list" as n),s) ->
                       (Ast.CgAnt (_loc, (mk_anti ~c:"class_sig_item" n s)) : 
                       'class_signature )
                   | _ -> assert false)))])]);
    Gram.extend (class_sig_item : 'class_sig_item Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (type_constraint : 'type_constraint Gram.t ));
             `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
             `Skeyword "=";
             `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : FanLoc.t)  ->
                    (Ast.CgCtr (_loc, t1, t2) : 'class_sig_item ))));
           ([`Skeyword "method";
            `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
            `Skeyword "virtual";
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  _ 
                   (pf : 'opt_private)  _  (_loc : FanLoc.t)  ->
                   (Ast.CgVir (_loc, l, pf, t) : 'class_sig_item ))));
           ([`Skeyword "method";
            `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  (pf : 'opt_private) 
                   _  (_loc : FanLoc.t)  ->
                   (Ast.CgMth (_loc, l, pf, t) : 'class_sig_item ))));
           ([`Skeyword "method";
            `Skeyword "virtual";
            `Snterm (Gram.obj (opt_private : 'opt_private Gram.t ));
            `Snterm (Gram.obj (label : 'label Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
             (Gram.mk_action
                (fun (t : 'poly_type)  _  (l : 'label)  (pf : 'opt_private) 
                   _  _  (_loc : FanLoc.t)  ->
                   (Ast.CgVir (_loc, l, pf, t) : 'class_sig_item ))));
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
           ([`Skeyword "inherit";
            `Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
             (Gram.mk_action
                (fun (cs : 'class_type)  _  (_loc : FanLoc.t)  ->
                   (Ast.CgInh (_loc, cs) : 'class_sig_item ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.class_sig_item_tag : 
                       'class_sig_item )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"csg"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"csg\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"csg"|"anti"|"list" as n),s) ->
                       (Ast.CgAnt (_loc, (mk_anti ~c:"class_sig_item" n s)) : 
                       'class_sig_item )
                   | _ -> assert false)))])]);
    Gram.extend (type_constraint : 'type_constraint Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "constraint"],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  -> (() : 'type_constraint ))));
           ([`Skeyword "type"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (() : 'type_constraint ))))])]);
    Gram.extend (class_description : 'class_description Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj
                   (class_info_for_class_type : 'class_info_for_class_type
                                                  Gram.t ));
             `Skeyword ":";
             `Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ))],
              (Gram.mk_action
                 (fun (ct : 'class_type_plus)  _ 
                    (ci : 'class_info_for_class_type)  (_loc : FanLoc.t)  ->
                    (Ast.CtCol (_loc, ci, ct) : 'class_description ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.class_type_tag : 
                       'class_description )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"typ"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti"|"list" as n),s) ->
                       (Ast.CtAnt (_loc, (mk_anti ~c:"class_type" n s)) : 
                       'class_description )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (cd2 : 'class_description)  _ 
                   (cd1 : 'class_description)  (_loc : FanLoc.t)  ->
                   (Ast.CtAnd (_loc, cd1, cd2) : 'class_description ))))])]);
    Gram.extend (class_type_declaration : 'class_type_declaration Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj
                   (class_info_for_class_type : 'class_info_for_class_type
                                                  Gram.t ));
             `Skeyword "=";
             `Snterm (Gram.obj (class_type : 'class_type Gram.t ))],
              (Gram.mk_action
                 (fun (ct : 'class_type)  _ 
                    (ci : 'class_info_for_class_type)  (_loc : FanLoc.t)  ->
                    (Ast.CtEq (_loc, ci, ct) : 'class_type_declaration ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.class_type_tag : 
                       'class_type_declaration )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"typ"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ"|"anti"|"list" as n),s) ->
                       (Ast.CtAnt (_loc, (mk_anti ~c:"class_type" n s)) : 
                       'class_type_declaration )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (cd2 : 'class_type_declaration)  _ 
                   (cd1 : 'class_type_declaration)  (_loc : FanLoc.t)  ->
                   (Ast.CtAnd (_loc, cd1, cd2) : 'class_type_declaration ))))])]);
    Gram.extend (field_expr_list : 'field_expr_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ))],
              (Gram.mk_action
                 (fun (b1 : 'field_expr)  (_loc : FanLoc.t)  ->
                    (b1 : 'field_expr_list ))));
           ([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (b1 : 'field_expr)  (_loc : FanLoc.t)  ->
                   (b1 : 'field_expr_list ))));
           ([`Snterm (Gram.obj (field_expr : 'field_expr Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (b2 : 'field_expr_list)  _  (b1 : 'field_expr) 
                   (_loc : FanLoc.t)  ->
                   (Ast.RbSem (_loc, b1, b2) : 'field_expr_list ))))])]);
    Gram.extend (field_expr : 'field_expr Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (label : 'label Gram.t ));
             `Skeyword "=";
             `Snterml ((Gram.obj (expr : 'expr Gram.t )), "top")],
              (Gram.mk_action
                 (fun (e : 'expr)  _  (l : 'label)  (_loc : FanLoc.t)  ->
                    (Ast.RbEq (_loc, (Ast.IdLid (_loc, l)), e) : 'field_expr ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'field_expr )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"bi"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"bi\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"bi"|"anti" as n),s) ->
                       (Ast.RbAnt (_loc, (mk_anti ~c:"rec_binding" n s)) : 
                       'field_expr )
                   | _ -> assert false)))])]);
    Gram.extend (meth_list : 'meth_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
             `Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
              (Gram.mk_action
                 (fun (v : 'opt_dot_dot)  (m : 'meth_decl)  (_loc : FanLoc.t)
                     -> ((m, v) : 'meth_list ))));
           ([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
            `Skeyword ";";
            `Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
             (Gram.mk_action
                (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl) 
                   (_loc : FanLoc.t)  -> ((m, v) : 'meth_list ))));
           ([`Snterm (Gram.obj (meth_decl : 'meth_decl Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  _  (m : 'meth_decl) 
                   (_loc : FanLoc.t)  ->
                   (((Ast.TySem (_loc, m, ml)), v) : 'meth_list ))))])]);
    Gram.extend (meth_decl : 'meth_decl Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
             `Skeyword ":";
             `Snterm (Gram.obj (poly_type : 'poly_type Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'poly_type)  _  (lab : 'a_LIDENT) 
                    (_loc : FanLoc.t)  ->
                    (Ast.TyCol
                       (_loc, (Ast.TyId (_loc, (Ast.IdLid (_loc, lab)))), t) : 
                    'meth_decl ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'meth_decl )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp;" n s)) : 
                       'meth_decl )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'meth_decl )
                   | _ -> assert false)))])]);
    Gram.extend (opt_meth_list : 'opt_meth_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (opt_dot_dot : 'opt_dot_dot Gram.t ))],
              (Gram.mk_action
                 (fun (v : 'opt_dot_dot)  (_loc : FanLoc.t)  ->
                    (Ast.TyObj (_loc, (Ast.TyNil _loc), v) : 'opt_meth_list ))));
           ([`Snterm (Gram.obj (meth_list : 'meth_list Gram.t ))],
             (Gram.mk_action
                (fun ((ml,v) : 'meth_list)  (_loc : FanLoc.t)  ->
                   (Ast.TyObj (_loc, ml, v) : 'opt_meth_list ))))])]);
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
    Gram.extend (typevars : 'typevars Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "'"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                    (Ast.TyQuo (_loc, i) : 'typevars ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'typevars )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'typevars )
                   | _ -> assert false)));
           ([`Sself; `Sself],
             (Gram.mk_action
                (fun (t2 : 'typevars)  (t1 : 'typevars)  (_loc : FanLoc.t) 
                   -> (Ast.TyApp (_loc, t1, t2) : 'typevars ))))])]);
    Gram.extend (unquoted_typevars : 'unquoted_typevars Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_ident)  (_loc : FanLoc.t)  ->
                    (Ast.TyId (_loc, (Ast.IdLid (_loc, i))) : 'unquoted_typevars ))));
           ([`Stoken
               (((function | `QUOTATION _ -> true | _ -> false)),
                 (`Normal, "`QUOTATION _"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `QUOTATION x ->
                       (Quotation.expand _loc x DynAst.ctyp_tag : 'unquoted_typevars )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'unquoted_typevars )
                   | _ -> assert false)));
           ([`Sself; `Sself],
             (Gram.mk_action
                (fun (t2 : 'unquoted_typevars)  (t1 : 'unquoted_typevars) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyApp (_loc, t1, t2) : 'unquoted_typevars ))))])]);
    Gram.extend (row_field : 'row_field Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'row_field ))));
           ([`Skeyword "`";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
            `Skeyword "of";
            `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'amp_ctyp)  _  (i : 'a_ident)  _  (_loc : FanLoc.t)
                    ->
                   (Ast.TyOf (_loc, (Ast.TyVrn (_loc, i)), t) : 'row_field ))));
           ([`Skeyword "`";
            `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ));
            `Skeyword "of";
            `Skeyword "&";
            `Snterm (Gram.obj (amp_ctyp : 'amp_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'amp_ctyp)  _  _  (i : 'a_ident)  _ 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyOfAmp (_loc, (Ast.TyVrn (_loc, i)), t) : 'row_field ))));
           ([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                   (Ast.TyVrn (_loc, i) : 'row_field ))));
           ([`Sself; `Skeyword "|"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'row_field)  _  (t1 : 'row_field) 
                   (_loc : FanLoc.t)  ->
                   (Ast.TyOr (_loc, t1, t2) : 'row_field ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp|" n s)) : 
                       'row_field )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'row_field )
                   | _ -> assert false)))])]);
    Gram.extend (amp_ctyp : 'amp_ctyp Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
              (Gram.mk_action
                 (fun (t : 'ctyp)  (_loc : FanLoc.t)  -> (t : 'amp_ctyp ))));
           ([`Stoken
               (((function | `ANTIQUOT ("list",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"list\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("list" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp&" n s)) : 
                       'amp_ctyp )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "&"; `Sself],
             (Gram.mk_action
                (fun (t2 : 'amp_ctyp)  _  (t1 : 'amp_ctyp)  (_loc : FanLoc.t)
                    -> (Ast.TyAmp (_loc, t1, t2) : 'amp_ctyp ))))])]);
    Gram.extend (name_tags : 'name_tags Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "`"; `Snterm (Gram.obj (a_ident : 'a_ident Gram.t ))],
              (Gram.mk_action
                 (fun (i : 'a_ident)  _  (_loc : FanLoc.t)  ->
                    (Ast.TyVrn (_loc, i) : 'name_tags ))));
           ([`Sself; `Sself],
             (Gram.mk_action
                (fun (t2 : 'name_tags)  (t1 : 'name_tags)  (_loc : FanLoc.t) 
                   -> (Ast.TyApp (_loc, t1, t2) : 'name_tags ))));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"typ"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"typ\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"typ" as n),s) ->
                       (Ast.TyAnt (_loc, (mk_anti ~c:"ctyp" n s)) : 'name_tags )
                   | _ -> assert false)))])]);
    Gram.extend (eq_expr : 'eq_expr Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (fun i  p  -> Ast.PaOlb (_loc, i, p) : 'eq_expr ))));
           ([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                   (fun i  p  -> Ast.PaOlbi (_loc, i, p, e) : 'eq_expr ))))])]);
    Gram.extend (patt_tcon : 'patt_tcon Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'patt)  (_loc : FanLoc.t)  -> (p : 'patt_tcon ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaTyc (_loc, p, t) : 'patt_tcon ))))])]);
    Gram.extend (ipatt_tcon : 'ipatt_tcon Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'ipatt)  (_loc : FanLoc.t)  -> (p : 'ipatt_tcon ))));
           ([`Snterm (Gram.obj (ipatt : 'ipatt Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ))],
             (Gram.mk_action
                (fun (t : 'ctyp)  _  (p : 'ipatt)  (_loc : FanLoc.t)  ->
                   (Ast.PaTyc (_loc, p, t) : 'ipatt_tcon ))))])]);
    Gram.extend (direction_flag : 'direction_flag Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function
                   | `ANTIQUOT (("to"|"anti"),_) -> true
                   | _ -> false)),
                  (`Normal, "`ANTIQUOT ((\"to\"|\"anti\"),_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `ANTIQUOT (("to"|"anti" as n),s) ->
                        (Ast.DiAnt (mk_anti n s) : 'direction_flag )
                    | _ -> assert false)));
           ([`Skeyword "downto"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (Ast.DiDownto : 'direction_flag ))));
           ([`Skeyword "to"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.DiTo : 'direction_flag ))))])]);
    Gram.extend (opt_private : 'opt_private Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.PrNil : 'opt_private ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("private"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"private\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("private"|"anti" as n),s) ->
                       (Ast.PrAnt (mk_anti n s) : 'opt_private )
                   | _ -> assert false)));
           ([`Skeyword "private"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.PrPrivate : 'opt_private ))))])]);
    Gram.extend (opt_mutable : 'opt_mutable Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.MuNil : 'opt_mutable ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("mutable"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"mutable\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("mutable"|"anti" as n),s) ->
                       (Ast.MuAnt (mk_anti n s) : 'opt_mutable )
                   | _ -> assert false)));
           ([`Skeyword "mutable"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.MuMutable : 'opt_mutable ))))])]);
    Gram.extend (opt_virtual : 'opt_virtual Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.ViNil : 'opt_virtual ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("virtual"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"virtual\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("virtual"|"anti" as n),s) ->
                       (Ast.ViAnt (mk_anti n s) : 'opt_virtual )
                   | _ -> assert false)));
           ([`Skeyword "virtual"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.ViVirtual : 'opt_virtual ))))])]);
    Gram.extend (opt_dot_dot : 'opt_dot_dot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.RvNil : 'opt_dot_dot ))));
           ([`Stoken
               (((function | `ANTIQUOT ((".."|"anti"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"..\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((".."|"anti" as n),s) ->
                       (Ast.RvAnt (mk_anti n s) : 'opt_dot_dot )
                   | _ -> assert false)));
           ([`Skeyword ".."],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.RvRowVar : 'opt_dot_dot ))))])]);
    Gram.extend (opt_rec : 'opt_rec Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.ReNil : 'opt_rec ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("rec"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"rec\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("rec"|"anti" as n),s) ->
                       (Ast.ReAnt (mk_anti n s) : 'opt_rec )
                   | _ -> assert false)));
           ([`Skeyword "rec"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (Ast.ReRecursive : 'opt_rec ))))])]);
    Gram.extend (opt_override : 'opt_override Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.OvNil : 'opt_override ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("!"|"override"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"!\"|\"override\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("!"|"override"|"anti" as n),s) ->
                       (Ast.OvAnt (mk_anti n s) : 'opt_override )
                   | _ -> assert false)));
           ([`Skeyword "!"],
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (Ast.OvOverride : 'opt_override ))))])]);
    Gram.extend (opt_expr : 'opt_expr Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.ExNil _loc : 'opt_expr ))));
           ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'opt_expr ))))])]);
    Gram.extend (interf : 'interf Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `EOI -> (([], None) : 'interf )
                    | _ -> assert false)));
           ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun ((sil,stopped) : 'interf)  _  (si : 'sig_item) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'interf ))));
           ([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ))],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (([Ast.SgDir (_loc, n, dp)], (stopped_at _loc)) : 
                   'interf ))))])]);
    Gram.extend (sig_items : 'sig_items Gram.t )
      (None,
        [(None, None,
           [([`Slist0
                (Gram.srules sig_items
                   [([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                            (sg : 'e__21 ))))])],
              (Gram.mk_action
                 (fun (l : 'e__21 list)  (_loc : FanLoc.t)  ->
                    (Ast.sgSem_of_list l : 'sig_items ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"sigi"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (sg : 'sig_items)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"sigi"|"anti"|"list" as n),s) ->
                       (Ast.SgSem
                          (_loc,
                            (Ast.SgAnt (_loc, (mk_anti n ~c:"sig_item" s))),
                            sg) : 'sig_items )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"sigi"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"sigi\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"sigi"|"anti"|"list" as n),s) ->
                       (Ast.SgAnt (_loc, (mk_anti n ~c:"sig_item" s)) : 
                       'sig_items )
                   | _ -> assert false)))])]);
    Gram.extend (implem : 'implem Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `EOI -> (([], None) : 'implem )
                    | _ -> assert false)));
           ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun ((sil,stopped) : 'implem)  _  (si : 'str_item) 
                   (_loc : FanLoc.t)  -> (((si :: sil), stopped) : 'implem ))));
           ([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ))],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (([Ast.StDir (_loc, n, dp)], (stopped_at _loc)) : 
                   'implem ))))])]);
    Gram.extend (str_items : 'str_items Gram.t )
      (None,
        [(None, None,
           [([`Slist0
                (Gram.srules str_items
                   [([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                     `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                      (Gram.mk_action
                         (fun _  (st : 'str_item)  (_loc : FanLoc.t)  ->
                            (st : 'e__22 ))))])],
              (Gram.mk_action
                 (fun (l : 'e__22 list)  (_loc : FanLoc.t)  ->
                    (Ast.stSem_of_list l : 'str_items ))));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"stri"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (st : 'str_items)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"stri"|"anti"|"list" as n),s) ->
                       (Ast.StSem
                          (_loc,
                            (Ast.StAnt (_loc, (mk_anti n ~c:"str_item" s))),
                            st) : 'str_items )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"stri"|"anti"|"list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"stri\"|\"anti\"|\"list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"stri"|"anti"|"list" as n),s) ->
                       (Ast.StAnt (_loc, (mk_anti n ~c:"str_item" s)) : 
                       'str_items )
                   | _ -> assert false)))])]);
    Gram.extend (phrase : 'phrase Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
             `Snterm (Gram.obj (semi : 'semi Gram.t ))],
              (Gram.mk_action
                 (fun _  (st : 'str_item)  (_loc : FanLoc.t)  ->
                    (st : 'phrase ))));
           ([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Skeyword ";;"],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  -> (Ast.StDir (_loc, n, dp) : 'phrase ))))])]);
    Gram.extend (top_phrase : 'top_phrase Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `EOI -> (None : 'top_phrase )
                    | _ -> assert false)));
           ([`Snterm (Gram.obj (phrase : 'phrase Gram.t ))],
             (Gram.mk_action
                (fun (ph : 'phrase)  (_loc : FanLoc.t)  ->
                   (Some ph : 'top_phrase ))))])]);
    Gram.extend (use_file : 'use_file Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `EOI -> (([], None) : 'use_file )
                    | _ -> assert false)));
           ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun ((sil,stopped) : 'use_file)  _  (si : 'str_item) 
                   (_loc : FanLoc.t)  ->
                   (((si :: sil), stopped) : 'use_file ))));
           ([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ))],
             (Gram.mk_action
                (fun _  (dp : 'opt_expr)  (n : 'a_LIDENT)  _ 
                   (_loc : FanLoc.t)  ->
                   (([Ast.StDir (_loc, n, dp)], (stopped_at _loc)) : 
                   'use_file ))))])]);
    Gram.extend (a_INT : 'a_INT Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `INT (_,_) -> true | _ -> false)),
                  (`Normal, "`INT (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `INT (_,s) -> (s : 'a_INT )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"int"|"`int"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"int\"|\"`int\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"int"|"`int" as n),s) ->
                       (mk_anti n s : 'a_INT )
                   | _ -> assert false)))])]);
    Gram.extend (a_INT32 : 'a_INT32 Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `INT32 (_,_) -> true | _ -> false)),
                  (`Normal, "`INT32 (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `INT32 (_,s) -> (s : 'a_INT32 )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"int32"|"`int32"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"int32\"|\"`int32\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"int32"|"`int32" as n),s) ->
                       (mk_anti n s : 'a_INT32 )
                   | _ -> assert false)))])]);
    Gram.extend (a_INT64 : 'a_INT64 Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `INT64 (_,_) -> true | _ -> false)),
                  (`Normal, "`INT64 (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `INT64 (_,s) -> (s : 'a_INT64 )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"int64"|"`int64"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"int64\"|\"`int64\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"int64"|"`int64" as n),s) ->
                       (mk_anti n s : 'a_INT64 )
                   | _ -> assert false)))])]);
    Gram.extend (a_NATIVEINT : 'a_NATIVEINT Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `NATIVEINT (_,_) -> true | _ -> false)),
                  (`Normal, "`NATIVEINT (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `NATIVEINT (_,s) -> (s : 'a_NATIVEINT )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"nativeint"|"`nativeint"),_) -> true
                  | _ -> false)),
                 (`Normal,
                   "`ANTIQUOT ((\"\"|\"nativeint\"|\"`nativeint\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"nativeint"|"`nativeint" as n),s) ->
                       (mk_anti n s : 'a_NATIVEINT )
                   | _ -> assert false)))])]);
    Gram.extend (a_FLOAT : 'a_FLOAT Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `FLOAT (_,_) -> true | _ -> false)),
                  (`Normal, "`FLOAT (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `FLOAT (_,s) -> (s : 'a_FLOAT )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"flo"|"`flo"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"flo\"|\"`flo\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"flo"|"`flo" as n),s) ->
                       (mk_anti n s : 'a_FLOAT )
                   | _ -> assert false)))])]);
    Gram.extend (a_CHAR : 'a_CHAR Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `CHAR (_,_) -> true | _ -> false)),
                  (`Normal, "`CHAR (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `CHAR (_,s) -> (s : 'a_CHAR )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"chr"|"`chr"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"chr\"|\"`chr\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"chr"|"`chr" as n),s) ->
                       (mk_anti n s : 'a_CHAR )
                   | _ -> assert false)))])]);
    Gram.extend (a_UIDENT : 'a_UIDENT Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `UID _ -> true | _ -> false)),
                  (`Normal, "`UID _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `UID s -> (s : 'a_UIDENT )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"uid"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"uid\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"uid" as n),s) ->
                       (mk_anti n s : 'a_UIDENT )
                   | _ -> assert false)))])]);
    Gram.extend (a_LIDENT : 'a_LIDENT Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `LID _ -> true | _ -> false)),
                  (`Normal, "`LID _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `LID s -> (s : 'a_LIDENT )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ((""|"lid"),_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"lid\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"lid" as n),s) ->
                       (mk_anti n s : 'a_LIDENT )
                   | _ -> assert false)))])]);
    Gram.extend (a_LABEL : 'a_LABEL Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `LABEL _ -> true | _ -> false)),
                  (`Normal, "`LABEL _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `LABEL s -> (s : 'a_LABEL )
                    | _ -> assert false)));
           ([`Skeyword "~";
            `Stoken
              (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"\",_)"));
            `Skeyword ":"],
             (Gram.mk_action
                (fun _  __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),s) -> (mk_anti n s : 'a_LABEL )
                   | _ -> assert false)))])]);
    Gram.extend (a_OPTLABEL : 'a_OPTLABEL Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `OPTLABEL _ -> true | _ -> false)),
                  (`Normal, "`OPTLABEL _"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `OPTLABEL s -> (s : 'a_OPTLABEL )
                    | _ -> assert false)));
           ([`Skeyword "?";
            `Stoken
              (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                (`Normal, "`ANTIQUOT (\"\",_)"));
            `Skeyword ":"],
             (Gram.mk_action
                (fun _  __camlp4_0  _  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),s) -> (mk_anti n s : 'a_OPTLABEL )
                   | _ -> assert false)))])]);
    Gram.extend (a_STRING : 'a_STRING Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `STR (_,_) -> true | _ -> false)),
                  (`Normal, "`STR (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `STR (_,s) -> (s : 'a_STRING )
                    | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"str"|"`str"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"str\"|\"`str\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"str"|"`str" as n),s) ->
                       (mk_anti n s : 'a_STRING )
                   | _ -> assert false)))])]);
    Gram.extend (string_list : 'string_list Gram.t )
      (None,
        [(None, None,
           [([`Stoken
                (((function | `STR (_,_) -> true | _ -> false)),
                  (`Normal, "`STR (_,_)"))],
              (Gram.mk_action
                 (fun __camlp4_0  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `STR (_,x) -> (Ast.LCons (x, Ast.LNil) : 'string_list )
                    | _ -> assert false)));
           ([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"));
            `Sself],
             (Gram.mk_action
                (fun (xs : 'string_list)  __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `STR (_,x) -> (Ast.LCons (x, xs) : 'string_list )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT ((""|"str_list"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"\"|\"str_list\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT ((""|"str_list"),s) ->
                       (Ast.LAnt (mk_anti "str_list" s) : 'string_list )
                   | _ -> assert false)))])]);
    Gram.extend (semi : 'semi Gram.t )
      (None,
        [(None, None,
           [([`Skeyword ";"],
              (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'semi ))))])]);
    Gram.extend (expr_quot : 'expr_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.ExNil _loc : 'expr_quot ))));
           ([`Snterm (Gram.obj (expr : 'expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'expr)  (_loc : FanLoc.t)  -> (e : 'expr_quot ))));
           ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword ";";
            `Snterm (Gram.obj (sem_expr : 'sem_expr Gram.t ))],
             (Gram.mk_action
                (fun (e2 : 'sem_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t)  ->
                   (Ast.ExSem (_loc, e1, e2) : 'expr_quot ))));
           ([`Snterm (Gram.obj (expr : 'expr Gram.t ));
            `Skeyword ",";
            `Snterm (Gram.obj (comma_expr : 'comma_expr Gram.t ))],
             (Gram.mk_action
                (fun (e2 : 'comma_expr)  _  (e1 : 'expr)  (_loc : FanLoc.t) 
                   -> (Ast.ExCom (_loc, e1, e2) : 'expr_quot ))))])]);
    Gram.extend (patt_quot : 'patt_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.PaNil _loc : 'patt_quot ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ))],
             (Gram.mk_action
                (fun (x : 'patt)  (_loc : FanLoc.t)  -> (x : 'patt_quot ))));
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
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ";";
            `Snterm (Gram.obj (sem_patt : 'sem_patt Gram.t ))],
             (Gram.mk_action
                (fun (y : 'sem_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaSem (_loc, x, y) : 'patt_quot ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword ",";
            `Snterm (Gram.obj (comma_patt : 'comma_patt Gram.t ))],
             (Gram.mk_action
                (fun (y : 'comma_patt)  _  (x : 'patt)  (_loc : FanLoc.t)  ->
                   (Ast.PaCom (_loc, x, y) : 'patt_quot ))))])]);
    Gram.extend (str_item_quot : 'str_item_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.StNil _loc : 'str_item_quot ))));
           ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ))],
             (Gram.mk_action
                (fun (st : 'str_item)  (_loc : FanLoc.t)  ->
                   (st : 'str_item_quot ))));
           ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (st2 : 'str_item_quot)  _  (st1 : 'str_item) 
                   (_loc : FanLoc.t)  ->
                   (match st2 with
                    | Ast.StNil _ -> st1
                    | _ -> Ast.StSem (_loc, st1, st2) : 'str_item_quot ))));
           ([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ))],
             (Gram.mk_action
                (fun (dp : 'opt_expr)  (n : 'a_LIDENT)  _  (_loc : FanLoc.t) 
                   -> (Ast.StDir (_loc, n, dp) : 'str_item_quot ))))])]);
    Gram.extend (sig_item_quot : 'sig_item_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.SgNil _loc : 'sig_item_quot ))));
           ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ))],
             (Gram.mk_action
                (fun (sg : 'sig_item)  (_loc : FanLoc.t)  ->
                   (sg : 'sig_item_quot ))));
           ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (sg2 : 'sig_item_quot)  _  (sg1 : 'sig_item) 
                   (_loc : FanLoc.t)  ->
                   (match sg2 with
                    | Ast.SgNil _ -> sg1
                    | _ -> Ast.SgSem (_loc, sg1, sg2) : 'sig_item_quot ))));
           ([`Skeyword "#";
            `Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ));
            `Snterm (Gram.obj (opt_expr : 'opt_expr Gram.t ))],
             (Gram.mk_action
                (fun (dp : 'opt_expr)  (n : 'a_LIDENT)  _  (_loc : FanLoc.t) 
                   -> (Ast.SgDir (_loc, n, dp) : 'sig_item_quot ))))])]);
    Gram.extend (module_type_quot : 'module_type_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.MtNil _loc : 'module_type_quot ))));
           ([`Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (x : 'module_type)  (_loc : FanLoc.t)  ->
                   (x : 'module_type_quot ))))])]);
    Gram.extend (module_expr_quot : 'module_expr_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.MeNil _loc : 'module_expr_quot ))));
           ([`Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (x : 'module_expr)  (_loc : FanLoc.t)  ->
                   (x : 'module_expr_quot ))))])]);
    Gram.extend (match_case_quot : 'match_case_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.McNil _loc : 'match_case_quot ))));
           ([`Slist0sep
               ((`Snterm (Gram.obj (match_case0 : 'match_case0 Gram.t ))),
                 (`Skeyword "|"))],
             (Gram.mk_action
                (fun (x : 'match_case0 list)  (_loc : FanLoc.t)  ->
                   (Ast.mcOr_of_list x : 'match_case_quot ))))])]);
    Gram.extend (binding_quot : 'binding_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> (Ast.BiNil _loc : 'binding_quot ))));
           ([`Snterm (Gram.obj (binding : 'binding Gram.t ))],
             (Gram.mk_action
                (fun (x : 'binding)  (_loc : FanLoc.t)  ->
                   (x : 'binding_quot ))))])]);
    Gram.extend (rec_binding_quot : 'rec_binding_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.RbNil _loc : 'rec_binding_quot ))));
           ([`Snterm (Gram.obj (label_expr_list : 'label_expr_list Gram.t ))],
             (Gram.mk_action
                (fun (x : 'label_expr_list)  (_loc : FanLoc.t)  ->
                   (x : 'rec_binding_quot ))))])]);
    Gram.extend (module_binding_quot : 'module_binding_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.MbNil _loc : 'module_binding_quot ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                   (m : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                   (Ast.MbColEq (_loc, m, mt, me) : 'module_binding_quot ))));
           ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  (m : 'a_UIDENT) 
                   (_loc : FanLoc.t)  ->
                   (Ast.MbCol (_loc, m, mt) : 'module_binding_quot ))));
           ([`Stoken
               (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"\",_)"));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (module_expr : 'module_expr Gram.t ))],
             (Gram.mk_action
                (fun (me : 'module_expr)  _  (mt : 'module_type)  _ 
                   __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),m) ->
                       (Ast.MbColEq (_loc, (mk_anti n m), mt, me) : 'module_binding_quot )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"\",_)"));
            `Skeyword ":";
            `Snterm (Gram.obj (module_type : 'module_type Gram.t ))],
             (Gram.mk_action
                (fun (mt : 'module_type)  _  __camlp4_0  (_loc : FanLoc.t) 
                   ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),m) ->
                       (Ast.MbCol (_loc, (mk_anti n m), mt) : 'module_binding_quot )
                   | _ -> assert false)));
           ([`Stoken
               (((function | `ANTIQUOT ("",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"\",_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("" as n),s) ->
                       (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_binding_quot )
                   | _ -> assert false)));
           ([`Stoken
               (((function
                  | `ANTIQUOT (("module_binding"|"anti"),_) -> true
                  | _ -> false)),
                 (`Normal, "`ANTIQUOT ((\"module_binding\"|\"anti\"),_)"))],
             (Gram.mk_action
                (fun __camlp4_0  (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("module_binding"|"anti" as n),s) ->
                       (Ast.MbAnt (_loc, (mk_anti ~c:"module_binding" n s)) : 
                       'module_binding_quot )
                   | _ -> assert false)));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (b2 : 'module_binding_quot)  _ 
                   (b1 : 'module_binding_quot)  (_loc : FanLoc.t)  ->
                   (Ast.MbAnd (_loc, b1, b2) : 'module_binding_quot ))))])]);
    Gram.extend (ident_quot : 'ident_quot Gram.t )
      (None,
        [((Some "apply"), None,
           [([`Sself; `Sself],
              (Gram.mk_action
                 (fun (j : 'ident_quot)  (i : 'ident_quot)  (_loc : FanLoc.t)
                     -> (Ast.IdApp (_loc, i, j) : 'ident_quot ))))]);
        ((Some "."), None,
          [([`Sself; `Skeyword "."; `Sself],
             (Gram.mk_action
                (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.IdAcc (_loc, i, j) : 'ident_quot ))))]);
        ((Some "simple"), None,
          [([`Skeyword "("; `Sself; `Skeyword ")"],
             (Gram.mk_action
                (fun _  (i : 'ident_quot)  _  (_loc : FanLoc.t)  ->
                   (i : 'ident_quot ))));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"));
           `Skeyword ".";
           `Sself],
            (Gram.mk_action
               (fun (i : 'ident_quot)  _  __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                      (Ast.IdAcc
                         (_loc, (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s))),
                           i) : 'ident_quot )
                  | _ -> assert false)));
          ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.IdLid (_loc, i) : 'ident_quot ))));
          ([`Snterm (Gram.obj (a_UIDENT : 'a_UIDENT Gram.t ))],
            (Gram.mk_action
               (fun (i : 'a_UIDENT)  (_loc : FanLoc.t)  ->
                  (Ast.IdUid (_loc, i) : 'ident_quot ))));
          ([`Stoken
              (((function
                 | `ANTIQUOT ((""|"id"|"anti"|"list"),_) -> true
                 | _ -> false)),
                (`Normal, "`ANTIQUOT ((\"\"|\"id\"|\"anti\"|\"list\"),_)"))],
            (Gram.mk_action
               (fun __camlp4_0  (_loc : FanLoc.t)  ->
                  match __camlp4_0 with
                  | `ANTIQUOT ((""|"id"|"anti"|"list" as n),s) ->
                      (Ast.IdAnt (_loc, (mk_anti ~c:"ident" n s)) : 'ident_quot )
                  | _ -> assert false)))])]);
    Gram.extend (class_expr_quot : 'class_expr_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.CeNil _loc : 'class_expr_quot ))));
           ([`Snterm (Gram.obj (class_expr : 'class_expr Gram.t ))],
             (Gram.mk_action
                (fun (x : 'class_expr)  (_loc : FanLoc.t)  ->
                   (x : 'class_expr_quot ))));
           ([`Stoken
               (((function | `ANTIQUOT ("virtual",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"virtual\",_)"));
            `Snterm (Gram.obj (ident : 'ident Gram.t ));
            `Snterm (Gram.obj (opt_comma_ctyp : 'opt_comma_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (ot : 'opt_comma_ctyp)  (i : 'ident)  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("virtual" as n),s) ->
                       (let anti = Ast.ViAnt (mk_anti ~c:"class_expr" n s) in
                        Ast.CeCon (_loc, anti, i, ot) : 'class_expr_quot )
                   | _ -> assert false)));
           ([`Skeyword "virtual";
            `Snterm
              (Gram.obj
                 (class_name_and_param : 'class_name_and_param Gram.t ))],
             (Gram.mk_action
                (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t) 
                   ->
                   (Ast.CeCon
                      (_loc, Ast.ViVirtual, (Ast.IdLid (_loc, i)), ot) : 
                   'class_expr_quot ))));
           ([`Sself; `Skeyword "="; `Sself],
             (Gram.mk_action
                (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeEq (_loc, ce1, ce2) : 'class_expr_quot ))));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (ce2 : 'class_expr_quot)  _  (ce1 : 'class_expr_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CeAnd (_loc, ce1, ce2) : 'class_expr_quot ))))])]);
    Gram.extend (class_type_quot : 'class_type_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.CtNil _loc : 'class_type_quot ))));
           ([`Snterm (Gram.obj (class_type_plus : 'class_type_plus Gram.t ))],
             (Gram.mk_action
                (fun (x : 'class_type_plus)  (_loc : FanLoc.t)  ->
                   (x : 'class_type_quot ))));
           ([`Stoken
               (((function | `ANTIQUOT ("virtual",_) -> true | _ -> false)),
                 (`Normal, "`ANTIQUOT (\"virtual\",_)"));
            `Snterm (Gram.obj (ident : 'ident Gram.t ));
            `Snterm (Gram.obj (opt_comma_ctyp : 'opt_comma_ctyp Gram.t ))],
             (Gram.mk_action
                (fun (ot : 'opt_comma_ctyp)  (i : 'ident)  __camlp4_0 
                   (_loc : FanLoc.t)  ->
                   match __camlp4_0 with
                   | `ANTIQUOT (("virtual" as n),s) ->
                       (let anti = Ast.ViAnt (mk_anti ~c:"class_type" n s) in
                        Ast.CtCon (_loc, anti, i, ot) : 'class_type_quot )
                   | _ -> assert false)));
           ([`Skeyword "virtual";
            `Snterm
              (Gram.obj
                 (class_name_and_param : 'class_name_and_param Gram.t ))],
             (Gram.mk_action
                (fun ((i,ot) : 'class_name_and_param)  _  (_loc : FanLoc.t) 
                   ->
                   (Ast.CtCon
                      (_loc, Ast.ViVirtual, (Ast.IdLid (_loc, i)), ot) : 
                   'class_type_quot ))));
           ([`Sself; `Skeyword ":"; `Sself],
             (Gram.mk_action
                (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CtCol (_loc, ct1, ct2) : 'class_type_quot ))));
           ([`Sself; `Skeyword "="; `Sself],
             (Gram.mk_action
                (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CtEq (_loc, ct1, ct2) : 'class_type_quot ))));
           ([`Sself; `Skeyword "and"; `Sself],
             (Gram.mk_action
                (fun (ct2 : 'class_type_quot)  _  (ct1 : 'class_type_quot) 
                   (_loc : FanLoc.t)  ->
                   (Ast.CtAnd (_loc, ct1, ct2) : 'class_type_quot ))))])]);
    Gram.extend (class_str_item_quot : 'class_str_item_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.CrNil _loc : 'class_str_item_quot ))));
           ([`Snterm (Gram.obj (class_str_item : 'class_str_item Gram.t ))],
             (Gram.mk_action
                (fun (x : 'class_str_item)  (_loc : FanLoc.t)  ->
                   (x : 'class_str_item_quot ))));
           ([`Snterm (Gram.obj (class_str_item : 'class_str_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (x2 : 'class_str_item_quot)  _  (x1 : 'class_str_item) 
                   (_loc : FanLoc.t)  ->
                   (match x2 with
                    | Ast.CrNil _ -> x1
                    | _ -> Ast.CrSem (_loc, x1, x2) : 'class_str_item_quot ))))])]);
    Gram.extend (class_sig_item_quot : 'class_sig_item_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.CgNil _loc : 'class_sig_item_quot ))));
           ([`Snterm (Gram.obj (class_sig_item : 'class_sig_item Gram.t ))],
             (Gram.mk_action
                (fun (x : 'class_sig_item)  (_loc : FanLoc.t)  ->
                   (x : 'class_sig_item_quot ))));
           ([`Snterm (Gram.obj (class_sig_item : 'class_sig_item Gram.t ));
            `Snterm (Gram.obj (semi : 'semi Gram.t ));
            `Sself],
             (Gram.mk_action
                (fun (x2 : 'class_sig_item_quot)  _  (x1 : 'class_sig_item) 
                   (_loc : FanLoc.t)  ->
                   (match x2 with
                    | Ast.CgNil _ -> x1
                    | _ -> Ast.CgSem (_loc, x1, x2) : 'class_sig_item_quot ))))])]);
    Gram.extend (with_constr_quot : 'with_constr_quot Gram.t )
      (None,
        [(None, None,
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  ->
                    (Ast.WcNil _loc : 'with_constr_quot ))));
           ([`Snterm (Gram.obj (with_constr : 'with_constr Gram.t ))],
             (Gram.mk_action
                (fun (x : 'with_constr)  (_loc : FanLoc.t)  ->
                   (x : 'with_constr_quot ))))])]);
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
                 (fun __camlp4_0  (x : 'patt)  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `EOI -> (x : 'patt_eoi )
                    | _ -> assert false)))])]);
    Gram.extend (expr_eoi : 'expr_eoi Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (expr : 'expr Gram.t ));
             `Stoken
               (((function | `EOI -> true | _ -> false)), (`Normal, "`EOI"))],
              (Gram.mk_action
                 (fun __camlp4_0  (x : 'expr)  (_loc : FanLoc.t)  ->
                    match __camlp4_0 with
                    | `EOI -> (x : 'expr_eoi )
                    | _ -> assert false)))])])
  end
module IdRevisedParserParser : Sig.Id =
  struct
  let name = "Camlp4OCamlRevisedParserParser"
  let version = Sys.ocaml_version
  end 
module MakeRevisedParserParser(Syntax:Sig.Camlp4Syntax) =
  struct
  include Syntax module Ast = Camlp4Ast open FanStreamTools
  let _ =
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
           [([`Skeyword "match";
             `Sself;
             `Skeyword "with";
             `Skeyword "parser";
             `Sopt
               (Gram.srules expr
                  [([`Stoken
                       (((function | `UID _ -> true | _ -> false)),
                         (`Normal, "`UID _"))],
                     (Gram.mk_action
                        (fun __camlp4_0  (_loc : FanLoc.t)  ->
                           match __camlp4_0 with
                           | `UID n -> (n : 'e__24 )
                           | _ -> assert false)))]);
             `Sopt
               (`Snterm (Gram.obj (parser_ipatt : 'parser_ipatt Gram.t )));
             `Snterm
               (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
              (Gram.mk_action
                 (fun (pcl : 'parser_case_list)  (po : 'parser_ipatt option) 
                    (name : 'e__24 option)  _  _  (e : 'expr)  _ 
                    (_loc : FanLoc.t)  ->
                    (match name with
                     | Some o ->
                         Ref.protect FanStreamTools.grammar_module_name o
                           (fun _  -> cparser_match _loc e po pcl)
                     | None  -> cparser_match _loc e po pcl : 'expr ))));
           ([`Skeyword "parser";
            `Sopt
              (Gram.srules expr
                 [([`Stoken
                      (((function | `UID _ -> true | _ -> false)),
                        (`Normal, "`UID _"))],
                    (Gram.mk_action
                       (fun __camlp4_0  (_loc : FanLoc.t)  ->
                          match __camlp4_0 with
                          | `UID n -> (n : 'e__23 )
                          | _ -> assert false)))]);
            `Sopt (`Snterm (Gram.obj (parser_ipatt : 'parser_ipatt Gram.t )));
            `Snterm (Gram.obj (parser_case_list : 'parser_case_list Gram.t ))],
             (Gram.mk_action
                (fun (pcl : 'parser_case_list)  (po : 'parser_ipatt option) 
                   (name : 'e__23 option)  _  (_loc : FanLoc.t)  ->
                   (match name with
                    | Some o ->
                        Ref.protect FanStreamTools.grammar_module_name o
                          (fun _  -> cparser _loc po pcl)
                    | None  -> cparser _loc po pcl : 'expr ))))])]);
    Gram.extend (expr : 'expr Gram.t )
      ((Some (`Level "simple")),
        [(None, None,
           [([`Snterm (Gram.obj (stream_begin : 'stream_begin Gram.t ));
             `Snterm
               (Gram.obj
                  (stream_expr_comp_list : 'stream_expr_comp_list Gram.t ));
             `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ))],
              (Gram.mk_action
                 (fun _  (sel : 'stream_expr_comp_list) 
                    (name : 'stream_begin)  (_loc : FanLoc.t)  ->
                    (match name with
                     | Some o ->
                         Ref.protect FanStreamTools.grammar_module_name o
                           (fun _  -> cstream _loc sel)
                     | None  -> cstream _loc sel : 'expr ))));
           ([`Snterm (Gram.obj (stream_begin : 'stream_begin Gram.t ));
            `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ))],
             (Gram.mk_action
                (fun _  (name : 'stream_begin)  (_loc : FanLoc.t)  ->
                   (match name with
                    | Some o ->
                        Ref.protect FanStreamTools.grammar_module_name o
                          (fun _  -> FanStreamTools.empty _loc)
                    | None  -> FanStreamTools.empty _loc : 'expr ))))])]);
    Gram.extend (parser_ipatt : 'parser_ipatt Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "_"],
              (Gram.mk_action
                 (fun _  (_loc : FanLoc.t)  ->
                    (Ast.PaAny _loc : 'parser_ipatt ))));
           ([`Snterm (Gram.obj (a_LIDENT : 'a_LIDENT Gram.t ))],
             (Gram.mk_action
                (fun (i : 'a_LIDENT)  (_loc : FanLoc.t)  ->
                   (Ast.PaId (_loc, (Ast.IdLid (_loc, i))) : 'parser_ipatt ))))])]);
    Gram.extend (parser_case_list : 'parser_case_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))],
              (Gram.mk_action
                 (fun (pc : 'parser_case)  (_loc : FanLoc.t)  ->
                    ([pc] : 'parser_case_list ))));
           ([`Skeyword "[";
            `Slist0sep
              ((`Snterm (Gram.obj (parser_case : 'parser_case Gram.t ))),
                (`Skeyword "|"));
            `Skeyword "]"],
             (Gram.mk_action
                (fun _  (pcl : 'parser_case list)  _  (_loc : FanLoc.t)  ->
                   (pcl : 'parser_case_list ))))])]);
    Gram.extend (parser_case : 'parser_case Gram.t )
      (None,
        [(None, None,
           [([`Skeyword "[<";
             `Snterm (Gram.obj (stream_patt : 'stream_patt Gram.t ));
             `Snterm (Gram.obj (stream_end : 'stream_end Gram.t ));
             `Sopt
               (`Snterm (Gram.obj (parser_ipatt : 'parser_ipatt Gram.t )));
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
                        (fun __camlp4_0  _  (_loc : FanLoc.t)  ->
                           match __camlp4_0 with
                           | `UID n -> (n : 'e__25 )
                           | _ -> assert false)))])],
              (Gram.mk_action
                 (fun (name : 'e__25 option)  _  (_loc : FanLoc.t)  ->
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
           [([],
              (Gram.mk_action
                 (fun (_loc : FanLoc.t)  -> ([] : 'stream_patt ))));
           ([`Snterm
               (Gram.obj (stream_patt_comp : 'stream_patt_comp Gram.t ));
            `Skeyword ";";
            `Snterm
              (Gram.obj
                 (stream_patt_comp_err_list : 'stream_patt_comp_err_list
                                                Gram.t ))],
             (Gram.mk_action
                (fun (sp : 'stream_patt_comp_err_list)  _ 
                   (spc : 'stream_patt_comp)  (_loc : FanLoc.t)  ->
                   ((spc, None) :: sp : 'stream_patt ))));
           ([`Snterm
               (Gram.obj (stream_patt_comp : 'stream_patt_comp Gram.t ))],
             (Gram.mk_action
                (fun (spc : 'stream_patt_comp)  (_loc : FanLoc.t)  ->
                   ([(spc, None)] : 'stream_patt ))))])]);
    Gram.extend (stream_patt_comp : 'stream_patt_comp Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (stream_quot : 'stream_quot Gram.t ));
             `Snterm (Gram.obj (patt : 'patt Gram.t ))],
              (Gram.mk_action
                 (fun (p : 'patt)  _  (_loc : FanLoc.t)  ->
                    (SpStr (_loc, p) : 'stream_patt_comp ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Skeyword "=";
            `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'stream_expr)  _  (p : 'patt)  (_loc : FanLoc.t) 
                   -> (SpNtr (_loc, p, e) : 'stream_patt_comp ))));
           ([`Snterm (Gram.obj (patt : 'patt Gram.t ));
            `Sopt
              (Gram.srules stream_patt_comp
                 [([`Skeyword "when";
                   `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
                    (Gram.mk_action
                       (fun (e : 'stream_expr)  _  (_loc : FanLoc.t)  ->
                          (e : 'e__26 ))))])],
             (Gram.mk_action
                (fun (eo : 'e__26 option)  (p : 'patt)  (_loc : FanLoc.t)  ->
                   (SpTrm (_loc, p, eo) : 'stream_patt_comp ))))])]);
    Gram.extend (stream_patt_comp_err : 'stream_patt_comp_err Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj (stream_patt_comp : 'stream_patt_comp Gram.t ));
             `Sopt
               (Gram.srules stream_patt_comp_err
                  [([`Skeyword "??";
                    `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
                     (Gram.mk_action
                        (fun (e : 'stream_expr)  _  (_loc : FanLoc.t)  ->
                           (e : 'e__27 ))))])],
              (Gram.mk_action
                 (fun (eo : 'e__27 option)  (spc : 'stream_patt_comp) 
                    (_loc : FanLoc.t)  ->
                    ((spc, eo) : 'stream_patt_comp_err ))))])]);
    Gram.extend
      (stream_patt_comp_err_list : 'stream_patt_comp_err_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj
                   (stream_patt_comp_err : 'stream_patt_comp_err Gram.t ));
             `Skeyword ";";
             `Sself],
              (Gram.mk_action
                 (fun (sp : 'stream_patt_comp_err_list)  _ 
                    (spc : 'stream_patt_comp_err)  (_loc : FanLoc.t)  -> (spc
                    :: sp : 'stream_patt_comp_err_list ))));
           ([`Snterm
               (Gram.obj
                  (stream_patt_comp_err : 'stream_patt_comp_err Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (spc : 'stream_patt_comp_err)  (_loc : FanLoc.t)  ->
                   ([spc] : 'stream_patt_comp_err_list ))));
           ([`Snterm
               (Gram.obj
                  (stream_patt_comp_err : 'stream_patt_comp_err Gram.t ))],
             (Gram.mk_action
                (fun (spc : 'stream_patt_comp_err)  (_loc : FanLoc.t)  ->
                   ([spc] : 'stream_patt_comp_err_list ))))])]);
    Gram.extend (stream_expr_comp_list : 'stream_expr_comp_list Gram.t )
      (None,
        [(None, None,
           [([`Snterm
                (Gram.obj (stream_expr_comp : 'stream_expr_comp Gram.t ))],
              (Gram.mk_action
                 (fun (se : 'stream_expr_comp)  (_loc : FanLoc.t)  ->
                    ([se] : 'stream_expr_comp_list ))));
           ([`Snterm
               (Gram.obj (stream_expr_comp : 'stream_expr_comp Gram.t ));
            `Skeyword ";"],
             (Gram.mk_action
                (fun _  (se : 'stream_expr_comp)  (_loc : FanLoc.t)  ->
                   ([se] : 'stream_expr_comp_list ))));
           ([`Snterm
               (Gram.obj (stream_expr_comp : 'stream_expr_comp Gram.t ));
            `Skeyword ";";
            `Sself],
             (Gram.mk_action
                (fun (sel : 'stream_expr_comp_list)  _ 
                   (se : 'stream_expr_comp)  (_loc : FanLoc.t)  -> (se ::
                   sel : 'stream_expr_comp_list ))))])]);
    Gram.extend (stream_expr_comp : 'stream_expr_comp Gram.t )
      (None,
        [(None, None,
           [([`Snterm (Gram.obj (stream_quot : 'stream_quot Gram.t ));
             `Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
              (Gram.mk_action
                 (fun (e : 'stream_expr)  _  (_loc : FanLoc.t)  ->
                    (SeNtr (_loc, e) : 'stream_expr_comp ))));
           ([`Snterm (Gram.obj (stream_expr : 'stream_expr Gram.t ))],
             (Gram.mk_action
                (fun (e : 'stream_expr)  (_loc : FanLoc.t)  ->
                   (SeTrm (_loc, e) : 'stream_expr_comp ))))])])
  end
module IdQuotationCommon = struct
  let name = "Camlp4QuotationCommon" let version = Sys.ocaml_version
  end
module MakeQuotationCommon(Syntax:Sig.Camlp4Syntax) = struct
  include Syntax open Quotation open Meta
  let _ =
    add_quotation "sig_item" sig_item_quot ME.meta_sig_item MP.meta_sig_item
  let _ =
    add_quotation "str_item" str_item_quot ME.meta_str_item MP.meta_str_item
  let _ = add_quotation "ctyp" ctyp_quot ME.meta_ctyp MP.meta_ctyp
  let _ = add_quotation "patt" patt_quot ME.meta_patt MP.meta_patt
  let _ = add_quotation "expr" expr_quot ME.meta_expr MP.meta_expr
  let _ =
    add_quotation "module_type" module_type_quot ME.meta_module_type
      MP.meta_module_type
  let _ =
    add_quotation "module_expr" module_expr_quot ME.meta_module_expr
      MP.meta_module_expr
  let _ =
    add_quotation "class_type" class_type_quot ME.meta_class_type
      MP.meta_class_type
  let _ =
    add_quotation "class_expr" class_expr_quot ME.meta_class_expr
      MP.meta_class_expr
  let _ =
    add_quotation "class_sig_item" class_sig_item_quot ME.meta_class_sig_item
      MP.meta_class_sig_item
  let _ =
    add_quotation "class_str_item" class_str_item_quot ME.meta_class_str_item
      MP.meta_class_str_item
  let _ =
    add_quotation "with_constr" with_constr_quot ME.meta_with_constr
      MP.meta_with_constr
  let _ =
    add_quotation "binding" binding_quot ME.meta_binding MP.meta_binding
  let _ =
    add_quotation "rec_binding" rec_binding_quot ME.meta_rec_binding
      MP.meta_rec_binding
  let _ =
    add_quotation "match_case" match_case_quot ME.meta_match_case
      MP.meta_match_case
  let _ =
    add_quotation "module_binding" module_binding_quot ME.meta_module_binding
      MP.meta_module_binding
  let _ = add_quotation "ident" ident_quot ME.meta_ident MP.meta_ident
  let _ =
    add_quotation "rec_flag" rec_flag_quot ME.meta_rec_flag MP.meta_rec_flag
  let _ =
    add_quotation "private_flag" private_flag_quot ME.meta_private_flag
      MP.meta_private_flag
  let _ =
    add_quotation "row_var_flag" row_var_flag_quot ME.meta_row_var_flag
      MP.meta_row_var_flag
  let _ =
    add_quotation "mutable_flag" mutable_flag_quot ME.meta_mutable_flag
      MP.meta_mutable_flag
  let _ =
    add_quotation "virtual_flag" virtual_flag_quot ME.meta_virtual_flag
      MP.meta_virtual_flag
  let _ =
    add_quotation "override_flag" override_flag_quot ME.meta_override_flag
      MP.meta_override_flag
  let _ =
    add_quotation "direction_flag" direction_flag_quot ME.meta_direction_flag
      MP.meta_direction_flag
  let _ =
    Options.add
      ("-dlang", (FanArg.Set_string Quotation.default),
        " Set the default language")
  end
module IdQuotationExpander = struct
  let name = "Camlp4QuotationExpander" let version = Sys.ocaml_version
  end
module MakeQuotationExpander(Syntax:Sig.Camlp4Syntax) =
  struct
  module M = MakeQuotationCommon(Syntax) include M
  end
let pa_r = "Camlp4OCamlRevisedParser"
let pa_r ((module P)  : (module Sig.PRECAST)) =
  P.syntax_extension (module IdRevisedParser) (module MakeRevisedParser)
let pa_rp ((module P)  : (module Sig.PRECAST)) =
  P.syntax_extension (module IdRevisedParserParser) (module
    MakeRevisedParserParser)
let pa_g ((module P)  : (module Sig.PRECAST)) =
  P.syntax_extension (module IdGrammarParser) (module MakeGrammarParser)
let pa_m ((module P)  : (module Sig.PRECAST)) =
  let () = P.syntax_extension (module IdMacroParser) (module MakeMacroParser) in
  P.syntax_plugin (module IdMacroParser) (module MakeNothing)
let pa_q ((module P)  : (module Sig.PRECAST)) =
  P.syntax_extension (module IdQuotationExpander) (module
    MakeQuotationExpander)
let pa_l ((module P)  : (module Sig.PRECAST)) =
  P.syntax_extension (module IdListComprehension) (module
    MakeListComprehension)
let pa_debug ((module P)  : (module Sig.PRECAST)) =
  P.syntax_extension (module IdDebugParser) (module MakeDebugParser)
