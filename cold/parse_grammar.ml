open FAst
open AstLib
open FGramDef
open FGramGen
open! Fsyntax
open LibUtil
let nonterminals: stru Fgram.t = Fgram.mk "nonterminals"
let nonterminalsclear: exp Fgram.t = Fgram.mk "nonterminalsclear"
let delete_rule_header = Fgram.mk "delete_rule_header"
let extend_header = Fgram.mk "extend_header"
let qualuid: vid Fgram.t = Fgram.mk "qualuid"
let qualid: vid Fgram.t = Fgram.mk "qualid"
let t_qualid: vid Fgram.t = Fgram.mk "t_qualid"
let entry_name: ([ `name of FToken.name | `non]* FGramDef.name) Fgram.t =
  Fgram.mk "entry_name"
let entry = Fgram.mk "entry"
let position = Fgram.mk "position"
let assoc = Fgram.mk "assoc"
let name = Fgram.mk "name"
let string = Fgram.mk "string"
let rules = Fgram.mk "rules"
let symbol = Fgram.mk "symbol"
let rule = Fgram.mk "rule"
let meta_rule = Fgram.mk "meta_rule"
let rule_list = Fgram.mk "rule_list"
let psymbol = Fgram.mk "psymbol"
let level = Fgram.mk "level"
let level_list = Fgram.mk "level_list"
let entry: FGramDef.entry Fgram.t = Fgram.mk "entry"
let pattern: action_pattern Fgram.t = Fgram.mk "pattern"
let extend_body = Fgram.mk "extend_body"
let newterminals = Fgram.mk "newterminals"
let unsafe_extend_body = Fgram.mk "unsafe_extend_body"
let delete_rule_body = Fgram.mk "delete_rule_body"
let simple_exp = Fgram.mk "simple_exp"
let delete_rules = Fgram.mk "delete_rules"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let ty: 'ty Fgram.t = grammar_entry_create "ty"
  and str: 'str Fgram.t = grammar_entry_create "str"
  and type_entry: 'type_entry Fgram.t = grammar_entry_create "type_entry"
  and psymbols: 'psymbols Fgram.t = grammar_entry_create "psymbols"
  and opt_action: 'opt_action Fgram.t = grammar_entry_create "opt_action"
  and brace_pattern: 'brace_pattern Fgram.t =
    grammar_entry_create "brace_pattern"
  and sep_symbol: 'sep_symbol Fgram.t = grammar_entry_create "sep_symbol"
  and level_str: 'level_str Fgram.t = grammar_entry_create "level_str" in
  Fgram.extend_single (ty : 'ty Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
          `Skeyword ")"],
           ("`Dyn (x, t)\n",
             (Fgram.mk_action
                (fun _  (t : 't_qualid)  _  (x : 'qualid)  _  (_loc : FLoc.t)
                    -> (`Dyn (x, t) : 'ty )))));
        ([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
          ("`Static t\n",
            (Fgram.mk_action
               (fun (t : 'qualuid)  (_loc : FLoc.t)  -> (`Static t : 'ty )))));
        ([],
          ("`Static (`Uid (_loc, \"Fgram\"))\n",
            (Fgram.mk_action
               (fun (_loc : FLoc.t)  ->
                  (`Static (`Uid (_loc, "Fgram")) : 'ty )))))]));
  Fgram.extend_single (nonterminals : 'nonterminals Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (ty : 'ty Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (type_entry : 'type_entry Fgram.t )))],
           ("let mk =\n  match t with\n  | `Static t ->\n      let t = (t : vid  :>exp) in\n      (`Field (_loc, t, (`Lid (_loc, \"mk\"))) : FAst.exp )\n  | `Dyn (x,t) ->\n      let x = (x : vid  :>exp) in\n      let t = (t : vid  :>exp) in\n      (`App (_loc, (`Field (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n        FAst.exp ) in\nsem_of_list\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : FAst.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            FAst.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : FAst.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            FAst.stru )) ls)\n",
             (Fgram.mk_action
                (fun (ls : 'type_entry list)  (t : 'ty)  (_loc : FLoc.t)  ->
                   (let mk =
                      match t with
                      | `Static t ->
                          let t = (t : vid  :>exp) in
                          (`Field (_loc, t, (`Lid (_loc, "mk"))) : FAst.exp )
                      | `Dyn (x,t) ->
                          let x = (x : vid  :>exp) in
                          let t = (t : vid  :>exp) in
                          (`App
                             (_loc,
                               (`Field (_loc, t, (`Lid (_loc, "mk_dynamic")))),
                               x) : FAst.exp ) in
                    sem_of_list
                      (List.map
                         (fun (_loc,x,descr,ty)  ->
                            match (descr, ty) with
                            | (Some d,None ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`App (_loc, mk, (`Str (_loc, d))))))) : 
                                FAst.stru )
                            | (Some d,Some typ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`Constraint
                                             (_loc,
                                               (`App
                                                  (_loc, mk,
                                                    (`Str (_loc, d)))), typ))))) : 
                                FAst.stru )
                            | (None ,None ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`App (_loc, mk, (`Str (_loc, x))))))) : 
                                FAst.stru )
                            | (None ,Some typ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`Constraint
                                             (_loc,
                                               (`App
                                                  (_loc, mk,
                                                    (`Str (_loc, x)))), typ))))) : 
                                FAst.stru )) ls) : 'nonterminals )))))]));
  Fgram.extend_single (str : 'str Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)),
               (`App ((`Vrn "Str"), `Any)), "`Str _")],
           ("y\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Str y -> (y : 'str )
                   | _ -> failwith "y\n"))))]));
  Fgram.extend_single (type_entry : 'type_entry Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("(_loc, x, None, None)\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Lid x -> ((_loc, x, None, None) : 'type_entry )
                   | _ -> failwith "(_loc, x, None, None)\n"))));
        ([`Skeyword "(";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _");
         `Stoken
           (((function | `Str _ -> true | _ -> false)),
             (`App ((`Vrn "Str"), `Any)), "`Str _");
         `Skeyword ")"],
          ("(_loc, x, (Some y), None)\n",
            (Fgram.mk_action
               (fun _  (__fan_2 : [> FToken.t])  (__fan_1 : [> FToken.t])  _ 
                  (_loc : FLoc.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`Str y,`Lid x) ->
                      ((_loc, x, (Some y), None) : 'type_entry )
                  | _ -> failwith "(_loc, x, (Some y), None)\n"))));
        ([`Skeyword "(";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _");
         `Stoken
           (((function | `Str _ -> true | _ -> false)),
             (`App ((`Vrn "Str"), `Any)), "`Str _");
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
         `Skeyword ")"],
          ("(_loc, x, (Some y), (Some t))\n",
            (Fgram.mk_action
               (fun _  (t : 'ctyp)  (__fan_2 : [> FToken.t]) 
                  (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`Str y,`Lid x) ->
                      ((_loc, x, (Some y), (Some t)) : 'type_entry )
                  | _ -> failwith "(_loc, x, (Some y), (Some t))\n"))));
        ([`Skeyword "(";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _");
         `Skeyword ":";
         `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
         `Sopt (`Snterm (Fgram.obj (str : 'str Fgram.t )));
         `Skeyword ")"],
          ("(_loc, x, y, (Some t))\n",
            (Fgram.mk_action
               (fun _  (y : 'str option)  (t : 'ctyp)  _ 
                  (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                  match __fan_1 with
                  | `Lid x -> ((_loc, x, y, (Some t)) : 'type_entry )
                  | _ -> failwith "(_loc, x, y, (Some t))\n"))))]));
  Fgram.extend_single (newterminals : 'newterminals Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
          `Skeyword ")";
          `Slist1 (`Snterm (Fgram.obj (type_entry : 'type_entry Fgram.t )))],
           ("let mk =\n  let x = (x : vid  :>exp) in\n  (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : FAst.exp ) in\nsem_of_list\n  ((`Value\n      (_loc, (`Negative _loc),\n        (`Bind\n           (_loc, (x :>pat),\n             (`App\n                (_loc,\n                  (`App\n                     (_loc,\n                       (`App\n                          (_loc,\n                            (`Dot (_loc, t, (`Lid (_loc, \"create_lexer\")))),\n                            (`Label\n                               (_loc, (`Lid (_loc, \"annot\")),\n                                 (`Str (_loc, \"\")))))),\n                       (`Label\n                          (_loc, (`Lid (_loc, \"keywords\")),\n                            (`Uid (_loc, \"[]\")))))), (`Uid (_loc, \"()\"))))))) : \n  FAst.stru ) ::\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : FAst.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            FAst.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : FAst.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            FAst.stru )) ls))\n",
             (Fgram.mk_action
                (fun (ls : 'type_entry list)  _  (t : 't_qualid)  _ 
                   (x : 'qualid)  _  (_loc : FLoc.t)  ->
                   (let mk =
                      let x = (x : vid  :>exp) in
                      (`App
                         (_loc,
                           (`Dot (_loc, t, (`Lid (_loc, "mk_dynamic")))), x) : 
                        FAst.exp ) in
                    sem_of_list
                      ((`Value
                          (_loc, (`Negative _loc),
                            (`Bind
                               (_loc, (x :>pat),
                                 (`App
                                    (_loc,
                                      (`App
                                         (_loc,
                                           (`App
                                              (_loc,
                                                (`Dot
                                                   (_loc, t,
                                                     (`Lid
                                                        (_loc,
                                                          "create_lexer")))),
                                                (`Label
                                                   (_loc,
                                                     (`Lid (_loc, "annot")),
                                                     (`Str (_loc, "")))))),
                                           (`Label
                                              (_loc,
                                                (`Lid (_loc, "keywords")),
                                                (`Uid (_loc, "[]")))))),
                                      (`Uid (_loc, "()"))))))) : FAst.stru )
                      ::
                      (List.map
                         (fun (_loc,x,descr,ty)  ->
                            match (descr, ty) with
                            | (Some d,None ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`App (_loc, mk, (`Str (_loc, d))))))) : 
                                FAst.stru )
                            | (Some d,Some typ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`Constraint
                                             (_loc,
                                               (`App
                                                  (_loc, mk,
                                                    (`Str (_loc, d)))), typ))))) : 
                                FAst.stru )
                            | (None ,None ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`App (_loc, mk, (`Str (_loc, x))))))) : 
                                FAst.stru )
                            | (None ,Some typ) ->
                                (`Value
                                   (_loc, (`Negative _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`Constraint
                                             (_loc,
                                               (`App
                                                  (_loc, mk,
                                                    (`Str (_loc, x)))), typ))))) : 
                                FAst.stru )) ls)) : 'newterminals )))))]));
  Fgram.extend_single (nonterminalsclear : 'nonterminalsclear Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t )))],
           ("let rest =\n  List.map\n    (fun (x : alident)  ->\n       let x = (x : alident  :>exp) in\n       let _loc = loc_of x in\n       let t = (t : vid  :>exp) in\n       (`App (_loc, (`Field (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n         FAst.exp )) ls in\nseq_sem rest\n",
             (Fgram.mk_action
                (fun (ls : 'a_lident list)  (t : 'qualuid)  (_loc : FLoc.t) 
                   ->
                   (let rest =
                      List.map
                        (fun (x : alident)  ->
                           let x = (x : alident  :>exp) in
                           let _loc = loc_of x in
                           let t = (t : vid  :>exp) in
                           (`App
                              (_loc,
                                (`Field (_loc, t, (`Lid (_loc, "clear")))),
                                x) : FAst.exp )) ls in
                    seq_sem rest : 'nonterminalsclear )))))]));
  Fgram.extend_single (extend_header : 'extend_header Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
          `Skeyword ")"],
           ("let old = gm () in let () = grammar_module_name := t in ((Some i), old)\n",
             (Fgram.mk_action
                (fun _  (t : 't_qualid)  _  (i : 'qualid)  _  (_loc : FLoc.t)
                    ->
                   (let old = gm () in
                    let () = grammar_module_name := t in ((Some i), old) : 
                   'extend_header )))));
        ([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
          ("let old = gm () in let () = grammar_module_name := t in (None, old)\n",
            (Fgram.mk_action
               (fun (t : 'qualuid)  (_loc : FLoc.t)  ->
                  (let old = gm () in
                   let () = grammar_module_name := t in (None, old) : 
                  'extend_header )))));
        ([],
          ("(None, (gm ()))\n",
            (Fgram.mk_action
               (fun (_loc : FLoc.t)  -> ((None, (gm ())) : 'extend_header )))))]));
  Fgram.extend_single (extend_body : 'extend_body Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (extend_header : 'extend_header Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (entry : 'entry Fgram.t )))],
           ("let res = text_of_functorial_extend _loc gram el in\nlet () = grammar_module_name := old in res\n",
             (Fgram.mk_action
                (fun (el : 'entry list)  ((gram,old) : 'extend_header) 
                   (_loc : FLoc.t)  ->
                   (let res = text_of_functorial_extend _loc gram el in
                    let () = grammar_module_name := old in res : 'extend_body )))))]));
  Fgram.extend_single (unsafe_extend_body : 'unsafe_extend_body Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (extend_header : 'extend_header Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (entry : 'entry Fgram.t )))],
           ("let res = text_of_functorial_extend ~safe:false _loc gram el in\nlet () = grammar_module_name := old in res\n",
             (Fgram.mk_action
                (fun (el : 'entry list)  ((gram,old) : 'extend_header) 
                   (_loc : FLoc.t)  ->
                   (let res =
                      text_of_functorial_extend ~safe:false _loc gram el in
                    let () = grammar_module_name := old in res : 'unsafe_extend_body )))))]));
  Fgram.extend_single (delete_rule_header : 'delete_rule_header Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
           ("let old = gm () in let () = grammar_module_name := g in old\n",
             (Fgram.mk_action
                (fun (g : 'qualuid)  (_loc : FLoc.t)  ->
                   (let old = gm () in
                    let () = grammar_module_name := g in old : 'delete_rule_header )))))]));
  Fgram.extend_single (delete_rule_body : 'delete_rule_body Fgram.t )
    (None,
      (None, None,
        [([`Snterm
             (Fgram.obj (delete_rule_header : 'delete_rule_header Fgram.t ));
          `Slist1
            (`Snterm (Fgram.obj (delete_rules : 'delete_rules Fgram.t )))],
           ("grammar_module_name := old; seq_sem es\n",
             (Fgram.mk_action
                (fun (es : 'delete_rules list)  (old : 'delete_rule_header) 
                   (_loc : FLoc.t)  ->
                   (grammar_module_name := old; seq_sem es : 'delete_rule_body )))))]));
  Fgram.extend_single (delete_rules : 'delete_rules Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (name : 'name Fgram.t ));
          `Skeyword ":";
          `Skeyword "[";
          `Slist1sep
            ((`Snterm (Fgram.obj (psymbols : 'psymbols Fgram.t ))),
              (`Skeyword "|"));
          `Skeyword "]"],
           ("exp_delete_rule _loc n sls\n",
             (Fgram.mk_action
                (fun _  (sls : 'psymbols list)  _  _  (n : 'name) 
                   (_loc : FLoc.t)  ->
                   (exp_delete_rule _loc n sls : 'delete_rules )))))]));
  Fgram.extend_single (psymbols : 'psymbols Fgram.t )
    (None,
      (None, None,
        [([`Slist0sep
             ((`Snterm (Fgram.obj (psymbol : 'psymbol Fgram.t ))),
               (`Skeyword ";"))],
           ("sl\n",
             (Fgram.mk_action
                (fun (sl : 'psymbol list)  (_loc : FLoc.t)  ->
                   (sl : 'psymbols )))))]));
  Fgram.extend_single (qualuid : 'qualuid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`App ((`Vrn "Uid"), `Any)), "`Uid _");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualuid)  _  (__fan_0 : [> FToken.t]) 
                   (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Uid x ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Uid x -> (`Uid (_loc, x) : 'qualuid )
                  | _ -> failwith "`Uid (_loc, x)\n"))))]));
  Fgram.extend_single (qualid : 'qualid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`App ((`Vrn "Uid"), `Any)), "`Uid _");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualid)  _  (__fan_0 : [> FToken.t]) 
                   (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("`Lid (_loc, i)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Lid i -> (`Lid (_loc, i) : 'qualid )
                  | _ -> failwith "`Lid (_loc, i)\n"))))]));
  Fgram.extend_single (t_qualid : 't_qualid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`App ((`Vrn "Uid"), `Any)), "`Uid _");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 't_qualid)  _  (__fan_0 : [> FToken.t]) 
                   (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Uid x ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _");
         `Skeyword ".";
         `Stoken
           (((function | `Lid "t" -> true | _ -> false)),
             (`App ((`Vrn "Lid"), (`Str "t"))), "`Lid \"t\"")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_2 : [> FToken.t])  _  (__fan_0 : [> FToken.t]) 
                  (_loc : FLoc.t)  ->
                  match (__fan_2, __fan_0) with
                  | (`Lid "t",`Uid x) -> (`Uid (_loc, x) : 't_qualid )
                  | _ -> failwith "`Uid (_loc, x)\n"))))]));
  Fgram.extend_single (name : 'name Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualid : 'qualid Fgram.t ))],
           ("mk_name _loc il\n",
             (Fgram.mk_action
                (fun (il : 'qualid)  (_loc : FLoc.t)  ->
                   (mk_name _loc il : 'name )))))]));
  Fgram.extend_single (entry_name : 'entry_name Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
          `Sopt (`Snterm (Fgram.obj (str : 'str Fgram.t )))],
           ("((match name with\n  | Some x ->\n      let old = AstQuotation.default.contents in\n      (AstQuotation.default := (FToken.resolve_name _loc ((`Sub []), x));\n       `name old)\n  | None  -> `non), (mk_name _loc il))\n",
             (Fgram.mk_action
                (fun (name : 'str option)  (il : 'qualid)  (_loc : FLoc.t) 
                   ->
                   (((match name with
                      | Some x ->
                          let old = AstQuotation.default.contents in
                          (AstQuotation.default :=
                             (FToken.resolve_name _loc ((`Sub []), x));
                           `name old)
                      | None  -> `non), (mk_name _loc il)) : 'entry_name )))))]));
  Fgram.extend_single (entry : 'entry Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (entry_name : 'entry_name Fgram.t ));
          `Skeyword ":";
          `Sopt (`Snterm (Fgram.obj (position : 'position Fgram.t )));
          `Snterm (Fgram.obj (level_list : 'level_list Fgram.t ))],
           ("(match n with | `name old -> AstQuotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> mk_entry ~local:false ~name:p ~pos ~levels)\n",
             (Fgram.mk_action
                (fun (levels : 'level_list)  (pos : 'position option)  _ 
                   ((n,p) : 'entry_name)  (_loc : FLoc.t)  ->
                   ((match n with
                     | `name old -> AstQuotation.default := old
                     | _ -> ());
                    (match (pos, levels) with
                     | (Some
                        (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),`Group _)
                         ->
                         failwithf
                           "For Group levels the position can not be applied to Level"
                     | _ -> mk_entry ~local:false ~name:p ~pos ~levels) : 
                   'entry )))));
        ([`Skeyword "let";
         `Snterm (Fgram.obj (entry_name : 'entry_name Fgram.t ));
         `Skeyword ":";
         `Sopt (`Snterm (Fgram.obj (position : 'position Fgram.t )));
         `Snterm (Fgram.obj (level_list : 'level_list Fgram.t ))],
          ("(match n with | `name old -> AstQuotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> mk_entry ~local:true ~name:p ~pos ~levels)\n",
            (Fgram.mk_action
               (fun (levels : 'level_list)  (pos : 'position option)  _ 
                  ((n,p) : 'entry_name)  _  (_loc : FLoc.t)  ->
                  ((match n with
                    | `name old -> AstQuotation.default := old
                    | _ -> ());
                   (match (pos, levels) with
                    | (Some
                       (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),`Group _)
                        ->
                        failwithf
                          "For Group levels the position can not be applied to Level"
                    | _ -> mk_entry ~local:true ~name:p ~pos ~levels) : 
                  'entry )))))]));
  Fgram.extend_single (position : 'position Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("First"|"Last") -> true | _ -> false)),
               (`App ((`Vrn "Uid"), (`Bar ((`Str "First"), (`Str "Last"))))),
               "`Uid \"First\"| \"Last\"")],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("First"|"Last" as x) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                   | _ -> failwith "(`Vrn (_loc, x) : FAst.exp )\n"))));
        ([`Stoken
            (((function
               | `Uid ("Before"|"After"|"Level") -> true
               | _ -> false)),
              (`App
                 ((`Vrn "Uid"),
                   (`Bar
                      ((`Bar ((`Str "Before"), (`Str "After"))),
                        (`Str "Level"))))),
              "`Uid \"Before\"| \"After\"| \"Level\"");
         `Snterm (Fgram.obj (string : 'string Fgram.t ))],
          ("(`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (n : 'string)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t) 
                  ->
                  match __fan_0 with
                  | `Uid ("Before"|"After"|"Level" as x) ->
                      ((`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp ) : 
                      'position )
                  | _ ->
                      failwith
                        "(`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp )\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("failwithf \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n  x\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Uid x ->
                      (failwithf
                         "%s is not the right position:(First|Last) or (Before|After|Level)"
                         x : 'position )
                  | _ ->
                      failwith
                        "failwithf \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n  x\n"))))]));
  Fgram.extend_single (level_list : 'level_list Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "{";
          `Slist1 (`Snterm (Fgram.obj (level : 'level Fgram.t )));
          `Skeyword "}"],
           ("`Group ll\n",
             (Fgram.mk_action
                (fun _  (ll : 'level list)  _  (_loc : FLoc.t)  ->
                   (`Group ll : 'level_list )))));
        ([`Snterm (Fgram.obj (level : 'level Fgram.t ))],
          ("`Single l\n",
            (Fgram.mk_action
               (fun (l : 'level)  (_loc : FLoc.t)  ->
                  (`Single l : 'level_list )))))]));
  Fgram.extend_single (level : 'level Fgram.t )
    (None,
      (None, None,
        [([`Sopt (`Snterm (Fgram.obj (str : 'str Fgram.t )));
          `Sopt (`Snterm (Fgram.obj (assoc : 'assoc Fgram.t )));
          `Snterm (Fgram.obj (rule_list : 'rule_list Fgram.t ))],
           ("mk_level ~label ~assoc ~rules\n",
             (Fgram.mk_action
                (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                   (label : 'str option)  (_loc : FLoc.t)  ->
                   (mk_level ~label ~assoc ~rules : 'level )))))]));
  Fgram.extend_single (assoc : 'assoc Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("LA"|"RA"|"NA") -> true | _ -> false)),
               (`App
                  ((`Vrn "Uid"),
                    (`Bar ((`Bar ((`Str "LA"), (`Str "RA"))), (`Str "NA"))))),
               "`Uid \"LA\"| \"RA\"| \"NA\"")],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("LA"|"RA"|"NA" as x) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                   | _ -> failwith "(`Vrn (_loc, x) : FAst.exp )\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Uid x ->
                      (failwithf
                         "%s is not a correct associativity:(LA|RA|NA)" x : 
                      'assoc )
                  | _ ->
                      failwith
                        "failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x\n"))))]));
  Fgram.extend_single (rule_list : 'rule_list Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "["; `Skeyword "]"],
           ("[]\n",
             (Fgram.mk_action
                (fun _  _  (_loc : FLoc.t)  -> ([] : 'rule_list )))));
        ([`Skeyword "[";
         `Slist1sep
           ((`Snterm (Fgram.obj (rule : 'rule Fgram.t ))), (`Skeyword "|"));
         `Skeyword "]"],
          ("retype_rule_list_without_patterns _loc rules\n",
            (Fgram.mk_action
               (fun _  (rules : 'rule list)  _  (_loc : FLoc.t)  ->
                  (retype_rule_list_without_patterns _loc rules : 'rule_list )))))]));
  Fgram.extend_single (rule : 'rule Fgram.t )
    (None,
      (None, None,
        [([`Slist0sep
             ((`Snterm (Fgram.obj (psymbol : 'psymbol Fgram.t ))),
               (`Skeyword ";"));
          `Sopt (`Snterm (Fgram.obj (opt_action : 'opt_action Fgram.t )))],
           ("mk_rule ~prod ~action\n",
             (Fgram.mk_action
                (fun (action : 'opt_action option)  (prod : 'psymbol list) 
                   (_loc : FLoc.t)  -> (mk_rule ~prod ~action : 'rule )))))]));
  Fgram.extend_single (opt_action : 'opt_action Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "->"; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("act\n",
             (Fgram.mk_action
                (fun (act : 'exp)  _  (_loc : FLoc.t)  ->
                   (act : 'opt_action )))))]));
  Fgram.extend_single (pattern : 'pattern Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("`Lid (_loc, i)\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'pattern )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
        ([`Skeyword "_"],
          ("`Any _loc\n",
            (Fgram.mk_action
               (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'pattern )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("p\n",
            (Fgram.mk_action
               (fun _  (p : 'pattern)  _  (_loc : FLoc.t)  -> (p : 'pattern )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword ",";
         `Slist1sep (`Sself, (`Skeyword ","));
         `Skeyword ")"],
          ("tuple_com (p1 :: ps)\n",
            (Fgram.mk_action
               (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _ 
                  (_loc : FLoc.t)  -> (tuple_com (p1 :: ps) : 'pattern )))))]));
  Fgram.extend_single (brace_pattern : 'brace_pattern Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "{";
          `Snterm (Fgram.obj (pattern : 'pattern Fgram.t ));
          `Skeyword "}"],
           ("p\n",
             (Fgram.mk_action
                (fun _  (p : 'pattern)  _  (_loc : FLoc.t)  ->
                   (p : 'brace_pattern )))))]));
  Fgram.extend_single (psymbol : 'psymbol Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (symbol : 'symbol Fgram.t ));
          `Sopt
            (`Snterm (Fgram.obj (brace_pattern : 'brace_pattern Fgram.t )))],
           ("match p with\n| Some _ -> { s with pattern = (p : action_pattern option  :>pat option) }\n| None  -> s\n",
             (Fgram.mk_action
                (fun (p : 'brace_pattern option)  (s : 'symbol) 
                   (_loc : FLoc.t)  ->
                   (match p with
                    | Some _ ->
                        {
                          s with
                          pattern = (p : action_pattern option  :>pat option)
                        }
                    | None  -> s : 'psymbol )))))]));
  Fgram.extend_single (sep_symbol : 'sep_symbol Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid "SEP" -> true | _ -> false)),
               (`App ((`Vrn "Uid"), (`Str "SEP"))), "`Uid \"SEP\"");
          `Snterm (Fgram.obj (symbol : 'symbol Fgram.t ))],
           ("t\n",
             (Fgram.mk_action
                (fun (t : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)
                    ->
                   match __fan_0 with
                   | `Uid "SEP" -> (t : 'sep_symbol )
                   | _ -> failwith "t\n"))))]));
  Fgram.extend_single (level_str : 'level_str Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid "Level" -> true | _ -> false)),
               (`App ((`Vrn "Uid"), (`Str "Level"))), "`Uid \"Level\"");
          `Stoken
            (((function | `Str _ -> true | _ -> false)),
              (`App ((`Vrn "Str"), `Any)), "`Str _")],
           ("s\n",
             (Fgram.mk_action
                (fun (__fan_1 : [> FToken.t])  (__fan_0 : [> FToken.t]) 
                   (_loc : FLoc.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str s,`Uid "Level") -> (s : 'level_str )
                   | _ -> failwith "s\n"))))]));
  Fgram.extend_single (symbol : 'symbol Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("L0"|"L1") -> true | _ -> false)),
               (`App ((`Vrn "Uid"), (`Bar ((`Str "L0"), (`Str "L1"))))),
               "`Uid \"L0\"| \"L1\"");
          `Sself;
          `Sopt (`Snterm (Fgram.obj (sep_symbol : 'sep_symbol Fgram.t )))],
           ("let () = check_not_tok s in\nlet styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text =\n  mk_slist _loc\n    (match x with\n     | \"L0\" -> false\n     | \"L1\" -> true\n     | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\nmk_symbol ~text ~styp ~pattern:None\n",
             (Fgram.mk_action
                (fun (sep : 'sep_symbol option)  (s : 'symbol) 
                   (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("L0"|"L1" as x) ->
                       (let () = check_not_tok s in
                        let styp =
                          `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                        let text =
                          mk_slist _loc
                            (match x with
                             | "L0" -> false
                             | "L1" -> true
                             | _ -> failwithf "only (L0|L1) allowed here")
                            sep s in
                        mk_symbol ~text ~styp ~pattern:None : 'symbol )
                   | _ ->
                       failwith
                         "let () = check_not_tok s in\nlet styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text =\n  mk_slist _loc\n    (match x with\n     | \"L0\" -> false\n     | \"L1\" -> true\n     | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\nmk_symbol ~text ~styp ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "OPT" -> true | _ -> false)),
              (`App ((`Vrn "Uid"), (`Str "OPT"))), "`Uid \"OPT\"");
         `Sself],
          ("let () = check_not_tok s in\nlet styp = `App (_loc, (`Lid (_loc, \"option\")), (s.styp)) in\nlet text = `Sopt (_loc, (s.text)) in mk_symbol ~text ~styp ~pattern:None\n",
            (Fgram.mk_action
               (fun (s : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t) 
                  ->
                  match __fan_0 with
                  | `Uid "OPT" ->
                      (let () = check_not_tok s in
                       let styp =
                         `App (_loc, (`Lid (_loc, "option")), (s.styp)) in
                       let text = `Sopt (_loc, (s.text)) in
                       mk_symbol ~text ~styp ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "let () = check_not_tok s in\nlet styp = `App (_loc, (`Lid (_loc, \"option\")), (s.styp)) in\nlet text = `Sopt (_loc, (s.text)) in mk_symbol ~text ~styp ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "TRY" -> true | _ -> false)),
              (`App ((`Vrn "Uid"), (`Str "TRY"))), "`Uid \"TRY\"");
         `Sself],
          ("let text = `Stry (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n",
            (Fgram.mk_action
               (fun (s : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t) 
                  ->
                  match __fan_0 with
                  | `Uid "TRY" ->
                      (let text = `Stry (_loc, (s.text)) in
                       mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                      'symbol )
                  | _ ->
                      failwith
                        "let text = `Stry (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "PEEK" -> true | _ -> false)),
              (`App ((`Vrn "Uid"), (`Str "PEEK"))), "`Uid \"PEEK\"");
         `Sself],
          ("let text = `Speek (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n",
            (Fgram.mk_action
               (fun (s : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t) 
                  ->
                  match __fan_0 with
                  | `Uid "PEEK" ->
                      (let text = `Speek (_loc, (s.text)) in
                       mk_symbol ~text ~styp:(s.styp) ~pattern:None : 
                      'symbol )
                  | _ ->
                      failwith
                        "let text = `Speek (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "S" -> true | _ -> false)),
              (`App ((`Vrn "Uid"), (`Str "S"))), "`Uid \"S\"")],
          ("mk_symbol ~text:(`Sself _loc) ~styp:(`Self _loc) ~pattern:None\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Uid "S" ->
                      (mk_symbol ~text:(`Sself _loc) ~styp:(`Self _loc)
                         ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "mk_symbol ~text:(`Sself _loc) ~styp:(`Self _loc) ~pattern:None\n"))));
        ([`Snterm (Fgram.obj (simple_pat : 'simple_pat Fgram.t ))],
          ("token_of_simple_pat _loc p\n",
            (Fgram.mk_action
               (fun (p : 'simple_pat)  (_loc : FLoc.t)  ->
                  (token_of_simple_pat _loc p : 'symbol )))));
        ([`Stoken
            (((function | `Str _ -> true | _ -> false)),
              (`App ((`Vrn "Str"), `Any)), "`Str _")],
          ("mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Str s ->
                      (mk_symbol ~text:(`Skeyword (_loc, s))
                         ~styp:(`Tok _loc) ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None\n"))));
        ([`Snterm (Fgram.obj (name : 'name Fgram.t ));
         `Sopt (`Snterm (Fgram.obj (level_str : 'level_str Fgram.t )))],
          ("mk_symbol ~text:(`Snterm (_loc, n, lev))\n  ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n  ~pattern:None\n",
            (Fgram.mk_action
               (fun (lev : 'level_str option)  (n : 'name)  (_loc : FLoc.t) 
                  ->
                  (mk_symbol ~text:(`Snterm (_loc, n, lev))
                     ~styp:(`Quote
                              (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))
                     ~pattern:None : 'symbol )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("s\n",
            (Fgram.mk_action
               (fun _  (s : 'symbol)  _  (_loc : FLoc.t)  -> (s : 'symbol )))))]));
  Fgram.extend_single (string : 'string Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)),
               (`App ((`Vrn "Str"), `Any)), "`Str _")],
           ("(`Str (_loc, s) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Str s -> ((`Str (_loc, s) : FAst.exp ) : 'string )
                   | _ -> failwith "(`Str (_loc, s) : FAst.exp )\n"))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (`App ((`App ((`Vrn "Ant"), (`Str ""))), `Any)),
              "`Ant (\"\",_)")],
          ("parse_exp _loc s\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Ant ("",s) -> (parse_exp _loc s : 'string )
                  | _ -> failwith "parse_exp _loc s\n"))))]));
  Fgram.extend_single (simple_exp : 'simple_exp Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
           ("(i : alident  :>exp)\n",
             (Fgram.mk_action
                (fun (i : 'a_lident)  (_loc : FLoc.t)  ->
                   ((i : alident  :>exp) : 'simple_exp )))));
        ([`Skeyword "(";
         `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
         `Skeyword ")"],
          ("e\n",
            (Fgram.mk_action
               (fun _  (e : 'exp)  _  (_loc : FLoc.t)  -> (e : 'simple_exp )))))]))
let _ =
  let d = `Absolute ["Fan"; "Lang"] in
  AstQuotation.of_exp ~name:(d, "extend") ~entry:extend_body;
  AstQuotation.of_exp ~name:(d, "unsafe_extend") ~entry:unsafe_extend_body;
  AstQuotation.of_stru ~name:(d, "create") ~entry:nonterminals;
  AstQuotation.of_stru ~name:(d, "new") ~entry:newterminals;
  AstQuotation.of_exp ~name:(d, "delete") ~entry:delete_rule_body;
  AstQuotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear