open Ast

open AstLoc

open FanGrammar

open FanGrammarTools

open PreCast.Syntax

open LibUtil

open FanUtil

let _ = FanConfig.antiquotations.contents <- true

let nonterminals: stru Gram.t = Gram.mk "nonterminals"

let nonterminalsclear: exp Gram.t = Gram.mk "nonterminalsclear"

let delete_rule_header = Gram.mk "delete_rule_header"

let extend_header = Gram.mk "extend_header"

let qualuid: vid Gram.t = Gram.mk "qualuid"

let qualid: vid Gram.t = Gram.mk "qualid"

let t_qualid: vid Gram.t = Gram.mk "t_qualid"

let entry_name: ([ `name of FanToken.name | `non] * FanGrammar.name) Gram.t =
  Gram.mk "entry_name"

let locals = Gram.mk "locals"

let entry = Gram.mk "entry"

let position = Gram.mk "position"

let assoc = Gram.mk "assoc"

let name = Gram.mk "name"

let string = Gram.mk "string"

let pattern: action_pattern Gram.t = Gram.mk "pattern"

let simple_exp = Gram.mk "simple_exp"

let delete_rules = Gram.mk "delete_rules"

let simple_pat: simple_pat Gram.t = Gram.mk "simple_pat"

let internal_pat = Gram.mk "internal_pat"

let _ =
  Gram.extend_single (nonterminals : 'nonterminals Gram.t )
    (None,
      (None, None,
        [([Gram.srules
             [([`Skeyword "(";
               `Snterm (Gram.obj (qualid : 'qualid Gram.t ));
               `Skeyword ":";
               `Snterm (Gram.obj (t_qualid : 't_qualid Gram.t ));
               `Skeyword ")"],
                ("Gram.mk_action\n  (fun _  (t : 't_qualid)  _  (x : 'qualid)  _  (_loc : FanLoc.t)  ->\n     (`dynamic (x, t) : 'e__1 ))\n",
                  (Gram.mk_action
                     (fun _  (t : 't_qualid)  _  (x : 'qualid)  _ 
                        (_loc : FanLoc.t)  -> (`dynamic (x, t) : 'e__1 )))));
             ([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
               ("Gram.mk_action\n  (fun (t : 'qualuid)  (_loc : FanLoc.t)  -> (`static t : 'e__1 ))\n",
                 (Gram.mk_action
                    (fun (t : 'qualuid)  (_loc : FanLoc.t)  ->
                       (`static t : 'e__1 )))))];
          `Slist1
            (Gram.srules
               [([`Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"))],
                  ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x -> ((_loc, x, None, None) : 'e__3 )\n     | _ -> failwith \"(_loc, x, None, None)\n\")\n",
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `Lid x -> ((_loc, x, None, None) : 'e__3 )
                          | _ -> failwith "(_loc, x, None, None)\n"))));
               ([`Skeyword "(";
                `Stoken
                  (((function | `Lid _ -> true | _ -> false)),
                    (`Normal, "`Lid _"));
                `Stoken
                  (((function | `STR (_,_) -> true | _ -> false)),
                    (`Normal, "`STR (_,_)"));
                `Skeyword ")"],
                 ("Gram.mk_action\n  (fun _  (__fan_2 : [> FanToken.t])  (__fan_1 : [> FanToken.t])  _ \n     (_loc : FanLoc.t)  ->\n     match (__fan_2, __fan_1) with\n     | (`STR (_,y),`Lid x) -> ((_loc, x, (Some y), None) : 'e__3 )\n     | _ -> failwith \"(_loc, x, (Some y), None)\n\")\n",
                   (Gram.mk_action
                      (fun _  (__fan_2 : [> FanToken.t]) 
                         (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                         match (__fan_2, __fan_1) with
                         | (`STR (_,y),`Lid x) ->
                             ((_loc, x, (Some y), None) : 'e__3 )
                         | _ -> failwith "(_loc, x, (Some y), None)\n"))));
               ([`Skeyword "(";
                `Stoken
                  (((function | `Lid _ -> true | _ -> false)),
                    (`Normal, "`Lid _"));
                `Stoken
                  (((function | `STR (_,_) -> true | _ -> false)),
                    (`Normal, "`STR (_,_)"));
                `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
                `Skeyword ")"],
                 ("Gram.mk_action\n  (fun _  (t : 'ctyp)  (__fan_2 : [> FanToken.t])  (__fan_1 : [> FanToken.t])\n      _  (_loc : FanLoc.t)  ->\n     match (__fan_2, __fan_1) with\n     | (`STR (_,y),`Lid x) -> ((_loc, x, (Some y), (Some t)) : 'e__3 )\n     | _ -> failwith \"(_loc, x, (Some y), (Some t))\n\")\n",
                   (Gram.mk_action
                      (fun _  (t : 'ctyp)  (__fan_2 : [> FanToken.t]) 
                         (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                         match (__fan_2, __fan_1) with
                         | (`STR (_,y),`Lid x) ->
                             ((_loc, x, (Some y), (Some t)) : 'e__3 )
                         | _ -> failwith "(_loc, x, (Some y), (Some t))\n"))));
               ([`Skeyword "(";
                `Stoken
                  (((function | `Lid _ -> true | _ -> false)),
                    (`Normal, "`Lid _"));
                `Skeyword ":";
                `Snterm (Gram.obj (ctyp : 'ctyp Gram.t ));
                `Sopt
                  (Gram.srules
                     [([`Stoken
                          (((function | `STR (_,_) -> true | _ -> false)),
                            (`Normal, "`STR (_,_)"))],
                        ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `STR (_,y) -> (y : 'e__2 ) | _ -> failwith \"y\n\")\n",
                          (Gram.mk_action
                             (fun (__fan_0 : [> FanToken.t]) 
                                (_loc : FanLoc.t)  ->
                                match __fan_0 with
                                | `STR (_,y) -> (y : 'e__2 )
                                | _ -> failwith "y\n"))))]);
                `Skeyword ")"],
                 ("Gram.mk_action\n  (fun _  (y : 'e__2 option)  (t : 'ctyp)  _  (__fan_1 : [> FanToken.t])  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Lid x -> ((_loc, x, y, (Some t)) : 'e__3 )\n     | _ -> failwith \"(_loc, x, y, (Some t))\n\")\n",
                   (Gram.mk_action
                      (fun _  (y : 'e__2 option)  (t : 'ctyp)  _ 
                         (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                         match __fan_1 with
                         | `Lid x -> ((_loc, x, y, (Some t)) : 'e__3 )
                         | _ -> failwith "(_loc, x, y, (Some t))\n"))))])],
           ("Gram.mk_action\n  (fun (ls : 'e__3 list)  (t : 'e__1)  (_loc : FanLoc.t)  ->\n     (let mk =\n        match t with\n        | `static t ->\n            let t = (t : vid  :>exp) in\n            (`Field (_loc, t, (`Lid (_loc, \"mk\"))) : Ast.exp )\n        | `dynamic (x,t) ->\n            let x = (x : vid  :>exp) in\n            let t = (t : vid  :>exp) in\n            (`App (_loc, (`Field (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n              Ast.exp ) in\n      sem_of_list &\n        (List.map\n           (fun (_loc,x,descr,ty)  ->\n              match (descr, ty) with\n              | (Some d,None ) ->\n                  (`Value\n                     (_loc, (`ReNil _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`App (_loc, mk, (`Str (_loc, d))))))) : \n                  Ast.stru )\n              | (Some d,Some typ) ->\n                  (`Value\n                     (_loc, (`ReNil _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`Constraint\n                               (_loc, (`App (_loc, mk, (`Str (_loc, d)))),\n                                 typ))))) : Ast.stru )\n              | (None ,None ) ->\n                  (`Value\n                     (_loc, (`ReNil _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`App (_loc, mk, (`Str (_loc, x))))))) : \n                  Ast.stru )\n              | (None ,Some typ) ->\n                  (`Value\n                     (_loc, (`ReNil _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`Constraint\n                               (_loc, (`App (_loc, mk, (`Str (_loc, x)))),\n                                 typ))))) : Ast.stru )) ls) : 'nonterminals ))\n",
             (Gram.mk_action
                (fun (ls : 'e__3 list)  (t : 'e__1)  (_loc : FanLoc.t)  ->
                   (let mk =
                      match t with
                      | `static t ->
                          let t = (t : vid  :>exp) in
                          (`Field (_loc, t, (`Lid (_loc, "mk"))) : Ast.exp )
                      | `dynamic (x,t) ->
                          let x = (x : vid  :>exp) in
                          let t = (t : vid  :>exp) in
                          (`App
                             (_loc,
                               (`Field (_loc, t, (`Lid (_loc, "mk_dynamic")))),
                               x) : Ast.exp ) in
                    sem_of_list &
                      (List.map
                         (fun (_loc,x,descr,ty)  ->
                            match (descr, ty) with
                            | (Some d,None ) ->
                                (`Value
                                   (_loc, (`ReNil _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`App (_loc, mk, (`Str (_loc, d))))))) : 
                                Ast.stru )
                            | (Some d,Some typ) ->
                                (`Value
                                   (_loc, (`ReNil _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`Constraint
                                             (_loc,
                                               (`App
                                                  (_loc, mk,
                                                    (`Str (_loc, d)))), typ))))) : 
                                Ast.stru )
                            | (None ,None ) ->
                                (`Value
                                   (_loc, (`ReNil _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`App (_loc, mk, (`Str (_loc, x))))))) : 
                                Ast.stru )
                            | (None ,Some typ) ->
                                (`Value
                                   (_loc, (`ReNil _loc),
                                     (`Bind
                                        (_loc, (`Lid (_loc, x)),
                                          (`Constraint
                                             (_loc,
                                               (`App
                                                  (_loc, mk,
                                                    (`Str (_loc, x)))), typ))))) : 
                                Ast.stru )) ls) : 'nonterminals )))))]));
  Gram.extend_single (nonterminalsclear : 'nonterminalsclear Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ));
          `Slist1
            (Gram.srules
               [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
                  ("Gram.mk_action (fun (x : 'a_lident)  (_loc : FanLoc.t)  -> (x : 'e__4 ))\n",
                    (Gram.mk_action
                       (fun (x : 'a_lident)  (_loc : FanLoc.t)  ->
                          (x : 'e__4 )))))])],
           ("Gram.mk_action\n  (fun (ls : 'e__4 list)  (t : 'qualuid)  (_loc : FanLoc.t)  ->\n     (let rest =\n        List.map\n          (fun (x : alident)  ->\n             let x = (x : alident  :>exp) in\n             let _loc = loc_of x in\n             let t = (t : vid  :>exp) in\n             (`App (_loc, (`Field (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n               Ast.exp )) ls in\n      seq_sem rest : 'nonterminalsclear ))\n",
             (Gram.mk_action
                (fun (ls : 'e__4 list)  (t : 'qualuid)  (_loc : FanLoc.t)  ->
                   (let rest =
                      List.map
                        (fun (x : alident)  ->
                           let x = (x : alident  :>exp) in
                           let _loc = loc_of x in
                           let t = (t : vid  :>exp) in
                           (`App
                              (_loc,
                                (`Field (_loc, t, (`Lid (_loc, "clear")))),
                                x) : Ast.exp )) ls in
                    seq_sem rest : 'nonterminalsclear )))))]))

let _ =
  Gram.extend_single (extend_header : 'extend_header Gram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Gram.obj (qualid : 'qualid Gram.t ));
          `Skeyword ":";
          `Snterm (Gram.obj (t_qualid : 't_qualid Gram.t ));
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (t : 't_qualid)  _  (i : 'qualid)  _  (_loc : FanLoc.t)  ->\n     (let old = gm () in\n      let () = grammar_module_name.contents <- t in ((Some i), old) : \n     'extend_header ))\n",
             (Gram.mk_action
                (fun _  (t : 't_qualid)  _  (i : 'qualid)  _ 
                   (_loc : FanLoc.t)  ->
                   (let old = gm () in
                    let () = grammar_module_name.contents <- t in
                    ((Some i), old) : 'extend_header )))));
        ([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
          ("Gram.mk_action\n  (fun (t : 'qualuid)  (_loc : FanLoc.t)  ->\n     (let old = gm () in\n      let () = grammar_module_name.contents <- t in (None, old) : 'extend_header ))\n",
            (Gram.mk_action
               (fun (t : 'qualuid)  (_loc : FanLoc.t)  ->
                  (let old = gm () in
                   let () = grammar_module_name.contents <- t in (None, old) : 
                  'extend_header )))));
        ([],
          ("Gram.mk_action\n  (fun (_loc : FanLoc.t)  -> ((None, (gm ())) : 'extend_header ))\n",
            (Gram.mk_action
               (fun (_loc : FanLoc.t)  -> ((None, (gm ())) : 'extend_header )))))]));
  Gram.extend_single (extend_body : 'extend_body Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (extend_header : 'extend_header Gram.t ));
          `Sopt (`Snterm (Gram.obj (locals : 'locals Gram.t )));
          `Slist1 (`Snterm (Gram.obj (entry : 'entry Gram.t )))],
           ("Gram.mk_action\n  (fun (el : 'entry list)  (locals : 'locals option) \n     ((gram,old) : 'extend_header)  (_loc : FanLoc.t)  ->\n     (let res = text_of_functorial_extend _loc gram locals el in\n      let () = grammar_module_name.contents <- old in res : 'extend_body ))\n",
             (Gram.mk_action
                (fun (el : 'entry list)  (locals : 'locals option) 
                   ((gram,old) : 'extend_header)  (_loc : FanLoc.t)  ->
                   (let res = text_of_functorial_extend _loc gram locals el in
                    let () = grammar_module_name.contents <- old in res : 
                   'extend_body )))))]));
  Gram.extend_single (delete_rule_header : 'delete_rule_header Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ))],
           ("Gram.mk_action\n  (fun (g : 'qualuid)  (_loc : FanLoc.t)  ->\n     (let old = gm () in let () = grammar_module_name.contents <- g in old : \n     'delete_rule_header ))\n",
             (Gram.mk_action
                (fun (g : 'qualuid)  (_loc : FanLoc.t)  ->
                   (let old = gm () in
                    let () = grammar_module_name.contents <- g in old : 
                   'delete_rule_header )))))]));
  Gram.extend_single (delete_rule_body : 'delete_rule_body Gram.t )
    (None,
      (None, None,
        [([`Snterm
             (Gram.obj (delete_rule_header : 'delete_rule_header Gram.t ));
          `Slist1 (`Snterm (Gram.obj (delete_rules : 'delete_rules Gram.t )))],
           ("Gram.mk_action\n  (fun (es : 'delete_rules list)  (old : 'delete_rule_header) \n     (_loc : FanLoc.t)  ->\n     (let () = grammar_module_name.contents <- old in seq_sem es : 'delete_rule_body ))\n",
             (Gram.mk_action
                (fun (es : 'delete_rules list)  (old : 'delete_rule_header) 
                   (_loc : FanLoc.t)  ->
                   (let () = grammar_module_name.contents <- old in
                    seq_sem es : 'delete_rule_body )))))]));
  Gram.extend_single (delete_rules : 'delete_rules Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (name : 'name Gram.t ));
          `Skeyword ":";
          `Skeyword "[";
          `Slist1sep
            ((Gram.srules
                [([`Slist0sep
                     ((`Snterm (Gram.obj (psymbol : 'psymbol Gram.t ))),
                       (`Skeyword ";"))],
                   ("Gram.mk_action\n  (fun (sl : 'psymbol list)  (_loc : FanLoc.t)  -> (sl : 'e__5 ))\n",
                     (Gram.mk_action
                        (fun (sl : 'psymbol list)  (_loc : FanLoc.t)  ->
                           (sl : 'e__5 )))))]), (`Skeyword "|"));
          `Skeyword "]"],
           ("Gram.mk_action\n  (fun _  (sls : 'e__5 list)  _  _  (n : 'name)  (_loc : FanLoc.t)  ->\n     (exp_delete_rule _loc n sls : 'delete_rules ))\n",
             (Gram.mk_action
                (fun _  (sls : 'e__5 list)  _  _  (n : 'name) 
                   (_loc : FanLoc.t)  ->
                   (exp_delete_rule _loc n sls : 'delete_rules )))))]));
  Gram.extend_single (qualuid : 'qualuid Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'qualuid)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, x)), xs)\n\")\n",
             (Gram.mk_action
                (fun (xs : 'qualuid)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid x ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)), (`Normal, "`Uid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Uid (_loc, x) : 'qualuid )\n     | _ -> failwith \"`Uid (_loc, x)\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid x -> (`Uid (_loc, x) : 'qualuid )
                  | _ -> failwith "`Uid (_loc, x)\n"))))]));
  Gram.extend_single (qualid : 'qualid Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 'qualid)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, x)), xs)\n\")\n",
             (Gram.mk_action
                (fun (xs : 'qualid)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'qualid )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid i -> (`Lid (_loc, i) : 'qualid )
                  | _ -> failwith "`Lid (_loc, i)\n"))))]));
  Gram.extend_single (t_qualid : 't_qualid Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid _ -> true | _ -> false)),
               (`Normal, "`Uid _"));
          `Skeyword ".";
          `Sself],
           ("Gram.mk_action\n  (fun (xs : 't_qualid)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, x)), xs)\n\")\n",
             (Gram.mk_action
                (fun (xs : 't_qualid)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid x ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                   | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)), (`Normal, "`Uid _"));
         `Skeyword ".";
         `Stoken
           (((function | `Lid "t" -> true | _ -> false)),
             (`Normal, "`Lid \"t\""))],
          ("Gram.mk_action\n  (fun (__fan_2 : [> FanToken.t])  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match (__fan_2, __fan_0) with\n     | (`Lid \"t\",`Uid x) -> (`Uid (_loc, x) : 't_qualid )\n     | _ -> failwith \"`Uid (_loc, x)\n\")\n",
            (Gram.mk_action
               (fun (__fan_2 : [> FanToken.t])  _  (__fan_0 : [> FanToken.t])
                   (_loc : FanLoc.t)  ->
                  match (__fan_2, __fan_0) with
                  | (`Lid "t",`Uid x) -> (`Uid (_loc, x) : 't_qualid )
                  | _ -> failwith "`Uid (_loc, x)\n"))))]));
  Gram.extend_single (locals : 'locals Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid "local" -> true | _ -> false)),
               (`Normal, "`Lid \"local\""));
          `Skeyword ":";
          `Slist1 (`Snterm (Gram.obj (name : 'name Gram.t )));
          `Skeyword ";"],
           ("Gram.mk_action\n  (fun _  (sl : 'name list)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)\n      ->\n     match __fan_0 with\n     | `Lid \"local\" -> (sl : 'locals )\n     | _ -> failwith \"sl\n\")\n",
             (Gram.mk_action
                (fun _  (sl : 'name list)  _  (__fan_0 : [> FanToken.t]) 
                   (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid "local" -> (sl : 'locals )
                   | _ -> failwith "sl\n"))))]));
  Gram.extend_single (name : 'name Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (qualid : 'qualid Gram.t ))],
           ("Gram.mk_action\n  (fun (il : 'qualid)  (_loc : FanLoc.t)  -> (mk_name _loc il : 'name ))\n",
             (Gram.mk_action
                (fun (il : 'qualid)  (_loc : FanLoc.t)  ->
                   (mk_name _loc il : 'name )))))]));
  Gram.extend_single (entry_name : 'entry_name Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (qualid : 'qualid Gram.t ));
          `Sopt
            (Gram.srules
               [([`Stoken
                    (((function | `STR (_,_) -> true | _ -> false)),
                      (`Normal, "`STR (_,_)"))],
                  ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `STR (_,x) -> (x : 'e__6 ) | _ -> failwith \"x\n\")\n",
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `STR (_,x) -> (x : 'e__6 )
                          | _ -> failwith "x\n"))))])],
           ("Gram.mk_action\n  (fun (name : 'e__6 option)  (il : 'qualid)  (_loc : FanLoc.t)  ->\n     (((match name with\n        | Some x ->\n            let old = AstQuotation.default.contents in\n            (AstQuotation.default.contents <-\n               FanToken.resolve_name ((`Sub []), x);\n             `name old)\n        | None  -> `non), (mk_name _loc il)) : 'entry_name ))\n",
             (Gram.mk_action
                (fun (name : 'e__6 option)  (il : 'qualid)  (_loc : FanLoc.t)
                    ->
                   (((match name with
                      | Some x ->
                          let old = AstQuotation.default.contents in
                          (AstQuotation.default.contents <-
                             FanToken.resolve_name ((`Sub []), x);
                           `name old)
                      | None  -> `non), (mk_name _loc il)) : 'entry_name )))))]));
  Gram.extend_single (entry : 'entry Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (entry_name : 'entry_name Gram.t ));
          `Skeyword ":";
          `Sopt (`Snterm (Gram.obj (position : 'position Gram.t )));
          `Snterm (Gram.obj (level_list : 'level_list Gram.t ))],
           ("Gram.mk_action\n  (fun (levels : 'level_list)  (pos : 'position option)  _ \n     ((n,p) : 'entry_name)  (_loc : FanLoc.t)  ->\n     ((match n with\n       | `name old -> AstQuotation.default.contents <- old\n       | _ -> ());\n      (match (pos, levels) with\n       | (Some (`App (_loc,`Vrn (_,\"Level\"),_)),`Group _) ->\n           failwithf\n             \"For Group levels the position can not be applied to Level\"\n       | _ -> mk_entry ~name:p ~pos ~levels) : 'entry ))\n",
             (Gram.mk_action
                (fun (levels : 'level_list)  (pos : 'position option)  _ 
                   ((n,p) : 'entry_name)  (_loc : FanLoc.t)  ->
                   ((match n with
                     | `name old -> AstQuotation.default.contents <- old
                     | _ -> ());
                    (match (pos, levels) with
                     | (Some (`App (_loc,`Vrn (_,"Level"),_)),`Group _) ->
                         failwithf
                           "For Group levels the position can not be applied to Level"
                     | _ -> mk_entry ~name:p ~pos ~levels) : 'entry )))))]));
  Gram.extend_single (position : 'position Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("First"|"Last") -> true | _ -> false)),
               (`Normal, "`Uid (\"First\"|\"Last\")"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"First\"|\"Last\" as x) -> (`Vrn (_loc, x) : 'position )\n     | _ -> failwith \"`Vrn (_loc, x)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("First"|"Last" as x) ->
                       (`Vrn (_loc, x) : 'position )
                   | _ -> failwith "`Vrn (_loc, x)\n"))));
        ([`Stoken
            (((function
               | `Uid ("Before"|"After"|"Level") -> true
               | _ -> false)),
              (`Normal, "`Uid (\"Before\"|\"After\"|\"Level\")"));
         `Snterm (Gram.obj (string : 'string Gram.t ))],
          ("Gram.mk_action\n  (fun (n : 'string)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"Before\"|\"After\"|\"Level\" as x) ->\n         (`App (_loc, (`Vrn (_loc, x)), n) : 'position )\n     | _ -> failwith \"`App (_loc, (`Vrn (_loc, x)), n)\n\")\n",
            (Gram.mk_action
               (fun (n : 'string)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid ("Before"|"After"|"Level" as x) ->
                      (`App (_loc, (`Vrn (_loc, x)), n) : 'position )
                  | _ -> failwith "`App (_loc, (`Vrn (_loc, x)), n)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)), (`Normal, "`Uid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid x ->\n         (failwithf\n            \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n            x : 'position )\n     | _ ->\n         failwith\n           \"failwithf \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n  x\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid x ->
                      (failwithf
                         "%s is not the right position:(First|Last) or (Before|After|Level)"
                         x : 'position )
                  | _ ->
                      failwith
                        "failwithf \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n  x\n"))))]));
  Gram.extend_single (level_list : 'level_list Gram.t )
    (None,
      (None, None,
        [([`Skeyword "{";
          `Slist1 (`Snterm (Gram.obj (level : 'level Gram.t )));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (ll : 'level list)  _  (_loc : FanLoc.t)  ->\n     (`Group ll : 'level_list ))\n",
             (Gram.mk_action
                (fun _  (ll : 'level list)  _  (_loc : FanLoc.t)  ->
                   (`Group ll : 'level_list )))));
        ([`Snterm (Gram.obj (level : 'level Gram.t ))],
          ("Gram.mk_action\n  (fun (l : 'level)  (_loc : FanLoc.t)  -> (`Single l : 'level_list ))\n",
            (Gram.mk_action
               (fun (l : 'level)  (_loc : FanLoc.t)  ->
                  (`Single l : 'level_list )))))]));
  Gram.extend_single (level : 'level Gram.t )
    (None,
      (None, None,
        [([`Sopt
             (Gram.srules
                [([`Stoken
                     (((function | `STR (_,_) -> true | _ -> false)),
                       (`Normal, "`STR (_,_)"))],
                   ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `STR (_,x) -> (x : 'e__7 ) | _ -> failwith \"x\n\")\n",
                     (Gram.mk_action
                        (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) 
                           ->
                           match __fan_0 with
                           | `STR (_,x) -> (x : 'e__7 )
                           | _ -> failwith "x\n"))))]);
          `Sopt (`Snterm (Gram.obj (assoc : 'assoc Gram.t )));
          `Snterm (Gram.obj (rule_list : 'rule_list Gram.t ))],
           ("Gram.mk_action\n  (fun (rules : 'rule_list)  (assoc : 'assoc option)  (label : 'e__7 option) \n     (_loc : FanLoc.t)  -> (mk_level ~label ~assoc ~rules : 'level ))\n",
             (Gram.mk_action
                (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                   (label : 'e__7 option)  (_loc : FanLoc.t)  ->
                   (mk_level ~label ~assoc ~rules : 'level )))))]));
  Gram.extend_single (assoc : 'assoc Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("LA"|"RA"|"NA") -> true | _ -> false)),
               (`Normal, "`Uid (\"LA\"|\"RA\"|\"NA\")"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"LA\"|\"RA\"|\"NA\" as x) -> (`Vrn (_loc, x) : 'assoc )\n     | _ -> failwith \"`Vrn (_loc, x)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("LA"|"RA"|"NA" as x) -> (`Vrn (_loc, x) : 'assoc )
                   | _ -> failwith "`Vrn (_loc, x)\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)), (`Normal, "`Uid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid x ->\n         (failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x : \n         'assoc )\n     | _ ->\n         failwith\n           \"failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid x ->
                      (failwithf
                         "%s is not a correct associativity:(LA|RA|NA)" x : 
                      'assoc )
                  | _ ->
                      failwith
                        "failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x\n"))))]));
  Gram.extend_single (rule_list : 'rule_list Gram.t )
    (None,
      (None, None,
        [([`Skeyword "["; `Skeyword "]"],
           ("Gram.mk_action (fun _  _  (_loc : FanLoc.t)  -> ([] : 'rule_list ))\n",
             (Gram.mk_action
                (fun _  _  (_loc : FanLoc.t)  -> ([] : 'rule_list )))));
        ([`Skeyword "[";
         `Slist1sep
           ((`Snterm (Gram.obj (rule : 'rule Gram.t ))), (`Skeyword "|"));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rules : 'rule list)  _  (_loc : FanLoc.t)  ->\n     (retype_rule_list_without_patterns _loc rules : 'rule_list ))\n",
            (Gram.mk_action
               (fun _  (rules : 'rule list)  _  (_loc : FanLoc.t)  ->
                  (retype_rule_list_without_patterns _loc rules : 'rule_list )))))]));
  Gram.extend_single (rule : 'rule Gram.t )
    (None,
      (None, None,
        [([`Slist0sep
             ((`Snterm (Gram.obj (psymbol : 'psymbol Gram.t ))),
               (`Skeyword ";"));
          `Sopt
            (Gram.srules
               [([`Skeyword "->"; `Snterm (Gram.obj (exp : 'exp Gram.t ))],
                  ("Gram.mk_action (fun (act : 'exp)  _  (_loc : FanLoc.t)  -> (act : 'e__8 ))\n",
                    (Gram.mk_action
                       (fun (act : 'exp)  _  (_loc : FanLoc.t)  ->
                          (act : 'e__8 )))))])],
           ("Gram.mk_action\n  (fun (action : 'e__8 option)  (psl : 'psymbol list)  (_loc : FanLoc.t)  ->\n     (mk_rule ~prod:psl ~action : 'rule ))\n",
             (Gram.mk_action
                (fun (action : 'e__8 option)  (psl : 'psymbol list) 
                   (_loc : FanLoc.t)  -> (mk_rule ~prod:psl ~action : 
                   'rule )))))]));
  Gram.extend_single (psymbol : 'psymbol Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (symbol : 'symbol Gram.t ));
          `Sopt
            (Gram.srules
               [([`Skeyword "{";
                 `Snterm (Gram.obj (pattern : 'pattern Gram.t ));
                 `Skeyword "}"],
                  ("Gram.mk_action (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  -> (p : 'e__9 ))\n",
                    (Gram.mk_action
                       (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  ->
                          (p : 'e__9 )))))])],
           ("Gram.mk_action\n  (fun (p : 'e__9 option)  (s : 'symbol)  (_loc : FanLoc.t)  ->\n     (match p with\n      | Some _ ->\n          { s with pattern = (p : action_pattern option  :>pat option) }\n      | None  -> s : 'psymbol ))\n",
             (Gram.mk_action
                (fun (p : 'e__9 option)  (s : 'symbol)  (_loc : FanLoc.t)  ->
                   (match p with
                    | Some _ ->
                        {
                          s with
                          pattern = (p : action_pattern option  :>pat option)
                        }
                    | None  -> s : 'psymbol )))))]));
  Gram.extend_single (symbol : 'symbol Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("L0"|"L1") -> true | _ -> false)),
               (`Normal, "`Uid (\"L0\"|\"L1\")"));
          `Sself;
          `Sopt
            (Gram.srules
               [([`Stoken
                    (((function | `Uid "SEP" -> true | _ -> false)),
                      (`Normal, "`Uid \"SEP\""));
                 `Snterm (Gram.obj (symbol : 'symbol Gram.t ))],
                  ("Gram.mk_action\n  (fun (t : 'symbol)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid \"SEP\" -> (t : 'e__10 ) | _ -> failwith \"t\n\")\n",
                    (Gram.mk_action
                       (fun (t : 'symbol)  (__fan_0 : [> FanToken.t]) 
                          (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `Uid "SEP" -> (t : 'e__10 )
                          | _ -> failwith "t\n"))))])],
           ("Gram.mk_action\n  (fun (sep : 'e__10 option)  (s : 'symbol)  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"L0\"|\"L1\" as x) ->\n         (let () = check_not_tok s in\n          let styp =\n            `App (_loc, (`Id (_loc, (`Lid (_loc, \"list\")))), (s.styp)) in\n          let text =\n            mk_slist _loc\n              (match x with\n               | \"L0\" -> false\n               | \"L1\" -> true\n               | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\n          mk_symbol ~text ~styp ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let () = check_not_tok s in\nlet styp = `App (_loc, (`Id (_loc, (`Lid (_loc, \"list\")))), (s.styp)) in\nlet text =\n  mk_slist _loc\n    (match x with\n     | \"L0\" -> false\n     | \"L1\" -> true\n     | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\nmk_symbol ~text ~styp ~pattern:None\n\")\n",
             (Gram.mk_action
                (fun (sep : 'e__10 option)  (s : 'symbol) 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("L0"|"L1" as x) ->
                       (let () = check_not_tok s in
                        let styp =
                          `App
                            (_loc, (`Id (_loc, (`Lid (_loc, "list")))),
                              (s.styp)) in
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
                         "let () = check_not_tok s in\nlet styp = `App (_loc, (`Id (_loc, (`Lid (_loc, \"list\")))), (s.styp)) in\nlet text =\n  mk_slist _loc\n    (match x with\n     | \"L0\" -> false\n     | \"L1\" -> true\n     | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\nmk_symbol ~text ~styp ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "OPT" -> true | _ -> false)),
              (`Normal, "`Uid \"OPT\""));
         `Sself],
          ("Gram.mk_action\n  (fun (s : 'symbol)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"OPT\" ->\n         (let () = check_not_tok s in\n          let styp =\n            `App (_loc, (`Id (_loc, (`Lid (_loc, \"option\")))), (s.styp)) in\n          let text = `Sopt (_loc, (s.text)) in\n          mk_symbol ~text ~styp ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let () = check_not_tok s in\nlet styp = `App (_loc, (`Id (_loc, (`Lid (_loc, \"option\")))), (s.styp)) in\nlet text = `Sopt (_loc, (s.text)) in mk_symbol ~text ~styp ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid "OPT" ->
                      (let () = check_not_tok s in
                       let styp =
                         `App
                           (_loc, (`Id (_loc, (`Lid (_loc, "option")))),
                             (s.styp)) in
                       let text = `Sopt (_loc, (s.text)) in
                       mk_symbol ~text ~styp ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "let () = check_not_tok s in\nlet styp = `App (_loc, (`Id (_loc, (`Lid (_loc, \"option\")))), (s.styp)) in\nlet text = `Sopt (_loc, (s.text)) in mk_symbol ~text ~styp ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "TRY" -> true | _ -> false)),
              (`Normal, "`Uid \"TRY\""));
         `Sself],
          ("Gram.mk_action\n  (fun (s : 'symbol)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"TRY\" ->\n         (let text = `Stry (_loc, (s.text)) in\n          mk_symbol ~text ~styp:(s.styp) ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let text = `Stry (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
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
              (`Normal, "`Uid \"PEEK\""));
         `Sself],
          ("Gram.mk_action\n  (fun (s : 'symbol)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"PEEK\" ->\n         (let text = `Speek (_loc, (s.text)) in\n          mk_symbol ~text ~styp:(s.styp) ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let text = `Speek (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (s : 'symbol)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
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
              (`Normal, "`Uid \"S\""))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"S\" ->\n         (mk_symbol ~text:(`Sself _loc) ~styp:(`Self (_loc, \"S\"))\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"mk_symbol ~text:(`Sself _loc) ~styp:(`Self (_loc, \"S\")) ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid "S" ->
                      (mk_symbol ~text:(`Sself _loc)
                         ~styp:(`Self (_loc, "S")) ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "mk_symbol ~text:(`Sself _loc) ~styp:(`Self (_loc, \"S\")) ~pattern:None\n"))));
        ([`Stoken
            (((function | `Uid "N" -> true | _ -> false)),
              (`Normal, "`Uid \"N\""))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"N\" ->\n         (mk_symbol ~text:(`Snext _loc) ~styp:(`Self (_loc, \"N\"))\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"mk_symbol ~text:(`Snext _loc) ~styp:(`Self (_loc, \"N\")) ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid "N" ->
                      (mk_symbol ~text:(`Snext _loc)
                         ~styp:(`Self (_loc, "N")) ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "mk_symbol ~text:(`Snext _loc) ~styp:(`Self (_loc, \"N\")) ~pattern:None\n"))));
        ([`Skeyword "[";
         `Slist1sep
           ((`Snterm (Gram.obj (rule : 'rule Gram.t ))), (`Skeyword "|"));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (rl : 'rule list)  _  (_loc : FanLoc.t)  ->\n     (let rl = retype_rule_list_without_patterns _loc rl in\n      let t = new_type_var () in\n      mk_symbol ~text:(`Srules (_loc, (mk_srules _loc t rl \"\")))\n        ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, t)))) ~pattern:None : \n     'symbol ))\n",
            (Gram.mk_action
               (fun _  (rl : 'rule list)  _  (_loc : FanLoc.t)  ->
                  (let rl = retype_rule_list_without_patterns _loc rl in
                   let t = new_type_var () in
                   mk_symbol ~text:(`Srules (_loc, (mk_srules _loc t rl "")))
                     ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, t))))
                     ~pattern:None : 'symbol )))));
        ([`Snterm (Gram.obj (simple_pat : 'simple_pat Gram.t ))],
          ("Gram.mk_action\n  (fun (p : 'simple_pat)  (_loc : FanLoc.t)  ->\n     (let (p,ls) =\n        Exp.filter_pat_with_captured_variables (p : simple_pat  :>pat) in\n      match ls with\n      | [] -> mk_tok _loc ~pattern:p (`Tok _loc)\n      | (x,y)::ys ->\n          let restrict =\n            List.fold_left\n              (fun acc  (x,y)  ->\n                 `App\n                   (_loc, (`App (_loc, (`Lid (_loc, \"&&\")), acc)),\n                     (`App\n                        (_loc,\n                          (`App (_loc, (`Id (_loc, (`Lid (_loc, \"=\")))), x)),\n                          y))))\n              (`App\n                 (_loc, (`App (_loc, (`Id (_loc, (`Lid (_loc, \"=\")))), x)),\n                   y)) ys in\n          mk_tok _loc ~restrict ~pattern:p (`Tok _loc) : 'symbol ))\n",
            (Gram.mk_action
               (fun (p : 'simple_pat)  (_loc : FanLoc.t)  ->
                  (let (p,ls) =
                     Exp.filter_pat_with_captured_variables
                       (p : simple_pat  :>pat) in
                   match ls with
                   | [] -> mk_tok _loc ~pattern:p (`Tok _loc)
                   | (x,y)::ys ->
                       let restrict =
                         List.fold_left
                           (fun acc  (x,y)  ->
                              `App
                                (_loc,
                                  (`App (_loc, (`Lid (_loc, "&&")), acc)),
                                  (`App
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`Id (_loc, (`Lid (_loc, "=")))),
                                            x)), y))))
                           (`App
                              (_loc,
                                (`App
                                   (_loc, (`Id (_loc, (`Lid (_loc, "=")))),
                                     x)), y)) ys in
                       mk_tok _loc ~restrict ~pattern:p (`Tok _loc) : 
                  'symbol )))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) ->\n         (mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc)\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (_,s) ->
                      (mk_symbol ~text:(`Skeyword (_loc, s))
                         ~styp:(`Tok _loc) ~pattern:None : 'symbol )
                  | _ ->
                      failwith
                        "mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None\n"))));
        ([`Snterm (Gram.obj (name : 'name Gram.t ));
         `Sopt
           (Gram.srules
              [([`Stoken
                   (((function | `Uid "Level" -> true | _ -> false)),
                     (`Normal, "`Uid \"Level\""));
                `Stoken
                  (((function | `STR (_,_) -> true | _ -> false)),
                    (`Normal, "`STR (_,_)"))],
                 ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match (__fan_1, __fan_0) with\n     | (`STR (_,s),`Uid \"Level\") -> (s : 'e__11 )\n     | _ -> failwith \"s\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_1 : [> FanToken.t]) 
                         (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match (__fan_1, __fan_0) with
                         | (`STR (_,s),`Uid "Level") -> (s : 'e__11 )
                         | _ -> failwith "s\n"))))])],
          ("Gram.mk_action\n  (fun (lev : 'e__11 option)  (n : 'name)  (_loc : FanLoc.t)  ->\n     (mk_symbol ~text:(`Snterm (_loc, n, lev))\n        ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n        ~pattern:None : 'symbol ))\n",
            (Gram.mk_action
               (fun (lev : 'e__11 option)  (n : 'name)  (_loc : FanLoc.t)  ->
                  (mk_symbol ~text:(`Snterm (_loc, n, lev))
                     ~styp:(`Quote
                              (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))
                     ~pattern:None : 'symbol )))));
        ([`Stoken
            (((function | `Ant (("nt"|""),_) -> true | _ -> false)),
              (`Normal, "`Ant ((\"nt\"|\"\"),_)"));
         `Sopt
           (Gram.srules
              [([`Stoken
                   (((function | `Uid "Level" -> true | _ -> false)),
                     (`Normal, "`Uid \"Level\""));
                `Stoken
                  (((function | `STR (_,_) -> true | _ -> false)),
                    (`Normal, "`STR (_,_)"))],
                 ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match (__fan_1, __fan_0) with\n     | (`STR (_,s),`Uid \"Level\") -> (s : 'e__12 )\n     | _ -> failwith \"s\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_1 : [> FanToken.t]) 
                         (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match (__fan_1, __fan_0) with
                         | (`STR (_,s),`Uid "Level") -> (s : 'e__12 )
                         | _ -> failwith "s\n"))))])],
          ("Gram.mk_action\n  (fun (lev : 'e__12 option)  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) \n     ->\n     match __fan_0 with\n     | `Ant ((\"nt\"|\"\"),s) ->\n         (let i = parse_ident _loc s in\n          let n = mk_name _loc (Id.to_vid i) in\n          mk_symbol ~text:(`Snterm (_loc, n, lev))\n            ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let i = parse_ident _loc s in\nlet n = mk_name _loc (Id.to_vid i) in\nmk_symbol ~text:(`Snterm (_loc, n, lev))\n  ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n  ~pattern:None\n\")\n",
            (Gram.mk_action
               (fun (lev : 'e__12 option)  (__fan_0 : [> FanToken.t]) 
                  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant (("nt"|""),s) ->
                      (let i = parse_ident _loc s in
                       let n = mk_name _loc (Id.to_vid i) in
                       mk_symbol ~text:(`Snterm (_loc, n, lev))
                         ~styp:(`Quote
                                  (_loc, (`Normal _loc),
                                    (`Lid (_loc, (n.tvar))))) ~pattern:None : 
                      'symbol )
                  | _ ->
                      failwith
                        "let i = parse_ident _loc s in\nlet n = mk_name _loc (Id.to_vid i) in\nmk_symbol ~text:(`Snterm (_loc, n, lev))\n  ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n  ~pattern:None\n"))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (s : 'symbol)  _  (_loc : FanLoc.t)  -> (s : 'symbol ))\n",
            (Gram.mk_action
               (fun _  (s : 'symbol)  _  (_loc : FanLoc.t)  -> (s : 'symbol )))))]));
  Gram.extend_single (simple_pat : 'simple_pat Gram.t )
    (None,
      (None, None,
        [([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->\n     (`Vrn (_loc, s) : 'simple_pat ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'simple_pat )))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (luident : 'luident Gram.t ));
         `Stoken
           (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
             (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
          ("Gram.mk_action\n  (fun (__fan_2 : [> FanToken.t])  (v : 'luident)  _  (_loc : FanLoc.t)  ->\n     match __fan_2 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`App (_loc, (`Vrn (_loc, v)), (mk_anti _loc ~c:\"pat\" n s)) : \n         'simple_pat )\n     | _ ->\n         failwith\n           \"`App (_loc, (`Vrn (_loc, v)), (mk_anti _loc ~c:\"pat\" n s))\n\")\n",
            (Gram.mk_action
               (fun (__fan_2 : [> FanToken.t])  (v : 'luident)  _ 
                  (_loc : FanLoc.t)  ->
                  match __fan_2 with
                  | `Ant ((""|"anti" as n),s) ->
                      (`App
                         (_loc, (`Vrn (_loc, v)),
                           (mk_anti _loc ~c:"pat" n s)) : 'simple_pat )
                  | _ ->
                      failwith
                        "`App (_loc, (`Vrn (_loc, v)), (mk_anti _loc ~c:\"pat\" n s))\n"))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (luident : 'luident Gram.t ));
         `Stoken
           (((function | `STR (_,_) -> true | _ -> false)),
             (`Normal, "`STR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_2 : [> FanToken.t])  (s : 'luident)  _  (_loc : FanLoc.t)  ->\n     match __fan_2 with\n     | `STR (_,v) ->\n         (`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v))) : 'simple_pat )\n     | _ -> failwith \"`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n\")\n",
            (Gram.mk_action
               (fun (__fan_2 : [> FanToken.t])  (s : 'luident)  _ 
                  (_loc : FanLoc.t)  ->
                  match __fan_2 with
                  | `STR (_,v) ->
                      (`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v))) : 
                      'simple_pat )
                  | _ ->
                      failwith
                        "`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n"))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (luident : 'luident Gram.t ));
         `Stoken
           (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
          ("Gram.mk_action\n  (fun (__fan_2 : [> FanToken.t])  (s : 'luident)  _  (_loc : FanLoc.t)  ->\n     match __fan_2 with\n     | `Lid x ->\n         (`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x))) : 'simple_pat )\n     | _ -> failwith \"`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x)))\n\")\n",
            (Gram.mk_action
               (fun (__fan_2 : [> FanToken.t])  (s : 'luident)  _ 
                  (_loc : FanLoc.t)  ->
                  match __fan_2 with
                  | `Lid x ->
                      (`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x))) : 
                      'simple_pat )
                  | _ ->
                      failwith
                        "`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x)))\n"))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (luident : 'luident Gram.t ));
         `Skeyword "_"],
          ("Gram.mk_action\n  (fun _  (s : 'luident)  _  (_loc : FanLoc.t)  ->\n     (`App (_loc, (`Vrn (_loc, s)), (`Any _loc)) : 'simple_pat ))\n",
            (Gram.mk_action
               (fun _  (s : 'luident)  _  (_loc : FanLoc.t)  ->
                  (`App (_loc, (`Vrn (_loc, s)), (`Any _loc)) : 'simple_pat )))));
        ([`Skeyword "`";
         `Snterm (Gram.obj (luident : 'luident Gram.t ));
         `Skeyword "(";
         `Slist1sep
           ((`Snterm (Gram.obj (internal_pat : 'internal_pat Gram.t ))),
             (`Skeyword ","));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (v : 'internal_pat list)  _  (s : 'luident)  _  (_loc : FanLoc.t) \n     ->\n     (match v with\n      | x::[] -> `App (_loc, (`Vrn (_loc, s)), x)\n      | x::xs ->\n          `App (_loc, (`App (_loc, (`Vrn (_loc, s)), x)), (com_of_list xs))\n      | _ -> assert false : 'simple_pat ))\n",
            (Gram.mk_action
               (fun _  (v : 'internal_pat list)  _  (s : 'luident)  _ 
                  (_loc : FanLoc.t)  ->
                  (match v with
                   | x::[] -> `App (_loc, (`Vrn (_loc, s)), x)
                   | x::xs ->
                       `App
                         (_loc, (`App (_loc, (`Vrn (_loc, s)), x)),
                           (com_of_list xs))
                   | _ -> assert false : 'simple_pat )))))]));
  Gram.extend (internal_pat : 'internal_pat Gram.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (s : 'a_lident)  _  (p1 : 'internal_pat)  (_loc : FanLoc.t)  ->\n     (`Alias (_loc, p1, s) : 'internal_pat ))\n",
              (Gram.mk_action
                 (fun (s : 'a_lident)  _  (p1 : 'internal_pat) 
                    (_loc : FanLoc.t)  ->
                    (`Alias (_loc, p1, s) : 'internal_pat )))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           ("Gram.mk_action\n  (fun (p2 : 'internal_pat)  _  (p1 : 'internal_pat)  (_loc : FanLoc.t)  ->\n     (`Bar (_loc, p1, p2) : 'internal_pat ))\n",
             (Gram.mk_action
                (fun (p2 : 'internal_pat)  _  (p1 : 'internal_pat) 
                   (_loc : FanLoc.t)  ->
                   (`Bar (_loc, p1, p2) : 'internal_pat )))))]);
      ((Some "simple"), None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'internal_pat )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) -> (`Str (_loc, s) : 'internal_pat )
                   | _ -> failwith "`Str (_loc, s)\n"))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'internal_pat ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'internal_pat )))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x -> (`Lid (_loc, x) : 'internal_pat )\n     | _ -> failwith \"`Lid (_loc, x)\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid x -> (`Lid (_loc, x) : 'internal_pat )
                  | _ -> failwith "`Lid (_loc, x)\n"))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (p : 'internal_pat)  _  (_loc : FanLoc.t)  -> (p : 'internal_pat ))\n",
            (Gram.mk_action
               (fun _  (p : 'internal_pat)  _  (_loc : FanLoc.t)  ->
                  (p : 'internal_pat )))))])]);
  Gram.extend_single (pattern : 'pattern Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'pattern )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Lid i -> (`Lid (_loc, i) : 'pattern )
                   | _ -> failwith "`Lid (_loc, i)\n"))));
        ([`Skeyword "_"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'pattern ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  -> (`Any _loc : 'pattern )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  -> (p : 'pattern ))\n",
            (Gram.mk_action
               (fun _  (p : 'pattern)  _  (_loc : FanLoc.t)  ->
                  (p : 'pattern )))));
        ([`Skeyword "(";
         `Sself;
         `Skeyword ",";
         `Slist1sep (`Sself, (`Skeyword ","));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _  (_loc : FanLoc.t)  ->\n     (tuple_com (p1 :: ps) : 'pattern ))\n",
            (Gram.mk_action
               (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _ 
                  (_loc : FanLoc.t)  -> (tuple_com (p1 :: ps) : 'pattern )))))]));
  Gram.extend_single (string : 'string Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'string )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) -> (`Str (_loc, s) : 'string )
                   | _ -> failwith "`Str (_loc, s)\n"))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              (`Normal, "`Ant (\"\",_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Ant (\"\",s) -> (parse_exp _loc s : 'string )\n     | _ -> failwith \"parse_exp _loc s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Ant ("",s) -> (parse_exp _loc s : 'string )
                  | _ -> failwith "parse_exp _loc s\n"))))]));
  Gram.extend_single (symbol : 'symbol Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid ("FOLD0"|"FOLD1") -> true | _ -> false)),
               (`Normal, "`Uid (\"FOLD0\"|\"FOLD1\")"));
          `Snterm (Gram.obj (simple_exp : 'simple_exp Gram.t ));
          `Snterm (Gram.obj (simple_exp : 'simple_exp Gram.t ));
          `Sself],
           ("Gram.mk_action\n  (fun (s : 'symbol)  (e : 'simple_exp)  (f : 'simple_exp) \n     (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"FOLD0\"|\"FOLD1\" as x) -> (sfold _loc [x] f e s : 'symbol )\n     | _ -> failwith \"sfold _loc [x] f e s\n\")\n",
             (Gram.mk_action
                (fun (s : 'symbol)  (e : 'simple_exp)  (f : 'simple_exp) 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `Uid ("FOLD0"|"FOLD1" as x) ->
                       (sfold _loc [x] f e s : 'symbol )
                   | _ -> failwith "sfold _loc [x] f e s\n"))));
        ([`Stoken
            (((function | `Uid ("FOLD0"|"FOLD1") -> true | _ -> false)),
              (`Normal, "`Uid (\"FOLD0\"|\"FOLD1\")"));
         `Snterm (Gram.obj (simple_exp : 'simple_exp Gram.t ));
         `Snterm (Gram.obj (simple_exp : 'simple_exp Gram.t ));
         `Sself;
         `Stoken
           (((function | `Uid "SEP" -> true | _ -> false)),
             (`Normal, "`Uid \"SEP\""));
         `Sself],
          ("Gram.mk_action\n  (fun (sep : 'symbol)  (__fan_4 : [> FanToken.t])  (s : 'symbol) \n     (e : 'simple_exp)  (f : 'simple_exp)  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match (__fan_4, __fan_0) with\n     | (`Uid (\"SEP\" as y),`Uid (\"FOLD0\"|\"FOLD1\" as x)) ->\n         (sfold ~sep _loc [x; y] f e s : 'symbol )\n     | _ -> failwith \"sfold ~sep _loc [x; y] f e s\n\")\n",
            (Gram.mk_action
               (fun (sep : 'symbol)  (__fan_4 : [> FanToken.t]) 
                  (s : 'symbol)  (e : 'simple_exp)  (f : 'simple_exp) 
                  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match (__fan_4, __fan_0) with
                  | (`Uid ("SEP" as y),`Uid ("FOLD0"|"FOLD1" as x)) ->
                      (sfold ~sep _loc [x; y] f e s : 'symbol )
                  | _ -> failwith "sfold ~sep _loc [x; y] f e s\n"))))]));
  Gram.extend_single (simple_exp : 'simple_exp Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->\n     ((i : alident  :>exp) : 'simple_exp ))\n",
             (Gram.mk_action
                (fun (i : 'a_lident)  (_loc : FanLoc.t)  ->
                   ((i : alident  :>exp) : 'simple_exp )))));
        ([`Skeyword "(";
         `Snterm (Gram.obj (exp : 'exp Gram.t ));
         `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (e : 'exp)  _  (_loc : FanLoc.t)  -> (e : 'simple_exp ))\n",
            (Gram.mk_action
               (fun _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                  (e : 'simple_exp )))))]))

let d = `Absolute ["Fan"; "Lang"]

let _ = AstQuotation.of_exp ~name:(d, "extend") ~entry:extend_body

let _ = AstQuotation.of_exp ~name:(d, "delete") ~entry:delete_rule_body

let _ = AstQuotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear

let _ = AstQuotation.of_stru ~name:(d, "create") ~entry:nonterminals