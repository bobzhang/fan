open FAst

open AstLib

open FGramDef

open FGramGen

open Fsyntax

open LibUtil

open FanUtil

let _ = FConfig.antiquotations := true

let nonterminals: stru Fgram.t = Fgram.mk "nonterminals"

let nonterminalsclear: exp Fgram.t = Fgram.mk "nonterminalsclear"

let delete_rule_header = Fgram.mk "delete_rule_header"

let extend_header = Fgram.mk "extend_header"

let qualuid: vid Fgram.t = Fgram.mk "qualuid"

let qualid: vid Fgram.t = Fgram.mk "qualid"

let t_qualid: vid Fgram.t = Fgram.mk "t_qualid"

let entry_name: ([ `name of FToken.name | `non] * FGramDef.name) Fgram.t =
  Fgram.mk "entry_name"

let locals = Fgram.mk "locals"

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

let simple_pat: simple_pat Fgram.t = Fgram.mk "simple_pat"

let internal_pat = Fgram.mk "internal_pat"

let _ =
  begin
    Fgram.extend_single (nonterminals : 'nonterminals Fgram.t )
      (None,
        (None, None,
          [([Fgram.srules
               [([`Skeyword "(";
                 `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
                 `Skeyword ":";
                 `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
                 `Skeyword ")"],
                  ("Fgram.mk_action\n  (fun _  (t : 't_qualid)  _  (x : 'qualid)  _  (_loc : FLoc.t)  ->\n     (`dynamic (x, t) : 'e__1 ))\n",
                    (Fgram.mk_action
                       (fun _  (t : 't_qualid)  _  (x : 'qualid)  _ 
                          (_loc : FLoc.t)  -> (`dynamic (x, t) : 'e__1 )))));
               ([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
                 ("Fgram.mk_action\n  (fun (t : 'qualuid)  (_loc : FLoc.t)  -> (`static t : 'e__1 ))\n",
                   (Fgram.mk_action
                      (fun (t : 'qualuid)  (_loc : FLoc.t)  ->
                         (`static t : 'e__1 )))))];
            `Slist1
              (Fgram.srules
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Lid x -> ((_loc, x, None, None) : 'e__3 )\n     | _ -> failwith \"(_loc, x, None, None)\n\")\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
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
                   ("Fgram.mk_action\n  (fun _  (__fan_2 : [> FToken.t])  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match (__fan_2, __fan_1) with\n     | (`STR (_,y),`Lid x) -> ((_loc, x, (Some y), None) : 'e__3 )\n     | _ -> failwith \"(_loc, x, (Some y), None)\n\")\n",
                     (Fgram.mk_action
                        (fun _  (__fan_2 : [> FToken.t]) 
                           (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
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
                  `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
                  `Skeyword ")"],
                   ("Fgram.mk_action\n  (fun _  (t : 'ctyp)  (__fan_2 : [> FToken.t])  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match (__fan_2, __fan_1) with\n     | (`STR (_,y),`Lid x) -> ((_loc, x, (Some y), (Some t)) : 'e__3 )\n     | _ -> failwith \"(_loc, x, (Some y), (Some t))\n\")\n",
                     (Fgram.mk_action
                        (fun _  (t : 'ctyp)  (__fan_2 : [> FToken.t]) 
                           (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                           match (__fan_2, __fan_1) with
                           | (`STR (_,y),`Lid x) ->
                               ((_loc, x, (Some y), (Some t)) : 'e__3 )
                           | _ -> failwith "(_loc, x, (Some y), (Some t))\n"))));
                 ([`Skeyword "(";
                  `Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"));
                  `Skeyword ":";
                  `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
                  `Sopt
                    (Fgram.srules
                       [([`Stoken
                            (((function | `STR (_,_) -> true | _ -> false)),
                              (`Normal, "`STR (_,_)"))],
                          ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `STR (_,y) -> (y : 'e__2 ) | _ -> failwith \"y\n\")\n",
                            (Fgram.mk_action
                               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)
                                   ->
                                  match __fan_0 with
                                  | `STR (_,y) -> (y : 'e__2 )
                                  | _ -> failwith "y\n"))))]);
                  `Skeyword ")"],
                   ("Fgram.mk_action\n  (fun _  (y : 'e__2 option)  (t : 'ctyp)  _  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Lid x -> ((_loc, x, y, (Some t)) : 'e__3 )\n     | _ -> failwith \"(_loc, x, y, (Some t))\n\")\n",
                     (Fgram.mk_action
                        (fun _  (y : 'e__2 option)  (t : 'ctyp)  _ 
                           (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                           match __fan_1 with
                           | `Lid x -> ((_loc, x, y, (Some t)) : 'e__3 )
                           | _ -> failwith "(_loc, x, y, (Some t))\n"))))])],
             ("Fgram.mk_action\n  (fun (ls : 'e__3 list)  (t : 'e__1)  (_loc : FLoc.t)  ->\n     (let mk =\n        match t with\n        | `static t ->\n            let t = (t : vid  :>exp) in\n            (`Field (_loc, t, (`Lid (_loc, \"mk\"))) : FAst.exp )\n        | `dynamic (x,t) ->\n            let x = (x : vid  :>exp) in\n            let t = (t : vid  :>exp) in\n            (`App (_loc, (`Field (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n              FAst.exp ) in\n      sem_of_list\n        (List.map\n           (fun (_loc,x,descr,ty)  ->\n              match (descr, ty) with\n              | (Some d,None ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`App (_loc, mk, (`Str (_loc, d))))))) : \n                  FAst.stru )\n              | (Some d,Some typ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`Constraint\n                               (_loc, (`App (_loc, mk, (`Str (_loc, d)))),\n                                 typ))))) : FAst.stru )\n              | (None ,None ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`App (_loc, mk, (`Str (_loc, x))))))) : \n                  FAst.stru )\n              | (None ,Some typ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`Constraint\n                               (_loc, (`App (_loc, mk, (`Str (_loc, x)))),\n                                 typ))))) : FAst.stru )) ls) : 'nonterminals ))\n",
               (Fgram.mk_action
                  (fun (ls : 'e__3 list)  (t : 'e__1)  (_loc : FLoc.t)  ->
                     (let mk =
                        match t with
                        | `static t ->
                            let t = (t : vid  :>exp) in
                            (`Field (_loc, t, (`Lid (_loc, "mk"))) : 
                              FAst.exp )
                        | `dynamic (x,t) ->
                            let x = (x : vid  :>exp) in
                            let t = (t : vid  :>exp) in
                            (`App
                               (_loc,
                                 (`Field
                                    (_loc, t, (`Lid (_loc, "mk_dynamic")))),
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
                                            (`App
                                               (_loc, mk, (`Str (_loc, d))))))) : 
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
                                                      (`Str (_loc, d)))),
                                                 typ))))) : FAst.stru )
                              | (None ,None ) ->
                                  (`Value
                                     (_loc, (`Negative _loc),
                                       (`Bind
                                          (_loc, (`Lid (_loc, x)),
                                            (`App
                                               (_loc, mk, (`Str (_loc, x))))))) : 
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
                                                      (`Str (_loc, x)))),
                                                 typ))))) : FAst.stru )) ls) : 
                     'nonterminals )))))]));
    Fgram.extend_single (newterminals : 'newterminals Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "(";
            `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
            `Skeyword ":";
            `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
            `Skeyword ")";
            `Slist1
              (Fgram.srules
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Lid x -> ((_loc, x, None, None) : 'e__5 )\n     | _ -> failwith \"(_loc, x, None, None)\n\")\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Lid x -> ((_loc, x, None, None) : 'e__5 )
                            | _ -> failwith "(_loc, x, None, None)\n"))));
                 ([`Skeyword "(";
                  `Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"));
                  `Stoken
                    (((function | `STR (_,_) -> true | _ -> false)),
                      (`Normal, "`STR (_,_)"));
                  `Skeyword ")"],
                   ("Fgram.mk_action\n  (fun _  (__fan_2 : [> FToken.t])  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match (__fan_2, __fan_1) with\n     | (`STR (_,y),`Lid x) -> ((_loc, x, (Some y), None) : 'e__5 )\n     | _ -> failwith \"(_loc, x, (Some y), None)\n\")\n",
                     (Fgram.mk_action
                        (fun _  (__fan_2 : [> FToken.t]) 
                           (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                           match (__fan_2, __fan_1) with
                           | (`STR (_,y),`Lid x) ->
                               ((_loc, x, (Some y), None) : 'e__5 )
                           | _ -> failwith "(_loc, x, (Some y), None)\n"))));
                 ([`Skeyword "(";
                  `Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"));
                  `Stoken
                    (((function | `STR (_,_) -> true | _ -> false)),
                      (`Normal, "`STR (_,_)"));
                  `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
                  `Skeyword ")"],
                   ("Fgram.mk_action\n  (fun _  (t : 'ctyp)  (__fan_2 : [> FToken.t])  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match (__fan_2, __fan_1) with\n     | (`STR (_,y),`Lid x) -> ((_loc, x, (Some y), (Some t)) : 'e__5 )\n     | _ -> failwith \"(_loc, x, (Some y), (Some t))\n\")\n",
                     (Fgram.mk_action
                        (fun _  (t : 'ctyp)  (__fan_2 : [> FToken.t]) 
                           (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                           match (__fan_2, __fan_1) with
                           | (`STR (_,y),`Lid x) ->
                               ((_loc, x, (Some y), (Some t)) : 'e__5 )
                           | _ -> failwith "(_loc, x, (Some y), (Some t))\n"))));
                 ([`Skeyword "(";
                  `Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"));
                  `Skeyword ":";
                  `Snterm (Fgram.obj (ctyp : 'ctyp Fgram.t ));
                  `Sopt
                    (Fgram.srules
                       [([`Stoken
                            (((function | `STR (_,_) -> true | _ -> false)),
                              (`Normal, "`STR (_,_)"))],
                          ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `STR (_,y) -> (y : 'e__4 ) | _ -> failwith \"y\n\")\n",
                            (Fgram.mk_action
                               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)
                                   ->
                                  match __fan_0 with
                                  | `STR (_,y) -> (y : 'e__4 )
                                  | _ -> failwith "y\n"))))]);
                  `Skeyword ")"],
                   ("Fgram.mk_action\n  (fun _  (y : 'e__4 option)  (t : 'ctyp)  _  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Lid x -> ((_loc, x, y, (Some t)) : 'e__5 )\n     | _ -> failwith \"(_loc, x, y, (Some t))\n\")\n",
                     (Fgram.mk_action
                        (fun _  (y : 'e__4 option)  (t : 'ctyp)  _ 
                           (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                           match __fan_1 with
                           | `Lid x -> ((_loc, x, y, (Some t)) : 'e__5 )
                           | _ -> failwith "(_loc, x, y, (Some t))\n"))))])],
             ("Fgram.mk_action\n  (fun (ls : 'e__5 list)  _  (t : 't_qualid)  _  (x : 'qualid)  _ \n     (_loc : FLoc.t)  ->\n     (let mk =\n        let x = (x : vid  :>exp) in\n        (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n          FAst.exp ) in\n      sem_of_list\n        ((`Value\n            (_loc, (`Negative _loc),\n              (`Bind\n                 (_loc, (x :>pat),\n                   (`App\n                      (_loc,\n                        (`App\n                           (_loc,\n                             (`App\n                                (_loc,\n                                  (`Dot\n                                     (_loc, t, (`Lid (_loc, \"create_lexer\")))),\n                                  (`Label\n                                     (_loc, (`Lid (_loc, \"annot\")),\n                                       (`Str (_loc, \"\")))))),\n                             (`Label\n                                (_loc, (`Lid (_loc, \"keywords\")),\n                                  (`Uid (_loc, \"[]\")))))),\n                        (`Uid (_loc, \"()\"))))))) : FAst.stru ) ::\n        (List.map\n           (fun (_loc,x,descr,ty)  ->\n              match (descr, ty) with\n              | (Some d,None ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`App (_loc, mk, (`Str (_loc, d))))))) : \n                  FAst.stru )\n              | (Some d,Some typ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`Constraint\n                               (_loc, (`App (_loc, mk, (`Str (_loc, d)))),\n                                 typ))))) : FAst.stru )\n              | (None ,None ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`App (_loc, mk, (`Str (_loc, x))))))) : \n                  FAst.stru )\n              | (None ,Some typ) ->\n                  (`Value\n                     (_loc, (`Negative _loc),\n                       (`Bind\n                          (_loc, (`Lid (_loc, x)),\n                            (`Constraint\n                               (_loc, (`App (_loc, mk, (`Str (_loc, x)))),\n                                 typ))))) : FAst.stru )) ls)) : 'newterminals ))\n",
               (Fgram.mk_action
                  (fun (ls : 'e__5 list)  _  (t : 't_qualid)  _ 
                     (x : 'qualid)  _  (_loc : FLoc.t)  ->
                     (let mk =
                        let x = (x : vid  :>exp) in
                        (`App
                           (_loc,
                             (`Dot (_loc, t, (`Lid (_loc, "mk_dynamic")))),
                             x) : FAst.exp ) in
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
                                            (`App
                                               (_loc, mk, (`Str (_loc, d))))))) : 
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
                                                      (`Str (_loc, d)))),
                                                 typ))))) : FAst.stru )
                              | (None ,None ) ->
                                  (`Value
                                     (_loc, (`Negative _loc),
                                       (`Bind
                                          (_loc, (`Lid (_loc, x)),
                                            (`App
                                               (_loc, mk, (`Str (_loc, x))))))) : 
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
                                                      (`Str (_loc, x)))),
                                                 typ))))) : FAst.stru )) ls)) : 
                     'newterminals )))))]));
    Fgram.extend_single (nonterminalsclear : 'nonterminalsclear Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ));
            `Slist1
              (Fgram.srules
                 [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
                    ("Fgram.mk_action (fun (x : 'a_lident)  (_loc : FLoc.t)  -> (x : 'e__6 ))\n",
                      (Fgram.mk_action
                         (fun (x : 'a_lident)  (_loc : FLoc.t)  ->
                            (x : 'e__6 )))))])],
             ("Fgram.mk_action\n  (fun (ls : 'e__6 list)  (t : 'qualuid)  (_loc : FLoc.t)  ->\n     (let rest =\n        List.map\n          (fun (x : alident)  ->\n             let x = (x : alident  :>exp) in\n             let _loc = loc_of x in\n             let t = (t : vid  :>exp) in\n             (`App (_loc, (`Field (_loc, t, (`Lid (_loc, \"clear\")))), x) : \n               FAst.exp )) ls in\n      seq_sem rest : 'nonterminalsclear ))\n",
               (Fgram.mk_action
                  (fun (ls : 'e__6 list)  (t : 'qualuid)  (_loc : FLoc.t)  ->
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
                      seq_sem rest : 'nonterminalsclear )))))]))
  end

let _ =
  begin
    Fgram.extend_single (extend_header : 'extend_header Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "(";
            `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
            `Skeyword ":";
            `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
            `Skeyword ")"],
             ("Fgram.mk_action\n  (fun _  (t : 't_qualid)  _  (i : 'qualid)  _  (_loc : FLoc.t)  ->\n     (let old = gm () in let () = grammar_module_name := t in ((Some i), old) : \n     'extend_header ))\n",
               (Fgram.mk_action
                  (fun _  (t : 't_qualid)  _  (i : 'qualid)  _ 
                     (_loc : FLoc.t)  ->
                     (let old = gm () in
                      let () = grammar_module_name := t in ((Some i), old) : 
                     'extend_header )))));
          ([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
            ("Fgram.mk_action\n  (fun (t : 'qualuid)  (_loc : FLoc.t)  ->\n     (let old = gm () in let () = grammar_module_name := t in (None, old) : \n     'extend_header ))\n",
              (Fgram.mk_action
                 (fun (t : 'qualuid)  (_loc : FLoc.t)  ->
                    (let old = gm () in
                     let () = grammar_module_name := t in (None, old) : 
                    'extend_header )))));
          ([],
            ("Fgram.mk_action (fun (_loc : FLoc.t)  -> ((None, (gm ())) : 'extend_header ))\n",
              (Fgram.mk_action
                 (fun (_loc : FLoc.t)  -> ((None, (gm ())) : 'extend_header )))))]));
    Fgram.extend_single (extend_body : 'extend_body Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (extend_header : 'extend_header Fgram.t ));
            `Sopt (`Snterm (Fgram.obj (locals : 'locals Fgram.t )));
            `Slist1 (`Snterm (Fgram.obj (entry : 'entry Fgram.t )))],
             ("Fgram.mk_action\n  (fun (el : 'entry list)  (locals : 'locals option) \n     ((gram,old) : 'extend_header)  (_loc : FLoc.t)  ->\n     (let res = text_of_functorial_extend _loc gram locals el in\n      let () = grammar_module_name := old in res : 'extend_body ))\n",
               (Fgram.mk_action
                  (fun (el : 'entry list)  (locals : 'locals option) 
                     ((gram,old) : 'extend_header)  (_loc : FLoc.t)  ->
                     (let res = text_of_functorial_extend _loc gram locals el in
                      let () = grammar_module_name := old in res : 'extend_body )))))]));
    Fgram.extend_single (unsafe_extend_body : 'unsafe_extend_body Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (extend_header : 'extend_header Fgram.t ));
            `Sopt (`Snterm (Fgram.obj (locals : 'locals Fgram.t )));
            `Slist1 (`Snterm (Fgram.obj (entry : 'entry Fgram.t )))],
             ("Fgram.mk_action\n  (fun (el : 'entry list)  (locals : 'locals option) \n     ((gram,old) : 'extend_header)  (_loc : FLoc.t)  ->\n     (let res = text_of_functorial_extend ~safe:false _loc gram locals el in\n      let () = grammar_module_name := old in res : 'unsafe_extend_body ))\n",
               (Fgram.mk_action
                  (fun (el : 'entry list)  (locals : 'locals option) 
                     ((gram,old) : 'extend_header)  (_loc : FLoc.t)  ->
                     (let res =
                        text_of_functorial_extend ~safe:false _loc gram
                          locals el in
                      let () = grammar_module_name := old in res : 'unsafe_extend_body )))))]));
    Fgram.extend_single (delete_rule_header : 'delete_rule_header Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
             ("Fgram.mk_action\n  (fun (g : 'qualuid)  (_loc : FLoc.t)  ->\n     (let old = gm () in let () = grammar_module_name := g in old : 'delete_rule_header ))\n",
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
             ("Fgram.mk_action\n  (fun (es : 'delete_rules list)  (old : 'delete_rule_header) \n     (_loc : FLoc.t)  ->\n     (begin grammar_module_name := old; seq_sem es end : 'delete_rule_body ))\n",
               (Fgram.mk_action
                  (fun (es : 'delete_rules list)  (old : 'delete_rule_header)
                      (_loc : FLoc.t)  ->
                     (begin grammar_module_name := old; seq_sem es end : 
                     'delete_rule_body )))))]));
    Fgram.extend_single (delete_rules : 'delete_rules Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (name : 'name Fgram.t ));
            `Skeyword ":";
            `Skeyword "[";
            `Slist1sep
              ((Fgram.srules
                  [([`Slist0sep
                       ((`Snterm (Fgram.obj (psymbol : 'psymbol Fgram.t ))),
                         (`Skeyword ";"))],
                     ("Fgram.mk_action (fun (sl : 'psymbol list)  (_loc : FLoc.t)  -> (sl : 'e__7 ))\n",
                       (Fgram.mk_action
                          (fun (sl : 'psymbol list)  (_loc : FLoc.t)  ->
                             (sl : 'e__7 )))))]), (`Skeyword "|"));
            `Skeyword "]"],
             ("Fgram.mk_action\n  (fun _  (sls : 'e__7 list)  _  _  (n : 'name)  (_loc : FLoc.t)  ->\n     (exp_delete_rule _loc n sls : 'delete_rules ))\n",
               (Fgram.mk_action
                  (fun _  (sls : 'e__7 list)  _  _  (n : 'name) 
                     (_loc : FLoc.t)  ->
                     (exp_delete_rule _loc n sls : 'delete_rules )))))]));
    Fgram.extend_single (qualuid : 'qualuid Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Uid _ -> true | _ -> false)),
                 (`Normal, "`Uid _"));
            `Skeyword ".";
            `Sself],
             ("Fgram.mk_action\n  (fun (xs : 'qualuid)  _  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, x)), xs)\n\")\n",
               (Fgram.mk_action
                  (fun (xs : 'qualuid)  _  (__fan_0 : [> FToken.t]) 
                     (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Uid x ->
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                     | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
          ([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Uid (_loc, x) : 'qualuid )\n     | _ -> failwith \"`Uid (_loc, x)\n\")\n",
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
                 (`Normal, "`Uid _"));
            `Skeyword ".";
            `Sself],
             ("Fgram.mk_action\n  (fun (xs : 'qualid)  _  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, x)), xs)\n\")\n",
               (Fgram.mk_action
                  (fun (xs : 'qualid)  _  (__fan_0 : [> FToken.t]) 
                     (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Uid x ->
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                     | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
          ([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'qualid )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
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
                 (`Normal, "`Uid _"));
            `Skeyword ".";
            `Sself],
             ("Fgram.mk_action\n  (fun (xs : 't_qualid)  _  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid x -> (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )\n     | _ -> failwith \"`Dot (_loc, (`Uid (_loc, x)), xs)\n\")\n",
               (Fgram.mk_action
                  (fun (xs : 't_qualid)  _  (__fan_0 : [> FToken.t]) 
                     (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Uid x ->
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                     | _ -> failwith "`Dot (_loc, (`Uid (_loc, x)), xs)\n"))));
          ([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"));
           `Skeyword ".";
           `Stoken
             (((function | `Lid "t" -> true | _ -> false)),
               (`Normal, "`Lid \"t\""))],
            ("Fgram.mk_action\n  (fun (__fan_2 : [> FToken.t])  _  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)\n      ->\n     match (__fan_2, __fan_0) with\n     | (`Lid \"t\",`Uid x) -> (`Uid (_loc, x) : 't_qualid )\n     | _ -> failwith \"`Uid (_loc, x)\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_2 : [> FToken.t])  _  (__fan_0 : [> FToken.t]) 
                    (_loc : FLoc.t)  ->
                    match (__fan_2, __fan_0) with
                    | (`Lid "t",`Uid x) -> (`Uid (_loc, x) : 't_qualid )
                    | _ -> failwith "`Uid (_loc, x)\n"))))]));
    Fgram.extend_single (locals : 'locals Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Lid "local" -> true | _ -> false)),
                 (`Normal, "`Lid \"local\""));
            `Skeyword ":";
            `Slist1 (`Snterm (Fgram.obj (name : 'name Fgram.t )));
            `Skeyword ";"],
             ("Fgram.mk_action\n  (fun _  (sl : 'name list)  _  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Lid \"local\" -> (sl : 'locals )\n     | _ -> failwith \"sl\n\")\n",
               (Fgram.mk_action
                  (fun _  (sl : 'name list)  _  (__fan_0 : [> FToken.t]) 
                     (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Lid "local" -> (sl : 'locals )
                     | _ -> failwith "sl\n"))))]));
    Fgram.extend_single (name : 'name Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (qualid : 'qualid Fgram.t ))],
             ("Fgram.mk_action\n  (fun (il : 'qualid)  (_loc : FLoc.t)  -> (mk_name _loc il : 'name ))\n",
               (Fgram.mk_action
                  (fun (il : 'qualid)  (_loc : FLoc.t)  ->
                     (mk_name _loc il : 'name )))))]));
    Fgram.extend_single (entry_name : 'entry_name Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
            `Sopt
              (Fgram.srules
                 [([`Stoken
                      (((function | `STR (_,_) -> true | _ -> false)),
                        (`Normal, "`STR (_,_)"))],
                    ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `STR (_,x) -> (x : 'e__8 ) | _ -> failwith \"x\n\")\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `STR (_,x) -> (x : 'e__8 )
                            | _ -> failwith "x\n"))))])],
             ("Fgram.mk_action\n  (fun (name : 'e__8 option)  (il : 'qualid)  (_loc : FLoc.t)  ->\n     (((match name with\n        | Some x ->\n            let old = AstQuotation.default.contents in\n            begin\n              AstQuotation.default :=\n                (FToken.resolve_name _loc ((`Sub []), x));\n              `name old\n            end\n        | None  -> `non), (mk_name _loc il)) : 'entry_name ))\n",
               (Fgram.mk_action
                  (fun (name : 'e__8 option)  (il : 'qualid)  (_loc : FLoc.t)
                      ->
                     (((match name with
                        | Some x ->
                            let old = AstQuotation.default.contents in
                            begin
                              AstQuotation.default :=
                                (FToken.resolve_name _loc ((`Sub []), x));
                              `name old
                            end
                        | None  -> `non), (mk_name _loc il)) : 'entry_name )))))]));
    Fgram.extend_single (entry : 'entry Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (entry_name : 'entry_name Fgram.t ));
            `Skeyword ":";
            `Sopt (`Snterm (Fgram.obj (position : 'position Fgram.t )));
            `Snterm (Fgram.obj (level_list : 'level_list Fgram.t ))],
             ("Fgram.mk_action\n  (fun (levels : 'level_list)  (pos : 'position option)  _ \n     ((n,p) : 'entry_name)  (_loc : FLoc.t)  ->\n     (begin\n        (match n with | `name old -> AstQuotation.default := old | _ -> ());\n        (match (pos, levels) with\n         | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n             failwithf\n               \"For Group levels the position can not be applied to Level\"\n         | _ -> mk_entry ~name:p ~pos ~levels)\n      end : 'entry ))\n",
               (Fgram.mk_action
                  (fun (levels : 'level_list)  (pos : 'position option)  _ 
                     ((n,p) : 'entry_name)  (_loc : FLoc.t)  ->
                     (begin
                        (match n with
                         | `name old -> AstQuotation.default := old
                         | _ -> ());
                        (match (pos, levels) with
                         | (Some
                            (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),
                            `Group _) ->
                             failwithf
                               "For Group levels the position can not be applied to Level"
                         | _ -> mk_entry ~name:p ~pos ~levels)
                      end : 'entry )))))]));
    Fgram.extend_single (position : 'position Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Uid ("First"|"Last") -> true | _ -> false)),
                 (`Normal, "`Uid (\"First\"|\"Last\")"))],
             ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"First\"|\"Last\" as x) ->\n         ((`Vrn (_loc, x) : FAst.exp ) : 'position )\n     | _ -> failwith \"(`Vrn (_loc, x) : FAst.exp )\n\")\n",
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
                (`Normal, "`Uid (\"Before\"|\"After\"|\"Level\")"));
           `Snterm (Fgram.obj (string : 'string Fgram.t ))],
            ("Fgram.mk_action\n  (fun (n : 'string)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"Before\"|\"After\"|\"Level\" as x) ->\n         ((`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp ) : 'position )\n     | _ -> failwith \"(`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp )\n\")\n",
              (Fgram.mk_action
                 (fun (n : 'string)  (__fan_0 : [> FToken.t]) 
                    (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `Uid ("Before"|"After"|"Level" as x) ->
                        ((`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp ) : 
                        'position )
                    | _ ->
                        failwith
                          "(`App (_loc, (`Vrn (_loc, x)), n) : FAst.exp )\n"))));
          ([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid x ->\n         (failwithf\n            \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n            x : 'position )\n     | _ ->\n         failwith\n           \"failwithf \"%s is not the right position:(First|Last) or (Before|After|Level)\"\n  x\n\")\n",
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
             ("Fgram.mk_action\n  (fun _  (ll : 'level list)  _  (_loc : FLoc.t)  ->\n     (`Group ll : 'level_list ))\n",
               (Fgram.mk_action
                  (fun _  (ll : 'level list)  _  (_loc : FLoc.t)  ->
                     (`Group ll : 'level_list )))));
          ([`Snterm (Fgram.obj (level : 'level Fgram.t ))],
            ("Fgram.mk_action\n  (fun (l : 'level)  (_loc : FLoc.t)  -> (`Single l : 'level_list ))\n",
              (Fgram.mk_action
                 (fun (l : 'level)  (_loc : FLoc.t)  ->
                    (`Single l : 'level_list )))))]));
    Fgram.extend_single (level : 'level Fgram.t )
      (None,
        (None, None,
          [([`Sopt
               (Fgram.srules
                  [([`Stoken
                       (((function | `STR (_,_) -> true | _ -> false)),
                         (`Normal, "`STR (_,_)"))],
                     ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `STR (_,x) -> (x : 'e__9 ) | _ -> failwith \"x\n\")\n",
                       (Fgram.mk_action
                          (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                             match __fan_0 with
                             | `STR (_,x) -> (x : 'e__9 )
                             | _ -> failwith "x\n"))))]);
            `Sopt (`Snterm (Fgram.obj (assoc : 'assoc Fgram.t )));
            `Snterm (Fgram.obj (rule_list : 'rule_list Fgram.t ))],
             ("Fgram.mk_action\n  (fun (rules : 'rule_list)  (assoc : 'assoc option)  (label : 'e__9 option) \n     (_loc : FLoc.t)  -> (mk_level ~label ~assoc ~rules : 'level ))\n",
               (Fgram.mk_action
                  (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                     (label : 'e__9 option)  (_loc : FLoc.t)  ->
                     (mk_level ~label ~assoc ~rules : 'level )))))]));
    Fgram.extend_single (assoc : 'assoc Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Uid ("LA"|"RA"|"NA") -> true | _ -> false)),
                 (`Normal, "`Uid (\"LA\"|\"RA\"|\"NA\")"))],
             ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"LA\"|\"RA\"|\"NA\" as x) -> ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )\n     | _ -> failwith \"(`Vrn (_loc, x) : FAst.exp )\n\")\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Uid ("LA"|"RA"|"NA" as x) ->
                         ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                     | _ -> failwith "(`Vrn (_loc, x) : FAst.exp )\n"))));
          ([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid x ->\n         (failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x : \n         'assoc )\n     | _ ->\n         failwith\n           \"failwithf \"%s is not a correct associativity:(LA|RA|NA)\" x\n\")\n",
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
             ("Fgram.mk_action (fun _  _  (_loc : FLoc.t)  -> ([] : 'rule_list ))\n",
               (Fgram.mk_action
                  (fun _  _  (_loc : FLoc.t)  -> ([] : 'rule_list )))));
          ([`Skeyword "[";
           `Slist1sep
             ((`Snterm (Fgram.obj (rule : 'rule Fgram.t ))), (`Skeyword "|"));
           `Skeyword "]"],
            ("Fgram.mk_action\n  (fun _  (rules : 'rule list)  _  (_loc : FLoc.t)  ->\n     (retype_rule_list_without_patterns _loc rules : 'rule_list ))\n",
              (Fgram.mk_action
                 (fun _  (rules : 'rule list)  _  (_loc : FLoc.t)  ->
                    (retype_rule_list_without_patterns _loc rules : 'rule_list )))))]));
    Fgram.extend_single (rule : 'rule Fgram.t )
      (None,
        (None, None,
          [([`Slist0sep
               ((`Snterm (Fgram.obj (psymbol : 'psymbol Fgram.t ))),
                 (`Skeyword ";"));
            `Sopt
              (Fgram.srules
                 [([`Skeyword "->";
                   `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
                    ("Fgram.mk_action (fun (act : 'exp)  _  (_loc : FLoc.t)  -> (act : 'e__10 ))\n",
                      (Fgram.mk_action
                         (fun (act : 'exp)  _  (_loc : FLoc.t)  ->
                            (act : 'e__10 )))))])],
             ("Fgram.mk_action\n  (fun (action : 'e__10 option)  (prod : 'psymbol list)  (_loc : FLoc.t)  ->\n     (mk_rule ~prod ~action : 'rule ))\n",
               (Fgram.mk_action
                  (fun (action : 'e__10 option)  (prod : 'psymbol list) 
                     (_loc : FLoc.t)  -> (mk_rule ~prod ~action : 'rule )))))]));
    Fgram.extend_single (psymbol : 'psymbol Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (symbol : 'symbol Fgram.t ));
            `Sopt
              (Fgram.srules
                 [([`Skeyword "{";
                   `Snterm (Fgram.obj (pattern : 'pattern Fgram.t ));
                   `Skeyword "}"],
                    ("Fgram.mk_action (fun _  (p : 'pattern)  _  (_loc : FLoc.t)  -> (p : 'e__11 ))\n",
                      (Fgram.mk_action
                         (fun _  (p : 'pattern)  _  (_loc : FLoc.t)  ->
                            (p : 'e__11 )))))])],
             ("Fgram.mk_action\n  (fun (p : 'e__11 option)  (s : 'symbol)  (_loc : FLoc.t)  ->\n     (match p with\n      | Some _ ->\n          { s with pattern = (p : action_pattern option  :>pat option) }\n      | None  -> s : 'psymbol ))\n",
               (Fgram.mk_action
                  (fun (p : 'e__11 option)  (s : 'symbol)  (_loc : FLoc.t) 
                     ->
                     (match p with
                      | Some _ ->
                          {
                            s with
                            pattern =
                              (p : action_pattern option  :>pat option)
                          }
                      | None  -> s : 'psymbol )))))]));
    Fgram.extend_single (symbol : 'symbol Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Uid ("L0"|"L1") -> true | _ -> false)),
                 (`Normal, "`Uid (\"L0\"|\"L1\")"));
            `Sself;
            `Sopt
              (Fgram.srules
                 [([`Stoken
                      (((function | `Uid "SEP" -> true | _ -> false)),
                        (`Normal, "`Uid \"SEP\""));
                   `Snterm (Fgram.obj (symbol : 'symbol Fgram.t ))],
                    ("Fgram.mk_action\n  (fun (t : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Uid \"SEP\" -> (t : 'e__12 ) | _ -> failwith \"t\n\")\n",
                      (Fgram.mk_action
                         (fun (t : 'symbol)  (__fan_0 : [> FToken.t]) 
                            (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Uid "SEP" -> (t : 'e__12 )
                            | _ -> failwith "t\n"))))])],
             ("Fgram.mk_action\n  (fun (sep : 'e__12 option)  (s : 'symbol)  (__fan_0 : [> FToken.t]) \n     (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid (\"L0\"|\"L1\" as x) ->\n         (let () = check_not_tok s in\n          let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\n          let text =\n            mk_slist _loc\n              (match x with\n               | \"L0\" -> false\n               | \"L1\" -> true\n               | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\n          mk_symbol ~text ~styp ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let () = check_not_tok s in\nlet styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text =\n  mk_slist _loc\n    (match x with\n     | \"L0\" -> false\n     | \"L1\" -> true\n     | _ -> failwithf \"only (L0|L1) allowed here\") sep s in\nmk_symbol ~text ~styp ~pattern:None\n\")\n",
               (Fgram.mk_action
                  (fun (sep : 'e__12 option)  (s : 'symbol) 
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
                (`Normal, "`Uid \"OPT\""));
           `Sself],
            ("Fgram.mk_action\n  (fun (s : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"OPT\" ->\n         (let () = check_not_tok s in\n          let styp = `App (_loc, (`Lid (_loc, \"option\")), (s.styp)) in\n          let text = `Sopt (_loc, (s.text)) in\n          mk_symbol ~text ~styp ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let () = check_not_tok s in\nlet styp = `App (_loc, (`Lid (_loc, \"option\")), (s.styp)) in\nlet text = `Sopt (_loc, (s.text)) in mk_symbol ~text ~styp ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (s : 'symbol)  (__fan_0 : [> FToken.t]) 
                    (_loc : FLoc.t)  ->
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
                (`Normal, "`Uid \"TRY\""));
           `Sself],
            ("Fgram.mk_action\n  (fun (s : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"TRY\" ->\n         (let text = `Stry (_loc, (s.text)) in\n          mk_symbol ~text ~styp:(s.styp) ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let text = `Stry (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (s : 'symbol)  (__fan_0 : [> FToken.t]) 
                    (_loc : FLoc.t)  ->
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
            ("Fgram.mk_action\n  (fun (s : 'symbol)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"PEEK\" ->\n         (let text = `Speek (_loc, (s.text)) in\n          mk_symbol ~text ~styp:(s.styp) ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let text = `Speek (_loc, (s.text)) in\nmk_symbol ~text ~styp:(s.styp) ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (s : 'symbol)  (__fan_0 : [> FToken.t]) 
                    (_loc : FLoc.t)  ->
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
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"S\" ->\n         (mk_symbol ~text:(`Sself _loc) ~styp:(`Self (_loc, \"S\"))\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"mk_symbol ~text:(`Sself _loc) ~styp:(`Self (_loc, \"S\")) ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `Uid "S" ->
                        (mk_symbol ~text:(`Sself _loc)
                           ~styp:(`Self (_loc, "S")) ~pattern:None : 
                        'symbol )
                    | _ ->
                        failwith
                          "mk_symbol ~text:(`Sself _loc) ~styp:(`Self (_loc, \"S\")) ~pattern:None\n"))));
          ([`Stoken
              (((function | `Uid "N" -> true | _ -> false)),
                (`Normal, "`Uid \"N\""))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Uid \"N\" ->\n         (mk_symbol ~text:(`Snext _loc) ~styp:(`Self (_loc, \"N\"))\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"mk_symbol ~text:(`Snext _loc) ~styp:(`Self (_loc, \"N\")) ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `Uid "N" ->
                        (mk_symbol ~text:(`Snext _loc)
                           ~styp:(`Self (_loc, "N")) ~pattern:None : 
                        'symbol )
                    | _ ->
                        failwith
                          "mk_symbol ~text:(`Snext _loc) ~styp:(`Self (_loc, \"N\")) ~pattern:None\n"))));
          ([`Skeyword "[";
           `Slist1sep
             ((`Snterm (Fgram.obj (rule : 'rule Fgram.t ))), (`Skeyword "|"));
           `Skeyword "]"],
            ("Fgram.mk_action\n  (fun _  (rl : 'rule list)  _  (_loc : FLoc.t)  ->\n     (let rl = retype_rule_list_without_patterns _loc rl in\n      let t = new_type_var () in\n      mk_symbol ~text:(`Srules (_loc, (mk_srules _loc t rl \"\")))\n        ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, t)))) ~pattern:None : \n     'symbol ))\n",
              (Fgram.mk_action
                 (fun _  (rl : 'rule list)  _  (_loc : FLoc.t)  ->
                    (let rl = retype_rule_list_without_patterns _loc rl in
                     let t = new_type_var () in
                     mk_symbol
                       ~text:(`Srules (_loc, (mk_srules _loc t rl "")))
                       ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, t))))
                       ~pattern:None : 'symbol )))));
          ([`Snterm (Fgram.obj (simple_pat : 'simple_pat Fgram.t ))],
            ("Fgram.mk_action\n  (fun (p : 'simple_pat)  (_loc : FLoc.t)  ->\n     (let (p,ls) =\n        Exp.filter_pat_with_captured_variables (p : simple_pat  :>pat) in\n      match ls with\n      | [] -> mk_tok _loc ~pattern:p (`Tok _loc)\n      | (x,y)::ys ->\n          let restrict =\n            List.fold_left\n              (fun acc  (x,y)  ->\n                 (`App\n                    (_loc, (`App (_loc, (`Lid (_loc, \"&&\")), acc)),\n                      (`App (_loc, (`App (_loc, (`Lid (_loc, \"=\")), x)), y))) : \n                 FAst.exp ))\n              (`App (_loc, (`App (_loc, (`Lid (_loc, \"=\")), x)), y) : \n              FAst.exp ) ys in\n          mk_tok _loc ~restrict ~pattern:p (`Tok _loc) : 'symbol ))\n",
              (Fgram.mk_action
                 (fun (p : 'simple_pat)  (_loc : FLoc.t)  ->
                    (let (p,ls) =
                       Exp.filter_pat_with_captured_variables
                         (p : simple_pat  :>pat) in
                     match ls with
                     | [] -> mk_tok _loc ~pattern:p (`Tok _loc)
                     | (x,y)::ys ->
                         let restrict =
                           List.fold_left
                             (fun acc  (x,y)  ->
                                (`App
                                   (_loc,
                                     (`App (_loc, (`Lid (_loc, "&&")), acc)),
                                     (`App
                                        (_loc,
                                          (`App (_loc, (`Lid (_loc, "=")), x)),
                                          y))) : FAst.exp ))
                             (`App
                                (_loc, (`App (_loc, (`Lid (_loc, "=")), x)),
                                  y) : FAst.exp ) ys in
                         mk_tok _loc ~restrict ~pattern:p (`Tok _loc) : 
                    'symbol )))));
          ([`Stoken
              (((function | `STR (_,_) -> true | _ -> false)),
                (`Normal, "`STR (_,_)"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) ->\n         (mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc)\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `STR (_,s) ->
                        (mk_symbol ~text:(`Skeyword (_loc, s))
                           ~styp:(`Tok _loc) ~pattern:None : 'symbol )
                    | _ ->
                        failwith
                          "mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None\n"))));
          ([`Snterm (Fgram.obj (name : 'name Fgram.t ));
           `Sopt
             (Fgram.srules
                [([`Stoken
                     (((function | `Uid "Level" -> true | _ -> false)),
                       (`Normal, "`Uid \"Level\""));
                  `Stoken
                    (((function | `STR (_,_) -> true | _ -> false)),
                      (`Normal, "`STR (_,_)"))],
                   ("Fgram.mk_action\n  (fun (__fan_1 : [> FToken.t])  (__fan_0 : [> FToken.t])  (_loc : FLoc.t) \n     ->\n     match (__fan_1, __fan_0) with\n     | (`STR (_,s),`Uid \"Level\") -> (s : 'e__13 )\n     | _ -> failwith \"s\n\")\n",
                     (Fgram.mk_action
                        (fun (__fan_1 : [> FToken.t]) 
                           (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                           match (__fan_1, __fan_0) with
                           | (`STR (_,s),`Uid "Level") -> (s : 'e__13 )
                           | _ -> failwith "s\n"))))])],
            ("Fgram.mk_action\n  (fun (lev : 'e__13 option)  (n : 'name)  (_loc : FLoc.t)  ->\n     (mk_symbol ~text:(`Snterm (_loc, n, lev))\n        ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n        ~pattern:None : 'symbol ))\n",
              (Fgram.mk_action
                 (fun (lev : 'e__13 option)  (n : 'name)  (_loc : FLoc.t)  ->
                    (mk_symbol ~text:(`Snterm (_loc, n, lev))
                       ~styp:(`Quote
                                (_loc, (`Normal _loc),
                                  (`Lid (_loc, (n.tvar))))) ~pattern:None : 
                    'symbol )))));
          ([`Stoken
              (((function | `Ant (("nt"|""),_) -> true | _ -> false)),
                (`Normal, "`Ant ((\"nt\"|\"\"),_)"));
           `Sopt
             (Fgram.srules
                [([`Stoken
                     (((function | `Uid "Level" -> true | _ -> false)),
                       (`Normal, "`Uid \"Level\""));
                  `Stoken
                    (((function | `STR (_,_) -> true | _ -> false)),
                      (`Normal, "`STR (_,_)"))],
                   ("Fgram.mk_action\n  (fun (__fan_1 : [> FToken.t])  (__fan_0 : [> FToken.t])  (_loc : FLoc.t) \n     ->\n     match (__fan_1, __fan_0) with\n     | (`STR (_,s),`Uid \"Level\") -> (s : 'e__14 )\n     | _ -> failwith \"s\n\")\n",
                     (Fgram.mk_action
                        (fun (__fan_1 : [> FToken.t]) 
                           (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                           match (__fan_1, __fan_0) with
                           | (`STR (_,s),`Uid "Level") -> (s : 'e__14 )
                           | _ -> failwith "s\n"))))])],
            ("Fgram.mk_action\n  (fun (lev : 'e__14 option)  (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Ant ((\"nt\"|\"\"),s) ->\n         (let i = parse_ident _loc s in\n          let rec to_vid (x : ident) =\n            (match x with\n             | `Apply _ -> failwithf \"Id.to_vid\"\n             | `Dot (_loc,a,b) -> `Dot (_loc, (to_vid a), (to_vid b))\n             | `Lid _|`Uid _|`Ant _ as x -> x : vid ) in\n          let n = mk_name _loc (to_vid i) in\n          mk_symbol ~text:(`Snterm (_loc, n, lev))\n            ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n            ~pattern:None : 'symbol )\n     | _ ->\n         failwith\n           \"let i = parse_ident _loc s in\nlet rec to_vid (x : ident) =\n  (match x with\n   | `Apply _ -> failwithf \"Id.to_vid\"\n   | `Dot (_loc,a,b) -> `Dot (_loc, (to_vid a), (to_vid b))\n   | `Lid _|`Uid _|`Ant _ as x -> x : vid ) in\nlet n = mk_name _loc (to_vid i) in\nmk_symbol ~text:(`Snterm (_loc, n, lev))\n  ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n  ~pattern:None\n\")\n",
              (Fgram.mk_action
                 (fun (lev : 'e__14 option)  (__fan_0 : [> FToken.t]) 
                    (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `Ant (("nt"|""),s) ->
                        (let i = parse_ident _loc s in
                         let rec to_vid (x : ident) =
                           (match x with
                            | `Apply _ -> failwithf "Id.to_vid"
                            | `Dot (_loc,a,b) ->
                                `Dot (_loc, (to_vid a), (to_vid b))
                            | `Lid _|`Uid _|`Ant _ as x -> x : vid ) in
                         let n = mk_name _loc (to_vid i) in
                         mk_symbol ~text:(`Snterm (_loc, n, lev))
                           ~styp:(`Quote
                                    (_loc, (`Normal _loc),
                                      (`Lid (_loc, (n.tvar))))) ~pattern:None : 
                        'symbol )
                    | _ ->
                        failwith
                          "let i = parse_ident _loc s in\nlet rec to_vid (x : ident) =\n  (match x with\n   | `Apply _ -> failwithf \"Id.to_vid\"\n   | `Dot (_loc,a,b) -> `Dot (_loc, (to_vid a), (to_vid b))\n   | `Lid _|`Uid _|`Ant _ as x -> x : vid ) in\nlet n = mk_name _loc (to_vid i) in\nmk_symbol ~text:(`Snterm (_loc, n, lev))\n  ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n  ~pattern:None\n"))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            ("Fgram.mk_action (fun _  (s : 'symbol)  _  (_loc : FLoc.t)  -> (s : 'symbol ))\n",
              (Fgram.mk_action
                 (fun _  (s : 'symbol)  _  (_loc : FLoc.t)  -> (s : 'symbol )))))]));
    Fgram.extend_single (simple_pat : 'simple_pat Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "`";
            `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
             ("Fgram.mk_action\n  (fun (s : 'luident)  _  (_loc : FLoc.t)  -> (`Vrn (_loc, s) : 'simple_pat ))\n",
               (Fgram.mk_action
                  (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                     (`Vrn (_loc, s) : 'simple_pat )))));
          ([`Skeyword "`";
           `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
           `Stoken
             (((function | `Ant ((""|"anti"),_) -> true | _ -> false)),
               (`Normal, "`Ant ((\"\"|\"anti\"),_)"))],
            ("Fgram.mk_action\n  (fun (__fan_2 : [> FToken.t])  (v : 'luident)  _  (_loc : FLoc.t)  ->\n     match __fan_2 with\n     | `Ant ((\"\"|\"anti\" as n),s) ->\n         (`App (_loc, (`Vrn (_loc, v)), (mk_anti _loc ~c:\"pat\" n s)) : \n         'simple_pat )\n     | _ ->\n         failwith\n           \"`App (_loc, (`Vrn (_loc, v)), (mk_anti _loc ~c:\"pat\" n s))\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_2 : [> FToken.t])  (v : 'luident)  _ 
                    (_loc : FLoc.t)  ->
                    match __fan_2 with
                    | `Ant ((""|"anti" as n),s) ->
                        (`App
                           (_loc, (`Vrn (_loc, v)),
                             (mk_anti _loc ~c:"pat" n s)) : 'simple_pat )
                    | _ ->
                        failwith
                          "`App (_loc, (`Vrn (_loc, v)), (mk_anti _loc ~c:\"pat\" n s))\n"))));
          ([`Skeyword "`";
           `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
           `Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
            ("Fgram.mk_action\n  (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _  (_loc : FLoc.t)  ->\n     match __fan_2 with\n     | `STR (_,v) ->\n         (`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v))) : 'simple_pat )\n     | _ -> failwith \"`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _ 
                    (_loc : FLoc.t)  ->
                    match __fan_2 with
                    | `STR (_,v) ->
                        (`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v))) : 
                        'simple_pat )
                    | _ ->
                        failwith
                          "`App (_loc, (`Vrn (_loc, s)), (`Str (_loc, v)))\n"))));
          ([`Skeyword "`";
           `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
           `Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`Normal, "`Lid _"))],
            ("Fgram.mk_action\n  (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _  (_loc : FLoc.t)  ->\n     match __fan_2 with\n     | `Lid x ->\n         (`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x))) : 'simple_pat )\n     | _ -> failwith \"`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x)))\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_2 : [> FToken.t])  (s : 'luident)  _ 
                    (_loc : FLoc.t)  ->
                    match __fan_2 with
                    | `Lid x ->
                        (`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x))) : 
                        'simple_pat )
                    | _ ->
                        failwith
                          "`App (_loc, (`Vrn (_loc, s)), (`Lid (_loc, x)))\n"))));
          ([`Skeyword "`";
           `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
           `Skeyword "_"],
            ("Fgram.mk_action\n  (fun _  (s : 'luident)  _  (_loc : FLoc.t)  ->\n     (`App (_loc, (`Vrn (_loc, s)), (`Any _loc)) : 'simple_pat ))\n",
              (Fgram.mk_action
                 (fun _  (s : 'luident)  _  (_loc : FLoc.t)  ->
                    (`App (_loc, (`Vrn (_loc, s)), (`Any _loc)) : 'simple_pat )))));
          ([`Skeyword "`";
           `Snterm (Fgram.obj (luident : 'luident Fgram.t ));
           `Skeyword "(";
           `Slist1sep
             ((`Snterm (Fgram.obj (internal_pat : 'internal_pat Fgram.t ))),
               (`Skeyword ","));
           `Skeyword ")"],
            ("Fgram.mk_action\n  (fun _  (v : 'internal_pat list)  _  (s : 'luident)  _  (_loc : FLoc.t)  ->\n     (match v with\n      | x::[] -> `App (_loc, (`Vrn (_loc, s)), x)\n      | x::xs ->\n          let xs = com_of_list xs in\n          `App (_loc, (`App (_loc, (`Vrn (_loc, s)), x)), xs)\n      | [] -> assert false : 'simple_pat ))\n",
              (Fgram.mk_action
                 (fun _  (v : 'internal_pat list)  _  (s : 'luident)  _ 
                    (_loc : FLoc.t)  ->
                    (match v with
                     | x::[] -> `App (_loc, (`Vrn (_loc, s)), x)
                     | x::xs ->
                         let xs = com_of_list xs in
                         `App (_loc, (`App (_loc, (`Vrn (_loc, s)), x)), xs)
                     | [] -> assert false : 'simple_pat )))))]));
    Fgram.extend (internal_pat : 'internal_pat Fgram.t )
      (None,
        [((Some "as"), None,
           [([`Sself;
             `Skeyword "as";
             `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
              ("Fgram.mk_action\n  (fun (s : 'a_lident)  _  (p1 : 'internal_pat)  (_loc : FLoc.t)  ->\n     (`Alias (_loc, p1, s) : 'internal_pat ))\n",
                (Fgram.mk_action
                   (fun (s : 'a_lident)  _  (p1 : 'internal_pat) 
                      (_loc : FLoc.t)  ->
                      (`Alias (_loc, p1, s) : 'internal_pat )))))]);
        ((Some "|"), None,
          [([`Sself; `Skeyword "|"; `Sself],
             ("Fgram.mk_action\n  (fun (p2 : 'internal_pat)  _  (p1 : 'internal_pat)  (_loc : FLoc.t)  ->\n     (`Bar (_loc, p1, p2) : 'internal_pat ))\n",
               (Fgram.mk_action
                  (fun (p2 : 'internal_pat)  _  (p1 : 'internal_pat) 
                     (_loc : FLoc.t)  ->
                     (`Bar (_loc, p1, p2) : 'internal_pat )))))]);
        ((Some "simple"), None,
          [([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
             ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> (`Str (_loc, s) : 'internal_pat )\n     | _ -> failwith \"`Str (_loc, s)\n\")\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `STR (_,s) -> (`Str (_loc, s) : 'internal_pat )
                     | _ -> failwith "`Str (_loc, s)\n"))));
          ([`Skeyword "_"],
            ("Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'internal_pat ))\n",
              (Fgram.mk_action
                 (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'internal_pat )))));
          ([`Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`Normal, "`Lid _"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Lid x -> (`Lid (_loc, x) : 'internal_pat )\n     | _ -> failwith \"`Lid (_loc, x)\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `Lid x -> (`Lid (_loc, x) : 'internal_pat )
                    | _ -> failwith "`Lid (_loc, x)\n"))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            ("Fgram.mk_action\n  (fun _  (p : 'internal_pat)  _  (_loc : FLoc.t)  -> (p : 'internal_pat ))\n",
              (Fgram.mk_action
                 (fun _  (p : 'internal_pat)  _  (_loc : FLoc.t)  ->
                    (p : 'internal_pat )))))])]);
    Fgram.extend_single (pattern : 'pattern Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Lid _ -> true | _ -> false)),
                 (`Normal, "`Lid _"))],
             ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Lid i -> (`Lid (_loc, i) : 'pattern )\n     | _ -> failwith \"`Lid (_loc, i)\n\")\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Lid i -> (`Lid (_loc, i) : 'pattern )
                     | _ -> failwith "`Lid (_loc, i)\n"))));
          ([`Skeyword "_"],
            ("Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'pattern ))\n",
              (Fgram.mk_action
                 (fun _  (_loc : FLoc.t)  -> (`Any _loc : 'pattern )))));
          ([`Skeyword "("; `Sself; `Skeyword ")"],
            ("Fgram.mk_action\n  (fun _  (p : 'pattern)  _  (_loc : FLoc.t)  -> (p : 'pattern ))\n",
              (Fgram.mk_action
                 (fun _  (p : 'pattern)  _  (_loc : FLoc.t)  ->
                    (p : 'pattern )))));
          ([`Skeyword "(";
           `Sself;
           `Skeyword ",";
           `Slist1sep (`Sself, (`Skeyword ","));
           `Skeyword ")"],
            ("Fgram.mk_action\n  (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _  (_loc : FLoc.t)  ->\n     (tuple_com (p1 :: ps) : 'pattern ))\n",
              (Fgram.mk_action
                 (fun _  (ps : 'pattern list)  _  (p1 : 'pattern)  _ 
                    (_loc : FLoc.t)  -> (tuple_com (p1 :: ps) : 'pattern )))))]));
    Fgram.extend_single (string : 'string Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
             ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) -> ((`Str (_loc, s) : FAst.exp ) : 'string )\n     | _ -> failwith \"(`Str (_loc, s) : FAst.exp )\n\")\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `STR (_,s) ->
                         ((`Str (_loc, s) : FAst.exp ) : 'string )
                     | _ -> failwith "(`Str (_loc, s) : FAst.exp )\n"))));
          ([`Stoken
              (((function | `Ant ("",_) -> true | _ -> false)),
                (`Normal, "`Ant (\"\",_)"))],
            ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with\n     | `Ant (\"\",s) -> (parse_exp _loc s : 'string )\n     | _ -> failwith \"parse_exp _loc s\n\")\n",
              (Fgram.mk_action
                 (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                    match __fan_0 with
                    | `Ant ("",s) -> (parse_exp _loc s : 'string )
                    | _ -> failwith "parse_exp _loc s\n"))))]));
    Fgram.extend_single (simple_exp : 'simple_exp Fgram.t )
      (None,
        (None, None,
          [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
             ("Fgram.mk_action\n  (fun (i : 'a_lident)  (_loc : FLoc.t)  ->\n     ((i : alident  :>exp) : 'simple_exp ))\n",
               (Fgram.mk_action
                  (fun (i : 'a_lident)  (_loc : FLoc.t)  ->
                     ((i : alident  :>exp) : 'simple_exp )))));
          ([`Skeyword "(";
           `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
           `Skeyword ")"],
            ("Fgram.mk_action\n  (fun _  (e : 'exp)  _  (_loc : FLoc.t)  -> (e : 'simple_exp ))\n",
              (Fgram.mk_action
                 (fun _  (e : 'exp)  _  (_loc : FLoc.t)  ->
                    (e : 'simple_exp )))))]))
  end

let _ =
  let d = `Absolute ["Fan"; "Lang"] in
  begin
    AstQuotation.of_exp ~name:(d, "extend") ~entry:extend_body;
    AstQuotation.of_exp ~name:(d, "unsafe_extend") ~entry:unsafe_extend_body;
    AstQuotation.of_stru ~name:(d, "create") ~entry:nonterminals;
    AstQuotation.of_stru ~name:(d, "new") ~entry:newterminals;
    AstQuotation.of_exp ~name:(d, "delete") ~entry:delete_rule_body;
    AstQuotation.of_exp ~name:(d, "clear") ~entry:nonterminalsclear
  end