let sem_of_list = Ast_gen.sem_of_list
open FAst
let qualid = Fgram.mk "qualid"
let nonterminals = Fgram.mk "nonterminals"
let newterminals = Fgram.mk "newterminals"
let t_qualid = Fgram.mk "t_qualid"
let qualuid = Fgram.mk "qualuid"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let str: 'str Fgram.t = grammar_entry_create "str"
  and type_entry: 'type_entry Fgram.t = grammar_entry_create "type_entry"
  and ty: 'ty Fgram.t = grammar_entry_create "ty" in
  Fgram.extend_single (str : 'str Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
               "`Str y")],
           ("y\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str (_,y) -> (y : 'str )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (type_entry : 'type_entry Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid x")],
           ("(_loc, x, None, None)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,x) -> ((_loc, x, None, None) : 'type_entry )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "(";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x");
         `Stoken
           (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
             "`Str y");
         `Skeyword ")"],
          ("(_loc, x, (Some y), None)\n",
            (Fgram.mk_action
               (fun _  (__fan_2 : Ftoken.t)  (__fan_1 : Ftoken.t)  _ 
                  (_loc : Locf.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`Str (_,y),`Lid (_,x)) ->
                      ((_loc, x, (Some y), None) : 'type_entry )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_2)
                           (Ftoken.token_to_string __fan_1))))));
        ([`Skeyword "(";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x");
         `Stoken
           (((function | `Str (_,_) -> true | _ -> false)), ("Str", `Any),
             "`Str y");
         `Snterm (Fgram.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Fgram.t ));
         `Skeyword ")"],
          ("(_loc, x, (Some y), (Some t))\n",
            (Fgram.mk_action
               (fun _  (t : 'Syntaxf__ctyp)  (__fan_2 : Ftoken.t) 
                  (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`Str (_,y),`Lid (_,x)) ->
                      ((_loc, x, (Some y), (Some t)) : 'type_entry )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_2)
                           (Ftoken.token_to_string __fan_1))))));
        ([`Skeyword "(";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x");
         `Skeyword ":";
         `Snterm (Fgram.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Fgram.t ));
         `Sopt (`Snterm (Fgram.obj (str : 'str Fgram.t )));
         `Skeyword ")"],
          ("(_loc, x, y, (Some t))\n",
            (Fgram.mk_action
               (fun _  (y : 'str option)  (t : 'Syntaxf__ctyp)  _ 
                  (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Lid (_,x) -> ((_loc, x, y, (Some t)) : 'type_entry )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_1))))))]));
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
                (fun _  (t : 't_qualid)  _  (x : 'qualid)  _  (_loc : Locf.t)
                    -> (`Dyn (x, t) : 'ty )))));
        ([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
          ("`Static t\n",
            (Fgram.mk_action
               (fun (t : 'qualuid)  (_loc : Locf.t)  -> (`Static t : 'ty )))));
        ([],
          ("`Static (`Uid (_loc, \"Fgram\"))\n",
            (Fgram.mk_action
               (fun (_loc : Locf.t)  ->
                  (`Static (`Uid (_loc, "Fgram")) : 'ty )))))]));
  Fgram.extend_single (qualuid : 'qualuid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualuid)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,x) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
              "`Uid x")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid (_,x) -> (`Uid (_loc, x) : 'qualuid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (qualid : 'qualid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualid)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)
                    ->
                   match __fan_0 with
                   | `Uid (_,x) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
              "`Lid i")],
          ("`Lid (_loc, i)\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Lid (_,i) -> (`Lid (_loc, i) : 'qualid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (t_qualid : 't_qualid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 't_qualid)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,x) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
              "`Uid x");
         `Skeyword ".";
         `Stoken
           (((function | `Lid (_,"t") -> true | _ -> false)),
             ("Lid", (`A "t")), "`Lid \"t\"")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_2 : Ftoken.t)  _  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_2, __fan_0) with
                  | (`Lid (_,"t"),`Uid (_,x)) ->
                      (`Uid (_loc, x) : 't_qualid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_2)
                           (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (nonterminals : 'nonterminals Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (ty : 'ty Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (type_entry : 'type_entry Fgram.t )))],
           ("let mk =\n  match t with\n  | `Static t ->\n      let t = (t : vid  :>exp) in\n      (`Field (_loc, t, (`Lid (_loc, \"mk\"))) : FAst.exp )\n  | `Dyn (x,t) ->\n      let x = (x : vid  :>exp) in\n      (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n        FAst.exp ) in\nsem_of_list\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : FAst.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            FAst.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : FAst.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            FAst.stru )) ls)\n",
             (Fgram.mk_action
                (fun (ls : 'type_entry list)  (t : 'ty)  (_loc : Locf.t)  ->
                   (let mk =
                      match t with
                      | `Static t ->
                          let t = (t : vid  :>exp) in
                          (`Field (_loc, t, (`Lid (_loc, "mk"))) : FAst.exp )
                      | `Dyn (x,t) ->
                          let x = (x : vid  :>exp) in
                          (`App
                             (_loc,
                               (`Dot (_loc, t, (`Lid (_loc, "mk_dynamic")))),
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
                   (x : 'qualid)  _  (_loc : Locf.t)  ->
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
                                FAst.stru )) ls)) : 'newterminals )))))]))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_stru ~name:(d, "create") ~entry:nonterminals ();
  Ast_quotation.of_stru ~name:(d, "new") ~entry:newterminals ()