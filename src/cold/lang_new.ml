let sem_of_list = Ast_gen.sem_of_list
open FAst
let qualid = Gramf.mk "qualid"
let nonterminals = Gramf.mk "nonterminals"
let newterminals = Gramf.mk "newterminals"
let t_qualid = Gramf.mk "t_qualid"
let qualuid = Gramf.mk "qualuid"
let _ =
  let grammar_entry_create x = Gramf.mk x in
  let str: 'str Gramf.t = grammar_entry_create "str"
  and type_entry: 'type_entry Gramf.t = grammar_entry_create "type_entry"
  and ty: 'ty Gramf.t = grammar_entry_create "ty" in
  Gramf.extend_single (str : 'str Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "Str")],
            ("y\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Str ({ txt = y;_} : Tokenf.txt) -> (y : 'str )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (type_entry : 'type_entry Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "Lid")],
            ("(_loc, x, None, None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Lid ({ txt = x;_} : Tokenf.txt) ->
                        ((_loc, x, None, None) : 'type_entry )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "(";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "Lid");
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "Str");
          `Keyword ")"],
           ("(_loc, x, (Some y), None)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(__fan_2 : Tokenf.t) 
                   ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   match (__fan_2, __fan_1) with
                   | (`Str ({ txt = y;_} : Tokenf.txt),`Lid
                                                         ({ txt = x;_} :
                                                           Tokenf.txt))
                       -> ((_loc, x, (Some y), None) : 'type_entry )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_1))))));
         ([`Keyword "(";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "Lid");
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "Str");
          `Nterm (Gramf.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Gramf.t ));
          `Keyword ")"],
           ("(_loc, x, (Some y), (Some t))\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(t : 'Syntaxf__ctyp) 
                   ~__fan_2:(__fan_2 : Tokenf.t) 
                   ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   match (__fan_2, __fan_1) with
                   | (`Str ({ txt = y;_} : Tokenf.txt),`Lid
                                                         ({ txt = x;_} :
                                                           Tokenf.txt))
                       -> ((_loc, x, (Some y), (Some t)) : 'type_entry )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_1))))));
         ([`Keyword "(";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "Lid");
          `Keyword ":";
          `Nterm (Gramf.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Gramf.t ));
          `Keyword ")"],
           ("(_loc, x, y, (Some t))\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(t : 'Syntaxf__ctyp)  ~__fan_2:_ 
                   ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   match __fan_1 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       let y = None in
                       ((_loc, x, y, (Some t)) : 'type_entry )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "(";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "Lid");
          `Keyword ":";
          `Nterm (Gramf.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Gramf.t ));
          `Nterm (Gramf.obj (str : 'str Gramf.t ));
          `Keyword ")"],
           ("(_loc, x, y, (Some t))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(y : 'str) 
                   ~__fan_3:(t : 'Syntaxf__ctyp)  ~__fan_2:_ 
                   ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   match __fan_1 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       let y = Some y in
                       ((_loc, x, y, (Some t)) : 'type_entry )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (ty : 'ty Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "(";
           `Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (t_qualid : 't_qualid Gramf.t ));
           `Keyword ")"],
            ("`Dyn (x, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(t : 't_qualid)  ~__fan_2:_ 
                    ~__fan_1:(x : 'qualid)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Dyn (x, t) : 'ty )))));
         ([`Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ))],
           ("`Static t\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'qualuid)  (_loc : Locf.t)  ->
                   (`Static t : 'ty )))));
         ([],
           ("`Static (`Uid (_loc, \"Gramf\"))\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  ->
                   (`Static (`Uid (_loc, "Gramf")) : 'ty )))))]) : Gramf.olevel ));
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualuid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "Uid")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = x;_} : Tokenf.txt) ->
                       (`Uid (_loc, x) : 'qualuid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (qualid : 'qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "Lid")],
           ("`Lid (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       (`Lid (_loc, i) : 'qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (t_qualid : 't_qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 't_qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "Uid");
          `Keyword ".";
          `Token
            (((function
               | `Lid ({ txt = "t";_} : Tokenf.txt) -> true
               | _ -> false)), (3802919, (`A "t")), "`Lid \"t\"")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`Lid ({ txt = "t";_} : Tokenf.txt),`Uid
                                                           ({ txt = x;_} :
                                                             Tokenf.txt))
                       -> (`Uid (_loc, x) : 't_qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))))]) : Gramf.olevel ));
  Gramf.extend_single (nonterminals : 'nonterminals Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (ty : 'ty Gramf.t ));
           `List1 (`Nterm (Gramf.obj (type_entry : 'type_entry Gramf.t )))],
            ("let mk =\n  match t with\n  | `Static t -> (`Dot (_loc, t, (`Lid (_loc, \"mk\"))) : FAst.exp )\n  | `Dyn (x,t) ->\n      let x = (x : vid  :>exp) in\n      (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n        FAst.exp ) in\nsem_of_list\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : FAst.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            FAst.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : FAst.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            FAst.stru )) ls)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(ls : 'type_entry list)  ~__fan_0:(t : 'ty) 
                    (_loc : Locf.t)  ->
                    (let mk =
                       match t with
                       | `Static t ->
                           (`Dot (_loc, t, (`Lid (_loc, "mk"))) : FAst.exp )
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
                                 FAst.stru )) ls) : 'nonterminals )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (newterminals : 'newterminals Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "(";
           `Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (t_qualid : 't_qualid Gramf.t ));
           `Keyword ")";
           `List1 (`Nterm (Gramf.obj (type_entry : 'type_entry Gramf.t )))],
            ("let mk: FAst.exp =\n  `App\n    (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), (x : vid  :>exp)) in\nsem_of_list\n  ((`Value\n      (_loc, (`Negative _loc),\n        (`Bind\n           (_loc, (x :>pat),\n             (`App\n                (_loc,\n                  (`App\n                     (_loc,\n                       (`App\n                          (_loc,\n                            (`Dot (_loc, t, (`Lid (_loc, \"create_lexer\")))),\n                            (`Label\n                               (_loc, (`Lid (_loc, \"annot\")),\n                                 (`Str (_loc, \"\")))))),\n                       (`Label\n                          (_loc, (`Lid (_loc, \"keywords\")),\n                            (`Uid (_loc, \"[]\")))))), (`Uid (_loc, \"()\"))))))) : \n  FAst.stru ) ::\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : FAst.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            FAst.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : FAst.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            FAst.stru )) ls))\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(ls : 'type_entry list)  ~__fan_4:_ 
                    ~__fan_3:(t : 't_qualid)  ~__fan_2:_ 
                    ~__fan_1:(x : 'qualid)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (let mk: FAst.exp =
                       `App
                         (_loc,
                           (`Dot (_loc, t, (`Lid (_loc, "mk_dynamic")))),
                           (x : vid  :>exp)) in
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
                                 FAst.stru )) ls)) : 'newterminals )))))]) : 
      Gramf.olevel ))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_stru ~name:(d, "create") ~entry:nonterminals ();
  Ast_quotation.of_stru ~name:(d, "new") ~entry:newterminals ()
