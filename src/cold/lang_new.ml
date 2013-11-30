let sem_of_list = Ast_gen.sem_of_list
open Astf
let qualid = Gramf.mk "qualid"
let nonterminals = Gramf.mk "nonterminals"
let newterminals = Gramf.mk "newterminals"
let t_qualid = Gramf.mk "t_qualid"
let qualuid = Gramf.mk "qualuid"
let _ =
  let type_entry: 'type_entry Gramf.t = Gramf.mk "type_entry"
  and ty: 'ty Gramf.t = Gramf.mk "ty" in
  Gramf.extend_single
    ({
       entry = (type_entry : 'type_entry Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "(_loc, x, None, None)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         ((_loc, x, None, None) : 'type_entry ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'type_entry ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                    Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                    Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "(_loc, x, (Some y), None)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_2 : Tokenf.txt)  (__fan_1 : Tokenf.txt) 
                        _  (_loc : Locf.t)  ->
                        let x = __fan_1.txt in
                        let y = __fan_2.txt in
                        ((_loc, x, (Some y), None) : 'type_entry ) : 
                     Tokenf.txt ->
                       Tokenf.txt ->
                         Tokenf.txt -> Tokenf.txt -> Locf.t -> 'type_entry ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                    Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                    Tokenf.pattern );
                  Nterm (Gramf.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "(_loc, x, (Some y), (Some t))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'Syntaxf__ctyp)  (__fan_2 : Tokenf.txt) 
                        (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let x = __fan_1.txt in
                        let y = __fan_2.txt in
                        ((_loc, x, (Some y), (Some t)) : 'type_entry ) : 
                     Tokenf.txt ->
                       'Syntaxf__ctyp ->
                         Tokenf.txt ->
                           Tokenf.txt -> Tokenf.txt -> Locf.t -> 'type_entry ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                    Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "(_loc, x, (Option.map (fun (x : Tokenf.txt)  -> x.txt) y), (Some t))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'Syntaxf__ctyp)  _  (__fan_1 : Tokenf.txt) 
                        _  (_loc : Locf.t)  ->
                        let x = __fan_1.txt in
                        let y = None in
                        ((_loc, x,
                           (Option.map (fun (x : Tokenf.txt)  -> x.txt) y),
                           (Some t)) : 'type_entry ) : Tokenf.txt ->
                                                         'Syntaxf__ctyp ->
                                                           Tokenf.txt ->
                                                             Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'type_entry ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                    Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (Syntaxf.ctyp : 'Syntaxf__ctyp Gramf.t ));
                  Token
                    ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                    Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "(_loc, x, (Option.map (fun (x : Tokenf.txt)  -> x.txt) y), (Some t))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (y : Tokenf.txt)  (t : 'Syntaxf__ctyp)  _ 
                        (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let x = __fan_1.txt in
                        let y = Some y in
                        ((_loc, x,
                           (Option.map (fun (x : Tokenf.txt)  -> x.txt) y),
                           (Some t)) : 'type_entry ) : Tokenf.txt ->
                                                         Tokenf.txt ->
                                                           'Syntaxf__ctyp ->
                                                             Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'type_entry ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ty : 'ty Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (t_qualid : 't_qualid Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Dyn (x, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (t : 't_qualid)  _  (x : 'qualid)  _ 
                         (_loc : Locf.t)  -> (`Dyn (x, t) : 'ty ) : Tokenf.txt
                                                                    ->
                                                                    't_qualid
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'qualid
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ty ))
               };
              {
                symbols = [Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ))];
                annot = "`Static t\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'qualuid)  (_loc : Locf.t)  ->
                        (`Static t : 'ty ) : 'qualuid -> Locf.t -> 'ty ))
              };
              {
                symbols = [];
                annot = "`Static (`Uid (_loc, \"Gramf\"))\n";
                fn =
                  (Gramf.mk_action
                     (fun (_loc : Locf.t)  ->
                        (`Static (`Uid (_loc, "Gramf")) : 'ty ) : Locf.t ->
                                                                    'ty ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (qualuid : 'qualuid Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'qualuid)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid ) : 
                      'qualuid ->
                        Tokenf.txt -> Tokenf.txt -> Locf.t -> 'qualuid ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                      } : Tokenf.pattern )];
                annot = "`Uid (_loc, x)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let x = __fan_0.txt in (`Uid (_loc, x) : 'qualuid ) : 
                     Tokenf.txt -> Locf.t -> 'qualuid ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (qualid : 'qualid Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'qualid)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid ) : 
                      'qualid ->
                        Tokenf.txt -> Tokenf.txt -> Locf.t -> 'qualid ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                      } : Tokenf.pattern )];
                annot = "`Lid (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let i = __fan_0.txt in (`Lid (_loc, i) : 'qualid ) : 
                     Tokenf.txt -> Locf.t -> 'qualid ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (t_qualid : 't_qualid Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 't_qualid)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid ) : 
                      't_qualid ->
                        Tokenf.txt -> Tokenf.txt -> Locf.t -> 't_qualid ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "."); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Lid; word = (A "t"); tag_name = "Lid" }
                     } : Tokenf.pattern )];
                annot = "`Uid (_loc, x)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let x = __fan_0.txt in (`Uid (_loc, x) : 't_qualid ) : 
                     Tokenf.txt ->
                       Tokenf.txt -> Tokenf.txt -> Locf.t -> 't_qualid ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (nonterminals : 'nonterminals Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (ty : 'ty Gramf.t ));
                   List1
                     (Nterm (Gramf.obj (type_entry : 'type_entry Gramf.t )))];
                 annot =
                   "let mk =\n  match t with\n  | `Static t -> (`Dot (_loc, t, (`Lid (_loc, \"mk\"))) : Astf.exp )\n  | `Dyn (x,t) ->\n      let x = (x : vid  :>exp) in\n      (`App (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), x) : \n        Astf.exp ) in\nsem_of_list\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : Astf.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            Astf.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : Astf.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            Astf.stru )) ls)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ls : 'type_entry list)  (t : 'ty) 
                         (_loc : Locf.t)  ->
                         (let mk =
                            match t with
                            | `Static t ->
                                (`Dot (_loc, t, (`Lid (_loc, "mk"))) : 
                                Astf.exp )
                            | `Dyn (x,t) ->
                                let x = (x : vid  :>exp) in
                                (`App
                                   (_loc,
                                     (`Dot
                                        (_loc, t,
                                          (`Lid (_loc, "mk_dynamic")))), x) : 
                                  Astf.exp ) in
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
                                                   (_loc, mk,
                                                     (`Str (_loc, d))))))) : 
                                      Astf.stru )
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
                                                     typ))))) : Astf.stru )
                                  | (None ,None ) ->
                                      (`Value
                                         (_loc, (`Negative _loc),
                                           (`Bind
                                              (_loc, (`Lid (_loc, x)),
                                                (`App
                                                   (_loc, mk,
                                                     (`Str (_loc, x))))))) : 
                                      Astf.stru )
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
                                                     typ))))) : Astf.stru ))
                               ls) : 'nonterminals ) : 'type_entry list ->
                                                         'ty ->
                                                           Locf.t ->
                                                             'nonterminals ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (newterminals : 'newterminals Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (t_qualid : 't_qualid Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   List1
                     (Nterm (Gramf.obj (type_entry : 'type_entry Gramf.t )))];
                 annot =
                   "let mk: Astf.exp =\n  `App\n    (_loc, (`Dot (_loc, t, (`Lid (_loc, \"mk_dynamic\")))), (x : vid  :>exp)) in\nsem_of_list\n  (List.map\n     (fun (_loc,x,descr,ty)  ->\n        match (descr, ty) with\n        | (Some d,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, d))))))) : Astf.stru )\n        | (Some d,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, d)))), typ))))) : \n            Astf.stru )\n        | (None ,None ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`App (_loc, mk, (`Str (_loc, x))))))) : Astf.stru )\n        | (None ,Some typ) ->\n            (`Value\n               (_loc, (`Negative _loc),\n                 (`Bind\n                    (_loc, (`Lid (_loc, x)),\n                      (`Constraint\n                         (_loc, (`App (_loc, mk, (`Str (_loc, x)))), typ))))) : \n            Astf.stru )) ls)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ls : 'type_entry list)  _  (t : 't_qualid)  _ 
                         (x : 'qualid)  _  (_loc : Locf.t)  ->
                         (let mk: Astf.exp =
                            `App
                              (_loc,
                                (`Dot (_loc, t, (`Lid (_loc, "mk_dynamic")))),
                                (x : vid  :>exp)) in
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
                                                   (_loc, mk,
                                                     (`Str (_loc, d))))))) : 
                                      Astf.stru )
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
                                                     typ))))) : Astf.stru )
                                  | (None ,None ) ->
                                      (`Value
                                         (_loc, (`Negative _loc),
                                           (`Bind
                                              (_loc, (`Lid (_loc, x)),
                                                (`App
                                                   (_loc, mk,
                                                     (`Str (_loc, x))))))) : 
                                      Astf.stru )
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
                                                     typ))))) : Astf.stru ))
                               ls) : 'newterminals ) : 'type_entry list ->
                                                         Tokenf.txt ->
                                                           't_qualid ->
                                                             Tokenf.txt ->
                                                               'qualid ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'newterminals ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  let domain = Ns.lang in
  Ast_quotation.of_stru ~name:{ domain; name = "create" } ~entry:nonterminals
    ();
  Ast_quotation.of_stru ~name:{ domain; name = "new" } ~entry:newterminals ()
