open! Syntaxf
let f (loc : Locf.t) meta content =
  let module_name =
    match meta with
    | None  -> Locf.failf loc "cexp module name must be specified via @"
    | Some x -> String.capitalize x in
  ();
  Gramf.protects
    [({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "let"); tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "!"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "in"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "Ast_basic.fold_and_right\n  (fun bind  acc  ->\n     match bind with\n     | (`Bind (_loc,p,e) : Astf.bind) ->\n         (`App\n            (_loc,\n              (`App\n                 (_loc,\n                   (`Dot\n                      (_loc, (`Uid (_loc, module_name)),\n                        (`Lid (_loc, \"bind\")))), e)),\n              (`Fun (_loc, (`Case (_loc, p, acc))))) : Astf.exp )\n     | _ -> assert false) bi x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'exp)  _  (bi : 'bind)  _  _ 
                          (_loc : Locf.t)  ->
                          (Ast_basic.fold_and_right
                             (fun bind  acc  ->
                                match bind with
                                | (`Bind (_loc,p,e) : Astf.bind) ->
                                    (`App
                                       (_loc,
                                         (`App
                                            (_loc,
                                              (`Dot
                                                 (_loc,
                                                   (`Uid (_loc, module_name)),
                                                   (`Lid (_loc, "bind")))),
                                              e)),
                                         (`Fun (_loc, (`Case (_loc, p, acc))))) : 
                                    Astf.exp )
                                | _ -> assert false) bi x : 'exp ) : 
                       'exp ->
                         Tokenf.txt ->
                           'bind ->
                             Tokenf.txt -> Tokenf.txt -> Locf.t -> 'exp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement )]
    (fun _  -> Gramlib.parse_string_eoi exp ~loc content)
let f2 (_loc : Locf.t) _meta content =
  let res = f _loc _meta content in (`StExp (_loc, res) : Astf.stru )
let () =
  let d = Ns.lang in
  Ast_quotation.add { domain = d; name = "cexp" } Dyn_tag.exp f;
  Ast_quotation.add { domain = d; name = "cexp" } Dyn_tag.stru f2
