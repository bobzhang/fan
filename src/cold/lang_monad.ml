open! Syntaxf
open Astf
type t =  {
  bind: bind;
  exp: exp} 
let specializer: (string,t -> exp) Hashtbl.t = Hashtbl.create 0
let _ =
  let (+>) = Hashtbl.add specializer in
  "Option" +>
    (fun t  ->
       Ast_basic.fold_and_right
         (fun bind  acc  ->
            match bind with
            | (`Bind (_loc,p,e) : Astf.bind) ->
                (`Match
                   (_loc,
                     (`Constraint
                        (_loc, (e :>Astf.exp),
                          (`App (_loc, (`Lid (_loc, "option")), (`Any _loc))))),
                     (`Bar
                        (_loc,
                          (`Case
                             (_loc,
                               (`App
                                  (_loc, (`Uid (_loc, "Some")),
                                    (p :>Astf.pat))), (acc :>Astf.exp))),
                          (`Case
                             (_loc, (`Uid (_loc, "None")),
                               (`Uid (_loc, "None"))))))) :>Astf.exp)
            | _ -> assert false) t.bind t.exp)
let f (loc : Locf.t) meta content =
  let module_name =
    match meta with
    | None  -> Locf.failf loc "cexp module name must be specified via \\@"
    | Some x -> String.capitalize x in
  let tmp_entry__001_ = Gramf.get_levels exp in
  try
    Gramf.extend_single
      ({
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
                     "(try\n   let f = Hashtbl.find specializer module_name in\n   fun ()  -> f { bind = bi; exp = x }\n with\n | Not_found  ->\n     (fun ()  ->\n        Ast_basic.fold_and_right\n          (fun bind  acc  ->\n             match bind with\n             | (`Bind (_loc,p,e) : Astf.bind) ->\n                 (`App\n                    (_loc,\n                      (`App\n                         (_loc,\n                           (`Dot\n                              (_loc, (`Uid (_loc, module_name)),\n                                (`Lid (_loc, \"bind\")))), (e :>Astf.exp))),\n                      (`Fun\n                         (_loc,\n                           (`Case (_loc, (p :>Astf.pat), (acc :>Astf.exp)))))) :>\n                 Astf.exp)\n             | _ -> assert false) bi x)) ()\n";
                   fn =
                     (Gramf.mk_action
                        (fun (x : 'exp)  _  (bi : 'bind)  _  _ 
                           (_loc : Locf.t)  ->
                           ((try
                               let f = Hashtbl.find specializer module_name in
                               fun ()  -> f { bind = bi; exp = x }
                             with
                             | Not_found  ->
                                 (fun ()  ->
                                    Ast_basic.fold_and_right
                                      (fun bind  acc  ->
                                         match bind with
                                         | (`Bind (_loc,p,e) : Astf.bind) ->
                                             (`App
                                                (_loc,
                                                  (`App
                                                     (_loc,
                                                       (`Dot
                                                          (_loc,
                                                            (`Uid
                                                               (_loc,
                                                                 module_name)),
                                                            (`Lid
                                                               (_loc, "bind")))),
                                                       (e :>Astf.exp))),
                                                  (`Fun
                                                     (_loc,
                                                       (`Case
                                                          (_loc,
                                                            (p :>Astf.pat),
                                                            (acc :>Astf.exp)))))) :>
                                             Astf.exp)
                                         | _ -> assert false) bi x)) () : 
                           'exp ) : 'exp ->
                                      Tokenf.txt ->
                                        'bind ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'exp ))
                 }]
            } : Gramf.olevel )
       } : _ Gramf.single_extend_statement );
    (let result = Gramlib.parse_string_eoi exp ~loc content in
     Gramf.fresh_with_levels exp tmp_entry__001_; result)
  with | x -> (Gramf.fresh_with_levels exp tmp_entry__001_; raise x)
let f2 (_loc : Locf.t) _meta content =
  let res = f _loc _meta content in
  (`StExp (_loc, (res :>Astf.exp)) :>Astf.stru)
let () =
  let d = Ns.lang in
  Ast_quotation.add { domain = d; name = "cexp" } Dyn_tag.exp f;
  Ast_quotation.add { domain = d; name = "cexp" } Dyn_tag.stru f2
