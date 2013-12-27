let fresh = Gramf.mk "fresh"
let _ =
  Gramf.extend_single
    ({
       entry = (fresh : 'fresh Gramf.t );
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
                 annot =
                   "(`App\n   (_loc,\n     (`App\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Gensym\")), (`Lid (_loc, \"fresh\")))),\n          (`Label (_loc, (`Lid (_loc, \"prefix\")), (`Str (_loc, x)))))),\n     (`Unit _loc)) : Astf.exp )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         ((`App
                             (_loc,
                               (`App
                                  (_loc,
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Gensym")),
                                         (`Lid (_loc, "fresh")))),
                                    (`Label
                                       (_loc, (`Lid (_loc, "prefix")),
                                         (`Str (_loc, x)))))), (`Unit _loc)) : 
                           Astf.exp ) : 'fresh ) : Tokenf.txt ->
                                                     Locf.t -> 'fresh ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot =
                  "(`App\n   (_loc,\n     (`App\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Gensym\")), (`Lid (_loc, \"fresh\")))),\n          (`Label\n             (_loc, (`Lid (_loc, \"prefix\")),\n               (Tokenf.ant_expand Parsef.exp x :>Astf.exp))))), (`Unit _loc)) : \nAstf.exp )\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let x = __fan_0 in
                        ((`App
                            (_loc,
                              (`App
                                 (_loc,
                                   (`Dot
                                      (_loc, (`Uid (_loc, "Gensym")),
                                        (`Lid (_loc, "fresh")))),
                                   (`Label
                                      (_loc, (`Lid (_loc, "prefix")),
                                        (Tokenf.ant_expand Parsef.exp x :>
                                        Astf.exp))))), (`Unit _loc)) : 
                          Astf.exp ) : 'fresh ) : Tokenf.ant ->
                                                    Locf.t -> 'fresh ))
              };
              {
                symbols = [];
                annot =
                  "(`App\n   (_loc, (`Dot (_loc, (`Uid (_loc, \"Gensym\")), (`Lid (_loc, \"fresh\")))),\n     (`Unit _loc)) : Astf.exp )\n";
                fn =
                  (Gramf.mk_action
                     (fun (_loc : Locf.t)  ->
                        ((`App
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, "Gensym")),
                                   (`Lid (_loc, "fresh")))), (`Unit _loc)) : 
                        Astf.exp ) : 'fresh ) : Locf.t -> 'fresh ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  Ast_quotation.of_exp ~name:{ domain = Ns.lang; name = "fresh" }
    ~entry:fresh ()
