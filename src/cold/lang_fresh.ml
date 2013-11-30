let fresh = Gramf.mk "fresh"
let _ =
  Gramf.extend_single (fresh : 'fresh Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                 Tokenf.pattern )];
            annot =
              "(`App\n   (_loc,\n     (`App\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Gensym\")), (`Lid (_loc, \"fresh\")))),\n          (`Label (_loc, (`Lid (_loc, \"prefix\")), (`Str (_loc, x)))))),\n     (`Uid (_loc, \"()\"))) : Astf.exp )\n";
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
                                    (`Str (_loc, x)))))),
                          (`Uid (_loc, "()"))) : Astf.exp ) : 'fresh ) : 
                 Tokenf.txt -> Locf.t -> 'fresh ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot =
             "(`App\n   (_loc,\n     (`App\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Gensym\")), (`Lid (_loc, \"fresh\")))),\n          (`Label\n             (_loc, (`Lid (_loc, \"prefix\")),\n               (Tokenf.ant_expand Parsef.exp x))))), (`Uid (_loc, \"()\"))) : \nAstf.exp )\n";
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
                                   (Tokenf.ant_expand Parsef.exp x))))),
                         (`Uid (_loc, "()"))) : Astf.exp ) : 'fresh ) : 
                Tokenf.ant -> Locf.t -> 'fresh ))
         };
         {
           symbols = [];
           annot =
             "(`App\n   (_loc, (`Dot (_loc, (`Uid (_loc, \"Gensym\")), (`Lid (_loc, \"fresh\")))),\n     (`Uid (_loc, \"()\"))) : Astf.exp )\n";
           fn =
             (Gramf.mk_action
                (fun (_loc : Locf.t)  ->
                   ((`App
                       (_loc,
                         (`Dot
                            (_loc, (`Uid (_loc, "Gensym")),
                              (`Lid (_loc, "fresh")))), (`Uid (_loc, "()"))) : 
                   Astf.exp ) : 'fresh ) : Locf.t -> 'fresh ))
         }]
     } : Gramf.olevel )
let () =
  let d = Ns.lang in
  Ast_quotation.of_exp ~name:{ domain = d; name = "fresh" } ~entry:fresh ()
