open Syntaxf
let code = Gramf.mk "code"
let code_name: string ref = ref ""
let _ =
  Gramf.extend_single
    ({
       entry = (code : 'code Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [Nterm (Gramf.obj (strus : 'strus Gramf.t ))];
                 annot =
                   "let code = (new Metafn.meta)#stru _loc (Strip.stru x) in\nlet name = \"code_of_\" ^ (!code_name) in\n(`Sem\n   (_loc, (x :>Astf.stru),\n     (`Value\n        (_loc, (`Negative _loc),\n          (`Bind\n             (_loc, (`Lid (_loc, name)),\n               (`Constraint\n                  (_loc, (code :>Astf.exp),\n                    (`Dot\n                       (_loc, (`Uid (_loc, \"Astfn\")), (`Lid (_loc, \"stru\"))))))))))) :>\n  Astf.stru)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'strus)  (_loc : Locf.t)  ->
                         (let code =
                            (new Metafn.meta)#stru _loc (Strip.stru x) in
                          let name = "code_of_" ^ (!code_name) in
                          (`Sem
                             (_loc, (x :>Astf.stru),
                               (`Value
                                  (_loc, (`Negative _loc),
                                    (`Bind
                                       (_loc, (`Lid (_loc, name)),
                                         (`Constraint
                                            (_loc, (code :>Astf.exp),
                                              (`Dot
                                                 (_loc,
                                                   (`Uid (_loc, "Astfn")),
                                                   (`Lid (_loc, "stru"))))))))))) :>
                            Astf.stru) : 'code ) : 'strus -> Locf.t -> 'code ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  let parser = Ast_quotation.make_parser ~lexer:Lex_fan.from_stream code in
  Ast_quotation.add { domain = Ns.lang; name = "code" } Dyn_tag.stru
    (fun loc  meta  s  ->
       match meta with
       | None  -> parser loc meta s
       | Some x -> Ref.protect code_name x (fun _  -> parser loc meta s))
