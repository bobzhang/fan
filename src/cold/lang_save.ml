let and_of_list = Ast_gen.and_of_list
let seq_sem = Ast_gen.seq_sem
let save_quot = Gramf.mk "save_quot"
let _ =
  Gramf.extend_single
    ({
       entry = (save_quot : 'save_quot Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [List1
                      (Token
                         ({
                            descr =
                              { tag = `Lid; word = Any; tag_name = "Lid" }
                          } : Tokenf.pattern ));
                   Token
                     ({
                        descr =
                          { tag = `Quot; word = Any; tag_name = "Quot" }
                      } : Tokenf.pattern )];
                 annot =
                   "let b =\n  if x.name = Tokenf.empty_name\n  then\n    let expander loc _ s = Gramlib.parse_string_eoi ~loc Syntaxf.exp s in\n    Tokenf.quot_expand expander x\n  else Ast_quotation.expand x Dyn_tag.exp in\nlet symbs =\n  List.map (fun ({ txt;_} : Tokenf.txt)  -> Gensym.fresh ~prefix:txt ()) ls in\nlet res = Gensym.fresh ~prefix:\"res\" () in\nlet exc = Gensym.fresh ~prefix:\"e\" () in\nlet binds =\n  and_of_list\n    (List.map2\n       (fun x  (y : Tokenf.txt)  ->\n          (`Bind\n             (_loc, (`Lid (_loc, x)),\n               (`App (_loc, (`Lid (_loc, \"!\")), (`Lid (_loc, (y.txt)))))) :>\n          Astf.bind)) symbs ls) in\nlet restore =\n  seq_sem\n    (List.map2\n       (fun (x : Tokenf.txt)  y  ->\n          (`App\n             (_loc,\n               (`App (_loc, (`Lid (_loc, \":=\")), (`Lid (_loc, (x.txt))))),\n               (`Lid (_loc, y))) :>Astf.exp)) ls symbs) in\n(`LetIn\n   (_loc, (`Negative _loc), (binds :>Astf.bind),\n     (`Try\n        (_loc,\n          (`Seq\n             (_loc,\n               (`LetIn\n                  (_loc, (`Negative _loc),\n                    (`Bind (_loc, (`Lid (_loc, res)), (b :>Astf.exp))),\n                    (`LetIn\n                       (_loc, (`Negative _loc),\n                         (`Bind (_loc, (`Any _loc), (restore :>Astf.exp))),\n                         (`Lid (_loc, res)))))))),\n          (`Case\n             (_loc, (`Lid (_loc, exc)),\n               (`Seq\n                  (_loc,\n                    (`Sem\n                       (_loc, (restore :>Astf.exp),\n                         (`App\n                            (_loc, (`Lid (_loc, \"raise\")),\n                              (`Lid (_loc, exc))))))))))))) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.quot)  (ls : Tokenf.txt list) 
                         (_loc : Locf.t)  ->
                         let x = __fan_1 in
                         (let b =
                            if x.name = Tokenf.empty_name
                            then
                              let expander loc _ s =
                                Gramlib.parse_string_eoi ~loc Syntaxf.exp s in
                              Tokenf.quot_expand expander x
                            else Ast_quotation.expand x Dyn_tag.exp in
                          let symbs =
                            List.map
                              (fun ({ txt;_} : Tokenf.txt)  ->
                                 Gensym.fresh ~prefix:txt ()) ls in
                          let res = Gensym.fresh ~prefix:"res" () in
                          let exc = Gensym.fresh ~prefix:"e" () in
                          let binds =
                            and_of_list
                              (List.map2
                                 (fun x  (y : Tokenf.txt)  ->
                                    (`Bind
                                       (_loc, (`Lid (_loc, x)),
                                         (`App
                                            (_loc, (`Lid (_loc, "!")),
                                              (`Lid (_loc, (y.txt)))))) :>
                                    Astf.bind)) symbs ls) in
                          let restore =
                            seq_sem
                              (List.map2
                                 (fun (x : Tokenf.txt)  y  ->
                                    (`App
                                       (_loc,
                                         (`App
                                            (_loc, (`Lid (_loc, ":=")),
                                              (`Lid (_loc, (x.txt))))),
                                         (`Lid (_loc, y))) :>Astf.exp)) ls
                                 symbs) in
                          (`LetIn
                             (_loc, (`Negative _loc), (binds :>Astf.bind),
                               (`Try
                                  (_loc,
                                    (`Seq
                                       (_loc,
                                         (`LetIn
                                            (_loc, (`Negative _loc),
                                              (`Bind
                                                 (_loc, (`Lid (_loc, res)),
                                                   (b :>Astf.exp))),
                                              (`LetIn
                                                 (_loc, (`Negative _loc),
                                                   (`Bind
                                                      (_loc, (`Any _loc),
                                                        (restore :>Astf.exp))),
                                                   (`Lid (_loc, res)))))))),
                                    (`Case
                                       (_loc, (`Lid (_loc, exc)),
                                         (`Seq
                                            (_loc,
                                              (`Sem
                                                 (_loc, (restore :>Astf.exp),
                                                   (`App
                                                      (_loc,
                                                        (`Lid (_loc, "raise")),
                                                        (`Lid (_loc, exc))))))))))))) :>
                            Astf.exp) : 'save_quot ) : Tokenf.quot ->
                                                         Tokenf.txt list ->
                                                           Locf.t ->
                                                             'save_quot ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  Ast_quotation.of_exp ~name:{ domain = Ns.lang; name = "save" }
    ~entry:save_quot ()
