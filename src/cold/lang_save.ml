let and_of_list = Ast_gen.and_of_list
let seq_sem = Ast_gen.seq_sem
let save_quot = Gramf.mk "save_quot"
let _ =
  let grammar_entry_create x = Gramf.mk x in
  let lid: 'lid Gramf.t = grammar_entry_create "lid" in
  Gramf.extend_single (save_quot : 'save_quot Gramf.t )
    (None,
      ((None, None,
         [([`List1 (`Nterm (Gramf.obj (lid : 'lid Gramf.t )));
           `Token
             (((function | `Quot _ -> true | _ -> false)), (904098089, `Any),
               "`Quot _")],
            ("let b =\n  if x.name = Tokenf.empty_name\n  then\n    let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\n    Tokenf.quot_expand expander x\n  else Ast_quotation.expand x Dyn_tag.exp in\nlet symbs = List.map (fun x  -> State.gensym x) ls in\nlet res = State.gensym \"res\" in\nlet exc = State.gensym \"e\" in\nlet binds =\n  and_of_list\n    (List.map2\n       (fun x  y  ->\n          (`Bind\n             (_loc, (`Lid (_loc, x)),\n               (`App (_loc, (`Lid (_loc, \"!\")), (`Lid (_loc, y))))) : \n          FAst.bind )) symbs ls) in\nlet restore =\n  seq_sem\n    (List.map2\n       (fun x  y  ->\n          (`App\n             (_loc, (`App (_loc, (`Lid (_loc, \":=\")), (`Lid (_loc, x)))),\n               (`Lid (_loc, y))) : FAst.exp )) ls symbs) in\n(`LetIn\n   (_loc, (`Negative _loc), binds,\n     (`Try\n        (_loc,\n          (`Seq\n             (_loc,\n               (`LetIn\n                  (_loc, (`Negative _loc),\n                    (`Bind (_loc, (`Lid (_loc, res)), b)),\n                    (`LetIn\n                       (_loc, (`Negative _loc),\n                         (`Bind (_loc, (`Any _loc), restore)),\n                         (`Lid (_loc, res)))))))),\n          (`Case\n             (_loc, (`Lid (_loc, exc)),\n               (`Seq\n                  (_loc,\n                    (`Sem\n                       (_loc, restore,\n                         (`App\n                            (_loc, (`Lid (_loc, \"raise\")),\n                              (`Lid (_loc, exc))))))))))))) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1  ~__fan_0:(ls : 'lid list)  (_loc : Locf.t) 
                    ->
                    match __fan_1 with
                    | (x : Tokenf.quot) ->
                        (let b =
                           if x.name = Tokenf.empty_name
                           then
                             let expander loc _ s =
                               Gramf.parse_string ~loc Syntaxf.exp s in
                             Tokenf.quot_expand expander x
                           else Ast_quotation.expand x Dyn_tag.exp in
                         let symbs = List.map (fun x  -> State.gensym x) ls in
                         let res = State.gensym "res" in
                         let exc = State.gensym "e" in
                         let binds =
                           and_of_list
                             (List.map2
                                (fun x  y  ->
                                   (`Bind
                                      (_loc, (`Lid (_loc, x)),
                                        (`App
                                           (_loc, (`Lid (_loc, "!")),
                                             (`Lid (_loc, y))))) : FAst.bind ))
                                symbs ls) in
                         let restore =
                           seq_sem
                             (List.map2
                                (fun x  y  ->
                                   (`App
                                      (_loc,
                                        (`App
                                           (_loc, (`Lid (_loc, ":=")),
                                             (`Lid (_loc, x)))),
                                        (`Lid (_loc, y))) : FAst.exp )) ls
                                symbs) in
                         (`LetIn
                            (_loc, (`Negative _loc), binds,
                              (`Try
                                 (_loc,
                                   (`Seq
                                      (_loc,
                                        (`LetIn
                                           (_loc, (`Negative _loc),
                                             (`Bind
                                                (_loc, (`Lid (_loc, res)), b)),
                                             (`LetIn
                                                (_loc, (`Negative _loc),
                                                  (`Bind
                                                     (_loc, (`Any _loc),
                                                       restore)),
                                                  (`Lid (_loc, res)))))))),
                                   (`Case
                                      (_loc, (`Lid (_loc, exc)),
                                        (`Seq
                                           (_loc,
                                             (`Sem
                                                (_loc, restore,
                                                  (`App
                                                     (_loc,
                                                       (`Lid (_loc, "raise")),
                                                       (`Lid (_loc, exc))))))))))))) : 
                           FAst.exp ) : 'save_quot )))))]) : Gramf.olevel ));
  Gramf.extend_single (lid : 'lid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
                "Lid")],
            ("x\n",
              (Gramf.mk_action
                 (fun ~__fan_0  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) -> (x : 'lid )))))]) : 
      Gramf.olevel ))
let _ = Ast_quotation.of_exp ~name:(Ns.lang, "save") ~entry:save_quot ()
