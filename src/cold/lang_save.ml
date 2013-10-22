let and_of_list = Ast_gen.and_of_list
let seq_sem = Ast_gen.seq_sem
let save_quot = Fgram.mk "save_quot"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let lid: 'lid Fgram.t = grammar_entry_create "lid" in
  Fgram.extend_single (save_quot : 'save_quot Fgram.t )
    (None,
      (None, None,
        [([`Slist1 (`Snterm (Fgram.obj (lid : 'lid Fgram.t )));
          `Stoken
            (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
              "`Quot _")],
           ("let b =\n  if x.name = Ftoken.empty_name\n  then\n    let expander loc _ s = Fgram.parse_string ~loc Syntaxf.exp s in\n    Ftoken.quot_expand expander x\n  else Ast_quotation.expand x Dyn_tag.exp in\nlet symbs = List.map (fun x  -> State.gensym x) ls in\nlet res = State.gensym \"res\" in\nlet exc = State.gensym \"e\" in\nlet binds =\n  and_of_list\n    (List.map2\n       (fun x  y  ->\n          (`Bind\n             (_loc, (`Lid (_loc, x)),\n               (`Field (_loc, (`Lid (_loc, y)), (`Lid (_loc, \"contents\"))))) : \n          FAst.bind )) symbs ls) in\nlet restore =\n  seq_sem\n    (List.map2\n       (fun x  y  ->\n          (`Assign\n             (_loc,\n               (`Field (_loc, (`Lid (_loc, x)), (`Lid (_loc, \"contents\")))),\n               (`Lid (_loc, y))) : FAst.exp )) ls symbs) in\n(`LetIn\n   (_loc, (`Negative _loc), binds,\n     (`Try\n        (_loc,\n          (`Seq\n             (_loc,\n               (`LetIn\n                  (_loc, (`Negative _loc),\n                    (`Bind (_loc, (`Lid (_loc, res)), b)),\n                    (`LetIn\n                       (_loc, (`Negative _loc),\n                         (`Bind (_loc, (`Any _loc), restore)),\n                         (`Lid (_loc, res)))))))),\n          (`Case\n             (_loc, (`Lid (_loc, exc)),\n               (`Seq\n                  (_loc,\n                    (`Sem\n                       (_loc, restore,\n                         (`App\n                            (_loc, (`Lid (_loc, \"raise\")),\n                              (`Lid (_loc, exc))))))))))))) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  (ls : 'lid list)  (_loc : Locf.t) 
                   ->
                   match __fan_1 with
                   | `Quot x ->
                       (let b =
                          if x.name = Ftoken.empty_name
                          then
                            let expander loc _ s =
                              Fgram.parse_string ~loc Syntaxf.exp s in
                            Ftoken.quot_expand expander x
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
                                       (`Field
                                          (_loc, (`Lid (_loc, y)),
                                            (`Lid (_loc, "contents"))))) : 
                                  FAst.bind )) symbs ls) in
                        let restore =
                          seq_sem
                            (List.map2
                               (fun x  y  ->
                                  (`Assign
                                     (_loc,
                                       (`Field
                                          (_loc, (`Lid (_loc, x)),
                                            (`Lid (_loc, "contents")))),
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
                          FAst.exp ) : 'save_quot )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]));
  Fgram.extend_single (lid : 'lid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid x")],
           ("x\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,x) -> (x : 'lid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]))
let _ = Ast_quotation.of_exp ~name:(Ns.lang, "save") ~entry:save_quot ()