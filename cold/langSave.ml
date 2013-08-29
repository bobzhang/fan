open AstLib
let save_quot = Fgram.mk "save_quot"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let lid: 'lid Fgram.t = grammar_entry_create "lid" in
  Fgram.extend_single (save_quot : 'save_quot Fgram.t )
    (None,
      (None, None,
        [([`Slist1 (`Snterm (Fgram.obj (lid : 'lid Fgram.t )));
          `Skeyword "->";
          `Snterm (Fgram.obj (Fsyntax.exp : 'Fsyntax__exp Fgram.t ))],
           ("let symbs = List.map (fun x  -> FState.gensym x) ls in\nlet res = FState.gensym \"res\" in\nlet exc = FState.gensym \"e\" in\nlet binds =\n  and_of_list\n    (List.map2\n       (fun x  y  ->\n          (`Bind\n             (_loc, (`Lid (_loc, x)),\n               (`Field (_loc, (`Lid (_loc, y)), (`Lid (_loc, \"contents\"))))) : \n          FAst.bind )) symbs ls) in\nlet restore =\n  seq_sem\n    (List.map2\n       (fun x  y  ->\n          (`Assign\n             (_loc,\n               (`Field (_loc, (`Lid (_loc, x)), (`Lid (_loc, \"contents\")))),\n               (`Lid (_loc, y))) : FAst.exp )) ls symbs) in\n(`LetIn\n   (_loc, (`Negative _loc), binds,\n     (`Try\n        (_loc,\n          (`Seq\n             (_loc,\n               (`LetIn\n                  (_loc, (`Negative _loc),\n                    (`Bind (_loc, (`Lid (_loc, res)), b)),\n                    (`LetIn\n                       (_loc, (`Negative _loc),\n                         (`Bind (_loc, (`Any _loc), restore)),\n                         (`Lid (_loc, res)))))))),\n          (`Case\n             (_loc, (`Lid (_loc, exc)),\n               (`Seq\n                  (_loc,\n                    (`Sem\n                       (_loc, restore,\n                         (`App\n                            (_loc, (`Lid (_loc, \"raise\")),\n                              (`Lid (_loc, exc))))))))))))) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (b : 'Fsyntax__exp)  _  (ls : 'lid list) 
                   (_loc : FLoc.t)  ->
                   (let symbs = List.map (fun x  -> FState.gensym x) ls in
                    let res = FState.gensym "res" in
                    let exc = FState.gensym "e" in
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
                                   (`Lid (_loc, y))) : FAst.exp )) ls symbs) in
                    (`LetIn
                       (_loc, (`Negative _loc), binds,
                         (`Try
                            (_loc,
                              (`Seq
                                 (_loc,
                                   (`LetIn
                                      (_loc, (`Negative _loc),
                                        (`Bind (_loc, (`Lid (_loc, res)), b)),
                                        (`LetIn
                                           (_loc, (`Negative _loc),
                                             (`Bind
                                                (_loc, (`Any _loc), restore)),
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
                      FAst.exp ) : 'save_quot )))))]));
  Fgram.extend_single (lid : 'lid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("x\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Lid x -> (x : 'lid )
                   | _ -> failwith "x\n"))))]))