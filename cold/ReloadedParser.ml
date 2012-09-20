open FanSig

module Id =
              struct
               let name = "Camlp4Reloaded"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      let _ = (Gram.Entry.clear match_case)

                      let _ = (Gram.Entry.clear semi)

                      let mkseq =
                       fun _loc ->
                        function
                        | (Ast.ExSem (_, _, _) as e) -> (Ast.ExSeq (_loc, e))
                        | e -> e

                      let _ = (Gram.delete_rule match_case0 (
                                [(
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (patt_as_patt_opt :
                                       'patt_as_patt_opt Gram.Entry.t) ))) );
                                 (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (opt_when_expr :
                                       'opt_when_expr Gram.Entry.t) ))) ); (
                                 (Gram.Skeyword ("->")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (expr : 'expr Gram.Entry.t) ))) )] ))

                      let revised =
                       (try
                         (
                        (Gram.delete_rule expr (
                          [( (Gram.Skeyword ("if")) ); Gram.Sself ; (
                           (Gram.Skeyword ("then")) ); Gram.Sself ; (
                           (Gram.Skeyword ("else")) ); Gram.Sself ] ))
                        );
                         (true)
                        with
                        Not_found ->
                         (
                         (Gram.delete_rule expr (
                           [( (Gram.Skeyword ("if")) ); Gram.Sself ; (
                            (Gram.Skeyword ("then")) ); (
                            (Gram.Snterml
                              ((
                               (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t)
                                 )) ), "top")) ); ( (Gram.Skeyword ("else"))
                            ); (
                            (Gram.Snterml
                              ((
                               (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t)
                                 )) ), "top")) )] ))
                         );
                         (
                         (Gram.delete_rule expr (
                           [( (Gram.Skeyword ("if")) ); Gram.Sself ; (
                            (Gram.Skeyword ("then")) ); (
                            (Gram.Snterml
                              ((
                               (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t)
                                 )) ), "top")) )] ))
                         );
                         (false))

                      let _ = if revised
                              then
                               begin
                               (
                               (Gram.delete_rule expr (
                                 [( (Gram.Skeyword ("fun")) ); (
                                  (Gram.Skeyword ("[")) ); (
                                  (Gram.Slist0sep
                                    ((
                                     (Gram.Snterm
                                       (Gram.Entry.obj (
                                         (match_case0 :
                                           'match_case0 Gram.Entry.t) ))) ),
                                     ( (Gram.Skeyword ("|")) ))) ); (
                                  (Gram.Skeyword ("]")) )] ))
                               );
                               (
                               (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                 ((fun ()
                                     ->
                                    ((
                                     (Some
                                       ((FanSig.Grammar.Level ("top"))))
                                     ), (
                                     [(None , None , (
                                       [((
                                         [( (Gram.Skeyword ("function")) ); (
                                          (Gram.Snterm
                                            (Gram.Entry.obj (
                                              (match_case :
                                                'match_case Gram.Entry.t) )))
                                          )] ), (
                                         (Gram.Action.mk (
                                           fun (a :
                                             'match_case) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExFun (_loc, a)) : 'expr)
                                           )) ))] ))] ))) () ) ))
                               );
                               (
                               (Gram.delete_rule value_let (
                                 [( (Gram.Skeyword ("value")) )] ))
                               );
                               (Gram.delete_rule value_val (
                                 [( (Gram.Skeyword ("value")) )] ))
                              end else begin
                               (
                               (Gram.delete_rule value_let (
                                 [( (Gram.Skeyword ("let")) )] ))
                               );
                               (Gram.delete_rule value_val (
                                 [( (Gram.Skeyword ("val")) )] ))
                              end

                      let _ = let _ = (match_case : 'match_case Gram.Entry.t)
                              and _ = (semi : 'semi Gram.Entry.t)
                              and _ = (value_val : 'value_val Gram.Entry.t)
                              and _ = (value_let : 'value_let Gram.Entry.t)
                              and _ = (expr : 'expr Gram.Entry.t)
                              and _ =
                               (match_case0 : 'match_case0 Gram.Entry.t) in
                              (
                              (Gram.extend (
                                (match_case : 'match_case Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.McNil (_loc)) :
                                              'match_case) )) ));
                                       ((
                                        [(
                                         (Gram.Sopt ((Gram.Skeyword ("|"))))
                                         ); (
                                         (Gram.Slist1sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (match_case0 :
                                                  'match_case0 Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword ("|")) ))) ); (
                                         (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (l :
                                             'match_case0 list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.mcOr_of_list l) :
                                                'match_case) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (match_case0 : 'match_case0 Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_as_patt_opt :
                                               'patt_as_patt_opt Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_when_expr :
                                               'opt_when_expr Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("->"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'sequence) ->
                                           fun _ ->
                                            fun (w :
                                              'opt_when_expr) ->
                                             fun (p :
                                               'patt_as_patt_opt) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.McArr
                                                  (_loc, p, w, (
                                                   (mkseq _loc e) ))) :
                                                 'match_case0) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some
                                      ((FanSig.Grammar.Level ("top"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("if")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("then")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e2 :
                                             'sequence) ->
                                            fun _ ->
                                             fun (e1 :
                                               'sequence) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExIfe
                                                   (_loc, ( (mkseq _loc e1)
                                                    ), ( (mkseq _loc e2) ), (
                                                    (Ast.ExId
                                                      (_loc, (
                                                       (Ast.IdUid
                                                         (_loc, "()")) ))) ))) :
                                                  'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("if")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("then")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("else")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e3 :
                                             'sequence) ->
                                            fun _ ->
                                             fun (e2 :
                                               'sequence) ->
                                              fun _ ->
                                               fun (e1 :
                                                 'sequence) ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  ((Ast.ExIfe
                                                     (_loc, ( (mkseq _loc e1)
                                                      ), ( (mkseq _loc e2) ),
                                                      ( (mkseq _loc e3) ))) :
                                                    'expr) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (value_let : 'value_let Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("val")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'value_let) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (value_val : 'value_val Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("val")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'value_val) )) ))] ))] )))
                                  () ) ))
                              );
                              (Gram.extend ( (semi : 'semi Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (() : 'semi) )) ));
                                       (( [( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'semi) )) ));
                                       (( [( (Gram.Skeyword (";;")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'semi) )) ))] ))] ))) () )
                                ))

                     end
