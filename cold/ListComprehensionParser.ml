open FanSig

module Id =
              struct
               let name = "Camlp4ListComprehension"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      let rec loop =
                       fun n ->
                        function
                        | [] -> (None)
                        | ((x, _) :: []) ->
                           if (n = 1) then ( (Some (x)) ) else (None)
                        | (_ :: l) -> (loop ( (n - 1) ) l)

                      let stream_peek_nth =
                       fun n ->
                        fun strm -> (loop n ( (Stream.npeek n strm) ))

                      let test_patt_lessminus =
                       (Gram.Entry.of_parser "test_patt_lessminus" (
                         fun strm ->
                          let rec skip_patt =
                           fun n ->
                            (match (stream_peek_nth n strm) with
                             | Some (KEYWORD ("<-")) -> n
                             | Some (KEYWORD ("[" | "[<")) ->
                                (skip_patt (
                                  (( (ignore_upto "]" ( (n + 1) )) ) + 1) ))
                             | Some (KEYWORD ("(")) ->
                                (skip_patt (
                                  (( (ignore_upto ")" ( (n + 1) )) ) + 1) ))
                             | Some (KEYWORD ("{")) ->
                                (skip_patt (
                                  (( (ignore_upto "}" ( (n + 1) )) ) + 1) ))
                             | (Some (KEYWORD ((("as" | "::") | ",") | "_"))
                                | Some (LIDENT (_) | UIDENT (_))) ->
                                (skip_patt ( (n + 1) ))
                             | (Some (_) | None) -> (raise Stream.Failure ))
                          and ignore_upto =
                           fun end_kwd ->
                            fun n ->
                             (match (stream_peek_nth n strm) with
                              | Some (KEYWORD (prm)) when (prm = end_kwd) ->
                                 n
                              | Some (KEYWORD ("[" | "[<")) ->
                                 (ignore_upto end_kwd (
                                   (( (ignore_upto "]" ( (n + 1) )) ) + 1) ))
                              | Some (KEYWORD ("(")) ->
                                 (ignore_upto end_kwd (
                                   (( (ignore_upto ")" ( (n + 1) )) ) + 1) ))
                              | Some (KEYWORD ("{")) ->
                                 (ignore_upto end_kwd (
                                   (( (ignore_upto "}" ( (n + 1) )) ) + 1) ))
                              | Some (_) -> (ignore_upto end_kwd ( (n + 1) ))
                              | None -> (raise Stream.Failure )) in
                          (skip_patt 1) ))

                      let map =
                       fun _loc ->
                        fun p ->
                         fun e ->
                          fun l ->
                           (match (p, e) with
                            | (Ast.PaId (_, Ast.IdLid (_, x)),
                               Ast.ExId (_, Ast.IdLid (_, y))) when (x = y) ->
                               l
                            | _ ->
                               if (Ast.is_irrefut_patt p) then
                                (
                                (Ast.ExApp
                                  (_loc, (
                                   (Ast.ExApp
                                     (_loc, (
                                      (Ast.ExId
                                        (_loc, (
                                         (Ast.IdAcc
                                           (_loc, (
                                            (Ast.IdUid (_loc, "List")) ), (
                                            (Ast.IdLid (_loc, "map")) ))) )))
                                      ), (
                                      (Ast.ExFun
                                        (_loc, (
                                         (Ast.McArr
                                           (_loc, p, ( (Ast.ExNil (_loc)) ),
                                            e)) ))) ))) ), l))
                                )
                               else
                                (Ast.ExApp
                                  (_loc, (
                                   (Ast.ExApp
                                     (_loc, (
                                      (Ast.ExApp
                                        (_loc, (
                                         (Ast.ExId
                                           (_loc, (
                                            (Ast.IdAcc
                                              (_loc, (
                                               (Ast.IdUid (_loc, "List")) ),
                                               (
                                               (Ast.IdLid
                                                 (_loc, "fold_right")) ))) )))
                                         ), (
                                         (Ast.ExFun
                                           (_loc, (
                                            (Ast.McOr
                                              (_loc, (
                                               (Ast.McArr
                                                 (_loc, p, (
                                                  (Ast.ExId
                                                    (_loc, (
                                                     (Ast.IdUid
                                                       (_loc, "True")) ))) ),
                                                  (
                                                  (Ast.ExApp
                                                    (_loc, (
                                                     (Ast.ExFun
                                                       (_loc, (
                                                        (Ast.McArr
                                                          (_loc, (
                                                           (Ast.PaId
                                                             (_loc, (
                                                              (Ast.IdLid
                                                                (_loc, "x"))
                                                              ))) ), (
                                                           (Ast.ExNil (_loc))
                                                           ), (
                                                           (Ast.ExFun
                                                             (_loc, (
                                                              (Ast.McArr
                                                                (_loc, (
                                                                 (Ast.PaId
                                                                   (_loc, (
                                                                    (
                                                                    Ast.IdLid
                                                                    (_loc,
                                                                    "xs")) )))
                                                                 ), (
                                                                 (Ast.ExNil
                                                                   (_loc)) ),
                                                                 (
                                                                 (Ast.ExApp
                                                                   (_loc, (
                                                                    (
                                                                    Ast.ExApp
                                                                    (_loc, (
                                                                    (Ast.ExId
                                                                    (_loc, (
                                                                    (Ast.IdUid
                                                                    (_loc,
                                                                    "::")) )))
                                                                    ), (
                                                                    (Ast.ExId
                                                                    (_loc, (
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "x")) )))
                                                                    ))) ), (
                                                                    (
                                                                    Ast.ExId
                                                                    (_loc, (
                                                                    (Ast.IdLid
                                                                    (_loc,
                                                                    "xs")) )))
                                                                    ))) )))
                                                              ))) ))) ))) ),
                                                     e)) ))) ), (
                                               (Ast.McArr
                                                 (_loc, ( (Ast.PaAny (_loc))
                                                  ), ( (Ast.ExNil (_loc)) ),
                                                  (
                                                  (Ast.ExFun
                                                    (_loc, (
                                                     (Ast.McArr
                                                       (_loc, (
                                                        (Ast.PaId
                                                          (_loc, (
                                                           (Ast.IdLid
                                                             (_loc, "l")) )))
                                                        ), (
                                                        (Ast.ExNil (_loc)) ),
                                                        (
                                                        (Ast.ExId
                                                          (_loc, (
                                                           (Ast.IdLid
                                                             (_loc, "l")) )))
                                                        ))) ))) ))) ))) )))
                                         ))) ), l)) ), (
                                   (Ast.ExId
                                     (_loc, ( (Ast.IdUid (_loc, "[]")) ))) ))))

                      let filter =
                       fun _loc ->
                        fun p ->
                         fun b ->
                          fun l ->
                           if (Ast.is_irrefut_patt p) then
                            (
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExId
                                    (_loc, (
                                     (Ast.IdAcc
                                       (_loc, ( (Ast.IdUid (_loc, "List")) ),
                                        ( (Ast.IdLid (_loc, "filter")) ))) )))
                                  ), (
                                  (Ast.ExFun
                                    (_loc, (
                                     (Ast.McArr
                                       (_loc, p, ( (Ast.ExNil (_loc)) ), b))
                                     ))) ))) ), l))
                            )
                           else
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExId
                                    (_loc, (
                                     (Ast.IdAcc
                                       (_loc, ( (Ast.IdUid (_loc, "List")) ),
                                        ( (Ast.IdLid (_loc, "filter")) ))) )))
                                  ), (
                                  (Ast.ExFun
                                    (_loc, (
                                     (Ast.McOr
                                       (_loc, (
                                        (Ast.McArr
                                          (_loc, p, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdUid (_loc, "True")) )))
                                           ), b)) ), (
                                        (Ast.McArr
                                          (_loc, ( (Ast.PaAny (_loc)) ), (
                                           (Ast.ExNil (_loc)) ), (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdUid (_loc, "False")) )))
                                           ))) ))) ))) ))) ), l))

                      let concat =
                       fun _loc ->
                        fun l ->
                         (Ast.ExApp
                           (_loc, (
                            (Ast.ExId
                              (_loc, (
                               (Ast.IdAcc
                                 (_loc, ( (Ast.IdUid (_loc, "List")) ), (
                                  (Ast.IdLid (_loc, "concat")) ))) ))) ), l))

                      let rec compr =
                       fun _loc ->
                        fun e ->
                         function
                         | ((`gen (p, l)) :: []) -> (map _loc p e l)
                         | ((`gen (p, l)) :: (`cond b) :: items) ->
                            (compr _loc e (
                              ( `gen (p, ( (filter _loc p b l) )) ) :: items 
                              ))
                         | ((`gen (p, l)) :: (((`gen (_, _)) :: _) as is)) ->
                            (concat _loc (
                              (map _loc p ( (compr _loc e is) ) l) ))
                         | _ -> (raise Stream.Failure )

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("[")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (sem_expr_for_list :
                                       'sem_expr_for_list Gram.Entry.t) )))
                                 ); ( (Gram.Skeyword ("]")) )] ))

                      let is_revised =
                       (try
                         (
                        (Gram.delete_rule expr (
                          [( (Gram.Skeyword ("[")) ); (
                           (Gram.Snterm
                             (Gram.Entry.obj (
                               (sem_expr_for_list :
                                 'sem_expr_for_list Gram.Entry.t) ))) ); (
                           (Gram.Skeyword ("::")) ); (
                           (Gram.Snterm
                             (Gram.Entry.obj ( (expr : 'expr Gram.Entry.t) )))
                           ); ( (Gram.Skeyword ("]")) )] ))
                        );
                         (true)
                        with
                        Not_found -> (false))

                      let comprehension_or_sem_expr_for_list =
                       (Gram.Entry.mk "comprehension_or_sem_expr_for_list")

                      let _ = let _ = (expr : 'expr Gram.Entry.t)
                              and _ =
                               (comprehension_or_sem_expr_for_list :
                                 'comprehension_or_sem_expr_for_list Gram.Entry.t) in
                              let grammar_entry_create = Gram.Entry.mk in
                              let item =
                               ((grammar_entry_create "item") :
                                 'item Gram.Entry.t) in
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some
                                      ((FanSig.Grammar.Level ("simple"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comprehension_or_sem_expr_for_list :
                                               'comprehension_or_sem_expr_for_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("]"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'comprehension_or_sem_expr_for_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (e : 'expr) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (comprehension_or_sem_expr_for_list :
                                  'comprehension_or_sem_expr_for_list Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExApp
                                               (_loc, (
                                                (Ast.ExApp
                                                  (_loc, (
                                                   (Ast.ExId
                                                     (_loc, (
                                                      (Ast.IdUid (_loc, "::"))
                                                      ))) ), e)) ), (
                                                (Ast.ExId
                                                  (_loc, (
                                                   (Ast.IdUid (_loc, "[]"))
                                                   ))) ))) :
                                              'comprehension_or_sem_expr_for_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword ("|")) ); (
                                         (Gram.Slist1sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (item : 'item Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword (";")) ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'item list) ->
                                           fun _ ->
                                            fun (e :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((compr _loc e l) :
                                                'comprehension_or_sem_expr_for_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExApp
                                                (_loc, (
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExId
                                                      (_loc, (
                                                       (Ast.IdUid
                                                         (_loc, "::")) ))) ),
                                                    e)) ), (
                                                 (Ast.ExId
                                                   (_loc, (
                                                    (Ast.IdUid (_loc, "[]"))
                                                    ))) ))) :
                                               'comprehension_or_sem_expr_for_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_expr_for_list :
                                               'sem_expr_for_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (mk :
                                            'sem_expr_for_list) ->
                                           fun _ ->
                                            fun (e :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp
                                                    (_loc, (
                                                     (Ast.ExId
                                                       (_loc, (
                                                        (Ast.IdUid
                                                          (_loc, "::")) )))
                                                     ), e)) ), (
                                                  (mk (
                                                    (Ast.ExId
                                                      (_loc, (
                                                       (Ast.IdUid
                                                         (_loc, "[]")) ))) ))
                                                  ))) :
                                                'comprehension_or_sem_expr_for_list)
                                          )) ))] ))] ))) () ) ))
                              );
                              (Gram.extend ( (item : 'item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (`cond e : 'item) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules item (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (patt :
                                                      'patt Gram.Entry.t) )))
                                                ); ( (Gram.Skeyword ("<-"))
                                                )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (p :
                                                    'patt) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (p : 'e__1) )) ))] ))) );
                                         (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (p :
                                             'e__1) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (`gen (p, e) : 'item) )) ))] ))]
                                    ))) () ) ))

                      let _ = if is_revised then
                               (
                               let _ = (expr : 'expr Gram.Entry.t)
                               and _ =
                                (comprehension_or_sem_expr_for_list :
                                  'comprehension_or_sem_expr_for_list Gram.Entry.t) in
                               (Gram.extend (
                                 (comprehension_or_sem_expr_for_list :
                                   'comprehension_or_sem_expr_for_list Gram.Entry.t)
                                 ) (
                                 ((fun ()
                                     ->
                                    (None , (
                                     [(None , None , (
                                       [((
                                         [(
                                          (Gram.Snterml
                                            ((
                                             (Gram.Entry.obj (
                                               (expr : 'expr Gram.Entry.t) ))
                                             ), "top")) ); (
                                          (Gram.Skeyword ("::")) ); (
                                          (Gram.Snterm
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) )))
                                          )] ), (
                                         (Gram.Action.mk (
                                           fun (last :
                                             'expr) ->
                                            fun _ ->
                                             fun (e :
                                               'expr) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExApp
                                                  (_loc, (
                                                   (Ast.ExApp
                                                     (_loc, (
                                                      (Ast.ExId
                                                        (_loc, (
                                                         (Ast.IdUid
                                                           (_loc, "::")) )))
                                                      ), e)) ), last)) :
                                                 'comprehension_or_sem_expr_for_list)
                                           )) ));
                                        ((
                                         [(
                                          (Gram.Snterml
                                            ((
                                             (Gram.Entry.obj (
                                               (expr : 'expr Gram.Entry.t) ))
                                             ), "top")) ); (
                                          (Gram.Skeyword (";")) ); (
                                          (Gram.Snterm
                                            (Gram.Entry.obj (
                                              (sem_expr_for_list :
                                                'sem_expr_for_list Gram.Entry.t)
                                              ))) ); ( (Gram.Skeyword ("::"))
                                          ); (
                                          (Gram.Snterm
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) )))
                                          )] ), (
                                         (Gram.Action.mk (
                                           fun (last :
                                             'expr) ->
                                            fun _ ->
                                             fun (mk :
                                               'sem_expr_for_list) ->
                                              fun _ ->
                                               fun (e :
                                                 'expr) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.ExApp
                                                    (_loc, (
                                                     (Ast.ExApp
                                                       (_loc, (
                                                        (Ast.ExId
                                                          (_loc, (
                                                           (Ast.IdUid
                                                             (_loc, "::")) )))
                                                        ), e)) ), ( (mk last)
                                                     ))) :
                                                   'comprehension_or_sem_expr_for_list)
                                           )) ))] ))] ))) () ) ))
                               )
                              else ()

                     end
