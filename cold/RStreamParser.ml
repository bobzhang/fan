open FanSig

module Id : Camlp4.Sig.Id =
              struct
               let name = "Camlp4OCamlRevisedParserParser"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      type spat_comp =
                         SpTrm of Loc.t * Ast.patt * Ast.expr option
                       | SpNtr of Loc.t * Ast.patt * Ast.expr
                       | SpStr of Loc.t * Ast.patt

                      type sexp_comp =
                         SeTrm of Loc.t * Ast.expr
                       | SeNtr of Loc.t * Ast.expr

                      let stream_expr = (Gram.Entry.mk "stream_expr")

                      let stream_begin = (Gram.Entry.mk "stream_begin")

                      let stream_end = (Gram.Entry.mk "stream_end")

                      let stream_quot = (Gram.Entry.mk "stream_quot")

                      let parser_case = (Gram.Entry.mk "parser_case")

                      let parser_case_list =
                       (Gram.Entry.mk "parser_case_list")

                      let strm_n = "__strm"

                      let peek_fun =
                       fun _loc ->
                        (Ast.ExId
                          (_loc, (
                           (Ast.IdAcc
                             (_loc, ( (Ast.IdUid (_loc, "Stream")) ), (
                              (Ast.IdLid (_loc, "peek")) ))) )))

                      let junk_fun =
                       fun _loc ->
                        (Ast.ExId
                          (_loc, (
                           (Ast.IdAcc
                             (_loc, ( (Ast.IdUid (_loc, "Stream")) ), (
                              (Ast.IdLid (_loc, "junk")) ))) )))

                      let rec pattern_eq_expression =
                       fun p ->
                        fun e ->
                         (match (p, e) with
                          | (Ast.PaId (_, Ast.IdLid (_, a)),
                             Ast.ExId (_, Ast.IdLid (_, b))) ->
                             (a = b)
                          | (Ast.PaId (_, Ast.IdUid (_, a)),
                             Ast.ExId (_, Ast.IdUid (_, b))) ->
                             (a = b)
                          | (Ast.PaApp (_, p1, p2), Ast.ExApp (_, e1, e2)) ->
                             (( (pattern_eq_expression p1 e1) ) && (
                               (pattern_eq_expression p2 e2) ))
                          | _ -> (false))

                      let is_raise =
                       fun e ->
                        (match e with
                         | Ast.ExApp
                            (_, Ast.ExId (_, Ast.IdLid (_, "raise")), _) ->
                            (true)
                         | _ -> (false))

                      let is_raise_failure =
                       fun e ->
                        (match e with
                         | Ast.ExApp
                            (_, Ast.ExId (_, Ast.IdLid (_, "raise")),
                             Ast.ExId
                              (_,
                               Ast.IdAcc
                                (_, Ast.IdUid (_, "Stream"),
                                 Ast.IdUid (_, "Failure")))) ->
                            (true)
                         | _ -> (false))

                      let rec handle_failure =
                       fun e ->
                        (match e with
                         | Ast.ExTry
                            (_, _,
                             Ast.McArr
                              (_,
                               Ast.PaId
                                (_,
                                 Ast.IdAcc
                                  (_, Ast.IdUid (_, "Stream"),
                                   Ast.IdUid (_, "Failure"))), Ast.ExNil (_),
                               e)) ->
                            (handle_failure e)
                         | Ast.ExMat (_, me, a) ->
                            let rec match_case_handle_failure =
                             function
                             | Ast.McOr (_, a1, a2) ->
                                (( (match_case_handle_failure a1) ) && (
                                  (match_case_handle_failure a2) ))
                             | Ast.McArr (_, _, Ast.ExNil (_), e) ->
                                (handle_failure e)
                             | _ -> (false) in
                            (( (handle_failure me) ) && (
                              (match_case_handle_failure a) ))
                         | Ast.ExLet (_, Ast.ReNil, bi, e) ->
                            let rec binding_handle_failure =
                             function
                             | Ast.BiAnd (_, b1, b2) ->
                                (( (binding_handle_failure b1) ) && (
                                  (binding_handle_failure b2) ))
                             | Ast.BiEq (_, _, e) -> (handle_failure e)
                             | _ -> (false) in
                            (( (binding_handle_failure bi) ) && (
                              (handle_failure e) ))
                         | (((((Ast.ExId (_, Ast.IdLid (_, _))
                                | Ast.ExInt (_, _)) | Ast.ExStr (_, _))
                              | Ast.ExChr (_, _)) | Ast.ExFun (_, _))
                            | Ast.ExId (_, Ast.IdUid (_, _))) ->
                            (true)
                         | Ast.ExApp
                            (_, Ast.ExId (_, Ast.IdLid (_, "raise")), e) ->
                            (match e with
                             | Ast.ExId
                                (_,
                                 Ast.IdAcc
                                  (_, Ast.IdUid (_, "Stream"),
                                   Ast.IdUid (_, "Failure"))) ->
                                (false)
                             | _ -> (true))
                         | Ast.ExApp (_, f, x) ->
                            (( (is_constr_apply f) ) && (
                              (( (handle_failure f) ) && ( (handle_failure x)
                                )) ))
                         | _ -> (false))
                      and is_constr_apply =
                       function
                       | Ast.ExId (_, Ast.IdUid (_, _)) -> (true)
                       | Ast.ExId (_, Ast.IdLid (_, _)) -> (false)
                       | Ast.ExApp (_, x, _) -> (is_constr_apply x)
                       | _ -> (false)

                      let rec subst =
                       fun v ->
                        fun e ->
                         let _loc = (Ast.loc_of_expr e) in
                         (match e with
                          | Ast.ExId (_, Ast.IdLid (_, x)) ->
                             let x = if (x = v) then strm_n else x in
                             (Ast.ExId (_loc, ( (Ast.IdLid (_loc, x)) )))
                          | Ast.ExId (_, Ast.IdUid (_, _)) -> e
                          | Ast.ExInt (_, _) -> e
                          | Ast.ExChr (_, _) -> e
                          | Ast.ExStr (_, _) -> e
                          | Ast.ExAcc (_, _, _) -> e
                          | Ast.ExLet (_, rf, bi, e) ->
                             (Ast.ExLet
                               (_loc, rf, ( (subst_binding v bi) ), (
                                (subst v e) )))
                          | Ast.ExApp (_, e1, e2) ->
                             (Ast.ExApp
                               (_loc, ( (subst v e1) ), ( (subst v e2) )))
                          | Ast.ExTup (_, e) ->
                             (Ast.ExTup (_loc, ( (subst v e) )))
                          | Ast.ExCom (_, e1, e2) ->
                             (Ast.ExCom
                               (_loc, ( (subst v e1) ), ( (subst v e2) )))
                          | _ -> (raise Not_found ))
                      and subst_binding =
                       fun v ->
                        function
                        | Ast.BiAnd (_loc, b1, b2) ->
                           (Ast.BiAnd
                             (_loc, ( (subst_binding v b1) ), (
                              (subst_binding v b2) )))
                        | Ast.BiEq (_loc, Ast.PaId (_, Ast.IdLid (_, v')), e) ->
                           (Ast.BiEq
                             (_loc, (
                              (Ast.PaId (_loc, ( (Ast.IdLid (_loc, v')) )))
                              ), ( if (v = v') then e else (subst v e) )))
                        | _ -> (raise Not_found )

                      let stream_pattern_component =
                       fun skont ->
                        fun ckont ->
                         function
                         | SpTrm (_loc, p, None) ->
                            (Ast.ExMat
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, ( (peek_fun _loc) ), (
                                  (Ast.ExId
                                    (_loc, ( (Ast.IdLid (_loc, strm_n)) )))
                                  ))) ), (
                               (Ast.McOr
                                 (_loc, (
                                  (Ast.McArr
                                    (_loc, (
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaId
                                          (_loc, ( (Ast.IdUid (_loc, "Some"))
                                           ))) ), p)) ), ( (Ast.ExNil (_loc))
                                     ), (
                                     (Ast.ExSeq
                                       (_loc, (
                                        (Ast.ExSem
                                          (_loc, (
                                           (Ast.ExApp
                                             (_loc, ( (junk_fun _loc) ), (
                                              (Ast.ExId
                                                (_loc, (
                                                 (Ast.IdLid (_loc, strm_n))
                                                 ))) ))) ), skont)) ))) )))
                                  ), (
                                  (Ast.McArr
                                    (_loc, ( (Ast.PaAny (_loc)) ), (
                                     (Ast.ExNil (_loc)) ), ckont)) ))) )))
                         | SpTrm (_loc, p, Some (w)) ->
                            (Ast.ExMat
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, ( (peek_fun _loc) ), (
                                  (Ast.ExId
                                    (_loc, ( (Ast.IdLid (_loc, strm_n)) )))
                                  ))) ), (
                               (Ast.McOr
                                 (_loc, (
                                  (Ast.McArr
                                    (_loc, (
                                     (Ast.PaApp
                                       (_loc, (
                                        (Ast.PaId
                                          (_loc, ( (Ast.IdUid (_loc, "Some"))
                                           ))) ), p)) ), w, (
                                     (Ast.ExSeq
                                       (_loc, (
                                        (Ast.ExSem
                                          (_loc, (
                                           (Ast.ExApp
                                             (_loc, ( (junk_fun _loc) ), (
                                              (Ast.ExId
                                                (_loc, (
                                                 (Ast.IdLid (_loc, strm_n))
                                                 ))) ))) ), skont)) ))) )))
                                  ), (
                                  (Ast.McArr
                                    (_loc, ( (Ast.PaAny (_loc)) ), (
                                     (Ast.ExNil (_loc)) ), ckont)) ))) )))
                         | SpNtr (_loc, p, e) ->
                            let e =
                             (match e with
                              | Ast.ExFun
                                 (_,
                                  Ast.McArr
                                   (_,
                                    Ast.PaTyc
                                     (_, Ast.PaId (_, Ast.IdLid (_, v)),
                                      Ast.TyApp
                                       (_,
                                        Ast.TyId
                                         (_,
                                          Ast.IdAcc
                                           (_, Ast.IdUid (_, "Stream"),
                                            Ast.IdLid (_, "t"))),
                                        Ast.TyAny (_))), Ast.ExNil (_), e))
                                 when (v = strm_n) ->
                                 e
                              | _ ->
                                 (Ast.ExApp
                                   (_loc, e, (
                                    (Ast.ExId
                                      (_loc, ( (Ast.IdLid (_loc, strm_n)) )))
                                    )))) in
                            if (pattern_eq_expression p skont) then
                             (
                             if (is_raise_failure ckont) then e
                             else if (handle_failure e) then e
                             else
                              (Ast.ExTry
                                (_loc, e, (
                                 (Ast.McArr
                                   (_loc, (
                                    (Ast.PaId
                                      (_loc, (
                                       (Ast.IdAcc
                                         (_loc, (
                                          (Ast.IdUid (_loc, "Stream")) ), (
                                          (Ast.IdUid (_loc, "Failure")) )))
                                       ))) ), ( (Ast.ExNil (_loc)) ), ckont))
                                 )))
                             )
                            else if (is_raise_failure ckont) then
                                  (
                                  (Ast.ExLet
                                    (_loc, Ast.ReNil , (
                                     (Ast.BiEq (_loc, p, e)) ), skont))
                                  )
                            else if (pattern_eq_expression (
                                      (Ast.PaApp
                                        (_loc, (
                                         (Ast.PaId
                                           (_loc, (
                                            (Ast.IdUid (_loc, "Some")) ))) ),
                                         p)) ) skont) then
                                  (
                                  (Ast.ExTry
                                    (_loc, (
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, ( (Ast.IdUid (_loc, "Some"))
                                           ))) ), e)) ), (
                                     (Ast.McArr
                                       (_loc, (
                                        (Ast.PaId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Stream")) ),
                                              ( (Ast.IdUid (_loc, "Failure"))
                                              ))) ))) ), ( (Ast.ExNil (_loc))
                                        ), ckont)) )))
                                  )
                            else if (is_raise ckont) then
                                  (
                                  let tst =
                                   if (handle_failure e) then e
                                   else
                                    (Ast.ExTry
                                      (_loc, e, (
                                       (Ast.McArr
                                         (_loc, (
                                          (Ast.PaId
                                            (_loc, (
                                             (Ast.IdAcc
                                               (_loc, (
                                                (Ast.IdUid (_loc, "Stream"))
                                                ), (
                                                (Ast.IdUid (_loc, "Failure"))
                                                ))) ))) ), (
                                          (Ast.ExNil (_loc)) ), ckont)) ))) in
                                  (Ast.ExLet
                                    (_loc, Ast.ReNil , (
                                     (Ast.BiEq (_loc, p, tst)) ), skont))
                                  )
                            else
                             (Ast.ExMat
                               (_loc, (
                                (Ast.ExTry
                                  (_loc, (
                                   (Ast.ExApp
                                     (_loc, (
                                      (Ast.ExId
                                        (_loc, ( (Ast.IdUid (_loc, "Some"))
                                         ))) ), e)) ), (
                                   (Ast.McArr
                                     (_loc, (
                                      (Ast.PaId
                                        (_loc, (
                                         (Ast.IdAcc
                                           (_loc, (
                                            (Ast.IdUid (_loc, "Stream")) ), (
                                            (Ast.IdUid (_loc, "Failure")) )))
                                         ))) ), ( (Ast.ExNil (_loc)) ), (
                                      (Ast.ExId
                                        (_loc, ( (Ast.IdUid (_loc, "None"))
                                         ))) ))) ))) ), (
                                (Ast.McOr
                                  (_loc, (
                                   (Ast.McArr
                                     (_loc, (
                                      (Ast.PaApp
                                        (_loc, (
                                         (Ast.PaId
                                           (_loc, (
                                            (Ast.IdUid (_loc, "Some")) ))) ),
                                         p)) ), ( (Ast.ExNil (_loc)) ),
                                      skont)) ), (
                                   (Ast.McArr
                                     (_loc, ( (Ast.PaAny (_loc)) ), (
                                      (Ast.ExNil (_loc)) ), ckont)) ))) )))
                         | SpStr (_loc, p) ->
                            (try
                              (match p with
                               | Ast.PaId (_, Ast.IdLid (_, v)) ->
                                  (subst v skont)
                               | _ -> (raise Not_found ))
                             with
                             Not_found ->
                              (Ast.ExLet
                                (_loc, Ast.ReNil , (
                                 (Ast.BiEq
                                   (_loc, p, (
                                    (Ast.ExId
                                      (_loc, ( (Ast.IdLid (_loc, strm_n)) )))
                                    ))) ), skont)))

                      let rec stream_pattern =
                       fun _loc ->
                        fun epo ->
                         fun e ->
                          fun ekont ->
                           function
                           | [] ->
                              (match epo with
                               | Some (ep) ->
                                  (Ast.ExLet
                                    (_loc, Ast.ReNil , (
                                     (Ast.BiEq
                                       (_loc, ep, (
                                        (Ast.ExApp
                                          (_loc, (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Stream"))
                                                 ), (
                                                 (Ast.IdLid (_loc, "count"))
                                                 ))) ))) ), (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdLid (_loc, strm_n)) )))
                                           ))) ))) ), e))
                               | _ -> e)
                           | ((spc, err) :: spcl) ->
                              let skont =
                               let ekont =
                                fun err ->
                                 let str =
                                  (match err with
                                   | Some (estr) -> estr
                                   | _ -> (Ast.ExStr (_loc, ""))) in
                                 (Ast.ExApp
                                   (_loc, (
                                    (Ast.ExId
                                      (_loc, ( (Ast.IdLid (_loc, "raise")) )))
                                    ), (
                                    (Ast.ExApp
                                      (_loc, (
                                       (Ast.ExId
                                         (_loc, (
                                          (Ast.IdAcc
                                            (_loc, (
                                             (Ast.IdUid (_loc, "Stream")) ),
                                             ( (Ast.IdUid (_loc, "Error")) )))
                                          ))) ), str)) ))) in
                               (stream_pattern _loc epo e ekont spcl) in
                              let ckont = (ekont err) in
                              (stream_pattern_component skont ckont spc)

                      let stream_patterns_term =
                       fun _loc ->
                        fun ekont ->
                         fun tspel ->
                          let pel =
                           (List.fold_right (
                             fun (p, w, _loc, spcl, epo, e) ->
                              fun acc ->
                               let p =
                                (Ast.PaApp
                                  (_loc, (
                                   (Ast.PaId
                                     (_loc, ( (Ast.IdUid (_loc, "Some")) )))
                                   ), p)) in
                               let e =
                                let ekont =
                                 fun err ->
                                  let str =
                                   (match err with
                                    | Some (estr) -> estr
                                    | _ -> (Ast.ExStr (_loc, ""))) in
                                  (Ast.ExApp
                                    (_loc, (
                                     (Ast.ExId
                                       (_loc, ( (Ast.IdLid (_loc, "raise"))
                                        ))) ), (
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Stream")) ),
                                              ( (Ast.IdUid (_loc, "Error"))
                                              ))) ))) ), str)) ))) in
                                let skont =
                                 (stream_pattern _loc epo e ekont spcl) in
                                (Ast.ExSeq
                                  (_loc, (
                                   (Ast.ExSem
                                     (_loc, (
                                      (Ast.ExApp
                                        (_loc, ( (junk_fun _loc) ), (
                                         (Ast.ExId
                                           (_loc, (
                                            (Ast.IdLid (_loc, strm_n)) ))) )))
                                      ), skont)) ))) in
                               (match w with
                                | Some (w) ->
                                   (Ast.McOr
                                     (_loc, ( (Ast.McArr (_loc, p, w, e)) ),
                                      acc))
                                | None ->
                                   (Ast.McOr
                                     (_loc, (
                                      (Ast.McArr
                                        (_loc, p, ( (Ast.ExNil (_loc)) ), e))
                                      ), acc))) ) tspel ( (Ast.McNil (_loc))
                             )) in
                          (Ast.ExMat
                            (_loc, (
                             (Ast.ExApp
                               (_loc, ( (peek_fun _loc) ), (
                                (Ast.ExId
                                  (_loc, ( (Ast.IdLid (_loc, strm_n)) ))) )))
                             ), (
                             (Ast.McOr
                               (_loc, pel, (
                                (Ast.McArr
                                  (_loc, ( (Ast.PaAny (_loc)) ), (
                                   (Ast.ExNil (_loc)) ), ( (ekont () ) ))) )))
                             )))

                      let rec group_terms =
                       function
                       | ((((SpTrm (_loc, p, w), None) :: spcl), epo, e) ::
                          spel) ->
                          let (tspel, spel) = (group_terms spel) in
                          (( ( (p, w, _loc, spcl, epo, e) ) :: tspel  ),
                           spel)
                       | spel -> ([] , spel)

                      let rec parser_cases =
                       fun _loc ->
                        function
                        | [] ->
                           (Ast.ExApp
                             (_loc, (
                              (Ast.ExId
                                (_loc, ( (Ast.IdLid (_loc, "raise")) ))) ), (
                              (Ast.ExId
                                (_loc, (
                                 (Ast.IdAcc
                                   (_loc, ( (Ast.IdUid (_loc, "Stream")) ), (
                                    (Ast.IdUid (_loc, "Failure")) ))) ))) )))
                        | spel ->
                           (match (group_terms spel) with
                            | ([], ((spcl, epo, e) :: spel)) ->
                               (stream_pattern _loc epo e (
                                 fun _ -> (parser_cases _loc spel) ) spcl)
                            | (tspel, spel) ->
                               (stream_patterns_term _loc (
                                 fun _ -> (parser_cases _loc spel) ) tspel))

                      let cparser =
                       fun _loc ->
                        fun bpo ->
                         fun pc ->
                          let e = (parser_cases _loc pc) in
                          let e =
                           (match bpo with
                            | Some (bp) ->
                               (Ast.ExLet
                                 (_loc, Ast.ReNil , (
                                  (Ast.BiEq
                                    (_loc, bp, (
                                     (Ast.ExApp
                                       (_loc, (
                                        (Ast.ExId
                                          (_loc, (
                                           (Ast.IdAcc
                                             (_loc, (
                                              (Ast.IdUid (_loc, "Stream")) ),
                                              ( (Ast.IdLid (_loc, "count"))
                                              ))) ))) ), (
                                        (Ast.ExId
                                          (_loc, ( (Ast.IdLid (_loc, strm_n))
                                           ))) ))) ))) ), e))
                            | None -> e) in
                          let p =
                           (Ast.PaTyc
                             (_loc, (
                              (Ast.PaId
                                (_loc, ( (Ast.IdLid (_loc, strm_n)) ))) ), (
                              (Ast.TyApp
                                (_loc, (
                                 (Ast.TyId
                                   (_loc, (
                                    (Ast.IdAcc
                                      (_loc, ( (Ast.IdUid (_loc, "Stream"))
                                       ), ( (Ast.IdLid (_loc, "t")) ))) )))
                                 ), ( (Ast.TyAny (_loc)) ))) ))) in
                          (Ast.ExFun
                            (_loc, (
                             (Ast.McArr (_loc, p, ( (Ast.ExNil (_loc)) ), e))
                             )))

                      let cparser_match =
                       fun _loc ->
                        fun me ->
                         fun bpo ->
                          fun pc ->
                           let pc = (parser_cases _loc pc) in
                           let e =
                            (match bpo with
                             | Some (bp) ->
                                (Ast.ExLet
                                  (_loc, Ast.ReNil , (
                                   (Ast.BiEq
                                     (_loc, bp, (
                                      (Ast.ExApp
                                        (_loc, (
                                         (Ast.ExId
                                           (_loc, (
                                            (Ast.IdAcc
                                              (_loc, (
                                               (Ast.IdUid (_loc, "Stream"))
                                               ), (
                                               (Ast.IdLid (_loc, "count")) )))
                                            ))) ), (
                                         (Ast.ExId
                                           (_loc, (
                                            (Ast.IdLid (_loc, strm_n)) ))) )))
                                      ))) ), pc))
                             | None -> pc) in
                           let me =
                            (match me with
                             | (Ast.ExSem (_loc, _, _) as e) ->
                                (Ast.ExSeq (_loc, e))
                             | e -> e) in
                           (match me with
                            | Ast.ExId (_, Ast.IdLid (_, x)) when
                               (x = strm_n) ->
                               e
                            | _ ->
                               (Ast.ExLet
                                 (_loc, Ast.ReNil , (
                                  (Ast.BiEq
                                    (_loc, (
                                     (Ast.PaTyc
                                       (_loc, (
                                        (Ast.PaId
                                          (_loc, ( (Ast.IdLid (_loc, strm_n))
                                           ))) ), (
                                        (Ast.TyApp
                                          (_loc, (
                                           (Ast.TyId
                                             (_loc, (
                                              (Ast.IdAcc
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "Stream"))
                                                 ), ( (Ast.IdLid (_loc, "t"))
                                                 ))) ))) ), (
                                           (Ast.TyAny (_loc)) ))) ))) ), me))
                                  ), e)))

                      let rec not_computing =
                       function
                       | (((((Ast.ExId (_, Ast.IdLid (_, _))
                              | Ast.ExId (_, Ast.IdUid (_, _)))
                             | Ast.ExInt (_, _)) | Ast.ExFlo (_, _))
                           | Ast.ExChr (_, _)) | Ast.ExStr (_, _)) ->
                          (true)
                       | Ast.ExApp (_, x, y) ->
                          (( (is_cons_apply_not_computing x) ) && (
                            (not_computing y) ))
                       | _ -> (false)
                      and is_cons_apply_not_computing =
                       function
                       | Ast.ExId (_, Ast.IdUid (_, _)) -> (true)
                       | Ast.ExId (_, Ast.IdLid (_, _)) -> (false)
                       | Ast.ExApp (_, x, y) ->
                          (( (is_cons_apply_not_computing x) ) && (
                            (not_computing y) ))
                       | _ -> (false)

                      let slazy =
                       fun _loc ->
                        fun e ->
                         (match e with
                          | Ast.ExApp
                             (_, f, Ast.ExId (_, Ast.IdUid (_, "()"))) ->
                             (match f with
                              | Ast.ExId (_, Ast.IdLid (_, _)) -> f
                              | _ ->
                                 (Ast.ExFun
                                   (_loc, (
                                    (Ast.McArr
                                      (_loc, ( (Ast.PaAny (_loc)) ), (
                                       (Ast.ExNil (_loc)) ), e)) ))))
                          | _ ->
                             (Ast.ExFun
                               (_loc, (
                                (Ast.McArr
                                  (_loc, ( (Ast.PaAny (_loc)) ), (
                                   (Ast.ExNil (_loc)) ), e)) ))))

                      let rec cstream =
                       fun gloc ->
                        function
                        | [] ->
                           let _loc = gloc in
                           (Ast.ExId
                             (_loc, (
                              (Ast.IdAcc
                                (_loc, ( (Ast.IdUid (_loc, "Stream")) ), (
                                 (Ast.IdLid (_loc, "sempty")) ))) )))
                        | (SeTrm (_loc, e) :: []) ->
                           if (not_computing e) then
                            (
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExId
                                 (_loc, (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Stream")) ),
                                     ( (Ast.IdLid (_loc, "ising")) ))) ))) ),
                               e))
                            )
                           else
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExId
                                 (_loc, (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Stream")) ),
                                     ( (Ast.IdLid (_loc, "lsing")) ))) ))) ),
                               ( (slazy _loc e) )))
                        | (SeTrm (_loc, e) :: secl) ->
                           if (not_computing e) then
                            (
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExId
                                    (_loc, (
                                     (Ast.IdAcc
                                       (_loc, ( (Ast.IdUid (_loc, "Stream"))
                                        ), ( (Ast.IdLid (_loc, "icons")) )))
                                     ))) ), e)) ), ( (cstream gloc secl) )))
                            )
                           else
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExId
                                    (_loc, (
                                     (Ast.IdAcc
                                       (_loc, ( (Ast.IdUid (_loc, "Stream"))
                                        ), ( (Ast.IdLid (_loc, "lcons")) )))
                                     ))) ), ( (slazy _loc e) ))) ), (
                               (cstream gloc secl) )))
                        | (SeNtr (_loc, e) :: []) ->
                           if (not_computing e) then e
                           else
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExId
                                 (_loc, (
                                  (Ast.IdAcc
                                    (_loc, ( (Ast.IdUid (_loc, "Stream")) ),
                                     ( (Ast.IdLid (_loc, "slazy")) ))) ))) ),
                               ( (slazy _loc e) )))
                        | (SeNtr (_loc, e) :: secl) ->
                           if (not_computing e) then
                            (
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExId
                                    (_loc, (
                                     (Ast.IdAcc
                                       (_loc, ( (Ast.IdUid (_loc, "Stream"))
                                        ), ( (Ast.IdLid (_loc, "iapp")) )))
                                     ))) ), e)) ), ( (cstream gloc secl) )))
                            )
                           else
                            (Ast.ExApp
                              (_loc, (
                               (Ast.ExApp
                                 (_loc, (
                                  (Ast.ExId
                                    (_loc, (
                                     (Ast.IdAcc
                                       (_loc, ( (Ast.IdUid (_loc, "Stream"))
                                        ), ( (Ast.IdLid (_loc, "lapp")) )))
                                     ))) ), ( (slazy _loc e) ))) ), (
                               (cstream gloc secl) )))

                      let _ = let _ = (expr : 'expr Gram.Entry.t)
                              and _ =
                               (parser_case_list :
                                 'parser_case_list Gram.Entry.t)
                              and _ =
                               (parser_case : 'parser_case Gram.Entry.t)
                              and _ =
                               (stream_quot : 'stream_quot Gram.Entry.t)
                              and _ = (stream_end : 'stream_end Gram.Entry.t)
                              and _ =
                               (stream_begin : 'stream_begin Gram.Entry.t)
                              and _ =
                               (stream_expr : 'stream_expr Gram.Entry.t) in
                              let grammar_entry_create = Gram.Entry.mk in
                              let stream_patt =
                               ((grammar_entry_create "stream_patt") :
                                 'stream_patt Gram.Entry.t)
                              and stream_expr_comp =
                               ((grammar_entry_create "stream_expr_comp") :
                                 'stream_expr_comp Gram.Entry.t)
                              and stream_expr_comp_list =
                               ((grammar_entry_create
                                  "stream_expr_comp_list") :
                                 'stream_expr_comp_list Gram.Entry.t)
                              and parser_ipatt =
                               ((grammar_entry_create "parser_ipatt") :
                                 'parser_ipatt Gram.Entry.t)
                              and stream_patt_comp =
                               ((grammar_entry_create "stream_patt_comp") :
                                 'stream_patt_comp Gram.Entry.t)
                              and stream_patt_comp_err_list =
                               ((grammar_entry_create
                                  "stream_patt_comp_err_list") :
                                 'stream_patt_comp_err_list Gram.Entry.t)
                              and stream_patt_comp_err =
                               ((grammar_entry_create "stream_patt_comp_err") :
                                 'stream_patt_comp_err Gram.Entry.t) in
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
                                        [( (Gram.Skeyword ("match")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("with")) ); (
                                         (Gram.Skeyword ("parser")) ); (
                                         (Gram.Sopt
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (parser_ipatt :
                                                  'parser_ipatt Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (parser_case_list :
                                               'parser_case_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (pcl :
                                            'parser_case_list) ->
                                           fun (po :
                                             'parser_ipatt option) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (e :
                                                'sequence) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((cparser_match _loc e po
                                                    pcl) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("parser")) ); (
                                         (Gram.Sopt
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (parser_ipatt :
                                                  'parser_ipatt Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (parser_case_list :
                                               'parser_case_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (pcl :
                                            'parser_case_list) ->
                                           fun (po :
                                             'parser_ipatt option) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((cparser _loc po pcl) : 'expr)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (parser_case_list :
                                  'parser_case_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (parser_case :
                                               'parser_case Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (pc :
                                            'parser_case) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ([pc] : 'parser_case_list) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Slist0sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (parser_case :
                                                  'parser_case Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword ("|")) ))) ); (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pcl :
                                             'parser_case list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (pcl : 'parser_case_list) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (parser_case : 'parser_case Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_begin :
                                               'stream_begin Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt :
                                               'stream_patt Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_end :
                                               'stream_end Gram.Entry.t) )))
                                         ); (
                                         (Gram.Sopt
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (parser_ipatt :
                                                  'parser_ipatt Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (po :
                                              'parser_ipatt option) ->
                                             fun _ ->
                                              fun (sp :
                                                'stream_patt) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((sp, po, e) : 'parser_case)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_begin : 'stream_begin Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("[:")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'stream_begin) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_end : 'stream_end Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword (":]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'stream_end) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_quot : 'stream_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("`")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'stream_quot) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_expr : 'stream_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (e : 'stream_expr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_patt : 'stream_patt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (([]) : 'stream_patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp :
                                               'stream_patt_comp Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp_err_list :
                                               'stream_patt_comp_err_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (sp :
                                            'stream_patt_comp_err_list) ->
                                           fun _ ->
                                            fun (spc :
                                              'stream_patt_comp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (( (spc, None ) ) :: sp  :
                                                'stream_patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp :
                                               'stream_patt_comp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (spc :
                                            'stream_patt_comp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ([(spc, None )] : 'stream_patt)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_patt_comp_err :
                                  'stream_patt_comp_err Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp :
                                               'stream_patt_comp Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Sopt
                                           (Gram.srules stream_patt_comp_err
                                             (
                                             [((
                                               [( (Gram.Skeyword ("??")) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (stream_expr :
                                                      'stream_expr Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun (e :
                                                   'stream_expr) ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (e : 'e__1) )) ))] ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (eo :
                                            'e__1 option) ->
                                           fun (spc :
                                             'stream_patt_comp) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((spc, eo) :
                                               'stream_patt_comp_err) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_patt_comp_err_list :
                                  'stream_patt_comp_err_list Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp_err :
                                               'stream_patt_comp_err Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (sp :
                                            'stream_patt_comp_err_list) ->
                                           fun _ ->
                                            fun (spc :
                                              'stream_patt_comp_err) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (( spc ) :: sp  :
                                                'stream_patt_comp_err_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp_err :
                                               'stream_patt_comp_err Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (spc :
                                             'stream_patt_comp_err) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ([spc] :
                                               'stream_patt_comp_err_list) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_patt_comp_err :
                                               'stream_patt_comp_err Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (spc :
                                            'stream_patt_comp_err) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ([spc] :
                                              'stream_patt_comp_err_list) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_patt_comp :
                                  'stream_patt_comp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((SpStr (_loc, p)) :
                                              'stream_patt_comp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr :
                                               'stream_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'stream_expr) ->
                                           fun _ ->
                                            fun (p :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((SpNtr (_loc, p, e)) :
                                                'stream_patt_comp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_quot :
                                               'stream_quot Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); (
                                         (Gram.Sopt
                                           (Gram.srules stream_patt_comp (
                                             [((
                                               [( (Gram.Skeyword ("when")) );
                                                (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (stream_expr :
                                                      'stream_expr Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun (e :
                                                   'stream_expr) ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (e : 'e__2) )) ))] ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (eo :
                                            'e__2 option) ->
                                           fun (p :
                                             'patt) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((SpTrm (_loc, p, eo)) :
                                                'stream_patt_comp) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (parser_ipatt : 'parser_ipatt Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaAny (_loc)) :
                                              'parser_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaId
                                               (_loc, ( (Ast.IdLid (_loc, i))
                                                ))) : 'parser_ipatt) )) ))]
                                      ))] ))) () ) ))
                              );
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
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_begin :
                                               'stream_begin Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr_comp_list :
                                               'stream_expr_comp_list Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_end :
                                               'stream_end Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (sel :
                                             'stream_expr_comp_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((cstream _loc sel) : 'expr) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_begin :
                                               'stream_begin Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_end :
                                               'stream_end Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((cstream _loc [] ) : 'expr) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (stream_expr_comp_list :
                                  'stream_expr_comp_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr_comp :
                                               'stream_expr_comp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (se :
                                            'stream_expr_comp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ([se] : 'stream_expr_comp_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr_comp :
                                               'stream_expr_comp Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (se :
                                             'stream_expr_comp) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ([se] : 'stream_expr_comp_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr_comp :
                                               'stream_expr_comp Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (sel :
                                            'stream_expr_comp_list) ->
                                           fun _ ->
                                            fun (se :
                                              'stream_expr_comp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (( se ) :: sel  :
                                                'stream_expr_comp_list) )) ))]
                                      ))] ))) () ) ))
                              );
                              (Gram.extend (
                                (stream_expr_comp :
                                  'stream_expr_comp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr :
                                               'stream_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'stream_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((SeNtr (_loc, e)) :
                                              'stream_expr_comp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_quot :
                                               'stream_quot Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (stream_expr :
                                               'stream_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'stream_expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((SeTrm (_loc, e)) :
                                               'stream_expr_comp) )) ))] ))]
                                    ))) () ) ))

                     end
