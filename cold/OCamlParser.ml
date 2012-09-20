open FanSig

module Id : Camlp4.Sig.Id =
              struct
               let name = "Camlp4OCamlParser"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      let _ = (FanConfig.constructors_arity := false )

                      let bigarray_set =
                       fun _loc ->
                        fun var ->
                         fun newval ->
                          (match var with
                           | Ast.ExApp
                              (_,
                               Ast.ExApp
                                (_,
                                 Ast.ExId
                                  (_,
                                   Ast.IdAcc
                                    (_, Ast.IdUid (_, "Bigarray"),
                                     Ast.IdAcc
                                      (_, Ast.IdUid (_, "Array1"),
                                       Ast.IdLid (_, "get")))), arr), c1) ->
                              (Some
                                ((Ast.ExApp
                                   (_loc, (
                                    (Ast.ExApp
                                      (_loc, (
                                       (Ast.ExApp
                                         (_loc, (
                                          (Ast.ExId
                                            (_loc, (
                                             (Ast.IdAcc
                                               (_loc, (
                                                (Ast.IdUid (_loc, "Bigarray"))
                                                ), (
                                                (Ast.IdAcc
                                                  (_loc, (
                                                   (Ast.IdUid
                                                     (_loc, "Array1")) ), (
                                                   (Ast.IdLid (_loc, "set"))
                                                   ))) ))) ))) ), arr)) ),
                                       c1)) ), newval))))
                           | Ast.ExApp
                              (_,
                               Ast.ExApp
                                (_,
                                 Ast.ExApp
                                  (_,
                                   Ast.ExId
                                    (_,
                                     Ast.IdAcc
                                      (_, Ast.IdUid (_, "Bigarray"),
                                       Ast.IdAcc
                                        (_, Ast.IdUid (_, "Array2"),
                                         Ast.IdLid (_, "get")))), arr), c1),
                               c2) ->
                              (Some
                                ((Ast.ExApp
                                   (_loc, (
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
                                                   (Ast.IdUid
                                                     (_loc, "Bigarray")) ), (
                                                   (Ast.IdAcc
                                                     (_loc, (
                                                      (Ast.IdUid
                                                        (_loc, "Array2")) ),
                                                      (
                                                      (Ast.IdLid
                                                        (_loc, "set")) ))) )))
                                                ))) ), arr)) ), c1)) ), c2))
                                    ), newval))))
                           | Ast.ExApp
                              (_,
                               Ast.ExApp
                                (_,
                                 Ast.ExApp
                                  (_,
                                   Ast.ExApp
                                    (_,
                                     Ast.ExId
                                      (_,
                                       Ast.IdAcc
                                        (_, Ast.IdUid (_, "Bigarray"),
                                         Ast.IdAcc
                                          (_, Ast.IdUid (_, "Array3"),
                                           Ast.IdLid (_, "get")))), arr), c1),
                                 c2), c3) ->
                              (Some
                                ((Ast.ExApp
                                   (_loc, (
                                    (Ast.ExApp
                                      (_loc, (
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
                                                      (Ast.IdUid
                                                        (_loc, "Bigarray"))
                                                      ), (
                                                      (Ast.IdAcc
                                                        (_loc, (
                                                         (Ast.IdUid
                                                           (_loc, "Array3"))
                                                         ), (
                                                         (Ast.IdLid
                                                           (_loc, "set")) )))
                                                      ))) ))) ), arr)) ), c1))
                                          ), c2)) ), c3)) ), newval))))
                           | Ast.ExApp
                              (_,
                               Ast.ExApp
                                (_,
                                 Ast.ExId
                                  (_,
                                   Ast.IdAcc
                                    (_, Ast.IdUid (_, "Bigarray"),
                                     Ast.IdAcc
                                      (_, Ast.IdUid (_, "Genarray"),
                                       Ast.IdLid (_, "get")))), arr),
                               Ast.ExArr (_, coords)) ->
                              (Some
                                ((Ast.ExApp
                                   (_loc, (
                                    (Ast.ExApp
                                      (_loc, (
                                       (Ast.ExApp
                                         (_loc, (
                                          (Ast.ExId
                                            (_loc, (
                                             (Ast.IdAcc
                                               (_loc, (
                                                (Ast.IdUid (_loc, "Bigarray"))
                                                ), (
                                                (Ast.IdAcc
                                                  (_loc, (
                                                   (Ast.IdUid
                                                     (_loc, "Genarray")) ), (
                                                   (Ast.IdLid (_loc, "set"))
                                                   ))) ))) ))) ), arr)) ), (
                                       (Ast.ExArr (_loc, coords)) ))) ),
                                    newval))))
                           | _ -> (None))

                      let mk_anti =
                       fun ?(c = "") ->
                        fun n ->
                         fun s -> ("\\$" ^ ( (n ^ ( (c ^ ( (":" ^ s) )) )) ))

                      let conc_seq =
                       fun e1 ->
                        fun e2 ->
                         (match (e1, e2) with
                          | (Ast.ExSeq (_loc, e1), Ast.ExSeq (_, e2)) ->
                             (Ast.ExSeq
                               (_loc, ( (Ast.ExSem (_loc, e1, e2)) )))
                          | (Ast.ExSeq (_loc, e1), _) ->
                             (Ast.ExSeq
                               (_loc, ( (Ast.ExSem (_loc, e1, e2)) )))
                          | (_, Ast.ExSeq (_loc, e2)) ->
                             (Ast.ExSeq
                               (_loc, ( (Ast.ExSem (_loc, e1, e2)) )))
                          | _ ->
                             let _loc =
                              (Loc.merge ( (Ast.loc_of_expr e1) ) (
                                (Ast.loc_of_expr e2) )) in
                             (Ast.ExSeq
                               (_loc, ( (Ast.ExSem (_loc, e1, e2)) ))))

                      let stream_peek_nth =
                       fun n ->
                        fun strm ->
                         let rec loop =
                          fun n ->
                           function
                           | [] -> (None)
                           | ((x, _) :: []) ->
                              if (n == 1) then ( (Some (x)) ) else (None)
                           | (_ :: l) -> (loop ( (n - 1) ) l) in
                         (loop n ( (Stream.npeek n strm) ))

                      let test_not_dot_nor_lparen =
                       (Gram.Entry.of_parser "test_not_dot_nor_lparen" (
                         fun strm ->
                          (match (Stream.peek strm) with
                           | Some (KEYWORD ("." | "("), _) ->
                              (raise Stream.Failure )
                           | _ -> ()) ))

                      let test_ctyp_minusgreater =
                       (Gram.Entry.of_parser "test_ctyp_minusgreater" (
                         fun strm ->
                          let rec skip_simple_ctyp =
                           fun n ->
                            (match (stream_peek_nth n strm) with
                             | Some (KEYWORD ("->")) -> n
                             | Some (KEYWORD ("[" | "[<")) ->
                                (skip_simple_ctyp (
                                  (( (ignore_upto "]" ( (n + 1) )) ) + 1) ))
                             | Some (KEYWORD ("(")) ->
                                (skip_simple_ctyp (
                                  (( (ignore_upto ")" ( (n + 1) )) ) + 1) ))
                             | Some
                                (KEYWORD
                                  ((((((((((("as" | "'") | ":") | "*") | ".")
                                         | "#") | "<") | ">") | "..") | ";")
                                    | "_") | "?")) ->
                                (skip_simple_ctyp ( (n + 1) ))
                             | Some (LIDENT (_) | UIDENT (_)) ->
                                (skip_simple_ctyp ( (n + 1) ))
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
                              | Some (_) -> (ignore_upto end_kwd ( (n + 1) ))
                              | None -> (raise Stream.Failure )) in
                          (match (Stream.peek strm) with
                           | Some
                              (((KEYWORD ("[") | LIDENT (_)) | UIDENT (_)), _) ->
                              (skip_simple_ctyp 1)
                           | Some (KEYWORD ("object"), _) ->
                              (raise Stream.Failure )
                           | _ -> 1) ))

                      let lident_colon =
                       (Gram.Entry.of_parser "lident_colon" (
                         fun strm ->
                          (match (Stream.npeek 2 strm) with
                           | ((LIDENT (i), _) :: (KEYWORD (":"), _) :: []) ->
                              (
                              (Stream.junk strm)
                              );
                              (
                              (Stream.junk strm)
                              );
                              i
                           | _ -> (raise Stream.Failure )) ))

                      let rec is_ident_constr_call =
                       function
                       | Ast.IdUid (_, _) -> (true)
                       | Ast.IdAcc (_, _, i) -> (is_ident_constr_call i)
                       | _ -> (false)

                      let rec is_expr_constr_call =
                       function
                       | Ast.ExId (_, i) -> (is_ident_constr_call i)
                       | Ast.ExVrn (_, _) -> (true)
                       | Ast.ExAcc (_, _, e) -> (is_expr_constr_call e)
                       | Ast.ExApp (_loc, e, _) ->
                          let res = (is_expr_constr_call e) in
                          if (( (not ( !FanConfig.constructors_arity )) ) &&
                               res) then
                           (
                           (Loc.raise _loc (
                             (Stream.Error ("currified constructor")) ))
                           )
                          else res
                       | _ -> (false)

                      let _ = (Gram.delete_rule expr (
                                [Gram.Sself ; ( (Gram.Skeyword ("where")) );
                                 (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (opt_rec : 'opt_rec Gram.Entry.t) ))) );
                                 (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (let_binding :
                                       'let_binding Gram.Entry.t) ))) )] ))

                      let _ = (Gram.delete_rule value_let (
                                [( (Gram.Skeyword ("value")) )] ))

                      let _ = (Gram.delete_rule value_val (
                                [( (Gram.Skeyword ("value")) )] ))

                      let _ = (Gram.delete_rule str_item (
                                [(
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (value_let : 'value_let Gram.Entry.t) )))
                                 ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (opt_rec : 'opt_rec Gram.Entry.t) ))) );
                                 (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (binding : 'binding Gram.Entry.t) ))) )]
                                ))

                      let _ = (Gram.delete_rule module_type (
                                [( (Gram.Skeyword ("'")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (a_ident : 'a_ident Gram.Entry.t) ))) )]
                                ))

                      let _ = (Gram.delete_rule module_type (
                                [Gram.Sself ; Gram.Sself ; (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (dummy : 'dummy Gram.Entry.t) ))) )] ))

                      let _ = (Gram.delete_rule module_type (
                                [Gram.Sself ; ( (Gram.Skeyword (".")) );
                                 Gram.Sself ] ))

                      let _ = (Gram.delete_rule label_expr (
                                [(
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (label_longident :
                                       'label_longident Gram.Entry.t) ))) );
                                 (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (fun_binding :
                                       'fun_binding Gram.Entry.t) ))) )] ))

                      let _ = (Gram.delete_rule meth_list (
                                [(
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (meth_decl : 'meth_decl Gram.Entry.t) )))
                                 ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (opt_dot_dot :
                                       'opt_dot_dot Gram.Entry.t) ))) )] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("let")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (opt_rec : 'opt_rec Gram.Entry.t) ))) );
                                 (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (binding : 'binding Gram.Entry.t) ))) );
                                 ( (Gram.Skeyword ("in")) ); Gram.Sself ] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("let")) ); (
                                 (Gram.Skeyword ("module")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (a_UIDENT : 'a_UIDENT Gram.Entry.t) )))
                                 ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (module_binding0 :
                                       'module_binding0 Gram.Entry.t) ))) );
                                 ( (Gram.Skeyword ("in")) ); Gram.Sself ] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("let")) ); (
                                 (Gram.Skeyword ("open")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (module_longident :
                                       'module_longident Gram.Entry.t) ))) );
                                 ( (Gram.Skeyword ("in")) ); Gram.Sself ] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("fun")) ); (
                                 (Gram.Skeyword ("[")) ); (
                                 (Gram.Slist0sep
                                   ((
                                    (Gram.Snterm
                                      (Gram.Entry.obj (
                                        (match_case0 :
                                          'match_case0 Gram.Entry.t) ))) ), (
                                    (Gram.Skeyword ("|")) ))) ); (
                                 (Gram.Skeyword ("]")) )] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("if")) ); Gram.Sself ; (
                                 (Gram.Skeyword ("then")) ); Gram.Sself ; (
                                 (Gram.Skeyword ("else")) ); Gram.Sself ] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("do")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (do_sequence :
                                       'do_sequence Gram.Entry.t) ))) )] ))

                      let _ = (Gram.delete_rule expr (
                                [Gram.Sself ; Gram.Sself ] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("new")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (class_longident :
                                       'class_longident Gram.Entry.t) ))) )]
                                ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("[")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (sem_expr_for_list :
                                       'sem_expr_for_list Gram.Entry.t) )))
                                 ); ( (Gram.Skeyword ("::")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (expr : 'expr Gram.Entry.t) ))) ); (
                                 (Gram.Skeyword ("]")) )] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("{")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (label_expr_list :
                                       'label_expr_list Gram.Entry.t) ))) );
                                 ( (Gram.Skeyword ("}")) )] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("{")) ); (
                                 (Gram.Skeyword ("(")) ); Gram.Sself ; (
                                 (Gram.Skeyword (")")) ); (
                                 (Gram.Skeyword ("with")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (label_expr_list :
                                       'label_expr_list Gram.Entry.t) ))) );
                                 ( (Gram.Skeyword ("}")) )] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                                 (Gram.Skeyword (",")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (comma_expr : 'comma_expr Gram.Entry.t)
                                     ))) ); ( (Gram.Skeyword (")")) )] ))

                      let _ = (Gram.delete_rule expr (
                                [Gram.Sself ; ( (Gram.Skeyword (":=")) );
                                 Gram.Sself ; (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (dummy : 'dummy Gram.Entry.t) ))) )] ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("~")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (a_LIDENT : 'a_LIDENT Gram.Entry.t) )))
                                 ); ( (Gram.Skeyword (":")) ); Gram.Sself ]
                                ))

                      let _ = (Gram.delete_rule expr (
                                [( (Gram.Skeyword ("?")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (a_LIDENT : 'a_LIDENT Gram.Entry.t) )))
                                 ); ( (Gram.Skeyword (":")) ); Gram.Sself ]
                                ))

                      let _ = (Gram.delete_rule constructor_declarations (
                                [(
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (a_UIDENT : 'a_UIDENT Gram.Entry.t) )))
                                 ); ( (Gram.Skeyword (":")) ); (
                                 (Gram.Snterm
                                   (Gram.Entry.obj (
                                     (ctyp : 'ctyp Gram.Entry.t) ))) )] ))

                      let clear = Gram.Entry.clear

                      let _ = (clear ctyp)

                      let _ = (clear patt)

                      let _ = (clear a_UIDENT)

                      let _ = (clear type_longident_and_parameters)

                      let _ = (clear type_parameters)

                      let _ = (clear ipatt)

                      let _ = (clear labeled_ipatt)

                      let _ = (clear semi)

                      let _ = (clear do_sequence)

                      let _ = (clear type_kind)

                      let _ = (clear constructor_arg_list)

                      let _ = (clear poly_type)

                      let _ = (clear class_name_and_param)

                      let _ = (clear class_longident_and_param)

                      let _ = (clear class_type_longident_and_param)

                      let _ = (clear class_type_plus)

                      let _ = (clear type_constraint)

                      let _ = (clear comma_patt)

                      let _ = (clear sequence)

                      let _ = (clear sem_expr_for_list)

                      let _ = (clear sem_expr)

                      let _ = (clear label_declaration)

                      let _ = (clear star_ctyp)

                      let _ = (clear match_case)

                      let _ = (clear with_constr)

                      let _ = (clear package_type)

                      let _ = (clear top_phrase)

                      let _ = let _ = (a_CHAR : 'a_CHAR Gram.Entry.t)
                              and _ =
                               (package_type : 'package_type Gram.Entry.t)
                              and _ =
                               (do_sequence : 'do_sequence Gram.Entry.t)
                              and _ = (infixop4 : 'infixop4 Gram.Entry.t)
                              and _ = (infixop3 : 'infixop3 Gram.Entry.t)
                              and _ = (infixop2 : 'infixop2 Gram.Entry.t)
                              and _ = (infixop1 : 'infixop1 Gram.Entry.t)
                              and _ = (infixop0 : 'infixop0 Gram.Entry.t)
                              and _ =
                               (with_constr_quot :
                                 'with_constr_quot Gram.Entry.t)
                              and _ =
                               (with_constr : 'with_constr Gram.Entry.t)
                              and _ = (value_val : 'value_val Gram.Entry.t)
                              and _ = (value_let : 'value_let Gram.Entry.t)
                              and _ =
                               (val_longident : 'val_longident Gram.Entry.t)
                              and _ = (use_file : 'use_file Gram.Entry.t)
                              and _ = (typevars : 'typevars Gram.Entry.t)
                              and _ =
                               (type_parameters :
                                 'type_parameters Gram.Entry.t)
                              and _ =
                               (type_parameter :
                                 'type_parameter Gram.Entry.t)
                              and _ =
                               (type_longident_and_parameters :
                                 'type_longident_and_parameters Gram.Entry.t)
                              and _ =
                               (type_longident :
                                 'type_longident Gram.Entry.t)
                              and _ = (type_kind : 'type_kind Gram.Entry.t)
                              and _ =
                               (type_ident_and_parameters :
                                 'type_ident_and_parameters Gram.Entry.t)
                              and _ =
                               (type_declaration :
                                 'type_declaration Gram.Entry.t)
                              and _ =
                               (type_constraint :
                                 'type_constraint Gram.Entry.t)
                              and _ = (top_phrase : 'top_phrase Gram.Entry.t)
                              and _ = (str_items : 'str_items Gram.Entry.t)
                              and _ =
                               (str_item_quot : 'str_item_quot Gram.Entry.t)
                              and _ = (str_item : 'str_item Gram.Entry.t)
                              and _ = (star_ctyp : 'star_ctyp Gram.Entry.t)
                              and _ = (sig_items : 'sig_items Gram.Entry.t)
                              and _ =
                               (sig_item_quot : 'sig_item_quot Gram.Entry.t)
                              and _ = (sig_item : 'sig_item Gram.Entry.t)
                              and _ = (sequence : 'sequence Gram.Entry.t)
                              and _ = (semi : 'semi Gram.Entry.t)
                              and _ =
                               (sem_patt_for_list :
                                 'sem_patt_for_list Gram.Entry.t)
                              and _ = (sem_patt : 'sem_patt Gram.Entry.t)
                              and _ =
                               (sem_expr_for_list :
                                 'sem_expr_for_list Gram.Entry.t)
                              and _ = (sem_expr : 'sem_expr Gram.Entry.t)
                              and _ = (row_field : 'row_field Gram.Entry.t)
                              and _ = (poly_type : 'poly_type Gram.Entry.t)
                              and _ = (phrase : 'phrase Gram.Entry.t)
                              and _ = (patt_tcon : 'patt_tcon Gram.Entry.t)
                              and _ = (patt_quot : 'patt_quot Gram.Entry.t)
                              and _ = (patt_eoi : 'patt_eoi Gram.Entry.t)
                              and _ =
                               (patt_as_patt_opt :
                                 'patt_as_patt_opt Gram.Entry.t)
                              and _ = (patt : 'patt Gram.Entry.t)
                              and _ =
                               (opt_when_expr : 'opt_when_expr Gram.Entry.t)
                              and _ =
                               (opt_virtual : 'opt_virtual Gram.Entry.t)
                              and _ = (opt_rec : 'opt_rec Gram.Entry.t)
                              and _ =
                               (opt_private : 'opt_private Gram.Entry.t)
                              and _ = (opt_polyt : 'opt_polyt Gram.Entry.t)
                              and _ =
                               (opt_mutable : 'opt_mutable Gram.Entry.t)
                              and _ =
                               (opt_meth_list : 'opt_meth_list Gram.Entry.t)
                              and _ = (opt_expr : 'opt_expr Gram.Entry.t)
                              and _ =
                               (opt_eq_ctyp : 'opt_eq_ctyp Gram.Entry.t)
                              and _ =
                               (opt_dot_dot : 'opt_dot_dot Gram.Entry.t)
                              and _ =
                               (opt_comma_ctyp :
                                 'opt_comma_ctyp Gram.Entry.t)
                              and _ =
                               (opt_class_self_type :
                                 'opt_class_self_type Gram.Entry.t)
                              and _ =
                               (opt_class_self_patt :
                                 'opt_class_self_patt Gram.Entry.t)
                              and _ =
                               (opt_as_lident : 'opt_as_lident Gram.Entry.t)
                              and _ = (name_tags : 'name_tags Gram.Entry.t)
                              and _ = (more_ctyp : 'more_ctyp Gram.Entry.t)
                              and _ =
                               (module_type_quot :
                                 'module_type_quot Gram.Entry.t)
                              and _ =
                               (module_type : 'module_type Gram.Entry.t)
                              and _ =
                               (module_rec_declaration :
                                 'module_rec_declaration Gram.Entry.t)
                              and _ =
                               (module_longident_with_app :
                                 'module_longident_with_app Gram.Entry.t)
                              and _ =
                               (module_longident :
                                 'module_longident Gram.Entry.t)
                              and _ =
                               (module_expr_quot :
                                 'module_expr_quot Gram.Entry.t)
                              and _ =
                               (module_expr : 'module_expr Gram.Entry.t)
                              and _ =
                               (module_declaration :
                                 'module_declaration Gram.Entry.t)
                              and _ =
                               (module_binding_quot :
                                 'module_binding_quot Gram.Entry.t)
                              and _ =
                               (module_binding0 :
                                 'module_binding0 Gram.Entry.t)
                              and _ =
                               (module_binding :
                                 'module_binding Gram.Entry.t)
                              and _ =
                               (let_binding : 'let_binding Gram.Entry.t)
                              and _ =
                               (labeled_ipatt : 'labeled_ipatt Gram.Entry.t)
                              and _ = (meth_list : 'meth_list Gram.Entry.t)
                              and _ =
                               (label_patt_list :
                                 'label_patt_list Gram.Entry.t)
                              and _ =
                               (label_longident :
                                 'label_longident Gram.Entry.t)
                              and _ =
                               (label_expr_list :
                                 'label_expr_list Gram.Entry.t)
                              and _ = (label_expr : 'label_expr Gram.Entry.t)
                              and _ =
                               (label_declaration_list :
                                 'label_declaration_list Gram.Entry.t)
                              and _ =
                               (label_declaration :
                                 'label_declaration Gram.Entry.t)
                              and _ = (label : 'label Gram.Entry.t)
                              and _ = (ipatt_tcon : 'ipatt_tcon Gram.Entry.t)
                              and _ = (ipatt : 'ipatt Gram.Entry.t)
                              and _ = (interf : 'interf Gram.Entry.t)
                              and _ = (implem : 'implem Gram.Entry.t)
                              and _ = (ident_quot : 'ident_quot Gram.Entry.t)
                              and _ = (ident : 'ident Gram.Entry.t)
                              and _ = (fun_def : 'fun_def Gram.Entry.t)
                              and _ =
                               (fun_binding : 'fun_binding Gram.Entry.t)
                              and _ = (expr_quot : 'expr_quot Gram.Entry.t)
                              and _ = (expr_eoi : 'expr_eoi Gram.Entry.t)
                              and _ = (expr : 'expr Gram.Entry.t)
                              and _ = (eq_expr : 'eq_expr Gram.Entry.t)
                              and _ = (dummy : 'dummy Gram.Entry.t)
                              and _ =
                               (direction_flag :
                                 'direction_flag Gram.Entry.t)
                              and _ =
                               (cvalue_binding :
                                 'cvalue_binding Gram.Entry.t)
                              and _ = (ctyp_quot : 'ctyp_quot Gram.Entry.t)
                              and _ = (ctyp : 'ctyp Gram.Entry.t)
                              and _ =
                               (constructor_declarations :
                                 'constructor_declarations Gram.Entry.t)
                              and _ =
                               (constructor_declaration :
                                 'constructor_declaration Gram.Entry.t)
                              and _ =
                               (constructor_arg_list :
                                 'constructor_arg_list Gram.Entry.t)
                              and _ = (constrain : 'constrain Gram.Entry.t)
                              and _ =
                               (comma_type_parameter :
                                 'comma_type_parameter Gram.Entry.t)
                              and _ = (comma_patt : 'comma_patt Gram.Entry.t)
                              and _ =
                               (comma_ipatt : 'comma_ipatt Gram.Entry.t)
                              and _ = (comma_expr : 'comma_expr Gram.Entry.t)
                              and _ = (comma_ctyp : 'comma_ctyp Gram.Entry.t)
                              and _ =
                               (class_type_quot :
                                 'class_type_quot Gram.Entry.t)
                              and _ =
                               (class_type_plus :
                                 'class_type_plus Gram.Entry.t)
                              and _ =
                               (class_type_longident_and_param :
                                 'class_type_longident_and_param Gram.Entry.t)
                              and _ =
                               (class_type_longident :
                                 'class_type_longident Gram.Entry.t)
                              and _ =
                               (class_type_declaration :
                                 'class_type_declaration Gram.Entry.t)
                              and _ = (class_type : 'class_type Gram.Entry.t)
                              and _ =
                               (class_structure :
                                 'class_structure Gram.Entry.t)
                              and _ =
                               (class_str_item_quot :
                                 'class_str_item_quot Gram.Entry.t)
                              and _ =
                               (class_str_item :
                                 'class_str_item Gram.Entry.t)
                              and _ =
                               (class_signature :
                                 'class_signature Gram.Entry.t)
                              and _ =
                               (class_sig_item_quot :
                                 'class_sig_item_quot Gram.Entry.t)
                              and _ =
                               (class_sig_item :
                                 'class_sig_item Gram.Entry.t)
                              and _ =
                               (class_name_and_param :
                                 'class_name_and_param Gram.Entry.t)
                              and _ =
                               (class_longident_and_param :
                                 'class_longident_and_param Gram.Entry.t)
                              and _ =
                               (class_longident :
                                 'class_longident Gram.Entry.t)
                              and _ =
                               (class_info_for_class_type :
                                 'class_info_for_class_type Gram.Entry.t)
                              and _ =
                               (class_info_for_class_expr :
                                 'class_info_for_class_expr Gram.Entry.t)
                              and _ =
                               (class_fun_def : 'class_fun_def Gram.Entry.t)
                              and _ =
                               (class_fun_binding :
                                 'class_fun_binding Gram.Entry.t)
                              and _ =
                               (class_expr_quot :
                                 'class_expr_quot Gram.Entry.t)
                              and _ = (class_expr : 'class_expr Gram.Entry.t)
                              and _ =
                               (class_description :
                                 'class_description Gram.Entry.t)
                              and _ =
                               (class_declaration :
                                 'class_declaration Gram.Entry.t)
                              and _ =
                               (binding_quot : 'binding_quot Gram.Entry.t)
                              and _ = (binding : 'binding Gram.Entry.t)
                              and _ =
                               (match_case_quot :
                                 'match_case_quot Gram.Entry.t)
                              and _ =
                               (match_case0 : 'match_case0 Gram.Entry.t)
                              and _ = (match_case : 'match_case Gram.Entry.t)
                              and _ = (and_ctyp : 'and_ctyp Gram.Entry.t)
                              and _ = (amp_ctyp : 'amp_ctyp Gram.Entry.t)
                              and _ = (a_ident : 'a_ident Gram.Entry.t)
                              and _ = (a_UIDENT : 'a_UIDENT Gram.Entry.t)
                              and _ = (a_STRING : 'a_STRING Gram.Entry.t)
                              and _ = (a_OPTLABEL : 'a_OPTLABEL Gram.Entry.t)
                              and _ =
                               (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t)
                              and _ = (a_LIDENT : 'a_LIDENT Gram.Entry.t)
                              and _ = (a_LABEL : 'a_LABEL Gram.Entry.t)
                              and _ = (a_INT64 : 'a_INT64 Gram.Entry.t)
                              and _ = (a_INT32 : 'a_INT32 Gram.Entry.t)
                              and _ = (a_INT : 'a_INT Gram.Entry.t)
                              and _ = (a_FLOAT : 'a_FLOAT Gram.Entry.t) in
                              let grammar_entry_create = Gram.Entry.mk in
                              let seq_expr =
                               ((grammar_entry_create "seq_expr") :
                                 'seq_expr Gram.Entry.t)
                              and optional_type_parameter =
                               ((grammar_entry_create
                                  "optional_type_parameter") :
                                 'optional_type_parameter Gram.Entry.t)
                              and comma_ctyp_app =
                               ((grammar_entry_create "comma_ctyp_app") :
                                 'comma_ctyp_app Gram.Entry.t)
                              and opt_private_ctyp =
                               ((grammar_entry_create "opt_private_ctyp") :
                                 'opt_private_ctyp Gram.Entry.t)
                              and package_type_cstrs =
                               ((grammar_entry_create "package_type_cstrs") :
                                 'package_type_cstrs Gram.Entry.t)
                              and package_type_cstr =
                               ((grammar_entry_create "package_type_cstr") :
                                 'package_type_cstr Gram.Entry.t)
                              and patt_constr =
                               ((grammar_entry_create "patt_constr") :
                                 'patt_constr Gram.Entry.t) in
                              (
                              (Gram.extend (
                                (sem_expr : 'sem_expr Gram.Entry.t) ) (
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
                                            (e : 'sem_expr) )) ));
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
                                             (e : 'sem_expr) )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword (";")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'sem_expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExSem (_loc, e1, e2)) :
                                                'sem_expr) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (sequence : 'sequence Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_expr :
                                               'sem_expr Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'sem_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (e : 'sequence) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (do_sequence : 'do_sequence Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("done")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (seq :
                                             'sequence) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (seq : 'do_sequence) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (sem_expr_for_list :
                                  'sem_expr_for_list Gram.Entry.t) ) (
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
                                            (fun acc ->
                                              (Ast.ExApp
                                                (_loc, (
                                                 (Ast.ExApp
                                                   (_loc, (
                                                    (Ast.ExId
                                                      (_loc, (
                                                       (Ast.IdUid
                                                         (_loc, "::")) ))) ),
                                                    e)) ), acc)) :
                                              'sem_expr_for_list) )) ));
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
                                             (fun acc ->
                                               (Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp
                                                    (_loc, (
                                                     (Ast.ExId
                                                       (_loc, (
                                                        (Ast.IdUid
                                                          (_loc, "::")) )))
                                                     ), e)) ), acc)) :
                                               'sem_expr_for_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword (";")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (el :
                                            'sem_expr_for_list) ->
                                           fun _ ->
                                            fun (e :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (fun acc ->
                                                (Ast.ExApp
                                                  (_loc, (
                                                   (Ast.ExApp
                                                     (_loc, (
                                                      (Ast.ExId
                                                        (_loc, (
                                                         (Ast.IdUid
                                                           (_loc, "::")) )))
                                                      ), e)) ), ( (el acc) ))) :
                                                'sem_expr_for_list) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (str_item : 'str_item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("top")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Skeyword ("open")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("in"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (i :
                                              'module_longident) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.StExp
                                                   (_loc, (
                                                    (Ast.ExOpI (_loc, i, e))
                                                    ))) : 'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_binding0 :
                                               'module_binding0 Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("in"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (mb :
                                              'module_binding0) ->
                                             fun (m :
                                               'a_UIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.StExp
                                                    (_loc, (
                                                     (Ast.ExLmd
                                                       (_loc, m, mb, e)) ))) :
                                                   'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_rec :
                                               'opt_rec Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (binding :
                                               'binding Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (bi :
                                            'binding) ->
                                           fun (r :
                                             'opt_rec) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match bi with
                                                | Ast.BiEq
                                                   (_, Ast.PaAny (_), e) ->
                                                   (Ast.StExp (_loc, e))
                                                | _ ->
                                                   (Ast.StVal (_loc, r, bi))) :
                                                'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_rec :
                                               'opt_rec Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (binding :
                                               'binding Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("in")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'expr) ->
                                           fun _ ->
                                            fun (bi :
                                              'binding) ->
                                             fun (r :
                                               'opt_rec) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.StExp
                                                   (_loc, (
                                                    (Ast.ExLet
                                                      (_loc, r, bi, x)) ))) :
                                                  'str_item) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (seq_expr : 'seq_expr Gram.Entry.t) ) (
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
                                          fun (e1 :
                                            'expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (e1 : 'seq_expr) )) ));
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
                                           fun (e1 :
                                             'expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (e1 : 'seq_expr) )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword (";")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'seq_expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((conc_seq e1 e2) : 'seq_expr)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Before ("top"))))
                                    ), (
                                    [(( (Some (";")) ), None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (seq_expr :
                                               'seq_expr Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'seq_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (e : 'expr) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (( (Some ((FanSig.Grammar.Level ("top"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("if")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("then")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExIfe
                                                  (_loc, e1, e2, (
                                                   (Ast.ExId
                                                     (_loc, (
                                                      (Ast.IdUid (_loc, "()"))
                                                      ))) ))) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("if")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("then")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) ); (
                                         (Gram.Skeyword ("else")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e3 :
                                            'expr) ->
                                           fun _ ->
                                            fun (e2 :
                                              'expr) ->
                                             fun _ ->
                                              fun (e1 :
                                                'expr) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.ExIfe
                                                    (_loc, e1, e2, e3)) :
                                                   'expr) )) ));
                                       ((
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
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Skeyword ("open")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("in"))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), ";")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (i :
                                              'module_longident) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExOpI (_loc, i, e)) :
                                                  'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_binding0 :
                                               'module_binding0 Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("in"))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), ";")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (mb :
                                              'module_binding0) ->
                                             fun (m :
                                               'a_UIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.ExLmd (_loc, m, mb, e)) :
                                                   'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_rec :
                                               'opt_rec Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (binding :
                                               'binding Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("in")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), ";")) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'expr) ->
                                           fun _ ->
                                            fun (bi :
                                              'binding) ->
                                             fun (r :
                                               'opt_rec) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExLet (_loc, r, bi, x)) :
                                                  'expr) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (( (Some ((FanSig.Grammar.Before ("||"))))
                                    ), (
                                    [(( (Some (",")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_expr :
                                               'comma_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'comma_expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExTup
                                                 (_loc, (
                                                  (Ast.ExCom (_loc, e1, e2))
                                                  ))) : 'expr) )) ))] ));
                                     (( (Some (":=")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("<-")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match
                                                  (bigarray_set _loc e1 e2) with
                                                | Some (e) -> e
                                                | None ->
                                                   (Ast.ExAss (_loc, e1, e2))) :
                                                'expr) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword (":=")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExAss
                                                 (_loc, (
                                                  (Ast.ExAcc
                                                    (_loc, e1, (
                                                     (Ast.ExId
                                                       (_loc, (
                                                        (Ast.IdLid
                                                          (_loc, "val")) )))
                                                     ))) ), e2)) : 'expr) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (( (Some ((FanSig.Grammar.After ("^"))))
                                    ), (
                                    [(( (Some ("::")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("::")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun _ ->
                                            fun (e1 :
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
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Level ("apply"))))
                                    ), (
                                    [(None , None , (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (e1 :
                                             'expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((match
                                                 (( (is_expr_constr_call e1)
                                                  ), e2) with
                                               | (true, Ast.ExTup (_, e)) ->
                                                  (List.fold_left (
                                                    fun e1 ->
                                                     fun e2 ->
                                                      (Ast.ExApp
                                                        (_loc, e1, e2)) ) e1
                                                    (
                                                    (Ast.list_of_expr e [] )
                                                    ))
                                               | _ ->
                                                  (Ast.ExApp (_loc, e1, e2))) :
                                               'expr) )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Level ("simple"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("new")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_longident :
                                               'class_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'class_longident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExNew (_loc, i)) : 'expr)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Stry
                                           (Gram.srules expr (
                                             [((
                                               [(
                                                (Gram.Snterml
                                                  ((
                                                   (Gram.Entry.obj (
                                                     (expr :
                                                       'expr Gram.Entry.t) ))
                                                   ), ".")) ); (
                                                (Gram.Skeyword ("with")) )]
                                               ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (e :
                                                    'expr) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (e : 'e__2) )) ))] ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr_list :
                                               'label_expr_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (lel :
                                             'label_expr_list) ->
                                            fun (e :
                                              'e__2) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExRec (_loc, lel, e)) :
                                                 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Stry
                                           (Gram.srules expr (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (label_expr_list :
                                                      'label_expr_list Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Skeyword ("}")) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (lel :
                                                    'label_expr_list) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (lel : 'e__1) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (lel :
                                            'e__1) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExRec
                                                (_loc, lel, (
                                                 (Ast.ExNil (_loc)) ))) :
                                               'expr) )) ));
                                       (( [( (Gram.Skeyword ("true")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "True")) ))) :
                                              'expr) )) ));
                                       (( [( (Gram.Skeyword ("false")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "False"))
                                                ))) : 'expr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (val_longident : 'val_longident Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (".")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'val_longident) ->
                                           fun _ ->
                                            fun (i :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc
                                                 (_loc, (
                                                  (Ast.IdUid (_loc, i)) ), j)) :
                                                'val_longident) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "id") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"id\") | \"anti\") | \"list\"), _)"))
                                         ); ( (Gram.Skeyword (".")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'val_longident) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((((("" | "id") | "anti")
                                                     | "list") as n), s) ->
                                                  ((Ast.IdAcc
                                                     (_loc, (
                                                      (Ast.IdAnt
                                                        (_loc, (
                                                         (mk_anti ~c:"ident"
                                                           n s) ))) ), i)) :
                                                    'val_longident)
                                               | _ -> assert false) )) ));
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
                                            ((Ast.IdLid (_loc, i)) :
                                              'val_longident) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_UIDENT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.IdUid (_loc, i)) :
                                              'val_longident) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "id") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"id\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "id") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.IdAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"ident" n s)
                                                    ))) : 'val_longident)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (match_case : 'match_case Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                            (Gram.Skeyword ("|")) ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'match_case0 list) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.mcOr_of_list l) :
                                               'match_case) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (patt_constr : 'patt_constr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaVrn (_loc, s)) :
                                               'patt_constr) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'module_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaId (_loc, i)) :
                                              'patt_constr) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend ( (patt : 'patt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("as")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("as")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun _ ->
                                            fun (p1 :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaAli
                                                 (_loc, p1, (
                                                  (Ast.PaId
                                                    (_loc, (
                                                     (Ast.IdLid (_loc, i)) )))
                                                  ))) : 'patt) )) ))] ));
                                     (( (Some ("|")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ), (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("|"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'patt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaOrp (_loc, p1, p2)) :
                                                'patt) )) ))] ));
                                     (( (Some (",")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); (
                                         (Gram.Slist1sep
                                           (Gram.Snext , (
                                            (Gram.Skeyword (",")) ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (pl :
                                            'patt list) ->
                                           fun _ ->
                                            fun (p :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaTup
                                                 (_loc, (
                                                  (Ast.PaCom
                                                    (_loc, p, (
                                                     (Ast.paCom_of_list pl)
                                                     ))) ))) : 'patt) )) ))]
                                      ));
                                     (( (Some ("::")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("::")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'patt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaApp
                                                 (_loc, (
                                                  (Ast.PaApp
                                                    (_loc, (
                                                     (Ast.PaId
                                                       (_loc, (
                                                        (Ast.IdUid
                                                          (_loc, "::")) )))
                                                     ), p1)) ), p2)) : 'patt)
                                          )) ))] ));
                                     (( (Some ("apply")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ), (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_constr :
                                               'patt_constr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt_constr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p : 'patt) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "pat") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"pat\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "pat") | "anti") as
                                                  n), s) ->
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt" n s)
                                                    ))) : 'patt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("lazy")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaLaz (_loc, p)) : 'patt)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_constr :
                                               'patt_constr Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'patt) ->
                                           fun (p1 :
                                             'patt_constr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((match p2 with
                                               | Ast.PaTup (_, p) ->
                                                  (List.fold_left (
                                                    fun p1 ->
                                                     fun p2 ->
                                                      (Ast.PaApp
                                                        (_loc, p1, p2)) ) p1
                                                    (
                                                    (Ast.list_of_patt p [] )
                                                    ))
                                               | _ ->
                                                  (Ast.PaApp (_loc, p1, p2))) :
                                               'patt) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("#")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_longident :
                                               'type_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'type_longident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaTyp (_loc, i)) : 'patt)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaVrn (_loc, s)) : 'patt)
                                          )) ));
                                       (( [( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaAny (_loc)) : 'patt) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'patt) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (p : 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (":"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (p :
                                               'patt) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaTyc (_loc, p, t)) :
                                                  'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (package_type :
                                               'package_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pt :
                                             'package_type) ->
                                            fun _ ->
                                             fun (m :
                                               'a_UIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaTyc
                                                    (_loc, (
                                                     (Ast.PaMod (_loc, m)) ),
                                                     ( (Ast.TyPkg (_loc, pt))
                                                     ))) : 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (m :
                                             'a_UIDENT) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaMod (_loc, m)) :
                                                 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaId
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "()")) ))) :
                                               'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_patt_list :
                                               'label_patt_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pl :
                                             'label_patt_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaRec (_loc, pl)) :
                                                'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[|")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_patt :
                                               'sem_patt Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("|]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pl :
                                             'sem_patt) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaArr (_loc, pl)) :
                                                'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[|")) ); (
                                         (Gram.Skeyword ("|]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaArr
                                                (_loc, ( (Ast.PaNil (_loc))
                                                 ))) : 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_patt_for_list :
                                               'sem_patt_for_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("]"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (mk_list :
                                             'sem_patt_for_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((mk_list (
                                                 (Ast.PaId
                                                   (_loc, (
                                                    (Ast.IdUid (_loc, "[]"))
                                                    ))) )) : 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_patt_for_list :
                                               'sem_patt_for_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("::"))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (last :
                                             'patt) ->
                                            fun _ ->
                                             fun (mk_list :
                                               'sem_patt_for_list) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((mk_list last) : 'patt) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaId
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "[]")) ))) :
                                               'patt) )) ));
                                       (( [( (Gram.Skeyword ("true")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "True")) ))) :
                                              'patt) )) ));
                                       (( [( (Gram.Skeyword ("false")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "False"))
                                                ))) : 'patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_CHAR : 'a_CHAR Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_CHAR) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaChr (_loc, s)) : 'patt)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_CHAR : 'a_CHAR Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (".."))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_CHAR : 'a_CHAR Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (s2 :
                                            'a_CHAR) ->
                                           fun _ ->
                                            fun (s1 :
                                              'a_CHAR) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaRng
                                                 (_loc, (
                                                  (Ast.PaChr (_loc, s1)) ), (
                                                  (Ast.PaChr (_loc, s2)) ))) :
                                                'patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_STRING :
                                               'a_STRING Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_STRING) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaStr (_loc, s)) : 'patt)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_FLOAT :
                                               'a_FLOAT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_FLOAT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaFlo (_loc, s)) : 'patt)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_FLOAT :
                                               'a_FLOAT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_FLOAT) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaFlo
                                                (_loc, ( ("-" ^ s) ))) :
                                               'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_NATIVEINT :
                                               'a_NATIVEINT Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_NATIVEINT) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaNativeInt
                                                (_loc, ( ("-" ^ s) ))) :
                                               'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_INT64 :
                                               'a_INT64 Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_INT64) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaInt64
                                                (_loc, ( ("-" ^ s) ))) :
                                               'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_INT32 :
                                               'a_INT32 Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_INT32) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaInt32
                                                (_loc, ( ("-" ^ s) ))) :
                                               'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_INT : 'a_INT Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_INT) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaInt
                                                (_loc, ( ("-" ^ s) ))) :
                                               'patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_NATIVEINT :
                                               'a_NATIVEINT Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_NATIVEINT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaNativeInt (_loc, s)) :
                                              'patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_INT64 :
                                               'a_INT64 Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_INT64) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaInt64 (_loc, s)) : 'patt)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_INT32 :
                                               'a_INT32 Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_INT32) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaInt32 (_loc, s)) : 'patt)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_INT : 'a_INT Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_INT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaInt (_loc, s)) : 'patt)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ident : 'ident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'ident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaId (_loc, i)) : 'patt) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | QUOTATION (_) -> (true)
                                            | _ -> (false) ),
                                            "QUOTATION (_)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | QUOTATION (x) ->
                                                ((Quotation.expand _loc x
                                                   Quotation.DynAst.patt_tag) :
                                                  'patt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("`bool", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"`bool\", _)")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("`bool" as n), s) ->
                                                ((Ast.PaAnt
                                                   (_loc, ( (mk_anti n s) ))) :
                                                  'patt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("tup", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"tup\", _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("tup" as n), s) ->
                                                ((Ast.PaTup
                                                   (_loc, (
                                                    (Ast.PaAnt
                                                      (_loc, (
                                                       (mk_anti ~c:"patt" n
                                                         s) ))) ))) : 'patt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "pat") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"pat\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "pat") | "anti") as
                                                  n), s) ->
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt" n s)
                                                    ))) : 'patt)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (comma_expr : 'comma_expr Gram.Entry.t) ) (
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
                                            ), ":=")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e1 :
                                            'expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (e1 : 'comma_expr) )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), ":=")) ); (
                                         (Gram.Skeyword (",")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'comma_expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExCom (_loc, e1, e2)) :
                                                'comma_expr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_constraint :
                                  'type_constraint Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("constraint")) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'type_constraint) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (with_constr : 'with_constr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ), (
                                      [((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (":="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident_with_app :
                                               'module_longident_with_app Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i2 :
                                            'module_longident_with_app) ->
                                           fun _ ->
                                            fun (i1 :
                                              'module_longident) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.WcMoS (_loc, i1, i2)) :
                                                 'with_constr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_longident_and_parameters :
                                               'type_longident_and_parameters Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (":="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private_ctyp :
                                               'opt_private_ctyp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'opt_private_ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'type_longident_and_parameters) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.WcTyS (_loc, t1, t2)) :
                                                 'with_constr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "typ") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"typ\") | \"anti\"), _)"))
                                         ); ( (Gram.Skeyword (":=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private_ctyp :
                                               'opt_private_ctyp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'opt_private_ctyp) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (match __camlp4_0 with
                                                | ANTIQUOT
                                                   (((("" | "typ") | "anti") as
                                                     n), s) ->
                                                   ((Ast.WcTyS
                                                      (_loc, (
                                                       (Ast.TyAnt
                                                         (_loc, (
                                                          (mk_anti ~c:"ctyp"
                                                            n s) ))) ), t)) :
                                                     'with_constr)
                                                | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident_with_app :
                                               'module_longident_with_app Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i2 :
                                            'module_longident_with_app) ->
                                           fun _ ->
                                            fun (i1 :
                                              'module_longident) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.WcMod (_loc, i1, i2)) :
                                                 'with_constr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_longident_and_parameters :
                                               'type_longident_and_parameters Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private_ctyp :
                                               'opt_private_ctyp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'opt_private_ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'type_longident_and_parameters) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.WcTyp (_loc, t1, t2)) :
                                                 'with_constr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "typ") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"typ\") | \"anti\"), _)"))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private_ctyp :
                                               'opt_private_ctyp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'opt_private_ctyp) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (match __camlp4_0 with
                                                | ANTIQUOT
                                                   (((("" | "typ") | "anti") as
                                                     n), s) ->
                                                   ((Ast.WcTyp
                                                      (_loc, (
                                                       (Ast.TyAnt
                                                         (_loc, (
                                                          (mk_anti ~c:"ctyp"
                                                            n s) ))) ), t)) :
                                                     'with_constr)
                                                | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | QUOTATION (_) -> (true)
                                            | _ -> (false) ),
                                            "QUOTATION (_)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | QUOTATION (x) ->
                                                ((Quotation.expand _loc x
                                                   Quotation.DynAst.with_constr_tag) :
                                                  'with_constr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "with_constr")
                                                  | "anti") | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"with_constr\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "with_constr")
                                                    | "anti") | "list") as n),
                                                 s) ->
                                                ((Ast.WcAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"with_constr"
                                                      n s) ))) :
                                                  'with_constr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (wc2 :
                                            'with_constr) ->
                                           fun _ ->
                                            fun (wc1 :
                                              'with_constr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.WcAnd (_loc, wc1, wc2)) :
                                                'with_constr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (package_type : 'package_type Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident_with_app :
                                               'module_longident_with_app Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("with")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (package_type_cstrs :
                                               'package_type_cstrs Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (cs :
                                            'package_type_cstrs) ->
                                           fun _ ->
                                            fun (i :
                                              'module_longident_with_app) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MtWit
                                                 (_loc, (
                                                  (Ast.MtId (_loc, i)) ), cs)) :
                                                'package_type) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident_with_app :
                                               'module_longident_with_app Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'module_longident_with_app) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.MtId (_loc, i)) :
                                              'package_type) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (package_type_cstr :
                                  'package_type_cstr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ident : 'ident Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ty :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (i :
                                              'ident) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.WcTyp
                                                  (_loc, (
                                                   (Ast.TyId (_loc, i)) ),
                                                   ty)) : 'package_type_cstr)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (package_type_cstrs :
                                  'package_type_cstrs Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (package_type_cstr :
                                               'package_type_cstr Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("and"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (cs :
                                            'package_type_cstrs) ->
                                           fun _ ->
                                            fun (c :
                                              'package_type_cstr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.WcAnd (_loc, c, cs)) :
                                                'package_type_cstrs) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (package_type_cstr :
                                               'package_type_cstr Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (c :
                                            'package_type_cstr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (c : 'package_type_cstrs) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_private_ctyp :
                                  'opt_private_ctyp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'opt_private_ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("private")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyPrv (_loc, t)) :
                                               'opt_private_ctyp) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_type_plus :
                                  'class_type_plus Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type :
                                               'class_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (ct : 'class_type_plus) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (test_ctyp_minusgreater :
                                               'test_ctyp_minusgreater Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_plus) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CtFun (_loc, t, ct)) :
                                                 'class_type_plus) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | OPTLABEL (_) -> (true)
                                            | _ -> (false) ), "OPTLABEL _"))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_plus) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun (i :
                                               Gram.Token.t) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (let i =
                                                 (Gram.Token.extract_string
                                                   i) in
                                                (Ast.CtFun
                                                  (_loc, (
                                                   (Ast.TyOlb (_loc, i, t))
                                                   ), ct)) :
                                                 'class_type_plus) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_plus) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (i :
                                                'a_LIDENT) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.CtFun
                                                    (_loc, (
                                                     (Ast.TyOlb (_loc, i, t))
                                                     ), ct)) :
                                                   'class_type_plus) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (lident_colon :
                                               'lident_colon Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_plus) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun (i :
                                               'lident_colon) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CtFun
                                                  (_loc, (
                                                   (Ast.TyLab (_loc, i, t))
                                                   ), ct)) :
                                                 'class_type_plus) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_type_longident_and_param :
                                  'class_type_longident_and_param Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_longident :
                                               'class_type_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'class_type_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.CtCon
                                               (_loc, Ast.ViNil , i, (
                                                (Ast.TyNil (_loc)) ))) :
                                              'class_type_longident_and_param)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp :
                                               'comma_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_longident :
                                               'class_type_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'class_type_longident) ->
                                           fun _ ->
                                            fun (t :
                                              'comma_ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CtCon
                                                  (_loc, Ast.ViNil , i, t)) :
                                                 'class_type_longident_and_param)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_longident_and_param :
                                  'class_longident_and_param Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_longident :
                                               'class_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ci :
                                            'class_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.CeCon
                                               (_loc, Ast.ViNil , ci, (
                                                (Ast.TyNil (_loc)) ))) :
                                              'class_longident_and_param) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp :
                                               'comma_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_longident :
                                               'class_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ci :
                                            'class_longident) ->
                                           fun _ ->
                                            fun (t :
                                              'comma_ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CeCon
                                                  (_loc, Ast.ViNil , ci, t)) :
                                                 'class_longident_and_param)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_name_and_param :
                                  'class_name_and_param Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                            ((i, ( (Ast.TyNil (_loc)) )) :
                                              'class_name_and_param) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_type_parameter :
                                               'comma_type_parameter Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("]"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun _ ->
                                            fun (x :
                                              'comma_type_parameter) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((i, x) :
                                                 'class_name_and_param) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (ctyp : 'ctyp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("as")) ); (
                                         (Gram.Skeyword ("'")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_ident) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (t1 :
                                               'ctyp) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyAli
                                                  (_loc, t1, (
                                                   (Ast.TyQuo (_loc, i)) ))) :
                                                 'ctyp) )) ))] ));
                                     (( (Some ("arrow")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ), (
                                      [((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (i :
                                                'a_LIDENT) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.TyArr
                                                    (_loc, (
                                                     (Ast.TyOlb (_loc, i, t1))
                                                     ), t2)) : 'ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (i :
                                               'a_OPTLABEL) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyArr
                                                  (_loc, (
                                                   (Ast.TyOlb (_loc, i, t1))
                                                   ), t2)) : 'ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules ctyp (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (a_LIDENT :
                                                      'a_LIDENT Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Skeyword (":")) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (i :
                                                    'a_LIDENT) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (i : 'e__3) )) ))] ))) );
                                         (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "star")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (i :
                                               'e__3) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyArr
                                                  (_loc, (
                                                   (Ast.TyLab (_loc, i, t1))
                                                   ), t2)) : 'ctyp) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyArr (_loc, t1, t2)) :
                                                'ctyp) )) ))] ));
                                     (( (Some ("star")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("*"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (star_ctyp :
                                               'star_ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (tl :
                                            'star_ctyp) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyTup
                                                 (_loc, (
                                                  (Ast.TySta (_loc, t, tl))
                                                  ))) : 'ctyp) )) ))] ));
                                     (( (Some ("ctyp1")) ), None , (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun (t1 :
                                             'ctyp) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyApp (_loc, t2, t1)) :
                                               'ctyp) )) ))] ));
                                     (( (Some ("ctyp2")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("("))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t2 :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (t1 :
                                               'ctyp) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (let t =
                                                 (Ast.TyApp (_loc, t1, t2)) in
                                                (try
                                                  (Ast.TyId
                                                    (_loc, (
                                                     (Ast.ident_of_ctyp t) )))
                                                 with
                                                 Invalid_argument (s) ->
                                                  (raise ( (Stream.Error (s))
                                                    ))) : 'ctyp) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((try
                                                 (Ast.TyId
                                                   (_loc, (
                                                    (Ast.IdAcc
                                                      (_loc, (
                                                       (Ast.ident_of_ctyp t1)
                                                       ), (
                                                       (Ast.ident_of_ctyp t2)
                                                       ))) )))
                                                with
                                                Invalid_argument (s) ->
                                                 (raise ( (Stream.Error (s))
                                                   ))) : 'ctyp) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (package_type :
                                               'package_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'package_type) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyPkg (_loc, p)) :
                                                 'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[<")) ); (
                                         (Gram.Sopt ((Gram.Skeyword ("|"))))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (row_field :
                                               'row_field Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (">")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (name_tags :
                                               'name_tags Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (ntl :
                                             'name_tags) ->
                                            fun _ ->
                                             fun (rfl :
                                               'row_field) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.TyVrnInfSup
                                                    (_loc, rfl, ntl)) :
                                                   'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[<")) ); (
                                         (Gram.Sopt ((Gram.Skeyword ("|"))))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (row_field :
                                               'row_field Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (rfl :
                                             'row_field) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyVrnInf (_loc, rfl)) :
                                                 'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword (">")) ); (
                                         (Gram.Sopt ((Gram.Skeyword ("|"))))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (row_field :
                                               'row_field Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (rfl :
                                             'row_field) ->
                                            fun _ ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyVrnSup (_loc, rfl)) :
                                                  'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword (">")) ); (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyVrnSup
                                                 (_loc, ( (Ast.TyNil (_loc))
                                                  ))) : 'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Sopt ((Gram.Skeyword ("|"))))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (row_field :
                                               'row_field Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (rfl :
                                             'row_field) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyVrnEq (_loc, rfl)) :
                                                 'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("<")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_meth_list :
                                               'opt_meth_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (">"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'opt_meth_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (t : 'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("#")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_longident :
                                               'class_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'class_longident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyCls (_loc, i)) : 'ctyp)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (t : 'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp_app :
                                               'comma_ctyp_app Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (")"))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "ctyp2")) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (mk :
                                              'comma_ctyp_app) ->
                                             fun _ ->
                                              fun (t :
                                                'ctyp) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((mk (
                                                    (Ast.TyApp (_loc, i, t))
                                                    )) : 'ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | QUOTATION (_) -> (true)
                                            | _ -> (false) ),
                                            "QUOTATION (_)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | QUOTATION (x) ->
                                                ((Quotation.expand _loc x
                                                   Quotation.DynAst.ctyp_tag) :
                                                  'ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("id", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"id\", _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("id" as n), s) ->
                                                ((Ast.TyId
                                                   (_loc, (
                                                    (Ast.IdAnt
                                                      (_loc, (
                                                       (mk_anti ~c:"ident" n
                                                         s) ))) ))) : 'ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("tup", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"tup\", _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("tup" as n), s) ->
                                                ((Ast.TyTup
                                                   (_loc, (
                                                    (Ast.TyAnt
                                                      (_loc, (
                                                       (mk_anti ~c:"ctyp" n
                                                         s) ))) ))) : 'ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "typ") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"typ\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "typ") | "anti") as
                                                  n), s) ->
                                                ((Ast.TyAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"ctyp" n s)
                                                    ))) : 'ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_UIDENT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyId
                                               (_loc, ( (Ast.IdUid (_loc, i))
                                                ))) : 'ctyp) )) ));
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
                                            ((Ast.TyId
                                               (_loc, ( (Ast.IdLid (_loc, i))
                                                ))) : 'ctyp) )) ));
                                       (( [( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyAny (_loc)) : 'ctyp) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("'")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyQuo (_loc, i)) : 'ctyp)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (meth_list : 'meth_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (meth_decl :
                                               'meth_decl Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (m :
                                            'meth_decl) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((m, Ast.RvNil ) : 'meth_list) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (comma_ctyp_app :
                                  'comma_ctyp_app Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (fun acc ->
                                              (Ast.TyApp (_loc, acc, t)) :
                                              'comma_ctyp_app) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (",")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'comma_ctyp_app) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (fun acc ->
                                                (t2 (
                                                  (Ast.TyApp (_loc, acc, t1))
                                                  )) : 'comma_ctyp_app) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (star_ctyp : 'star_ctyp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "ctyp1")) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'star_ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "ctyp1")) ); (
                                         (Gram.Skeyword ("*")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'star_ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TySta (_loc, t1, t2)) :
                                                'star_ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("list", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"list\", _)")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("list" as n), s) ->
                                                ((Ast.TyAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"ctyp*" n s)
                                                    ))) : 'star_ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "typ"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"typ\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "typ") as n), s) ->
                                                ((Ast.TyAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"ctyp" n s)
                                                    ))) : 'star_ctyp)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (constructor_declarations :
                                  'constructor_declarations Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ret :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (s :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match
                                                  (Ast.list_of_ctyp ret [] ) with
                                                | (c :: []) ->
                                                   (Ast.TyCol
                                                     (_loc, (
                                                      (Ast.TyId
                                                        (_loc, (
                                                         (Ast.IdUid (_loc, s))
                                                         ))) ), c))
                                                | _ ->
                                                   (raise (
                                                     (Stream.Error
                                                       ("invalid generalized constructor type"))
                                                     ))) :
                                                'constructor_declarations) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("->"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ret :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t :
                                              'constructor_arg_list) ->
                                             fun _ ->
                                              fun (s :
                                                'a_UIDENT) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyCol
                                                   (_loc, (
                                                    (Ast.TyId
                                                      (_loc, (
                                                       (Ast.IdUid (_loc, s))
                                                       ))) ), (
                                                    (Ast.TyArr (_loc, t, ret))
                                                    ))) :
                                                  'constructor_declarations)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
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
                                       (( [( (Gram.Skeyword (";;")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'semi) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend ( (ipatt : 'ipatt Gram.Entry.t) )
                                (
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
                                            (p : 'ipatt) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (type_longident_and_parameters :
                                  'type_longident_and_parameters Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_longident :
                                               'type_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'type_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyId (_loc, i)) :
                                              'type_longident_and_parameters)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_parameter :
                                               'type_parameter Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_longident :
                                               'type_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'type_longident) ->
                                           fun (tp :
                                             'type_parameter) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyApp
                                                (_loc, ( (Ast.TyId (_loc, i))
                                                 ), tp)) :
                                               'type_longident_and_parameters)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_parameters :
                                               'type_parameters Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (")"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_longident :
                                               'type_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'type_longident) ->
                                           fun _ ->
                                            fun (tpl :
                                              'type_parameters) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((tpl ( (Ast.TyId (_loc, i))
                                                  )) :
                                                 'type_longident_and_parameters)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_parameters :
                                  'type_parameters Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_parameter :
                                               'type_parameter Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'type_parameter) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (fun acc ->
                                              (Ast.TyApp (_loc, acc, t)) :
                                              'type_parameters) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_parameter :
                                               'type_parameter Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (","))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'type_parameters) ->
                                           fun _ ->
                                            fun (t1 :
                                              'type_parameter) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (fun acc ->
                                                (t2 (
                                                  (Ast.TyApp (_loc, acc, t1))
                                                  )) : 'type_parameters) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (optional_type_parameter :
                                  'optional_type_parameter Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("'")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyQuo (_loc, i)) :
                                               'optional_type_parameter) ))
                                        ));
                                       (( [( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyAny (_loc)) :
                                              'optional_type_parameter) )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Skeyword ("'")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_ident) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyQuM (_loc, i)) :
                                                'optional_type_parameter) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("-")) ); (
                                         (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyAnM (_loc)) :
                                               'optional_type_parameter) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("+")) ); (
                                         (Gram.Skeyword ("'")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_ident) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyQuP (_loc, i)) :
                                                'optional_type_parameter) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("+")) ); (
                                         (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyAnP (_loc)) :
                                               'optional_type_parameter) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | QUOTATION (_) -> (true)
                                            | _ -> (false) ),
                                            "QUOTATION (_)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | QUOTATION (x) ->
                                                ((Quotation.expand _loc x
                                                   Quotation.DynAst.ctyp_tag) :
                                                  'optional_type_parameter)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "typ") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"typ\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "typ") | "anti") as
                                                  n), s) ->
                                                ((Ast.TyAnt
                                                   (_loc, ( (mk_anti n s) ))) :
                                                  'optional_type_parameter)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_ident_and_parameters :
                                  'type_ident_and_parameters Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                            ((i, [] ) :
                                              'type_ident_and_parameters) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (optional_type_parameter :
                                               'optional_type_parameter Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun (t :
                                             'optional_type_parameter) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((i, ( [t] )) :
                                               'type_ident_and_parameters) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Slist1sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (optional_type_parameter :
                                                  'optional_type_parameter Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword (",")) ))) ); (
                                         (Gram.Skeyword (")")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun _ ->
                                            fun (tpl :
                                              'optional_type_parameter list) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((i, tpl) :
                                                 'type_ident_and_parameters)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_kind : 'type_kind Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration_list :
                                               'label_declaration_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'label_declaration_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyRec (_loc, t)) :
                                                'type_kind) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (ctyp : 'ctyp Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Skeyword ("=")) ); (
                                         (Gram.Sopt ((Gram.Skeyword ("|"))))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declarations :
                                               'constructor_declarations Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'constructor_declarations) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (t1 :
                                               'ctyp) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyMan
                                                  (_loc, t1, (
                                                   (Ast.TySum (_loc, t2)) ))) :
                                                 'type_kind) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (ctyp : 'ctyp Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Skeyword ("=")) ); (
                                         (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration_list :
                                               'label_declaration_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t2 :
                                             'label_declaration_list) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (t1 :
                                                'ctyp) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyMan
                                                   (_loc, t1, (
                                                    (Ast.TyRec (_loc, t2)) ))) :
                                                  'type_kind) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (ctyp : 'ctyp Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Skeyword ("=")) ); (
                                         (Gram.Skeyword ("private")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (tk :
                                            'type_kind) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (t :
                                               'ctyp) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyMan
                                                  (_loc, t, (
                                                   (Ast.TyPrv (_loc, tk)) ))) :
                                                 'type_kind) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (ctyp : 'ctyp Gram.Entry.t)
                                                ))))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'type_kind) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules type_kind (
                                             [((
                                               [(
                                                (Gram.Sopt
                                                  ((Gram.Skeyword ("|")))) );
                                                (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (constructor_declarations :
                                                      'constructor_declarations Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (test_not_dot_nor_lparen :
                                                      'test_not_dot_nor_lparen Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (t :
                                                    'constructor_declarations) ->
                                                   fun (x :
                                                     Gram.Token.t option) ->
                                                    fun (_loc :
                                                      Gram.Loc.t) ->
                                                     ((x, t) : 'e__4) )) ))]
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun ((x, t) :
                                            'e__4) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((match (x, t) with
                                              | (None, Ast.TyAnt (_)) -> t
                                              | _ -> (Ast.TySum (_loc, t))) :
                                              'type_kind) )) ));
                                       ((
                                        [( (Gram.Skeyword ("private")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (tk :
                                            'type_kind) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyPrv (_loc, tk)) :
                                               'type_kind) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (ctyp_quot : 'ctyp_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration_list :
                                               'label_declaration_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'label_declaration_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyRec (_loc, t)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'ctyp_quot) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyMan (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [( (Gram.Skeyword ("|")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declarations :
                                               'constructor_declarations Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'constructor_declarations) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TySum (_loc, t)) :
                                               'ctyp_quot) )) ));
                                       ((
                                        [( (Gram.Skeyword ("private")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp_quot) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyPrv (_loc, t)) :
                                               'ctyp_quot) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (module_expr : 'module_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Level ("apply"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("("))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (j :
                                             'module_expr) ->
                                            fun _ ->
                                             fun (i :
                                               'module_expr) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.MeApp (_loc, i, j)) :
                                                 'module_expr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (ident_quot : 'ident_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Level ("apply"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("("))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (j :
                                             'ident_quot) ->
                                            fun _ ->
                                             fun (i :
                                               'ident_quot) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.IdApp (_loc, i, j)) :
                                                 'ident_quot) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_longident_with_app :
                                  'module_longident_with_app Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Level ("apply"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("("))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (j :
                                             'module_longident_with_app) ->
                                            fun _ ->
                                             fun (i :
                                               'module_longident_with_app) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.IdApp (_loc, i, j)) :
                                                 'module_longident_with_app)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_longident :
                                  'type_longident Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some ((FanSig.Grammar.Level ("apply"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("("))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (j :
                                             'type_longident) ->
                                            fun _ ->
                                             fun (i :
                                               'type_longident) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.IdApp (_loc, i, j)) :
                                                 'type_longident) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (constructor_arg_list :
                                  'constructor_arg_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "ctyp1")) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'constructor_arg_list) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("*"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (t1 :
                                              'constructor_arg_list) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAnd (_loc, t1, t2)) :
                                                'constructor_arg_list) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (value_let : 'value_let Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("let")) )] ), (
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
                              (
                              (Gram.extend (
                                (label_declaration :
                                  'label_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("mutable")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (s :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyCol
                                                  (_loc, (
                                                   (Ast.TyId
                                                     (_loc, (
                                                      (Ast.IdLid (_loc, s))
                                                      ))) ), (
                                                   (Ast.TyMut (_loc, t)) ))) :
                                                 'label_declaration) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (s :
                                              'a_LIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyCol
                                                 (_loc, (
                                                  (Ast.TyId
                                                    (_loc, (
                                                     (Ast.IdLid (_loc, s)) )))
                                                  ), t)) :
                                                'label_declaration) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | QUOTATION (_) -> (true)
                                            | _ -> (false) ),
                                            "QUOTATION (_)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | QUOTATION (x) ->
                                                ((Quotation.expand _loc x
                                                   Quotation.DynAst.ctyp_tag) :
                                                  'label_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "typ"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"typ\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "typ") as n), s) ->
                                                ((Ast.TyAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"ctyp" n s)
                                                    ))) : 'label_declaration)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (poly_type : 'poly_type Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (ctyp : 'ctyp Gram.Entry.t)
                                                ))))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'poly_type) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules poly_type (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (typevars :
                                                      'typevars Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Skeyword (".")) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (t :
                                                    'typevars) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (t : 'e__5) )) ))] ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun (t1 :
                                             'e__5) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyPol (_loc, t1, t2)) :
                                               'poly_type) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (labeled_ipatt : 'labeled_ipatt Gram.Entry.t)
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
                                              (patt : 'patt Gram.Entry.t) ))
                                            ), "simple")) )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p : 'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (i :
                                               'a_LIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaOlb
                                                    (_loc, "", (
                                                     (Ast.PaTyc
                                                       (_loc, (
                                                        (Ast.PaId
                                                          (_loc, (
                                                           (Ast.IdLid
                                                             (_loc, i)) )))
                                                        ), t)) ))) :
                                                   'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (i :
                                             'a_LIDENT) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaOlb
                                                  (_loc, i, (
                                                   (Ast.PaNil (_loc)) ))) :
                                                 'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (t :
                                               'ctyp) ->
                                              fun _ ->
                                               fun (i :
                                                 'a_LIDENT) ->
                                                fun _ ->
                                                 fun _ ->
                                                  fun (_loc :
                                                    Gram.Loc.t) ->
                                                   ((Ast.PaOlbi
                                                      (_loc, "", (
                                                       (Ast.PaTyc
                                                         (_loc, (
                                                          (Ast.PaId
                                                            (_loc, (
                                                             (Ast.IdLid
                                                               (_loc, i)) )))
                                                          ), t)) ), e)) :
                                                     'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (i :
                                               'a_LIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaOlbi
                                                    (_loc, "", (
                                                     (Ast.PaId
                                                       (_loc, (
                                                        (Ast.IdLid (_loc, i))
                                                        ))) ), e)) :
                                                   'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaOlb
                                                (_loc, i, (
                                                 (Ast.PaNil (_loc)) ))) :
                                               'labeled_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (t :
                                               'ctyp) ->
                                              fun _ ->
                                               fun (p :
                                                 'patt) ->
                                                fun _ ->
                                                 fun (i :
                                                   'a_OPTLABEL) ->
                                                  fun (_loc :
                                                    Gram.Loc.t) ->
                                                   ((Ast.PaOlbi
                                                      (_loc, i, (
                                                       (Ast.PaTyc
                                                         (_loc, p, t)) ), e)) :
                                                     'labeled_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (p :
                                               'patt) ->
                                              fun _ ->
                                               fun (i :
                                                 'a_OPTLABEL) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaOlb
                                                    (_loc, i, (
                                                     (Ast.PaTyc (_loc, p, t))
                                                     ))) : 'labeled_ipatt) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (p :
                                               'patt) ->
                                              fun _ ->
                                               fun (i :
                                                 'a_OPTLABEL) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaOlbi (_loc, i, p, e)) :
                                                   'labeled_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'patt) ->
                                            fun _ ->
                                             fun (i :
                                               'a_OPTLABEL) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaOlb (_loc, i, p)) :
                                                 'labeled_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (i :
                                             'a_OPTLABEL) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaOlb
                                                (_loc, i, (
                                                 (Ast.PaAny (_loc)) ))) :
                                               'labeled_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'a_LIDENT) ->
                                           fun (i :
                                             'a_OPTLABEL) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaOlb
                                                (_loc, i, (
                                                 (Ast.PaId
                                                   (_loc, (
                                                    (Ast.IdLid (_loc, j)) )))
                                                 ))) : 'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (i :
                                               'a_LIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaLab
                                                    (_loc, i, (
                                                     (Ast.PaTyc
                                                       (_loc, (
                                                        (Ast.PaId
                                                          (_loc, (
                                                           (Ast.IdLid
                                                             (_loc, i)) )))
                                                        ), t)) ))) :
                                                   'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (i :
                                             'a_LIDENT) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaLab
                                                  (_loc, i, (
                                                   (Ast.PaNil (_loc)) ))) :
                                                 'labeled_ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_LIDENT) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaLab
                                                (_loc, i, (
                                                 (Ast.PaNil (_loc)) ))) :
                                               'labeled_ipatt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LABEL :
                                               'a_LABEL Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (patt : 'patt Gram.Entry.t) ))
                                            ), "simple")) )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun (i :
                                             'a_LABEL) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaLab (_loc, i, p)) :
                                               'labeled_ipatt) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_expr : 'label_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_longident :
                                               'label_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
                                         ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (i :
                                              'label_longident) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.RbEq (_loc, i, e)) :
                                                'label_expr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_UIDENT : 'a_UIDENT Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | UIDENT (_) -> (true)
                                            | _ -> (false) ), "UIDENT (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | UIDENT (s) -> (s : 'a_UIDENT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | UIDENT ("False") -> (true)
                                            | _ -> (false) ),
                                            "UIDENT (\"False\")")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | UIDENT ("False") ->
                                                (" False" : 'a_UIDENT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | UIDENT ("True") -> (true)
                                            | _ -> (false) ),
                                            "UIDENT (\"True\")")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | UIDENT ("True") ->
                                                (" True" : 'a_UIDENT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "uid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"uid\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "uid") as n), s) ->
                                                ((mk_anti n s) : 'a_UIDENT)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (Gram.extend (
                                (top_phrase : 'top_phrase Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | EOI -> (true)
                                            | _ -> (false) ), "EOI")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | EOI -> ((None) : 'top_phrase)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Slist1
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (str_item :
                                                  'str_item Gram.Entry.t) )))))
                                         ); ( (Gram.Skeyword (";;")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (l :
                                             'str_item list) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Some (Ast.stSem_of_list l)) :
                                               'top_phrase) )) ));
                                       ((
                                        [( (Gram.Skeyword ("#")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_expr :
                                               'opt_expr Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (";;")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (dp :
                                             'opt_expr) ->
                                            fun (n :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Some
                                                  ((Ast.StDir (_loc, n, dp)))) :
                                                 'top_phrase) )) ))] ))] )))
                                  () ) ))

                      let _ = (Gram.delete_rule module_longident_with_app (
                                [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                                 (Gram.Skeyword (")")) )] ))

                      let _ = (Gram.delete_rule type_longident (
                                [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                                 (Gram.Skeyword (")")) )] ))

                      let _ = (Gram.delete_rule ident_quot (
                                [( (Gram.Skeyword ("(")) ); Gram.Sself ; (
                                 (Gram.Skeyword (")")) )] ))

                      let _ = (Gram.delete_rule module_longident_with_app (
                                [Gram.Sself ; Gram.Sself ] ))

                      let _ = (Gram.delete_rule type_longident (
                                [Gram.Sself ; Gram.Sself ] ))

                      let _ = (Gram.delete_rule ident_quot (
                                [Gram.Sself ; Gram.Sself ] ))

                      let _ = (Gram.delete_rule module_expr (
                                [Gram.Sself ; Gram.Sself ] ))

                     end
