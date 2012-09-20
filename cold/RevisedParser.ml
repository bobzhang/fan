open FanSig

module Id =
              struct
               let name = "Camlp4OCamlRevisedParser"

               let version = Sys.ocaml_version

              end

module Make =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      let _ = (FanConfig.constructors_arity := false )

                      let help_sequences =
                       fun ()
                         ->
                        (
                        (Printf.eprintf
                          "New syntax:\n    (e1; e2; ... ; en) OR begin e1; e2; ... ; en end\n    while e do e1; e2; ... ; en done\n    for v = v1 to/downto v2 do e1; e2; ... ; en done\nOld syntax (still supported):\n    do {e1; e2; ... ; en}\n    while e do {e1; e2; ... ; en}\n    for v = v1 to/downto v2 do {e1; e2; ... ; en}\nVery old (no more supported) syntax:\n    do e1; e2; ... ; en-1; return en\n    while e do e1; e2; ... ; en; done\n    for v = v1 to/downto v2 do e1; e2; ... ; en; done\n")
                        );
                        (
                        (flush stderr)
                        );
                        (exit 1)

                      let _ = (Camlp4.Options.add "-help_seq" (
                                (Arg.Unit (help_sequences)) )
                                "Print explanations about new sequences and exit.")

                      let _ = (Gram.Entry.clear a_CHAR)

                      let _ = (Gram.Entry.clear a_FLOAT)

                      let _ = (Gram.Entry.clear a_INT)

                      let _ = (Gram.Entry.clear a_INT32)

                      let _ = (Gram.Entry.clear a_INT64)

                      let _ = (Gram.Entry.clear a_LABEL)

                      let _ = (Gram.Entry.clear a_LIDENT)

                      let _ = (Gram.Entry.clear a_NATIVEINT)

                      let _ = (Gram.Entry.clear a_OPTLABEL)

                      let _ = (Gram.Entry.clear a_STRING)

                      let _ = (Gram.Entry.clear a_UIDENT)

                      let _ = (Gram.Entry.clear a_ident)

                      let _ = (Gram.Entry.clear amp_ctyp)

                      let _ = (Gram.Entry.clear and_ctyp)

                      let _ = (Gram.Entry.clear match_case)

                      let _ = (Gram.Entry.clear match_case0)

                      let _ = (Gram.Entry.clear match_case_quot)

                      let _ = (Gram.Entry.clear binding)

                      let _ = (Gram.Entry.clear binding_quot)

                      let _ = (Gram.Entry.clear rec_binding_quot)

                      let _ = (Gram.Entry.clear class_declaration)

                      let _ = (Gram.Entry.clear class_description)

                      let _ = (Gram.Entry.clear class_expr)

                      let _ = (Gram.Entry.clear class_expr_quot)

                      let _ = (Gram.Entry.clear class_fun_binding)

                      let _ = (Gram.Entry.clear class_fun_def)

                      let _ = (Gram.Entry.clear class_info_for_class_expr)

                      let _ = (Gram.Entry.clear class_info_for_class_type)

                      let _ = (Gram.Entry.clear class_longident)

                      let _ = (Gram.Entry.clear class_longident_and_param)

                      let _ = (Gram.Entry.clear class_name_and_param)

                      let _ = (Gram.Entry.clear class_sig_item)

                      let _ = (Gram.Entry.clear class_sig_item_quot)

                      let _ = (Gram.Entry.clear class_signature)

                      let _ = (Gram.Entry.clear class_str_item)

                      let _ = (Gram.Entry.clear class_str_item_quot)

                      let _ = (Gram.Entry.clear class_structure)

                      let _ = (Gram.Entry.clear class_type)

                      let _ = (Gram.Entry.clear class_type_declaration)

                      let _ = (Gram.Entry.clear class_type_longident)

                      let _ = (Gram.Entry.clear
                                class_type_longident_and_param)

                      let _ = (Gram.Entry.clear class_type_plus)

                      let _ = (Gram.Entry.clear class_type_quot)

                      let _ = (Gram.Entry.clear comma_ctyp)

                      let _ = (Gram.Entry.clear comma_expr)

                      let _ = (Gram.Entry.clear comma_ipatt)

                      let _ = (Gram.Entry.clear comma_patt)

                      let _ = (Gram.Entry.clear comma_type_parameter)

                      let _ = (Gram.Entry.clear constrain)

                      let _ = (Gram.Entry.clear constructor_arg_list)

                      let _ = (Gram.Entry.clear constructor_declaration)

                      let _ = (Gram.Entry.clear constructor_declarations)

                      let _ = (Gram.Entry.clear ctyp)

                      let _ = (Gram.Entry.clear ctyp_quot)

                      let _ = (Gram.Entry.clear cvalue_binding)

                      let _ = (Gram.Entry.clear direction_flag)

                      let _ = (Gram.Entry.clear dummy)

                      let _ = (Gram.Entry.clear eq_expr)

                      let _ = (Gram.Entry.clear expr)

                      let _ = (Gram.Entry.clear expr_eoi)

                      let _ = (Gram.Entry.clear expr_quot)

                      let _ = (Gram.Entry.clear field_expr)

                      let _ = (Gram.Entry.clear field_expr_list)

                      let _ = (Gram.Entry.clear fun_binding)

                      let _ = (Gram.Entry.clear fun_def)

                      let _ = (Gram.Entry.clear ident)

                      let _ = (Gram.Entry.clear ident_quot)

                      let _ = (Gram.Entry.clear implem)

                      let _ = (Gram.Entry.clear interf)

                      let _ = (Gram.Entry.clear ipatt)

                      let _ = (Gram.Entry.clear ipatt_tcon)

                      let _ = (Gram.Entry.clear label)

                      let _ = (Gram.Entry.clear label_declaration)

                      let _ = (Gram.Entry.clear label_declaration_list)

                      let _ = (Gram.Entry.clear label_expr_list)

                      let _ = (Gram.Entry.clear label_expr)

                      let _ = (Gram.Entry.clear label_ipatt)

                      let _ = (Gram.Entry.clear label_ipatt_list)

                      let _ = (Gram.Entry.clear label_longident)

                      let _ = (Gram.Entry.clear label_patt)

                      let _ = (Gram.Entry.clear label_patt_list)

                      let _ = (Gram.Entry.clear labeled_ipatt)

                      let _ = (Gram.Entry.clear let_binding)

                      let _ = (Gram.Entry.clear meth_list)

                      let _ = (Gram.Entry.clear meth_decl)

                      let _ = (Gram.Entry.clear module_binding)

                      let _ = (Gram.Entry.clear module_binding0)

                      let _ = (Gram.Entry.clear module_binding_quot)

                      let _ = (Gram.Entry.clear module_declaration)

                      let _ = (Gram.Entry.clear module_expr)

                      let _ = (Gram.Entry.clear module_expr_quot)

                      let _ = (Gram.Entry.clear module_longident)

                      let _ = (Gram.Entry.clear module_longident_with_app)

                      let _ = (Gram.Entry.clear module_rec_declaration)

                      let _ = (Gram.Entry.clear module_type)

                      let _ = (Gram.Entry.clear module_type_quot)

                      let _ = (Gram.Entry.clear more_ctyp)

                      let _ = (Gram.Entry.clear name_tags)

                      let _ = (Gram.Entry.clear opt_as_lident)

                      let _ = (Gram.Entry.clear opt_class_self_patt)

                      let _ = (Gram.Entry.clear opt_class_self_type)

                      let _ = (Gram.Entry.clear opt_comma_ctyp)

                      let _ = (Gram.Entry.clear opt_dot_dot)

                      let _ = (Gram.Entry.clear opt_eq_ctyp)

                      let _ = (Gram.Entry.clear opt_expr)

                      let _ = (Gram.Entry.clear opt_meth_list)

                      let _ = (Gram.Entry.clear opt_mutable)

                      let _ = (Gram.Entry.clear opt_polyt)

                      let _ = (Gram.Entry.clear opt_private)

                      let _ = (Gram.Entry.clear opt_rec)

                      let _ = (Gram.Entry.clear opt_virtual)

                      let _ = (Gram.Entry.clear opt_when_expr)

                      let _ = (Gram.Entry.clear patt)

                      let _ = (Gram.Entry.clear patt_as_patt_opt)

                      let _ = (Gram.Entry.clear patt_eoi)

                      let _ = (Gram.Entry.clear patt_quot)

                      let _ = (Gram.Entry.clear patt_tcon)

                      let _ = (Gram.Entry.clear phrase)

                      let _ = (Gram.Entry.clear poly_type)

                      let _ = (Gram.Entry.clear row_field)

                      let _ = (Gram.Entry.clear sem_expr)

                      let _ = (Gram.Entry.clear sem_expr_for_list)

                      let _ = (Gram.Entry.clear sem_patt)

                      let _ = (Gram.Entry.clear sem_patt_for_list)

                      let _ = (Gram.Entry.clear semi)

                      let _ = (Gram.Entry.clear sequence)

                      let _ = (Gram.Entry.clear sig_item)

                      let _ = (Gram.Entry.clear sig_item_quot)

                      let _ = (Gram.Entry.clear sig_items)

                      let _ = (Gram.Entry.clear star_ctyp)

                      let _ = (Gram.Entry.clear str_item)

                      let _ = (Gram.Entry.clear str_item_quot)

                      let _ = (Gram.Entry.clear str_items)

                      let _ = (Gram.Entry.clear top_phrase)

                      let _ = (Gram.Entry.clear type_constraint)

                      let _ = (Gram.Entry.clear type_declaration)

                      let _ = (Gram.Entry.clear type_ident_and_parameters)

                      let _ = (Gram.Entry.clear type_kind)

                      let _ = (Gram.Entry.clear type_longident)

                      let _ = (Gram.Entry.clear
                                type_longident_and_parameters)

                      let _ = (Gram.Entry.clear type_parameter)

                      let _ = (Gram.Entry.clear type_parameters)

                      let _ = (Gram.Entry.clear typevars)

                      let _ = (Gram.Entry.clear use_file)

                      let _ = (Gram.Entry.clear val_longident)

                      let _ = (Gram.Entry.clear value_let)

                      let _ = (Gram.Entry.clear value_val)

                      let _ = (Gram.Entry.clear with_constr)

                      let _ = (Gram.Entry.clear with_constr_quot)

                      let neg_string =
                       fun n ->
                        let len = (String.length n) in
                        if (( (len > 0) ) && ( (( (String.get n 0) ) = '-')
                             )) then
                         (
                         (String.sub n 1 ( (len - 1) ))
                         )
                        else ("-" ^ n)

                      let mkumin =
                       fun _loc ->
                        fun f ->
                         fun arg ->
                          (match arg with
                           | Ast.ExInt (_, n) ->
                              (Ast.ExInt (_loc, ( (neg_string n) )))
                           | Ast.ExInt32 (_, n) ->
                              (Ast.ExInt32 (_loc, ( (neg_string n) )))
                           | Ast.ExInt64 (_, n) ->
                              (Ast.ExInt64 (_loc, ( (neg_string n) )))
                           | Ast.ExNativeInt (_, n) ->
                              (Ast.ExNativeInt (_loc, ( (neg_string n) )))
                           | Ast.ExFlo (_, n) ->
                              (Ast.ExFlo (_loc, ( (neg_string n) )))
                           | _ ->
                              (Ast.ExApp
                                (_loc, (
                                 (Ast.ExId
                                   (_loc, ( (Ast.IdLid (_loc, ( ("~" ^ f) )))
                                    ))) ), arg)))

                      let mklistexp =
                       fun _loc ->
                        fun last ->
                         let rec loop =
                          fun top ->
                           function
                           | [] ->
                              (match last with
                               | Some (e) -> e
                               | None ->
                                  (Ast.ExId
                                    (_loc, ( (Ast.IdUid (_loc, "[]")) ))))
                           | (e1 :: el) ->
                              let _loc =
                               if top then _loc
                               else (Loc.merge ( (Ast.loc_of_expr e1) ) _loc) in
                              (Ast.ExApp
                                (_loc, (
                                 (Ast.ExApp
                                   (_loc, (
                                    (Ast.ExId
                                      (_loc, ( (Ast.IdUid (_loc, "::")) )))
                                    ), e1)) ), ( (loop false  el) ))) in
                         (loop true )

                      let mkassert =
                       fun _loc ->
                        function
                        | Ast.ExId (_, Ast.IdUid (_, "False")) ->
                           (Ast.ExAsf (_loc))
                        | e -> (Ast.ExAsr (_loc, e))

                      let append_eLem = fun el -> fun e -> (el @ ( [e] ))

                      let mk_anti =
                       fun ?(c = "") ->
                        fun n ->
                         fun s -> ("\\$" ^ ( (n ^ ( (c ^ ( (":" ^ s) )) )) ))

                      let mksequence =
                       fun _loc ->
                        function
                        | ((Ast.ExSem (_, _, _) | Ast.ExAnt (_, _)) as e) ->
                           (Ast.ExSeq (_loc, e))
                        | e -> e

                      let mksequence' =
                       fun _loc ->
                        function
                        | (Ast.ExSem (_, _, _) as e) -> (Ast.ExSeq (_loc, e))
                        | e -> e

                      let rec lid_of_ident =
                       function
                       | Ast.IdAcc (_, _, i) -> (lid_of_ident i)
                       | Ast.IdLid (_, lid) -> lid
                       | _ -> assert false

                      let module_type_app =
                       fun mt1 ->
                        fun mt2 ->
                         (match (mt1, mt2) with
                          | (Ast.MtId (_loc, i1), Ast.MtId (_, i2)) ->
                             (Ast.MtId (_loc, ( (Ast.IdApp (_loc, i1, i2)) )))
                          | _ -> (raise Stream.Failure ))

                      let module_type_acc =
                       fun mt1 ->
                        fun mt2 ->
                         (match (mt1, mt2) with
                          | (Ast.MtId (_loc, i1), Ast.MtId (_, i2)) ->
                             (Ast.MtId (_loc, ( (Ast.IdAcc (_loc, i1, i2)) )))
                          | _ -> (raise Stream.Failure ))

                      let bigarray_get =
                       fun _loc ->
                        fun arr ->
                         fun arg ->
                          let coords =
                           (match arg with
                            | (Ast.ExTup (_, Ast.ExCom (_, e1, e2))
                               | Ast.ExCom (_, e1, e2)) ->
                               (Ast.list_of_expr e1 (
                                 (Ast.list_of_expr e2 [] ) ))
                            | _ -> [arg]) in
                          (match coords with
                           | (c1 :: []) ->
                              (Ast.ExApp
                                (_loc, (
                                 (Ast.ExApp
                                   (_loc, (
                                    (Ast.ExId
                                      (_loc, (
                                       (Ast.IdAcc
                                         (_loc, (
                                          (Ast.IdUid (_loc, "Bigarray")) ), (
                                          (Ast.IdAcc
                                            (_loc, (
                                             (Ast.IdUid (_loc, "Array1")) ),
                                             ( (Ast.IdLid (_loc, "get")) )))
                                          ))) ))) ), arr)) ), c1))
                           | (c1 :: c2 :: []) ->
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
                                             (Ast.IdUid (_loc, "Bigarray"))
                                             ), (
                                             (Ast.IdAcc
                                               (_loc, (
                                                (Ast.IdUid (_loc, "Array2"))
                                                ), (
                                                (Ast.IdLid (_loc, "get")) )))
                                             ))) ))) ), arr)) ), c1)) ), c2))
                           | (c1 :: c2 :: c3 :: []) ->
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
                                                (Ast.IdUid (_loc, "Bigarray"))
                                                ), (
                                                (Ast.IdAcc
                                                  (_loc, (
                                                   (Ast.IdUid
                                                     (_loc, "Array3")) ), (
                                                   (Ast.IdLid (_loc, "get"))
                                                   ))) ))) ))) ), arr)) ),
                                       c1)) ), c2)) ), c3))
                           | coords ->
                              (Ast.ExApp
                                (_loc, (
                                 (Ast.ExApp
                                   (_loc, (
                                    (Ast.ExId
                                      (_loc, (
                                       (Ast.IdAcc
                                         (_loc, (
                                          (Ast.IdUid (_loc, "Bigarray")) ), (
                                          (Ast.IdAcc
                                            (_loc, (
                                             (Ast.IdUid (_loc, "Genarray"))
                                             ), ( (Ast.IdLid (_loc, "get"))
                                             ))) ))) ))) ), arr)) ), (
                                 (Ast.ExArr
                                   (_loc, ( (Ast.exSem_of_list coords) ))) ))))

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

                      let stopped_at =
                       fun _loc -> (Some (Loc.move_line 1 _loc))

                      let rec generalized_type_of_type =
                       function
                       | Ast.TyArr (_, t1, t2) ->
                          let (tl, rt) = (generalized_type_of_type t2) in
                          (( ( t1 ) :: tl  ), rt)
                       | t -> ([] , t)

                      let symbolchar =
                       let list =
                        ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':';
                         '<'; '='; '>'; '?'; '@'; '^'; '|'; '~'; '\\'] in
                       let rec loop =
                        fun s ->
                         fun i ->
                          if (i == ( (String.length s) )) then true 
                          else if (List.mem ( (String.get s i) ) list) then
                                (
                                (loop s ( (i + 1) ))
                                )
                          else (false) in
                       loop

                      let setup_op_parser =
                       fun entry ->
                        fun p ->
                         (Gram.Entry.setup_parser entry (
                           fun (__strm :
                             _ Stream.t) ->
                            (match (Stream.peek __strm) with
                             | Some ((KEYWORD (x) | SYMBOL (x)), ti) when
                                (p x) ->
                                (
                                (Stream.junk __strm)
                                );
                                let _loc = (Gram.token_location ti) in
                                (Ast.ExId (_loc, ( (Ast.IdLid (_loc, x)) )))
                             | _ -> (raise Stream.Failure )) ))

                      let _ = let list = ['!'; '?'; '~'] in
                              let excl = ["!="; "??"] in
                              (setup_op_parser prefixop (
                                fun x ->
                                 (( (not ( (List.mem x excl) )) ) && (
                                   (( (( (String.length x) ) >= 2) ) && (
                                     (( (List.mem ( (String.get x 0) ) list)
                                       ) && ( (symbolchar x 1) )) )) )) ))

                      let _ = let list_ok =
                               ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!=";
                                "$"] in
                              let list_first_char_ok =
                               ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
                              let excl = ["<-"; "||"; "&&"] in
                              (setup_op_parser infixop0 (
                                fun x ->
                                 (( (List.mem x list_ok) ) || (
                                   (( (not ( (List.mem x excl) )) ) && (
                                     (( (( (String.length x) ) >= 2) ) && (
                                       ((
                                         (List.mem ( (String.get x 0) )
                                           list_first_char_ok) ) && (
                                         (symbolchar x 1) )) )) )) )) ))

                      let _ = let list = ['@'; '^'] in
                              (setup_op_parser infixop1 (
                                fun x ->
                                 (( (( (String.length x) ) >= 1) ) && (
                                   (( (List.mem ( (String.get x 0) ) list) )
                                     && ( (symbolchar x 1) )) )) ))

                      let _ = let list = ['+'; '-'] in
                              (setup_op_parser infixop2 (
                                fun x ->
                                 (( (x <> "->") ) && (
                                   (( (( (String.length x) ) >= 1) ) && (
                                     (( (List.mem ( (String.get x 0) ) list)
                                       ) && ( (symbolchar x 1) )) )) )) ))

                      let _ = let list = ['*'; '/'; '%'; '\\'] in
                              (setup_op_parser infixop3 (
                                fun x ->
                                 (( (( (String.length x) ) >= 1) ) && (
                                   (( (List.mem ( (String.get x 0) ) list) )
                                     && (
                                     ((
                                       (( (( (String.get x 0) ) <> '*') ) ||
                                         (
                                         (( (( (String.length x) ) < 2) ) ||
                                           ( (( (String.get x 1) ) <> '*') ))
                                         )) ) && ( (symbolchar x 1) )) )) ))
                                ))

                      let _ = (setup_op_parser infixop4 (
                                fun x ->
                                 (( (( (String.length x) ) >= 2) ) && (
                                   (( (( (String.get x 0) ) == '*') ) && (
                                     (( (( (String.get x 1) ) == '*') ) && (
                                       (symbolchar x 2) )) )) )) ))

                      let rec infix_kwds_filter =
                       fun (__strm :
                         _ Stream.t) ->
                        (match (Stream.peek __strm) with
                         | Some ((KEYWORD ("("), _) as tok) ->
                            (
                            (Stream.junk __strm)
                            );
                            let xs = __strm in
                            let (__strm : _ Stream.t) = xs in
                            (match (Stream.peek __strm) with
                             | Some
                                (KEYWORD
                                  (((((((("or" | "mod") | "land") | "lor")
                                       | "lxor") | "lsl") | "lsr") | "asr") as
                                   i), _loc) ->
                                (
                                (Stream.junk __strm)
                                );
                                (match (Stream.peek __strm) with
                                 | Some (KEYWORD (")"), _) ->
                                    (
                                    (Stream.junk __strm)
                                    );
                                    let xs = __strm in
                                    (Stream.lcons (
                                      fun _ -> (( (LIDENT (i)) ), _loc) ) (
                                      (Stream.slazy (
                                        fun _ -> (infix_kwds_filter xs) )) ))
                                 | _ -> (raise ( (Stream.Error ("")) )))
                             | _ ->
                                let xs = __strm in
                                (Stream.icons tok (
                                  (Stream.slazy (
                                    fun _ -> (infix_kwds_filter xs) )) )))
                         | Some (x) ->
                            (
                            (Stream.junk __strm)
                            );
                            let xs = __strm in
                            (Stream.icons x (
                              (Stream.slazy ( fun _ -> (infix_kwds_filter xs)
                                )) ))
                         | _ -> (raise Stream.Failure ))

                      let _ = (Token.Filter.define_filter (
                                (Gram.get_filter () ) ) (
                                fun f ->
                                 fun strm -> (infix_kwds_filter ( (f strm) ))
                                ))

                      let _ = (Gram.Entry.setup_parser sem_expr (
                                let symb1 =
                                 (Gram.parse_tokens_after_filter expr) in
                                let symb =
                                 fun (__strm :
                                   _ Stream.t) ->
                                  (match (Stream.peek __strm) with
                                   | Some (ANTIQUOT (("list" as n), s), ti) ->
                                      (
                                      (Stream.junk __strm)
                                      );
                                      let _loc = (Gram.token_location ti) in
                                      (Ast.ExAnt
                                        (_loc, ( (mk_anti ~c:"expr;" n s) )))
                                   | _ -> (symb1 __strm)) in
                                let rec kont =
                                 fun al ->
                                  fun (__strm :
                                    _ Stream.t) ->
                                   (match (Stream.peek __strm) with
                                    | Some (KEYWORD (";"), _) ->
                                       (
                                       (Stream.junk __strm)
                                       );
                                       let a =
                                        (try (symb __strm) with
                                         Stream.Failure ->
                                          (raise ( (Stream.Error ("")) ))) in
                                       let s = __strm in
                                       let _loc =
                                        (Loc.merge ( (Ast.loc_of_expr al) ) (
                                          (Ast.loc_of_expr a) )) in
                                       (kont ( (Ast.ExSem (_loc, al, a)) ) s)
                                    | _ -> al) in
                                fun (__strm :
                                  _ Stream.t) ->
                                 let a = (symb __strm) in (kont a __strm) ))

                      let _ = let _ = (a_CHAR : 'a_CHAR Gram.Entry.t)
                              and _ =
                               (override_flag_quot :
                                 'override_flag_quot Gram.Entry.t)
                              and _ =
                               (row_var_flag_quot :
                                 'row_var_flag_quot Gram.Entry.t)
                              and _ =
                               (virtual_flag_quot :
                                 'virtual_flag_quot Gram.Entry.t)
                              and _ =
                               (private_flag_quot :
                                 'private_flag_quot Gram.Entry.t)
                              and _ =
                               (mutable_flag_quot :
                                 'mutable_flag_quot Gram.Entry.t)
                              and _ =
                               (direction_flag_quot :
                                 'direction_flag_quot Gram.Entry.t)
                              and _ =
                               (rec_flag_quot : 'rec_flag_quot Gram.Entry.t)
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
                              and _ = (meth_decl : 'meth_decl Gram.Entry.t)
                              and _ = (meth_list : 'meth_list Gram.Entry.t)
                              and _ =
                               (let_binding : 'let_binding Gram.Entry.t)
                              and _ =
                               (labeled_ipatt : 'labeled_ipatt Gram.Entry.t)
                              and _ =
                               (label_patt_list :
                                 'label_patt_list Gram.Entry.t)
                              and _ = (label_patt : 'label_patt Gram.Entry.t)
                              and _ =
                               (label_longident :
                                 'label_longident Gram.Entry.t)
                              and _ =
                               (label_ipatt_list :
                                 'label_ipatt_list Gram.Entry.t)
                              and _ =
                               (label_ipatt : 'label_ipatt Gram.Entry.t)
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
                              and _ =
                               (field_expr_list :
                                 'field_expr_list Gram.Entry.t)
                              and _ = (field_expr : 'field_expr Gram.Entry.t)
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
                              and _ =
                               (rec_binding_quot :
                                 'rec_binding_quot Gram.Entry.t)
                              and _ = (a_LIDENT : 'a_LIDENT Gram.Entry.t)
                              and _ = (a_LABEL : 'a_LABEL Gram.Entry.t)
                              and _ = (a_INT64 : 'a_INT64 Gram.Entry.t)
                              and _ = (a_INT32 : 'a_INT32 Gram.Entry.t)
                              and _ = (a_INT : 'a_INT Gram.Entry.t)
                              and _ = (a_FLOAT : 'a_FLOAT Gram.Entry.t) in
                              let grammar_entry_create = Gram.Entry.mk in
                              let infixop5 =
                               ((grammar_entry_create "infixop5") :
                                 'infixop5 Gram.Entry.t)
                              and string_list =
                               ((grammar_entry_create "string_list") :
                                 'string_list Gram.Entry.t)
                              and opt_override =
                               ((grammar_entry_create "opt_override") :
                                 'opt_override Gram.Entry.t)
                              and unquoted_typevars =
                               ((grammar_entry_create "unquoted_typevars") :
                                 'unquoted_typevars Gram.Entry.t)
                              and value_val_opt_override =
                               ((grammar_entry_create
                                  "value_val_opt_override") :
                                 'value_val_opt_override Gram.Entry.t)
                              and method_opt_override =
                               ((grammar_entry_create "method_opt_override") :
                                 'method_opt_override Gram.Entry.t)
                              and module_longident_dot_lparen =
                               ((grammar_entry_create
                                  "module_longident_dot_lparen") :
                                 'module_longident_dot_lparen Gram.Entry.t)
                              and optional_type_parameter =
                               ((grammar_entry_create
                                  "optional_type_parameter") :
                                 'optional_type_parameter Gram.Entry.t)
                              and fun_def_cont_no_when =
                               ((grammar_entry_create "fun_def_cont_no_when") :
                                 'fun_def_cont_no_when Gram.Entry.t)
                              and fun_def_cont =
                               ((grammar_entry_create "fun_def_cont") :
                                 'fun_def_cont Gram.Entry.t)
                              and sequence' =
                               ((grammar_entry_create "sequence'") :
                                 'sequence' Gram.Entry.t)
                              and infixop6 =
                               ((grammar_entry_create "infixop6") :
                                 'infixop6 Gram.Entry.t) in
                              (
                              (Gram.extend (
                                (module_expr : 'module_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("top")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("struct")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (str_items :
                                               'str_items Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (st :
                                             'str_items) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MeStr (_loc, st)) :
                                                'module_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("functor")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (t :
                                               'module_type) ->
                                              fun _ ->
                                               fun (i :
                                                 'a_UIDENT) ->
                                                fun _ ->
                                                 fun _ ->
                                                  fun (_loc :
                                                    Gram.Loc.t) ->
                                                   ((Ast.MeFun
                                                      (_loc, i, t, me)) :
                                                     'module_expr) )) ))] ));
                                     (( (Some ("apply")) ), None , (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (me2 :
                                            'module_expr) ->
                                           fun (me1 :
                                             'module_expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.MeApp (_loc, me1, me2)) :
                                               'module_expr) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
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
                                             fun (e :
                                               'expr) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.MePkg
                                                    (_loc, (
                                                     (Ast.ExTyc
                                                       (_loc, e, (
                                                        (Ast.TyPkg (_loc, p))
                                                        ))) ))) :
                                                   'module_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.MePkg (_loc, e)) :
                                                 'module_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (me :
                                             'module_expr) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (me : 'module_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (":"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (mt :
                                             'module_type) ->
                                            fun _ ->
                                             fun (me :
                                               'module_expr) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.MeTyc (_loc, me, mt)) :
                                                  'module_expr) )) ));
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
                                            ((Ast.MeId (_loc, i)) :
                                              'module_expr) )) ));
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
                                                   Quotation.DynAst.module_expr_tag) :
                                                  'module_expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "mexp") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"mexp\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "mexp") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.MeAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"module_expr"
                                                      n s) ))) :
                                                  'module_expr)
                                             | _ -> assert false) )) ))] ))]
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
                                            ((Ast.StExp (_loc, e)) :
                                              'str_item) )) ));
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
                                                   Quotation.DynAst.str_item_tag) :
                                                  'str_item)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "stri") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"stri\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "stri") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.StAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"str_item" n
                                                      s) ))) : 'str_item)
                                             | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("class")) ); (
                                         (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_declaration :
                                               'class_type_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ctd :
                                            'class_type_declaration) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.StClt (_loc, ctd)) :
                                                'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("class")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_declaration :
                                               'class_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (cd :
                                            'class_declaration) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.StCls (_loc, cd)) :
                                               'str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_let :
                                               'value_let Gram.Entry.t) )))
                                         ); (
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
                                              ((Ast.StVal (_loc, r, bi)) :
                                                'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_declaration :
                                               'type_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (td :
                                            'type_declaration) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.StTyp (_loc, td)) :
                                               'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("open")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'module_longident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.StOpn (_loc, i)) :
                                               'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (i :
                                              'a_ident) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.StMty (_loc, i, mt)) :
                                                  'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Skeyword ("rec")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_binding :
                                               'module_binding Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (mb :
                                            'module_binding) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.StRecMod (_loc, mb)) :
                                                'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_binding0 :
                                               'module_binding0 Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (mb :
                                            'module_binding0) ->
                                           fun (i :
                                             'a_UIDENT) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.StMod (_loc, i, mb)) :
                                                'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("include")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.StInc (_loc, me)) :
                                               'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("external")) ); (
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
                                             (string_list :
                                               'string_list Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (sl :
                                            'string_list) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (i :
                                                'a_LIDENT) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.StExt (_loc, i, t, sl)) :
                                                   'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("exception")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declaration :
                                               'constructor_declaration Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
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
                                            fun (t :
                                              'constructor_declaration) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.StExc
                                                  (_loc, t, ( (Ast.OSome (i))
                                                   ))) : 'str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("exception")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declaration :
                                               'constructor_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'constructor_declaration) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.StExc
                                                (_loc, t, Ast.ONone )) :
                                               'str_item) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (module_binding0 :
                                  'module_binding0 Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (me : 'module_binding0) )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.MeTyc (_loc, me, mt)) :
                                                 'module_binding0) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (mb :
                                            'module_binding0) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun _ ->
                                              fun (m :
                                                'a_UIDENT) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.MeFun
                                                    (_loc, m, mt, mb)) :
                                                   'module_binding0) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_binding :
                                  'module_binding Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun _ ->
                                              fun (m :
                                                'a_UIDENT) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.MbColEq
                                                   (_loc, m, mt, me)) :
                                                  'module_binding) )) ));
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
                                                   Quotation.DynAst.module_binding_tag) :
                                                  'module_binding)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) ); (
                                         (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun _ ->
                                              fun (__camlp4_0 :
                                                Gram.Token.t) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (match __camlp4_0 with
                                                 | ANTIQUOT (("" as n), m) ->
                                                    ((Ast.MbColEq
                                                       (_loc, ( (mk_anti n m)
                                                        ), mt, me)) :
                                                      'module_binding)
                                                 | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("" as n), s) ->
                                                ((Ast.MbAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"module_binding" n
                                                      s) ))) :
                                                  'module_binding)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("module_binding" | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"module_binding\" | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("module_binding"
                                                    | "anti") | "list") as n),
                                                 s) ->
                                                ((Ast.MbAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"module_binding" n
                                                      s) ))) :
                                                  'module_binding)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (b2 :
                                            'module_binding) ->
                                           fun _ ->
                                            fun (b1 :
                                              'module_binding) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MbAnd (_loc, b1, b2)) :
                                                'module_binding) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_type : 'module_type Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("top")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("functor")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         ); ( (Gram.Skeyword ("->")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (t :
                                               'module_type) ->
                                              fun _ ->
                                               fun (i :
                                                 'a_UIDENT) ->
                                                fun _ ->
                                                 fun _ ->
                                                  fun (_loc :
                                                    Gram.Loc.t) ->
                                                   ((Ast.MtFun
                                                      (_loc, i, t, mt)) :
                                                     'module_type) )) ))] ));
                                     (( (Some ("with")) ), None , (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("with")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (with_constr :
                                               'with_constr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (wc :
                                            'with_constr) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MtWit (_loc, mt, wc)) :
                                                'module_type) )) ))] ));
                                     (( (Some ("apply")) ), None , (
                                      [((
                                        [Gram.Sself ; Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (dummy : 'dummy Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (mt2 :
                                             'module_type) ->
                                            fun (mt1 :
                                              'module_type) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((module_type_app mt1 mt2) :
                                                'module_type) )) ))] ));
                                     (( (Some (".")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (mt2 :
                                            'module_type) ->
                                           fun _ ->
                                            fun (mt1 :
                                              'module_type) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((module_type_acc mt1 mt2) :
                                                'module_type) )) ))] ));
                                     (( (Some ("sig")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("sig")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sig_items :
                                               'sig_items Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (sg :
                                             'sig_items) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MtSig (_loc, sg)) :
                                                'module_type) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Skeyword ("type")) ); (
                                         (Gram.Skeyword ("of")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.MtOf (_loc, me)) :
                                                 'module_type) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (mt :
                                             'module_type) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (mt : 'module_type) )) ));
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
                                             ((Ast.MtQuo (_loc, i)) :
                                               'module_type) )) ));
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
                                              'module_type) )) ));
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
                                                   Quotation.DynAst.module_type_tag) :
                                                  'module_type)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "mtyp") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"mtyp\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "mtyp") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.MtAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"module_type"
                                                      n s) ))) :
                                                  'module_type)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (sig_item : 'sig_item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("top")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("class")) ); (
                                         (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_declaration :
                                               'class_type_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ctd :
                                            'class_type_declaration) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.SgClt (_loc, ctd)) :
                                                'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("class")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_description :
                                               'class_description Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (cd :
                                            'class_description) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.SgCls (_loc, cd)) :
                                               'sig_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.SgVal (_loc, i, t)) :
                                                 'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_declaration :
                                               'type_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'type_declaration) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.SgTyp (_loc, t)) :
                                               'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("open")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'module_longident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.SgOpn (_loc, i)) :
                                               'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Skeyword ("type")) ); (
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
                                              ((Ast.SgMty
                                                 (_loc, i, (
                                                  (Ast.MtNil (_loc)) ))) :
                                                'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (i :
                                              'a_ident) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.SgMty (_loc, i, mt)) :
                                                  'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Skeyword ("rec")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_rec_declaration :
                                               'module_rec_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (mb :
                                            'module_rec_declaration) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.SgRecMod (_loc, mb)) :
                                                'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_declaration :
                                               'module_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_declaration) ->
                                           fun (i :
                                             'a_UIDENT) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.SgMod (_loc, i, mt)) :
                                                'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("include")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.SgInc (_loc, mt)) :
                                               'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("external")) ); (
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
                                             (string_list :
                                               'string_list Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (sl :
                                            'string_list) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (i :
                                                'a_LIDENT) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.SgExt (_loc, i, t, sl)) :
                                                   'sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("exception")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declaration :
                                               'constructor_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'constructor_declaration) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.SgExc (_loc, t)) :
                                               'sig_item) )) ));
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
                                                   Quotation.DynAst.sig_item_tag) :
                                                  'sig_item)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "sigi") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"sigi\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "sigi") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.SgAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"sig_item" n
                                                      s) ))) : 'sig_item)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_declaration :
                                  'module_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_declaration) ->
                                           fun _ ->
                                            fun (t :
                                              'module_type) ->
                                             fun _ ->
                                              fun (i :
                                                'a_UIDENT) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.MtFun (_loc, i, t, mt)) :
                                                   'module_declaration) )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (mt : 'module_declaration) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_rec_declaration :
                                  'module_rec_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (m :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MbCol (_loc, m, mt)) :
                                                'module_rec_declaration) ))
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
                                                   Quotation.DynAst.module_binding_tag) :
                                                  'module_rec_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "module_binding")
                                                  | "anti") | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"module_binding\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "module_binding")
                                                    | "anti") | "list") as n),
                                                 s) ->
                                                ((Ast.MbAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"module_binding" n
                                                      s) ))) :
                                                  'module_rec_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (m2 :
                                            'module_rec_declaration) ->
                                           fun _ ->
                                            fun (m1 :
                                              'module_rec_declaration) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MbAnd (_loc, m1, m2)) :
                                                'module_rec_declaration) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (with_constr : 'with_constr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
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
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
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
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
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
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
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
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
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
                              (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("top")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("object")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_class_self_patt :
                                               'opt_class_self_patt Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_structure :
                                               'class_structure Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("end"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (cst :
                                             'class_structure) ->
                                            fun (csp :
                                              'opt_class_self_patt) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExObj (_loc, csp, cst)) :
                                                 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("while")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("do")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (do_sequence :
                                               'do_sequence Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (seq :
                                            'do_sequence) ->
                                           fun _ ->
                                            fun (e :
                                              'sequence) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExWhi
                                                  (_loc, (
                                                   (mksequence' _loc e) ),
                                                   seq)) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("for")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (direction_flag :
                                               'direction_flag Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("do")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (do_sequence :
                                               'do_sequence Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (seq :
                                            'do_sequence) ->
                                           fun _ ->
                                            fun (e2 :
                                              'sequence) ->
                                             fun (df :
                                               'direction_flag) ->
                                              fun (e1 :
                                                'sequence) ->
                                               fun _ ->
                                                fun (i :
                                                  'a_LIDENT) ->
                                                 fun _ ->
                                                  fun (_loc :
                                                    Gram.Loc.t) ->
                                                   ((Ast.ExFor
                                                      (_loc, i, (
                                                       (mksequence' _loc e1)
                                                       ), (
                                                       (mksequence' _loc e2)
                                                       ), df, seq)) : 'expr)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("do")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (do_sequence :
                                               'do_sequence Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (seq :
                                            'do_sequence) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((mksequence _loc seq) : 'expr)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("if")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("then")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("else")) );
                                         Gram.Sself ] ), (
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
                                        [( (Gram.Skeyword ("try")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("with")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (match_case :
                                               'match_case Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (a :
                                            'match_case) ->
                                           fun _ ->
                                            fun (e :
                                              'sequence) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExTry
                                                  (_loc, (
                                                   (mksequence' _loc e) ), a)) :
                                                 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("match")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("with")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (match_case :
                                               'match_case Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (a :
                                            'match_case) ->
                                           fun _ ->
                                            fun (e :
                                              'sequence) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExMat
                                                  (_loc, (
                                                   (mksequence' _loc e) ), a)) :
                                                 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("fun")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_def :
                                               'fun_def Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_def) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (e : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("fun")) ); (
                                         (Gram.Skeyword ("[")) ); (
                                         (Gram.Slist0sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (match_case0 :
                                                  'match_case0 Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword ("|")) ))) ); (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (a :
                                             'match_case0 list) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExFun
                                                  (_loc, (
                                                   (Ast.mcOr_of_list a) ))) :
                                                 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Skeyword ("open")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("in"))
                                         ); Gram.Sself ] ), (
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
                                         ); Gram.Sself ] ), (
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
                                         ( (Gram.Skeyword ("in")) );
                                         Gram.Sself ] ), (
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
                                                  'expr) )) ))] ));
                                     (( (Some ("where")) ), None , (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("where")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_rec :
                                               'opt_rec Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (let_binding :
                                               'let_binding Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (lb :
                                            'let_binding) ->
                                           fun (rf :
                                             'opt_rec) ->
                                            fun _ ->
                                             fun (e :
                                               'expr) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExLet (_loc, rf, lb, e)) :
                                                 'expr) )) ))] ));
                                     (( (Some (":=")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword (":=")) ); Gram.Sself
                                         ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (dummy : 'dummy Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
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
                                                 'expr) )) ))] ));
                                     (( (Some ("||")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop6 :
                                               'infixop6 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop6) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ))] ));
                                     (( (Some ("&&")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop5 :
                                               'infixop5 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop5) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ))] ));
                                     (( (Some ("<")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop0 :
                                               'infixop0 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop0) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ))] ));
                                     (( (Some ("^")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop1 :
                                               'infixop1 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop1) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ))] ));
                                     (( (Some ("+")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop2 :
                                               'infixop2 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop2) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ))] ));
                                     (( (Some ("*")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop3 :
                                               'infixop3 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop3) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("mod")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "mod")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("lxor")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "lxor")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("lor")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "lor")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("land")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "land")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ))] ));
                                     (( (Some ("**")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (infixop4 :
                                               'infixop4 Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (op :
                                             'infixop4) ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExApp
                                                 (_loc, (
                                                  (Ast.ExApp (_loc, op, e1))
                                                  ), e2)) : 'expr) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("lsr")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "lsr")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("lsl")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "lsl")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("asr")) );
                                         Gram.Sself ] ), (
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
                                                        (Ast.IdLid
                                                          (_loc, "asr")) )))
                                                     ), e1)) ), e2)) : 'expr)
                                          )) ))] ));
                                     (( (Some ("unary minus")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [( (Gram.Skeyword ("-.")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((mkumin _loc "-." e) : 'expr)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("-")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((mkumin _loc "-" e) : 'expr) ))
                                        ))] ));
                                     (( (Some ("apply")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("lazy")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExLaz (_loc, e)) : 'expr)
                                          )) ));
                                       ((
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
                                        [( (Gram.Skeyword ("assert")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((mkassert _loc e) : 'expr) ))
                                        ));
                                       (( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun (e1 :
                                             'expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExApp (_loc, e1, e2)) :
                                               'expr) )) ))] ));
                                     (( (Some ("label")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
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
                                             ((Ast.ExOlb
                                                (_loc, i, (
                                                 (Ast.ExNil (_loc)) ))) :
                                               'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExOlb (_loc, i, e)) :
                                                 'expr) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | OPTLABEL (_) -> (true)
                                            | _ -> (false) ), "OPTLABEL (_)"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | OPTLABEL (i) ->
                                                 ((Ast.ExOlb (_loc, i, e)) :
                                                   'expr)
                                              | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LABEL (_) -> (true)
                                            | _ -> (false) ), "LABEL (_)"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LABEL (i) ->
                                                 ((Ast.ExLab (_loc, i, e)) :
                                                   'expr)
                                              | _ -> assert false) )) ));
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
                                             ((Ast.ExLab
                                                (_loc, i, (
                                                 (Ast.ExNil (_loc)) ))) :
                                               'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExLab (_loc, i, e)) :
                                                 'expr) )) ))] ));
                                     (( (Some (".")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("#"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (lab :
                                            'label) ->
                                           fun _ ->
                                            fun (e :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExSnd (_loc, e, lab)) :
                                                'expr) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExAcc (_loc, e1, e2)) :
                                                'expr) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); ( (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_expr :
                                               'comma_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("}")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e2 :
                                             'comma_expr) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (e1 :
                                                'expr) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((bigarray_get _loc e1 e2) :
                                                  'expr) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); ( (Gram.Skeyword ("[")) );
                                         Gram.Sself ; ( (Gram.Skeyword ("]"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e2 :
                                             'expr) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (e1 :
                                                'expr) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExSte (_loc, e1, e2)) :
                                                  'expr) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); ( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e2 :
                                             'expr) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (e1 :
                                                'expr) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExAre (_loc, e1, e2)) :
                                                  'expr) )) ))] ));
                                     (( (Some ("~-")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (prefixop :
                                               'prefixop Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (f :
                                             'prefixop) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExApp (_loc, f, e)) :
                                               'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("!")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExAcc
                                                (_loc, e, (
                                                 (Ast.ExId
                                                   (_loc, (
                                                    (Ast.IdLid (_loc, "val"))
                                                    ))) ))) : 'expr) )) ))]
                                      ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
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
                                             fun (me :
                                               'module_expr) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.ExPkg
                                                    (_loc, (
                                                     (Ast.MeTyc
                                                       (_loc, me, pt)) ))) :
                                                   'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword ("module")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (me :
                                             'module_expr) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExPkg (_loc, me)) :
                                                 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("begin")) ); (
                                         (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExId
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "()")) ))) :
                                               'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("begin")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("end")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (seq :
                                             'sequence) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((mksequence _loc seq) : 'expr)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (e : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword (":>")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (e :
                                               'expr) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExCoe
                                                   (_loc, e, (
                                                    (Ast.TyNil (_loc)) ), t)) :
                                                  'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (":"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":>")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t2 :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (t :
                                               'ctyp) ->
                                              fun _ ->
                                               fun (e :
                                                 'expr) ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  ((Ast.ExCoe
                                                     (_loc, e, t, t2)) :
                                                    'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (";"))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (e :
                                              'expr) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((mksequence _loc e) : 'expr)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (";"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (seq :
                                             'sequence) ->
                                            fun _ ->
                                             fun (e :
                                               'expr) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((mksequence _loc (
                                                   (Ast.ExSem (_loc, e, seq))
                                                   )) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_expr :
                                               'comma_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (el :
                                             'comma_expr) ->
                                            fun _ ->
                                             fun (e :
                                               'expr) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExTup
                                                   (_loc, (
                                                    (Ast.ExCom (_loc, e, el))
                                                    ))) : 'expr) )) ));
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
                                             fun (e :
                                               'expr) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExTyc (_loc, e, t)) :
                                                  'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExId
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "()")) ))) :
                                               'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{<")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (field_expr_list :
                                               'field_expr_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (">}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (fel :
                                             'field_expr_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExOvr (_loc, fel)) :
                                                'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{<")) ); (
                                         (Gram.Skeyword (">}")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExOvr
                                                (_loc, ( (Ast.RbNil (_loc))
                                                 ))) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Skeyword ("(")) ); Gram.Sself
                                         ; ( (Gram.Skeyword (")")) ); (
                                         (Gram.Skeyword ("with")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr_list :
                                               'label_expr_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (el :
                                             'label_expr_list) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (e :
                                                'expr) ->
                                               fun _ ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  ((Ast.ExRec (_loc, el, e)) :
                                                    'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr_list :
                                               'label_expr_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (el :
                                             'label_expr_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExRec
                                                 (_loc, el, (
                                                  (Ast.ExNil (_loc)) ))) :
                                                'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[|")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_expr :
                                               'sem_expr Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("|]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (el :
                                             'sem_expr) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExArr (_loc, el)) :
                                                'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[|")) ); (
                                         (Gram.Skeyword ("|]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExArr
                                                (_loc, ( (Ast.ExNil (_loc))
                                                 ))) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_expr_for_list :
                                               'sem_expr_for_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("]"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (mk_list :
                                             'sem_expr_for_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((mk_list (
                                                 (Ast.ExId
                                                   (_loc, (
                                                    (Ast.IdUid (_loc, "[]"))
                                                    ))) )) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_expr_for_list :
                                               'sem_expr_for_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("::"))
                                         ); Gram.Sself ; (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (last :
                                             'expr) ->
                                            fun _ ->
                                             fun (mk_list :
                                               'sem_expr_for_list) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((mk_list last) : 'expr) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExId
                                                (_loc, (
                                                 (Ast.IdUid (_loc, "[]")) ))) :
                                               'expr) )) ));
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
                                             ((Ast.ExVrn (_loc, s)) : 'expr)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (val_longident :
                                                  'val_longident Gram.Entry.t)
                                                ))))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'val_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId (_loc, i)) : 'expr) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (module_longident_dot_lparen :
                                                  'module_longident_dot_lparen Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'sequence) ->
                                            fun (i :
                                              'module_longident_dot_lparen) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExOpI (_loc, i, e)) :
                                                'expr) )) ));
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
                                            ((Ast.ExChr (_loc, s)) : 'expr)
                                          )) ));
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
                                            ((Ast.ExStr (_loc, s)) : 'expr)
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
                                            ((Ast.ExFlo (_loc, s)) : 'expr)
                                          )) ));
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
                                            ((Ast.ExNativeInt (_loc, s)) :
                                              'expr) )) ));
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
                                            ((Ast.ExInt64 (_loc, s)) : 'expr)
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
                                            ((Ast.ExInt32 (_loc, s)) : 'expr)
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
                                            ((Ast.ExInt (_loc, s)) : 'expr)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("seq", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"seq\", _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("seq" as n), s) ->
                                                ((Ast.ExSeq
                                                   (_loc, (
                                                    (Ast.ExAnt
                                                      (_loc, (
                                                       (mk_anti ~c:"expr" n
                                                         s) ))) ))) : 'expr)
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
                                                ((Ast.ExTup
                                                   (_loc, (
                                                    (Ast.ExAnt
                                                      (_loc, (
                                                       (mk_anti ~c:"expr" n
                                                         s) ))) ))) : 'expr)
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
                                                ((Ast.ExId
                                                   (_loc, (
                                                    (Ast.IdAnt
                                                      (_loc, ( (mk_anti n s)
                                                       ))) ))) : 'expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("exp" | "") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"exp\" | \"\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("exp" | "") | "anti") as
                                                  n), s) ->
                                                ((Ast.ExAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"expr" n s)
                                                    ))) : 'expr)
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
                                                   Quotation.DynAst.expr_tag) :
                                                  'expr)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (do_sequence : 'do_sequence Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("done")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "()")) ))) :
                                              'do_sequence) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules do_sequence (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (sequence :
                                                      'sequence Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Skeyword ("done")) )]
                                               ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (seq :
                                                    'sequence) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (seq : 'e__3) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (seq :
                                            'e__3) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (seq : 'do_sequence) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules do_sequence (
                                             [((
                                               [( (Gram.Skeyword ("{")) ); (
                                                (Gram.Skeyword ("}")) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (() : 'e__2) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "()")) ))) :
                                              'do_sequence) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules do_sequence (
                                             [((
                                               [( (Gram.Skeyword ("{")) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (sequence :
                                                      'sequence Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Skeyword ("}")) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (seq :
                                                    'sequence) ->
                                                   fun _ ->
                                                    fun (_loc :
                                                      Gram.Loc.t) ->
                                                     (seq : 'e__1) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (seq :
                                            'e__1) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (seq : 'do_sequence) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (infixop5 : 'infixop5 Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.srules infixop5 (
                                           [(( [( (Gram.Skeyword ("&&")) )]
                                             ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__4) )) ));
                                            (( [( (Gram.Skeyword ("&")) )] ),
                                             (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__4) )) ))] )) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'e__4) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, ( (Ast.IdLid (_loc, x))
                                                ))) : 'infixop5) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (infixop6 : 'infixop6 Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.srules infixop6 (
                                           [(( [( (Gram.Skeyword ("||")) )]
                                             ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__5) )) ));
                                            (( [( (Gram.Skeyword ("or")) )]
                                             ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__5) )) ))] )) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'e__5) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, ( (Ast.IdLid (_loc, x))
                                                ))) : 'infixop6) )) ))] ))]
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
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
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (e : 'comma_expr) )) ));
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
                                                ((Ast.ExAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"expr," n s)
                                                    ))) : 'comma_expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'comma_expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'comma_expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExCom (_loc, e1, e2)) :
                                                'comma_expr) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend ( (dummy : 'dummy Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (() : 'dummy) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (sequence' : 'sequence' Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence :
                                               'sequence Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (el :
                                            'sequence) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (fun e ->
                                               (Ast.ExSem (_loc, e, el)) :
                                               'sequence') )) ));
                                       (( [( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (fun e -> e : 'sequence') )) ));
                                       ([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (fun e -> e : 'sequence') )) ))]
                                      ))] ))) () ) ))
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
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence' :
                                               'sequence' Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (k :
                                            'sequence') ->
                                           fun (e :
                                             'expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((k e) : 'sequence) )) ));
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
                                                ((Ast.ExAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"expr;" n s)
                                                    ))) : 'sequence)
                                             | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("let")) ); (
                                         (Gram.Skeyword ("open")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_longident :
                                               'module_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("in"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'sequence) ->
                                           fun _ ->
                                            fun (i :
                                              'module_longident) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExOpI (_loc, i, e)) :
                                                  'sequence) )) ));
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
                                             ))) ); ( (Gram.Skeyword (";"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (el :
                                            'sequence) ->
                                           fun _ ->
                                            fun (mb :
                                              'module_binding0) ->
                                             fun (m :
                                               'a_UIDENT) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.ExLmd
                                                    (_loc, m, mb, (
                                                     (mksequence _loc el) ))) :
                                                   'sequence) )) ));
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
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence' :
                                               'sequence' Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (k :
                                            'sequence') ->
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
                                                  ((k (
                                                     (Ast.ExLmd
                                                       (_loc, m, mb, e)) )) :
                                                    'sequence) )) ));
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
                                         ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (el :
                                            'sequence) ->
                                           fun _ ->
                                            fun (bi :
                                              'binding) ->
                                             fun (rf :
                                               'opt_rec) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.ExLet
                                                   (_loc, rf, bi, (
                                                    (mksequence _loc el) ))) :
                                                  'sequence) )) ));
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
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sequence' :
                                               'sequence' Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (k :
                                            'sequence') ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (bi :
                                               'binding) ->
                                              fun (rf :
                                                'opt_rec) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((k (
                                                    (Ast.ExLet
                                                      (_loc, rf, bi, e)) )) :
                                                   'sequence) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (binding : 'binding Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (let_binding :
                                               'let_binding Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (b :
                                            'let_binding) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (b : 'binding) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (b2 :
                                            'binding) ->
                                           fun _ ->
                                            fun (b1 :
                                              'binding) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.BiAnd (_loc, b1, b2)) :
                                                'binding) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "anti") as n), s) ->
                                                ((Ast.BiAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"binding" n
                                                      s) ))) : 'binding)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((("" | "anti") as n), s) ->
                                                  ((Ast.BiEq
                                                     (_loc, (
                                                      (Ast.PaAnt
                                                        (_loc, (
                                                         (mk_anti ~c:"patt" n
                                                           s) ))) ), e)) :
                                                    'binding)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (("binding" | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"binding\" | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("binding" | "list") as n),
                                                 s) ->
                                                ((Ast.BiAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"binding" n
                                                      s) ))) : 'binding)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (let_binding : 'let_binding Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_binding :
                                               'fun_binding Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_binding) ->
                                           fun (p :
                                             'ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.BiEq (_loc, p, e)) :
                                               'let_binding) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (fun_binding : 'fun_binding Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (cvalue_binding :
                                               'cvalue_binding Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (bi :
                                            'cvalue_binding) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (bi : 'fun_binding) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (labeled_ipatt :
                                                  'labeled_ipatt Gram.Entry.t)
                                                ))))) ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_binding) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExFun
                                                (_loc, (
                                                 (Ast.McArr
                                                   (_loc, p, (
                                                    (Ast.ExNil (_loc)) ), e))
                                                 ))) : 'fun_binding) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules fun_binding (
                                             [((
                                               [( (Gram.Skeyword ("(")) ); (
                                                (Gram.Skeyword ("type")) )]
                                               ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (() : 'e__6) )) ))] )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_binding) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExFUN (_loc, i, e)) :
                                                 'fun_binding) )) ))] ))] )))
                                  () ) ))
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (p :
                                              'ipatt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.McArr
                                                 (_loc, p, (
                                                  (Ast.ExNil (_loc)) ), e)) :
                                                'match_case) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Slist0sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (match_case0 :
                                                  'match_case0 Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword ("|")) ))) ); (
                                         (Gram.Skeyword ("]")) )] ), (
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
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (w :
                                              'opt_when_expr) ->
                                             fun (p :
                                               'patt_as_patt_opt) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.McArr (_loc, p, w, e)) :
                                                 'match_case0) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         ); ( (Gram.Skeyword ("when")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (w :
                                              'expr) ->
                                             fun _ ->
                                              fun (__camlp4_0 :
                                                Gram.Token.t) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (match __camlp4_0 with
                                                 | ANTIQUOT
                                                    ((("" | "anti") as n), s) ->
                                                    ((Ast.McArr
                                                       (_loc, (
                                                        (Ast.PaAnt
                                                          (_loc, (
                                                           (mk_anti ~c:"patt"
                                                             n s) ))) ), w,
                                                        e)) : 'match_case0)
                                                 | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         ); ( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((("" | "anti") as n), s) ->
                                                  ((Ast.McArr
                                                     (_loc, (
                                                      (Ast.PaAnt
                                                        (_loc, (
                                                         (mk_anti ~c:"patt" n
                                                           s) ))) ), (
                                                      (Ast.ExNil (_loc)) ),
                                                      e)) : 'match_case0)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "anti") as n), s) ->
                                                ((Ast.McAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"match_case"
                                                      n s) ))) :
                                                  'match_case0)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (("match_case" | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"match_case\" | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("match_case" | "list") as
                                                  n), s) ->
                                                ((Ast.McAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"match_case"
                                                      n s) ))) :
                                                  'match_case0)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_when_expr : 'opt_when_expr Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.ExNil (_loc)) :
                                             'opt_when_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("when")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (w :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (w : 'opt_when_expr) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (patt_as_patt_opt :
                                  'patt_as_patt_opt Gram.Entry.t) ) (
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
                                            (p : 'patt_as_patt_opt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("as")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'patt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaAli (_loc, p1, p2)) :
                                                'patt_as_patt_opt) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_expr_list :
                                  'label_expr_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr :
                                               'label_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (b1 :
                                            'label_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (b1 : 'label_expr_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr :
                                               'label_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (b1 :
                                             'label_expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (b1 : 'label_expr_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr :
                                               'label_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (b2 :
                                            'label_expr_list) ->
                                           fun _ ->
                                            fun (b1 :
                                              'label_expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.RbSem (_loc, b1, b2)) :
                                                'label_expr_list) )) ))] ))]
                                    ))) () ) ))
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
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'label_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.RbEq
                                               (_loc, i, (
                                                (Ast.ExId
                                                  (_loc, (
                                                   (Ast.IdLid
                                                     (_loc, (
                                                      (lid_of_ident i) ))) )))
                                                ))) : 'label_expr) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_longident :
                                               'label_longident Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_binding :
                                               'fun_binding Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_binding) ->
                                           fun (i :
                                             'label_longident) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.RbEq (_loc, i, e)) :
                                               'label_expr) )) ));
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
                                                ((Ast.RbAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"rec_binding"
                                                      n s) ))) : 'label_expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((("" | "anti") as n), s) ->
                                                  ((Ast.RbEq
                                                     (_loc, (
                                                      (Ast.IdAnt
                                                        (_loc, (
                                                         (mk_anti ~c:"ident"
                                                           n s) ))) ), e)) :
                                                    'label_expr)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "anti") as n), s) ->
                                                ((Ast.RbAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"rec_binding"
                                                      n s) ))) : 'label_expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("rec_binding", _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"rec_binding\", _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (("rec_binding" as n), s) ->
                                                ((Ast.RbAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"rec_binding"
                                                      n s) ))) : 'label_expr)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (fun_def : 'fun_def Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (labeled_ipatt :
                                                  'labeled_ipatt Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_def_cont :
                                               'fun_def_cont Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun ((w, e) :
                                            'fun_def_cont) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExFun
                                                (_loc, (
                                                 (Ast.McArr (_loc, p, w, e))
                                                 ))) : 'fun_def) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules fun_def (
                                             [((
                                               [( (Gram.Skeyword ("(")) ); (
                                                (Gram.Skeyword ("type")) )]
                                               ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (() : 'e__7) )) ))] )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_def_cont_no_when :
                                               'fun_def_cont_no_when Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_def_cont_no_when) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExFUN (_loc, i, e)) :
                                                 'fun_def) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (fun_def_cont : 'fun_def_cont Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((( (Ast.ExNil (_loc)) ), e) :
                                               'fun_def_cont) )) ));
                                       ((
                                        [( (Gram.Skeyword ("when")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (w :
                                              'expr) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((w, e) : 'fun_def_cont) )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (labeled_ipatt :
                                                  'labeled_ipatt Gram.Entry.t)
                                                ))))) ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun ((w, e) :
                                            'fun_def_cont) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((( (Ast.ExNil (_loc)) ), (
                                               (Ast.ExFun
                                                 (_loc, (
                                                  (Ast.McArr (_loc, p, w, e))
                                                  ))) )) : 'fun_def_cont) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules fun_def_cont (
                                             [((
                                               [( (Gram.Skeyword ("(")) ); (
                                                (Gram.Skeyword ("type")) )]
                                               ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (() : 'e__8) )) ))] )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_def_cont_no_when :
                                               'fun_def_cont_no_when Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_def_cont_no_when) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((( (Ast.ExNil (_loc)) ), (
                                                 (Ast.ExFUN (_loc, i, e)) )) :
                                                 'fun_def_cont) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (fun_def_cont_no_when :
                                  'fun_def_cont_no_when Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (e : 'fun_def_cont_no_when) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (labeled_ipatt :
                                                  'labeled_ipatt Gram.Entry.t)
                                                ))))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_def_cont :
                                               'fun_def_cont Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun ((w, e) :
                                            'fun_def_cont) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExFun
                                                (_loc, (
                                                 (Ast.McArr (_loc, p, w, e))
                                                 ))) : 'fun_def_cont_no_when)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Stry
                                           (Gram.srules fun_def_cont_no_when
                                             (
                                             [((
                                               [( (Gram.Skeyword ("(")) ); (
                                                (Gram.Skeyword ("type")) )]
                                               ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun _ ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (() : 'e__9) )) ))] )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_def_cont_no_when) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExFUN (_loc, i, e)) :
                                                 'fun_def_cont_no_when) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (patt : 'patt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("|")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
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
                                     (( (Some ("..")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("..")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'patt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaRng (_loc, p1, p2)) :
                                                'patt) )) ))] ));
                                     (( (Some ("apply")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
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
                                       (( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'patt) ->
                                           fun (p1 :
                                             'patt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaApp (_loc, p1, p2)) :
                                               'patt) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_tcon :
                                               'patt_tcon Gram.Entry.t) )))
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
                                               'patt_tcon) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaOlbi
                                                    (_loc, "", p, e)) :
                                                   'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_tcon :
                                               'patt_tcon Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'patt_tcon) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaOlb (_loc, "", p)) :
                                                 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | ANTIQUOT
                                                 ((("" | "lid") as n), i) ->
                                                 ((Ast.PaOlb
                                                    (_loc, ( (mk_anti n i) ),
                                                     ( (Ast.PaNil (_loc)) ))) :
                                                   'patt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LIDENT (_) -> (true)
                                            | _ -> (false) ), "LIDENT (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LIDENT (i) ->
                                                 ((Ast.PaOlb
                                                    (_loc, i, (
                                                     (Ast.PaNil (_loc)) ))) :
                                                   'patt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_tcon :
                                               'patt_tcon Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (eq_expr :
                                               'eq_expr Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (f :
                                             'eq_expr) ->
                                            fun (p :
                                              'patt_tcon) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (__camlp4_0 :
                                                 Gram.Token.t) ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  (match __camlp4_0 with
                                                   | ANTIQUOT
                                                      ((("" | "lid") as n), i) ->
                                                      ((f ( (mk_anti n i) )
                                                         p) : 'patt)
                                                   | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | OPTLABEL (_) -> (true)
                                            | _ -> (false) ), "OPTLABEL (_)"))
                                         ); ( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt_tcon :
                                               'patt_tcon Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (eq_expr :
                                               'eq_expr Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (f :
                                             'eq_expr) ->
                                            fun (p :
                                              'patt_tcon) ->
                                             fun _ ->
                                              fun (__camlp4_0 :
                                                Gram.Token.t) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (match __camlp4_0 with
                                                 | OPTLABEL (i) ->
                                                    ((f i p) : 'patt)
                                                 | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LIDENT (_) -> (true)
                                            | _ -> (false) ), "LIDENT (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LIDENT (i) ->
                                                 ((Ast.PaLab
                                                    (_loc, i, (
                                                     (Ast.PaNil (_loc)) ))) :
                                                   'patt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | ANTIQUOT
                                                 ((("" | "lid") as n), i) ->
                                                 ((Ast.PaLab
                                                    (_loc, ( (mk_anti n i) ),
                                                     ( (Ast.PaNil (_loc)) ))) :
                                                   'patt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         ); ( (Gram.Skeyword (":")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (match __camlp4_0 with
                                                | ANTIQUOT
                                                   ((("" | "lid") as n), i) ->
                                                   ((Ast.PaLab
                                                      (_loc, ( (mk_anti n i)
                                                       ), p)) : 'patt)
                                                | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LABEL (_) -> (true)
                                            | _ -> (false) ), "LABEL (_)"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LABEL (i) ->
                                                 ((Ast.PaLab (_loc, i, p)) :
                                                   'patt)
                                              | _ -> assert false) )) ));
                                       ((
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
                                       (( [( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaAny (_loc)) : 'patt) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_patt :
                                               'comma_patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pl :
                                             'comma_patt) ->
                                            fun _ ->
                                             fun (p :
                                               'patt) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaTup
                                                   (_loc, (
                                                    (Ast.PaCom (_loc, p, pl))
                                                    ))) : 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("as")) ); Gram.Sself
                                         ; ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p2 :
                                             'patt) ->
                                            fun _ ->
                                             fun (p :
                                               'patt) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaAli (_loc, p, p2)) :
                                                  'patt) )) ));
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
                                                (_loc, ( (neg_string s) ))) :
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
                                                (_loc, ( (neg_string s) ))) :
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
                                                (_loc, ( (neg_string s) ))) :
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
                                                (_loc, ( (neg_string s) ))) :
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
                                                (_loc, ( (neg_string s) ))) :
                                               'patt) )) ));
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
                                                ((Ast.PaId
                                                   (_loc, (
                                                    (Ast.IdAnt
                                                      (_loc, ( (mk_anti n s)
                                                       ))) ))) : 'patt)
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
                                (comma_patt : 'comma_patt Gram.Entry.t) ) (
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
                                            (p : 'comma_patt) )) ));
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
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt," n s)
                                                    ))) : 'comma_patt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'comma_patt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'comma_patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaCom (_loc, p1, p2)) :
                                                'comma_patt) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (sem_patt : 'sem_patt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
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
                                            (p : 'sem_patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'patt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (p : 'sem_patt) )) ));
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
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt;" n s)
                                                    ))) : 'sem_patt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'sem_patt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaSem (_loc, p1, p2)) :
                                                'sem_patt) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (sem_patt_for_list :
                                  'sem_patt_for_list Gram.Entry.t) ) (
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
                                            (fun acc ->
                                              (Ast.PaApp
                                                (_loc, (
                                                 (Ast.PaApp
                                                   (_loc, (
                                                    (Ast.PaId
                                                      (_loc, (
                                                       (Ast.IdUid
                                                         (_loc, "::")) ))) ),
                                                    p)) ), acc)) :
                                              'sem_patt_for_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'patt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (fun acc ->
                                               (Ast.PaApp
                                                 (_loc, (
                                                  (Ast.PaApp
                                                    (_loc, (
                                                     (Ast.PaId
                                                       (_loc, (
                                                        (Ast.IdUid
                                                          (_loc, "::")) )))
                                                     ), p)) ), acc)) :
                                               'sem_patt_for_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (pl :
                                            'sem_patt_for_list) ->
                                           fun _ ->
                                            fun (p :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (fun acc ->
                                                (Ast.PaApp
                                                  (_loc, (
                                                   (Ast.PaApp
                                                     (_loc, (
                                                      (Ast.PaId
                                                        (_loc, (
                                                         (Ast.IdUid
                                                           (_loc, "::")) )))
                                                      ), p)) ), ( (pl acc) ))) :
                                                'sem_patt_for_list) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_patt_list :
                                  'label_patt_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_patt :
                                               'label_patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p1 :
                                            'label_patt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p1 : 'label_patt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_patt :
                                               'label_patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p1 :
                                             'label_patt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (p1 : 'label_patt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_patt :
                                               'label_patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Skeyword ("_")) ); (
                                         (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun _ ->
                                             fun (p1 :
                                               'label_patt) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaSem
                                                  (_loc, p1, (
                                                   (Ast.PaAny (_loc)) ))) :
                                                 'label_patt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_patt :
                                               'label_patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (p1 :
                                              'label_patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaSem
                                                 (_loc, p1, (
                                                  (Ast.PaAny (_loc)) ))) :
                                                'label_patt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_patt :
                                               'label_patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'label_patt_list) ->
                                           fun _ ->
                                            fun (p1 :
                                              'label_patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaSem (_loc, p1, p2)) :
                                                'label_patt_list) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_patt : 'label_patt Gram.Entry.t) ) (
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
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'label_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaEq
                                               (_loc, i, (
                                                (Ast.PaId
                                                  (_loc, (
                                                   (Ast.IdLid
                                                     (_loc, (
                                                      (lid_of_ident i) ))) )))
                                                ))) : 'label_patt) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_longident :
                                               'label_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'patt) ->
                                           fun _ ->
                                            fun (i :
                                              'label_longident) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaEq (_loc, i, p)) :
                                                'label_patt) )) ));
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
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt;" n s)
                                                    ))) : 'label_patt)
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
                                                   Quotation.DynAst.patt_tag) :
                                                  'label_patt)
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
                                                    ))) : 'label_patt)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend ( (ipatt : 'ipatt Gram.Entry.t) )
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
                                            ((Ast.PaAny (_loc)) : 'ipatt) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_LIDENT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PaId
                                               (_loc, ( (Ast.IdLid (_loc, s))
                                                ))) : 'ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ipatt :
                                               'comma_ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pl :
                                             'comma_ipatt) ->
                                            fun _ ->
                                             fun (p :
                                               'ipatt) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaTup
                                                   (_loc, (
                                                    (Ast.PaCom (_loc, p, pl))
                                                    ))) : 'ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("as")) ); Gram.Sself
                                         ; ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p2 :
                                             'ipatt) ->
                                            fun _ ->
                                             fun (p :
                                               'ipatt) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaAli (_loc, p, p2)) :
                                                  'ipatt) )) ));
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
                                               'ipatt) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaTyc (_loc, p, t)) :
                                                  'ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'ipatt) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (p : 'ipatt) )) ));
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
                                                     ))) : 'ipatt) )) ));
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
                                                 'ipatt) )) ));
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
                                               'ipatt) )) ));
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
                                                  'ipatt)
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
                                                         s) ))) ))) : 'ipatt)
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
                                                    ))) : 'ipatt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("{")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_ipatt_list :
                                               'label_ipatt_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("}"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (pl :
                                             'label_ipatt_list) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaRec (_loc, pl)) :
                                                'ipatt) )) ))] ))] ))) () )
                                ))
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'ipatt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p : 'labeled_ipatt) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (comma_ipatt : 'comma_ipatt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'ipatt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p : 'comma_ipatt) )) ));
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
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt," n s)
                                                    ))) : 'comma_ipatt)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'comma_ipatt) ->
                                           fun _ ->
                                            fun (p1 :
                                              'comma_ipatt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaCom (_loc, p1, p2)) :
                                                'comma_ipatt) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_ipatt_list :
                                  'label_ipatt_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_ipatt :
                                               'label_ipatt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p1 :
                                            'label_ipatt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p1 : 'label_ipatt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_ipatt :
                                               'label_ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p1 :
                                             'label_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (p1 : 'label_ipatt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_ipatt :
                                               'label_ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Skeyword ("_")) ); (
                                         (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun _ ->
                                             fun (p1 :
                                               'label_ipatt) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaSem
                                                  (_loc, p1, (
                                                   (Ast.PaAny (_loc)) ))) :
                                                 'label_ipatt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_ipatt :
                                               'label_ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (p1 :
                                              'label_ipatt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaSem
                                                 (_loc, p1, (
                                                  (Ast.PaAny (_loc)) ))) :
                                                'label_ipatt_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_ipatt :
                                               'label_ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p2 :
                                            'label_ipatt_list) ->
                                           fun _ ->
                                            fun (p1 :
                                              'label_ipatt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaSem (_loc, p1, p2)) :
                                                'label_ipatt_list) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_ipatt : 'label_ipatt Gram.Entry.t) ) (
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'ipatt) ->
                                           fun _ ->
                                            fun (i :
                                              'label_longident) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaEq (_loc, i, p)) :
                                                'label_ipatt) )) ));
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
                                                  'label_ipatt)
                                             | _ -> assert false) )) ));
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
                                                ((Ast.PaAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"patt;" n s)
                                                    ))) : 'label_ipatt)
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
                                                    ))) : 'label_ipatt)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_declaration :
                                  'type_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_ident_and_parameters :
                                               'type_ident_and_parameters Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_eq_ctyp :
                                               'opt_eq_ctyp Gram.Entry.t) )))
                                         ); (
                                         (Gram.Slist0
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (constrain :
                                                  'constrain Gram.Entry.t) )))))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (cl :
                                            'constrain list) ->
                                           fun (tk :
                                             'opt_eq_ctyp) ->
                                            fun ((n, tpl) :
                                              'type_ident_and_parameters) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyDcl
                                                 (_loc, n, tpl, tk, cl)) :
                                                'type_declaration) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'type_declaration) ->
                                           fun _ ->
                                            fun (t1 :
                                              'type_declaration) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAnd (_loc, t1, t2)) :
                                                'type_declaration) )) ));
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
                                                  'type_declaration)
                                             | _ -> assert false) )) ));
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
                                                    (mk_anti ~c:"ctypand" n
                                                      s) ))) :
                                                  'type_declaration)
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
                                                    ))) : 'type_declaration)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (constrain : 'constrain Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("constraint")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((t1, t2) : 'constrain) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_eq_ctyp : 'opt_eq_ctyp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.TyNil (_loc)) :
                                             'opt_eq_ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_kind :
                                               'type_kind Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (tk :
                                            'type_kind) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (tk : 'opt_eq_ctyp) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_kind : 'type_kind Gram.Entry.t) ) (
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
                                            (t : 'type_kind) )) ))] ))] )))
                                  () ) ))
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
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         (
                                         (Gram.Slist0
                                           ((Gram.Snterm
                                              (Gram.Entry.obj (
                                                (optional_type_parameter :
                                                  'optional_type_parameter Gram.Entry.t)
                                                ))))) )] ), (
                                        (Gram.Action.mk (
                                          fun (tpl :
                                            'optional_type_parameter list) ->
                                           fun (i :
                                             'a_LIDENT) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((i, tpl) :
                                               'type_ident_and_parameters) ))
                                        ))] ))] ))) () ) ))
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
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_parameters :
                                               'type_parameters Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (tpl :
                                            'type_parameters) ->
                                           fun (i :
                                             'type_longident) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((tpl ( (Ast.TyId (_loc, i)) )) :
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
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (fun t -> t : 'type_parameters) ))
                                        ));
                                       ((
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
                                             ))) ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'type_parameters) ->
                                           fun (t1 :
                                             'type_parameter) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (fun acc ->
                                               (t2 (
                                                 (Ast.TyApp (_loc, acc, t1))
                                                 )) : 'type_parameters) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_parameter :
                                  'type_parameter Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                                'type_parameter) )) ));
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
                                                'type_parameter) )) ));
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
                                             ((Ast.TyQuo (_loc, i)) :
                                               'type_parameter) )) ));
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
                                                  'type_parameter)
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
                                                  'type_parameter)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (optional_type_parameter :
                                  'optional_type_parameter Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("_")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyAny (_loc)) :
                                              'optional_type_parameter) )) ));
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
                              (Gram.extend ( (ctyp : 'ctyp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("==")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("==")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyMan (_loc, t1, t2)) :
                                                'ctyp) )) ))] ));
                                     (( (Some ("private")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [( (Gram.Skeyword ("private")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (ctyp : 'ctyp Gram.Entry.t) ))
                                            ), "alias")) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyPrv (_loc, t)) : 'ctyp)
                                          )) ))] ));
                                     (( (Some ("alias")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("as")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAli (_loc, t1, t2)) :
                                                'ctyp) )) ))] ));
                                     (( (Some ("forall")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("!")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (typevars :
                                               'typevars Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (".")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'typevars) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyPol (_loc, t1, t2)) :
                                                 'ctyp) )) ))] ));
                                     (( (Some ("arrow")) ), (
                                      (Some ((FanSig.Grammar.RightA))) ),
                                      (
                                      [((
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
                                     (( (Some ("label")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_OPTLABEL :
                                               'a_OPTLABEL Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (i :
                                             'a_OPTLABEL) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyOlb (_loc, i, t)) :
                                               'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyOlb (_loc, i, t)) :
                                                 'ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LABEL :
                                               'a_LABEL Gram.Entry.t) ))) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (i :
                                             'a_LABEL) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyLab (_loc, i, t)) :
                                               'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (i :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyLab (_loc, i, t)) :
                                                 'ctyp) )) ))] ));
                                     (( (Some ("apply")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
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
                                               Invalid_argument (_) -> t) :
                                               'ctyp) )) ))] ));
                                     (( (Some (".")) ), (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
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
                                              ((Ast.TyRec (_loc, t)) : 'ctyp)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("[<")) ); (
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
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyVrnInfSup
                                                   (_loc, rfl, ntl)) : 'ctyp)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("[<")) ); (
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
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyVrnInf (_loc, rfl)) :
                                                'ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword ("<")) ); (
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
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword ("<")) ); (
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
                                         (Gram.Skeyword ("=")) ); (
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
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declarations :
                                               'constructor_declarations Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("]"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'constructor_declarations) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TySum (_loc, t)) : 'ctyp)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TySum
                                                (_loc, ( (Ast.TyNil (_loc))
                                                 ))) : 'ctyp) )) ));
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
                                         Gram.Sself ; ( (Gram.Skeyword ("*"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (star_ctyp :
                                               'star_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (tl :
                                             'star_ctyp) ->
                                            fun _ ->
                                             fun (t :
                                               'ctyp) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyTup
                                                   (_loc, (
                                                    (Ast.TySta (_loc, t, tl))
                                                    ))) : 'ctyp) )) ));
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
                                (star_ctyp : 'star_ctyp Gram.Entry.t) ) (
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
                                            (t : 'star_ctyp) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("*"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'star_ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'star_ctyp) ->
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
                                               'a_UIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_UIDENT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyId
                                               (_loc, ( (Ast.IdUid (_loc, s))
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
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (s :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (let (tl, rt) =
                                                (generalized_type_of_type t) in
                                               (Ast.TyCol
                                                 (_loc, (
                                                  (Ast.TyId
                                                    (_loc, (
                                                     (Ast.IdUid (_loc, s)) )))
                                                  ), (
                                                  (Ast.TyArr
                                                    (_loc, (
                                                     (Ast.tyAnd_of_list tl)
                                                     ), rt)) ))) :
                                                'constructor_declarations) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (s :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyOf
                                                 (_loc, (
                                                  (Ast.TyId
                                                    (_loc, (
                                                     (Ast.IdUid (_loc, s)) )))
                                                  ), t)) :
                                                'constructor_declarations) ))
                                        ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("|"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'constructor_declarations) ->
                                           fun _ ->
                                            fun (t1 :
                                              'constructor_declarations) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyOr (_loc, t1, t2)) :
                                                'constructor_declarations) ))
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
                                                  'constructor_declarations)
                                             | _ -> assert false) )) ));
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
                                                    (mk_anti ~c:"ctyp|" n s)
                                                    ))) :
                                                  'constructor_declarations)
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
                                                    ))) :
                                                  'constructor_declarations)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (constructor_declaration :
                                  'constructor_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_UIDENT) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyId
                                               (_loc, ( (Ast.IdUid (_loc, s))
                                                ))) :
                                              'constructor_declaration) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (s :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyOf
                                                 (_loc, (
                                                  (Ast.TyId
                                                    (_loc, (
                                                     (Ast.IdUid (_loc, s)) )))
                                                  ), t)) :
                                                'constructor_declaration) ))
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
                                                  'constructor_declaration)
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
                                                    ))) :
                                                  'constructor_declaration)
                                             | _ -> assert false) )) ))] ))]
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'constructor_arg_list) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (t1 :
                                              'constructor_arg_list) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAnd (_loc, t1, t2)) :
                                                'constructor_arg_list) )) ));
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
                                                    (mk_anti ~c:"ctypand" n
                                                      s) ))) :
                                                  'constructor_arg_list)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_declaration_list :
                                  'label_declaration_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration :
                                               'label_declaration Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (t1 :
                                            'label_declaration) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t1 : 'label_declaration_list) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration :
                                               'label_declaration Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t1 :
                                             'label_declaration) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (t1 : 'label_declaration_list)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration :
                                               'label_declaration Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (";"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'label_declaration_list) ->
                                           fun _ ->
                                            fun (t1 :
                                              'label_declaration) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TySem (_loc, t1, t2)) :
                                                'label_declaration_list) ))
                                        ))] ))] ))) () ) ))
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
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Skeyword ("mutable")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (s :
                                               'a_LIDENT) ->
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
                                                    (mk_anti ~c:"ctyp;" n s)
                                                    ))) : 'label_declaration)
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
                                (a_ident : 'a_ident Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                            (i : 'a_ident) )) ));
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
                                            (i : 'a_ident) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend ( (ident : 'ident Gram.Entry.t) )
                                (
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
                                            'ident) ->
                                           fun _ ->
                                            fun (i :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc
                                                 (_loc, (
                                                  (Ast.IdUid (_loc, i)) ), j)) :
                                                'ident) )) ));
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
                                            'ident) ->
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
                                                    'ident)
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
                                            ((Ast.IdLid (_loc, i)) : 'ident)
                                          )) ));
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
                                            ((Ast.IdUid (_loc, i)) : 'ident)
                                          )) ));
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
                                                    ))) : 'ident)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_longident :
                                  'module_longident Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                              'module_longident) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (".")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'module_longident) ->
                                           fun _ ->
                                            fun (m :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc
                                                 (_loc, (
                                                  (Ast.IdUid (_loc, m)) ), l)) :
                                                'module_longident) )) ));
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
                                                    ))) : 'module_longident)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_longident_with_app :
                                  'module_longident_with_app Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("apply")) ), None , (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'module_longident_with_app) ->
                                           fun (i :
                                             'module_longident_with_app) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.IdApp (_loc, i, j)) :
                                               'module_longident_with_app) ))
                                        ))] ));
                                     (( (Some (".")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'module_longident_with_app) ->
                                           fun _ ->
                                            fun (i :
                                              'module_longident_with_app) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc (_loc, i, j)) :
                                                'module_longident_with_app)
                                          )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (i :
                                             'module_longident_with_app) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (i :
                                                'module_longident_with_app)
                                          )) ));
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
                                              'module_longident_with_app) ))
                                        ));
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
                                                    ))) :
                                                  'module_longident_with_app)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_longident_dot_lparen :
                                  'module_longident_dot_lparen Gram.Entry.t)
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
                                         ( (Gram.Skeyword (".")) ); (
                                         (Gram.Skeyword ("(")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (i :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdUid (_loc, i)) :
                                                'module_longident_dot_lparen)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (".")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'module_longident_dot_lparen) ->
                                           fun _ ->
                                            fun (m :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc
                                                 (_loc, (
                                                  (Ast.IdUid (_loc, m)) ), l)) :
                                                'module_longident_dot_lparen)
                                          )) ));
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
                                         ); ( (Gram.Skeyword (".")) ); (
                                         (Gram.Skeyword ("(")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
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
                                                      (mk_anti ~c:"ident" n
                                                        s) ))) :
                                                    'module_longident_dot_lparen)
                                               | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (type_longident :
                                  'type_longident Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("apply")) ), None , (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'type_longident) ->
                                           fun (i :
                                             'type_longident) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.IdApp (_loc, i, j)) :
                                               'type_longident) )) ))] ));
                                     (( (Some (".")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'type_longident) ->
                                           fun _ ->
                                            fun (i :
                                              'type_longident) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc (_loc, i, j)) :
                                                'type_longident) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (i :
                                             'type_longident) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (i : 'type_longident) )) ));
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
                                              'type_longident) )) ));
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
                                              'type_longident) )) ));
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
                                                    ))) : 'type_longident)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (label_longident :
                                  'label_longident Gram.Entry.t) ) (
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
                                            ((Ast.IdLid (_loc, i)) :
                                              'label_longident) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (".")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'label_longident) ->
                                           fun _ ->
                                            fun (m :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc
                                                 (_loc, (
                                                  (Ast.IdUid (_loc, m)) ), l)) :
                                                'label_longident) )) ));
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
                                                    ))) : 'label_longident)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_type_longident :
                                  'class_type_longident Gram.Entry.t) ) (
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
                                          fun (x :
                                            'type_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'class_type_longident) )) ))]
                                      ))] ))) () ) ))
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
                                             (ident : 'ident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'ident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'val_longident) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_longident :
                                  'class_longident Gram.Entry.t) ) (
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
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'label_longident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'class_longident) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_declaration :
                                  'class_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_info_for_class_expr :
                                               'class_info_for_class_expr Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_fun_binding :
                                               'class_fun_binding Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_fun_binding) ->
                                           fun (ci :
                                             'class_info_for_class_expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CeEq (_loc, ci, ce)) :
                                               'class_declaration) )) ));
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
                                                   Quotation.DynAst.class_expr_tag) :
                                                  'class_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "cdcl") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"cdcl\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "cdcl") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CeAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"class_expr"
                                                      n s) ))) :
                                                  'class_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (c2 :
                                            'class_declaration) ->
                                           fun _ ->
                                            fun (c1 :
                                              'class_declaration) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CeAnd (_loc, c1, c2)) :
                                                'class_declaration) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_fun_binding :
                                  'class_fun_binding Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (labeled_ipatt :
                                               'labeled_ipatt Gram.Entry.t)
                                             ))) ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (cfb :
                                            'class_fun_binding) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CeFun (_loc, p, cfb)) :
                                               'class_fun_binding) )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_plus :
                                               'class_type_plus Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_expr :
                                               'class_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_expr) ->
                                           fun _ ->
                                            fun (ct :
                                              'class_type_plus) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CeTyc (_loc, ce, ct)) :
                                                 'class_fun_binding) )) ));
                                       ((
                                        [( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_expr :
                                               'class_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (ce : 'class_fun_binding) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_info_for_class_type :
                                  'class_info_for_class_type Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_virtual :
                                               'opt_virtual Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_name_and_param :
                                               'class_name_and_param Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun ((i, ot) :
                                            'class_name_and_param) ->
                                           fun (mv :
                                             'opt_virtual) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CtCon
                                                (_loc, mv, (
                                                 (Ast.IdLid (_loc, i)) ), ot)) :
                                               'class_info_for_class_type) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_info_for_class_expr :
                                  'class_info_for_class_expr Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_virtual :
                                               'opt_virtual Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_name_and_param :
                                               'class_name_and_param Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun ((i, ot) :
                                            'class_name_and_param) ->
                                           fun (mv :
                                             'opt_virtual) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CeCon
                                                (_loc, mv, (
                                                 (Ast.IdLid (_loc, i)) ), ot)) :
                                               'class_info_for_class_expr) ))
                                        ))] ))] ))) () ) ))
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
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_LIDENT :
                                               'a_LIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_type_parameter :
                                               'comma_type_parameter Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("]"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (x :
                                             'comma_type_parameter) ->
                                            fun _ ->
                                             fun (i :
                                               'a_LIDENT) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((i, x) :
                                                 'class_name_and_param) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (comma_type_parameter :
                                  'comma_type_parameter Gram.Entry.t) ) (
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
                                            (t : 'comma_type_parameter) )) ));
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
                                                    (mk_anti ~c:"ctyp," n s)
                                                    ))) :
                                                  'comma_type_parameter)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'comma_type_parameter) ->
                                           fun _ ->
                                            fun (t1 :
                                              'comma_type_parameter) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyCom (_loc, t1, t2)) :
                                                'comma_type_parameter) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_comma_ctyp :
                                  'opt_comma_ctyp Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.TyNil (_loc)) :
                                             'opt_comma_ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp :
                                               'comma_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (x :
                                             'comma_ctyp) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (x : 'opt_comma_ctyp) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (comma_ctyp : 'comma_ctyp Gram.Entry.t) ) (
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
                                            (t : 'comma_ctyp) )) ));
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
                                                    (mk_anti ~c:"ctyp," n s)
                                                    ))) : 'comma_ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword (","))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'comma_ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'comma_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyCom (_loc, t1, t2)) :
                                                'comma_ctyp) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_fun_def : 'class_fun_def Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("->")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_expr :
                                               'class_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (ce : 'class_fun_def) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (labeled_ipatt :
                                               'labeled_ipatt Gram.Entry.t)
                                             ))) ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_fun_def) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CeFun (_loc, p, ce)) :
                                               'class_fun_def) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_expr : 'class_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("top")) ), None , (
                                      [((
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
                                         ( (Gram.Skeyword ("in")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_expr) ->
                                           fun _ ->
                                            fun (bi :
                                              'binding) ->
                                             fun (rf :
                                               'opt_rec) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.CeLet
                                                   (_loc, rf, bi, ce)) :
                                                  'class_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("fun")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (labeled_ipatt :
                                               'labeled_ipatt Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_fun_def :
                                               'class_fun_def Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_fun_def) ->
                                           fun (p :
                                             'labeled_ipatt) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CeFun (_loc, p, ce)) :
                                                'class_expr) )) ))] ));
                                     (( (Some ("apply")) ), (
                                      (Some ((FanSig.Grammar.NonA))) ), (
                                      [((
                                        [Gram.Sself ; (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "label")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun (ce :
                                             'class_expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CeApp (_loc, ce, e)) :
                                               'class_expr) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (ce :
                                             'class_expr) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (ce : 'class_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (":"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type :
                                               'class_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (ct :
                                             'class_type) ->
                                            fun _ ->
                                             fun (ce :
                                               'class_expr) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.CeTyc (_loc, ce, ct)) :
                                                  'class_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("object")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_class_self_patt :
                                               'opt_class_self_patt Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_structure :
                                               'class_structure Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("end"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (cst :
                                             'class_structure) ->
                                            fun (csp :
                                              'opt_class_self_patt) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CeStr (_loc, csp, cst)) :
                                                 'class_expr) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_longident_and_param :
                                               'class_longident_and_param Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ce :
                                            'class_longident_and_param) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (ce : 'class_expr) )) ));
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
                                                   Quotation.DynAst.class_expr_tag) :
                                                  'class_expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "cexp") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"cexp\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "cexp") | "anti") as
                                                  n), s) ->
                                                ((Ast.CeAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"class_expr"
                                                      n s) ))) : 'class_expr)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
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
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_longident :
                                               'class_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("["))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp :
                                               'comma_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'comma_ctyp) ->
                                            fun _ ->
                                             fun (ci :
                                               'class_longident) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CeCon
                                                  (_loc, Ast.ViNil , ci, t)) :
                                                 'class_longident_and_param)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_structure :
                                  'class_structure Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist0
                                           (Gram.srules class_structure (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (class_str_item :
                                                      'class_str_item Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t) )))
                                                )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (cst :
                                                    'class_str_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (cst : 'e__10) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'e__10 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.crSem_of_list l) :
                                              'class_structure) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "cst") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"cst\") | \"anti\") | \"list\"), _)"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (cst :
                                            'class_structure) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((((("" | "cst") | "anti")
                                                     | "list") as n), s) ->
                                                  ((Ast.CrSem
                                                     (_loc, (
                                                      (Ast.CrAnt
                                                        (_loc, (
                                                         (mk_anti
                                                           ~c:"class_str_item"
                                                           n s) ))) ), cst)) :
                                                    'class_structure)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "cst") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"cst\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "cst") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CrAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"class_str_item" n
                                                      s) ))) :
                                                  'class_structure)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_class_self_patt :
                                  'opt_class_self_patt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.PaNil (_loc)) :
                                             'opt_class_self_patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
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
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.PaTyc (_loc, p, t)) :
                                                  'opt_class_self_patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'patt) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (p : 'opt_class_self_patt) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_str_item :
                                  'class_str_item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [( (Gram.Skeyword ("initializer")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (se :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CrIni (_loc, se)) :
                                               'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_constraint :
                                               'type_constraint Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CrCtr (_loc, t1, t2)) :
                                                 'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (method_opt_override :
                                               'method_opt_override Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("virtual")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun _ ->
                                              fun (pf :
                                                'opt_private) ->
                                               fun (o :
                                                 'method_opt_override) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 (if (o <> Ast.OvNil ) then
                                                   (
                                                   (raise (
                                                     (Stream.Error
                                                       ("override (!) is incompatible with virtual"))
                                                     ))
                                                   )
                                                  else
                                                   (Ast.CrVir
                                                     (_loc, l, pf, t)) :
                                                   'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (method_opt_override :
                                               'method_opt_override Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_polyt :
                                               'opt_polyt Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (fun_binding :
                                               'fun_binding Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'fun_binding) ->
                                           fun (topt :
                                             'opt_polyt) ->
                                            fun (l :
                                              'label) ->
                                             fun (pf :
                                               'opt_private) ->
                                              fun (o :
                                                'method_opt_override) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.CrMth
                                                   (_loc, l, o, pf, e, topt)) :
                                                  'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (method_opt_override :
                                               'method_opt_override Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("virtual")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun (pf :
                                               'opt_private) ->
                                              fun _ ->
                                               fun (o :
                                                 'method_opt_override) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 (if (o <> Ast.OvNil ) then
                                                   (
                                                   (raise (
                                                     (Stream.Error
                                                       ("override (!) is incompatible with virtual"))
                                                     ))
                                                   )
                                                  else
                                                   (Ast.CrVir
                                                     (_loc, l, pf, t)) :
                                                   'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val_opt_override :
                                               'value_val_opt_override Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("virtual")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_mutable :
                                               'opt_mutable Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun (mf :
                                               'opt_mutable) ->
                                              fun _ ->
                                               fun (o :
                                                 'value_val_opt_override) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 (if (o <> Ast.OvNil ) then
                                                   (
                                                   (raise (
                                                     (Stream.Error
                                                       ("override (!) is incompatible with virtual"))
                                                     ))
                                                   )
                                                  else
                                                   (Ast.CrVvr
                                                     (_loc, l, mf, t)) :
                                                   'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val_opt_override :
                                               'value_val_opt_override Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_mutable :
                                               'opt_mutable Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("virtual")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun _ ->
                                              fun (mf :
                                                'opt_mutable) ->
                                               fun (o :
                                                 'value_val_opt_override) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 (if (o <> Ast.OvNil ) then
                                                   (
                                                   (raise (
                                                     (Stream.Error
                                                       ("override (!) is incompatible with virtual"))
                                                     ))
                                                   )
                                                  else
                                                   (Ast.CrVvr
                                                     (_loc, l, mf, t)) :
                                                   'class_str_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val_opt_override :
                                               'value_val_opt_override Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_mutable :
                                               'opt_mutable Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (cvalue_binding :
                                               'cvalue_binding Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'cvalue_binding) ->
                                           fun (lab :
                                             'label) ->
                                            fun (mf :
                                              'opt_mutable) ->
                                             fun (o :
                                               'value_val_opt_override) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CrVal
                                                  (_loc, lab, o, mf, e)) :
                                                 'class_str_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("inherit")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_override :
                                               'opt_override Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_expr :
                                               'class_expr Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_as_lident :
                                               'opt_as_lident Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (pb :
                                            'opt_as_lident) ->
                                           fun (ce :
                                             'class_expr) ->
                                            fun (o :
                                              'opt_override) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CrInh (_loc, o, ce, pb)) :
                                                 'class_str_item) )) ));
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
                                                   Quotation.DynAst.class_str_item_tag) :
                                                  'class_str_item)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "cst") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"cst\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "cst") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CrAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"class_str_item" n
                                                      s) ))) :
                                                  'class_str_item)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (method_opt_override :
                                  'method_opt_override Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("method")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.OvNil) :
                                              'method_opt_override) )) ));
                                       ((
                                        [( (Gram.Skeyword ("method")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("!" | "override") | "anti"),
                                                _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"!\" | \"override\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | ANTIQUOT
                                                 (((("!" | "override")
                                                    | "anti") as n), s) ->
                                                 ((Ast.OvAnt (mk_anti n s)) :
                                                   'method_opt_override)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("method")) ); (
                                         (Gram.Skeyword ("!")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.OvOverride) :
                                               'method_opt_override) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (value_val_opt_override :
                                  'value_val_opt_override Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.OvNil) :
                                              'value_val_opt_override) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("!" | "override") | "anti"),
                                                _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"!\" | \"override\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | ANTIQUOT
                                                 (((("!" | "override")
                                                    | "anti") as n), s) ->
                                                 ((Ast.OvAnt (mk_anti n s)) :
                                                   'value_val_opt_override)
                                              | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("!")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.OvOverride) :
                                               'value_val_opt_override) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_as_lident : 'opt_as_lident Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ("" : 'opt_as_lident) )) ));
                                       ((
                                        [( (Gram.Skeyword ("as")) ); (
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
                                             (i : 'opt_as_lident) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_polyt : 'opt_polyt Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.TyNil (_loc)) : 'opt_polyt)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (t : 'opt_polyt) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (cvalue_binding :
                                  'cvalue_binding Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword (":>")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (t :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExCoe
                                                  (_loc, e, (
                                                   (Ast.TyNil (_loc)) ), t)) :
                                                 'cvalue_binding) )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":>")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (t2 :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (t :
                                                'poly_type) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((match t with
                                                   | Ast.TyPol (_, _, _) ->
                                                      (raise (
                                                        (Stream.Error
                                                          ("unexpected polytype here"))
                                                        ))
                                                   | _ ->
                                                      (Ast.ExCoe
                                                        (_loc, e, t, t2))) :
                                                   'cvalue_binding) )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (t :
                                              'poly_type) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.ExTyc (_loc, e, t)) :
                                                 'cvalue_binding) )) ));
                                       ((
                                        [( (Gram.Skeyword (":")) ); (
                                         (Gram.Skeyword ("type")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (unquoted_typevars :
                                               'unquoted_typevars Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("."))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (t2 :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (t1 :
                                                'unquoted_typevars) ->
                                               fun _ ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  (let u =
                                                    (Ast.TyTypePol
                                                      (_loc, t1, t2)) in
                                                   (Ast.ExTyc (_loc, e, u)) :
                                                    'cvalue_binding) )) ));
                                       ((
                                        [( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (e : 'cvalue_binding) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend ( (label : 'label Gram.Entry.t) )
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
                                            (i : 'label) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (class_type : 'class_type Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("object")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_class_self_type :
                                               'opt_class_self_type Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_signature :
                                               'class_signature Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("end"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (csg :
                                             'class_signature) ->
                                            fun (cst :
                                              'opt_class_self_type) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CtSig (_loc, cst, csg)) :
                                                 'class_type) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_longident_and_param :
                                               'class_type_longident_and_param Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_longident_and_param) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (ct : 'class_type) )) ));
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
                                                   Quotation.DynAst.class_type_tag) :
                                                  'class_type)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "ctyp") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"ctyp\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "ctyp") | "anti") as
                                                  n), s) ->
                                                ((Ast.CtAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"class_type"
                                                      n s) ))) : 'class_type)
                                             | _ -> assert false) )) ))] ))]
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
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_longident :
                                               'class_type_longident Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("["))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp :
                                               'comma_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'comma_ctyp) ->
                                            fun _ ->
                                             fun (i :
                                               'class_type_longident) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CtCon
                                                  (_loc, Ast.ViNil , i, t)) :
                                                 'class_type_longident_and_param)
                                          )) ))] ))] ))) () ) ))
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
                                        [( (Gram.Skeyword ("[")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("]")) ); (
                                         (Gram.Skeyword ("->")) ); Gram.Sself
                                         ] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_plus) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (t :
                                               'ctyp) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.CtFun (_loc, t, ct)) :
                                                  'class_type_plus) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_class_self_type :
                                  'opt_class_self_type Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.TyNil (_loc)) :
                                             'opt_class_self_type) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (t :
                                             'ctyp) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (t : 'opt_class_self_type) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_signature :
                                  'class_signature Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist0
                                           (Gram.srules class_signature (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (class_sig_item :
                                                      'class_sig_item Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t) )))
                                                )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (csg :
                                                    'class_sig_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (csg : 'e__11) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'e__11 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.cgSem_of_list l) :
                                              'class_signature) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "csg") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"csg\") | \"anti\") | \"list\"), _)"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (csg :
                                            'class_signature) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((((("" | "csg") | "anti")
                                                     | "list") as n), s) ->
                                                  ((Ast.CgSem
                                                     (_loc, (
                                                      (Ast.CgAnt
                                                        (_loc, (
                                                         (mk_anti
                                                           ~c:"class_sig_item"
                                                           n s) ))) ), csg)) :
                                                    'class_signature)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "csg") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"csg\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "csg") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CgAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"class_sig_item" n
                                                      s) ))) :
                                                  'class_signature)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_sig_item :
                                  'class_sig_item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (type_constraint :
                                               'type_constraint Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'ctyp) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.CgCtr (_loc, t1, t2)) :
                                                 'class_sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("method")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("virtual")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun _ ->
                                              fun (pf :
                                                'opt_private) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.CgVir (_loc, l, pf, t)) :
                                                   'class_sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("method")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun (pf :
                                               'opt_private) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.CgMth (_loc, l, pf, t)) :
                                                  'class_sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("method")) ); (
                                         (Gram.Skeyword ("virtual")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (poly_type :
                                               'poly_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'poly_type) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun (pf :
                                               'opt_private) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.CgVir (_loc, l, pf, t)) :
                                                   'class_sig_item) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (value_val :
                                               'value_val Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_mutable :
                                               'opt_mutable Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_virtual :
                                               'opt_virtual Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun (mv :
                                               'opt_virtual) ->
                                              fun (mf :
                                                'opt_mutable) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.CgVal
                                                    (_loc, l, mf, mv, t)) :
                                                   'class_sig_item) )) ));
                                       ((
                                        [( (Gram.Skeyword ("inherit")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type :
                                               'class_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (cs :
                                            'class_type) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CgInh (_loc, cs)) :
                                               'class_sig_item) )) ));
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
                                                   Quotation.DynAst.class_sig_item_tag) :
                                                  'class_sig_item)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "csg") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"csg\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "csg") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CgAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"class_sig_item" n
                                                      s) ))) :
                                                  'class_sig_item)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
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
                                            (() : 'type_constraint) )) ));
                                       (( [( (Gram.Skeyword ("type")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'type_constraint) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_description :
                                  'class_description Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_info_for_class_type :
                                               'class_info_for_class_type Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword (":"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_plus :
                                               'class_type_plus Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type_plus) ->
                                           fun _ ->
                                            fun (ci :
                                              'class_info_for_class_type) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtCol (_loc, ci, ct)) :
                                                'class_description) )) ));
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
                                                   Quotation.DynAst.class_type_tag) :
                                                  'class_description)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "typ") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"typ\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "typ") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CtAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"class_type"
                                                      n s) ))) :
                                                  'class_description)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (cd2 :
                                            'class_description) ->
                                           fun _ ->
                                            fun (cd1 :
                                              'class_description) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtAnd (_loc, cd1, cd2)) :
                                                'class_description) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_type_declaration :
                                  'class_type_declaration Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_info_for_class_type :
                                               'class_info_for_class_type Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("="))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type :
                                               'class_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (ct :
                                            'class_type) ->
                                           fun _ ->
                                            fun (ci :
                                              'class_info_for_class_type) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtEq (_loc, ci, ct)) :
                                                'class_type_declaration) ))
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
                                                   Quotation.DynAst.class_type_tag) :
                                                  'class_type_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "typ") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"typ\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "typ") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.CtAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"class_type"
                                                      n s) ))) :
                                                  'class_type_declaration)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (cd2 :
                                            'class_type_declaration) ->
                                           fun _ ->
                                            fun (cd1 :
                                              'class_type_declaration) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtAnd (_loc, cd1, cd2)) :
                                                'class_type_declaration) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (field_expr_list :
                                  'field_expr_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (field_expr :
                                               'field_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (b1 :
                                            'field_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (b1 : 'field_expr_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (field_expr :
                                               'field_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (b1 :
                                             'field_expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (b1 : 'field_expr_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (field_expr :
                                               'field_expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (b2 :
                                            'field_expr_list) ->
                                           fun _ ->
                                            fun (b1 :
                                              'field_expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.RbSem (_loc, b1, b2)) :
                                                'field_expr_list) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (field_expr : 'field_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label : 'label Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterml
                                           ((
                                            (Gram.Entry.obj (
                                              (expr : 'expr Gram.Entry.t) ))
                                            ), "top")) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (l :
                                              'label) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.RbEq
                                                 (_loc, (
                                                  (Ast.IdLid (_loc, l)) ), e)) :
                                                'field_expr) )) ));
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
                                                ((Ast.RbAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"rec_binding"
                                                      n s) ))) : 'field_expr)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "bi") | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"bi\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "bi") | "anti") as
                                                  n), s) ->
                                                ((Ast.RbAnt
                                                   (_loc, (
                                                    (mk_anti ~c:"rec_binding"
                                                      n s) ))) : 'field_expr)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
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
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_dot_dot :
                                               'opt_dot_dot Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (v :
                                            'opt_dot_dot) ->
                                           fun (m :
                                             'meth_decl) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((m, v) : 'meth_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (meth_decl :
                                               'meth_decl Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_dot_dot :
                                               'opt_dot_dot Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (v :
                                            'opt_dot_dot) ->
                                           fun _ ->
                                            fun (m :
                                              'meth_decl) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((m, v) : 'meth_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (meth_decl :
                                               'meth_decl Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun ((ml, v) :
                                            'meth_list) ->
                                           fun _ ->
                                            fun (m :
                                              'meth_decl) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((( (Ast.TySem (_loc, m, ml))
                                                ), v) : 'meth_list) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (meth_decl : 'meth_decl Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
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
                                            fun (lab :
                                              'a_LIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyCol
                                                 (_loc, (
                                                  (Ast.TyId
                                                    (_loc, (
                                                     (Ast.IdLid (_loc, lab))
                                                     ))) ), t)) : 'meth_decl)
                                          )) ));
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
                                                  'meth_decl)
                                             | _ -> assert false) )) ));
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
                                                    (mk_anti ~c:"ctyp;" n s)
                                                    ))) : 'meth_decl)
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
                                                    ))) : 'meth_decl)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_meth_list : 'opt_meth_list Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_dot_dot :
                                               'opt_dot_dot Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (v :
                                            'opt_dot_dot) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyObj
                                               (_loc, ( (Ast.TyNil (_loc)) ),
                                                v)) : 'opt_meth_list) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (meth_list :
                                               'meth_list Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun ((ml, v) :
                                            'meth_list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyObj (_loc, ml, v)) :
                                              'opt_meth_list) )) ))] ))] )))
                                  () ) ))
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (t : 'poly_type) )) ))] ))] )))
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
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'module_type) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p : 'package_type) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (typevars : 'typevars Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
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
                                               'typevars) )) ));
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
                                                  'typevars)
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
                                                    ))) : 'typevars)
                                             | _ -> assert false) )) ));
                                       (( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'typevars) ->
                                           fun (t1 :
                                             'typevars) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyApp (_loc, t1, t2)) :
                                               'typevars) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (unquoted_typevars :
                                  'unquoted_typevars Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , (
                                      (Some ((FanSig.Grammar.LeftA))) ),
                                      (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'a_ident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.TyId
                                               (_loc, ( (Ast.IdLid (_loc, i))
                                                ))) : 'unquoted_typevars) ))
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
                                                  'unquoted_typevars)
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
                                                    ))) : 'unquoted_typevars)
                                             | _ -> assert false) )) ));
                                       (( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'unquoted_typevars) ->
                                           fun (t1 :
                                             'unquoted_typevars) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyApp (_loc, t1, t2)) :
                                               'unquoted_typevars) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (row_field : 'row_field Gram.Entry.t) ) (
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
                                            (t : 'row_field) )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (amp_ctyp :
                                               'amp_ctyp Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'amp_ctyp) ->
                                           fun _ ->
                                            fun (i :
                                              'a_ident) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyOf
                                                  (_loc, (
                                                   (Ast.TyVrn (_loc, i)) ),
                                                   t)) : 'row_field) )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Skeyword ("&")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (amp_ctyp :
                                               'amp_ctyp Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'amp_ctyp) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (i :
                                               'a_ident) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyOfAmp
                                                   (_loc, (
                                                    (Ast.TyVrn (_loc, i)) ),
                                                    t)) : 'row_field) )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
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
                                             ((Ast.TyVrn (_loc, i)) :
                                               'row_field) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("|"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'row_field) ->
                                           fun _ ->
                                            fun (t1 :
                                              'row_field) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyOr (_loc, t1, t2)) :
                                                'row_field) )) ));
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
                                                    (mk_anti ~c:"ctyp|" n s)
                                                    ))) : 'row_field)
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
                                                    ))) : 'row_field)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (amp_ctyp : 'amp_ctyp Gram.Entry.t) ) (
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
                                            (t : 'amp_ctyp) )) ));
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
                                                    (mk_anti ~c:"ctyp&" n s)
                                                    ))) : 'amp_ctyp)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("&"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'amp_ctyp) ->
                                           fun _ ->
                                            fun (t1 :
                                              'amp_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAmp (_loc, t1, t2)) :
                                                'amp_ctyp) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (name_tags : 'name_tags Gram.Entry.t) ) (
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
                                          fun (i :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyVrn (_loc, i)) :
                                               'name_tags) )) ));
                                       (( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (t2 :
                                            'name_tags) ->
                                           fun (t1 :
                                             'name_tags) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyApp (_loc, t1, t2)) :
                                               'name_tags) )) ));
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
                                                    ))) : 'name_tags)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (eq_expr : 'eq_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (fun i ->
                                             fun p ->
                                              (Ast.PaOlb (_loc, i, p)) :
                                             'eq_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (fun i ->
                                               fun p ->
                                                (Ast.PaOlbi (_loc, i, p, e)) :
                                               'eq_expr) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (patt_tcon : 'patt_tcon Gram.Entry.t) ) (
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
                                            (p : 'patt_tcon) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (p :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaTyc (_loc, p, t)) :
                                                'patt_tcon) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend ( (ipatt : 'ipatt Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt_tcon :
                                               'ipatt_tcon Gram.Entry.t) )))
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
                                               'ipatt_tcon) ->
                                              fun _ ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.PaOlbi
                                                    (_loc, "", p, e)) :
                                                   'ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt_tcon :
                                               'ipatt_tcon Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p :
                                             'ipatt_tcon) ->
                                            fun _ ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.PaOlb (_loc, "", p)) :
                                                 'ipatt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | ANTIQUOT
                                                 ((("" | "lid") as n), i) ->
                                                 ((Ast.PaOlb
                                                    (_loc, ( (mk_anti n i) ),
                                                     ( (Ast.PaNil (_loc)) ))) :
                                                   'ipatt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LIDENT (_) -> (true)
                                            | _ -> (false) ), "LIDENT (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LIDENT (i) ->
                                                 ((Ast.PaOlb
                                                    (_loc, i, (
                                                     (Ast.PaNil (_loc)) ))) :
                                                   'ipatt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt_tcon :
                                               'ipatt_tcon Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (eq_expr :
                                               'eq_expr Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (f :
                                             'eq_expr) ->
                                            fun (p :
                                              'ipatt_tcon) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (__camlp4_0 :
                                                 Gram.Token.t) ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  (match __camlp4_0 with
                                                   | ANTIQUOT
                                                      ((("" | "lid") as n), i) ->
                                                      ((f ( (mk_anti n i) )
                                                         p) : 'ipatt)
                                                   | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | OPTLABEL (_) -> (true)
                                            | _ -> (false) ), "OPTLABEL (_)"))
                                         ); ( (Gram.Skeyword ("(")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt_tcon :
                                               'ipatt_tcon Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (eq_expr :
                                               'eq_expr Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (")")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (f :
                                             'eq_expr) ->
                                            fun (p :
                                              'ipatt_tcon) ->
                                             fun _ ->
                                              fun (__camlp4_0 :
                                                Gram.Token.t) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (match __camlp4_0 with
                                                 | OPTLABEL (i) ->
                                                    ((f i p) : 'ipatt)
                                                 | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LIDENT (_) -> (true)
                                            | _ -> (false) ), "LIDENT (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LIDENT (i) ->
                                                 ((Ast.PaLab
                                                    (_loc, i, (
                                                     (Ast.PaNil (_loc)) ))) :
                                                   'ipatt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | ANTIQUOT
                                                 ((("" | "lid") as n), i) ->
                                                 ((Ast.PaLab
                                                    (_loc, ( (mk_anti n i) ),
                                                     ( (Ast.PaNil (_loc)) ))) :
                                                   'ipatt)
                                              | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         ); ( (Gram.Skeyword (":")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'ipatt) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (match __camlp4_0 with
                                                | ANTIQUOT
                                                   ((("" | "lid") as n), i) ->
                                                   ((Ast.PaLab
                                                      (_loc, ( (mk_anti n i)
                                                       ), p)) : 'ipatt)
                                                | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LABEL (_) -> (true)
                                            | _ -> (false) ), "LABEL (_)"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'ipatt) ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | LABEL (i) ->
                                                 ((Ast.PaLab (_loc, i, p)) :
                                                   'ipatt)
                                              | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (ipatt_tcon : 'ipatt_tcon Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (p :
                                            'ipatt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (p : 'ipatt_tcon) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ipatt : 'ipatt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (t :
                                            'ctyp) ->
                                           fun _ ->
                                            fun (p :
                                              'ipatt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaTyc (_loc, p, t)) :
                                                'ipatt_tcon) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (direction_flag :
                                  'direction_flag Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("to" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"to\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("to" | "anti") as n), s) ->
                                                ((Ast.DiAnt (mk_anti n s)) :
                                                  'direction_flag)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("downto")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.DiDownto) :
                                              'direction_flag) )) ));
                                       (( [( (Gram.Skeyword ("to")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.DiTo) : 'direction_flag) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_private : 'opt_private Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.PrNil) : 'opt_private) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (("private" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"private\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("private" | "anti") as n),
                                                 s) ->
                                                ((Ast.PrAnt (mk_anti n s)) :
                                                  'opt_private)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("private")) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.PrPrivate) : 'opt_private)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_mutable : 'opt_mutable Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.MuNil) : 'opt_mutable) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (("mutable" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"mutable\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("mutable" | "anti") as n),
                                                 s) ->
                                                ((Ast.MuAnt (mk_anti n s)) :
                                                  'opt_mutable)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("mutable")) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.MuMutable) : 'opt_mutable)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_virtual : 'opt_virtual Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.ViNil) : 'opt_virtual) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (("virtual" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"virtual\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("virtual" | "anti") as n),
                                                 s) ->
                                                ((Ast.ViAnt (mk_anti n s)) :
                                                  'opt_virtual)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("virtual")) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ViVirtual) : 'opt_virtual)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_dot_dot : 'opt_dot_dot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.RvNil) : 'opt_dot_dot) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ((".." | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"..\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((".." | "anti") as n), s) ->
                                                ((Ast.RvAnt (mk_anti n s)) :
                                                  'opt_dot_dot)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("..")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.RvRowVar) : 'opt_dot_dot)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_rec : 'opt_rec Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.ReNil) : 'opt_rec) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("rec" | "anti"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"rec\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("rec" | "anti") as n), s) ->
                                                ((Ast.ReAnt (mk_anti n s)) :
                                                  'opt_rec)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("rec")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ReRecursive) : 'opt_rec) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_override : 'opt_override Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.OvNil) : 'opt_override) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("!" | "override") | "anti"),
                                                _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"!\" | \"override\") | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("!" | "override")
                                                   | "anti") as n), s) ->
                                                ((Ast.OvAnt (mk_anti n s)) :
                                                  'opt_override)
                                             | _ -> assert false) )) ));
                                       (( [( (Gram.Skeyword ("!")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.OvOverride) :
                                              'opt_override) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_expr : 'opt_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.ExNil (_loc)) : 'opt_expr)
                                          )) ));
                                       ((
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
                                            (e : 'opt_expr) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend ( (interf : 'interf Gram.Entry.t)
                                ) (
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
                                             | EOI ->
                                                (([] , None ) : 'interf)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sig_item :
                                               'sig_item Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun ((sil, stopped) :
                                            'interf) ->
                                           fun _ ->
                                            fun (si :
                                              'sig_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((( ( si ) :: sil  ), stopped) :
                                                'interf) )) ));
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
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (dp :
                                             'opt_expr) ->
                                            fun (n :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (((
                                                 [( (Ast.SgDir (_loc, n, dp))
                                                  )] ), ( (stopped_at _loc)
                                                 )) : 'interf) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (sig_items : 'sig_items Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist0
                                           (Gram.srules sig_items (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (sig_item :
                                                      'sig_item Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t) )))
                                                )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (sg :
                                                    'sig_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (sg : 'e__12) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'e__12 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.sgSem_of_list l) :
                                              'sig_items) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "sigi") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"sigi\") | \"anti\") | \"list\"), _)"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (sg :
                                            'sig_items) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((((("" | "sigi") | "anti")
                                                     | "list") as n), s) ->
                                                  ((Ast.SgSem
                                                     (_loc, (
                                                      (Ast.SgAnt
                                                        (_loc, (
                                                         (mk_anti n
                                                           ~c:"sig_item" s)
                                                         ))) ), sg)) :
                                                    'sig_items)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "sigi") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"sigi\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "sigi") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.SgAnt
                                                   (_loc, (
                                                    (mk_anti n ~c:"sig_item"
                                                      s) ))) : 'sig_items)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend ( (implem : 'implem Gram.Entry.t)
                                ) (
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
                                             | EOI ->
                                                (([] , None ) : 'implem)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (str_item :
                                               'str_item Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun ((sil, stopped) :
                                            'implem) ->
                                           fun _ ->
                                            fun (si :
                                              'str_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((( ( si ) :: sil  ), stopped) :
                                                'implem) )) ));
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
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (dp :
                                             'opt_expr) ->
                                            fun (n :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (((
                                                 [( (Ast.StDir (_loc, n, dp))
                                                  )] ), ( (stopped_at _loc)
                                                 )) : 'implem) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (str_items : 'str_items Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist0
                                           (Gram.srules str_items (
                                             [((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (str_item :
                                                      'str_item Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t) )))
                                                )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (st :
                                                    'str_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (st : 'e__13) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (l :
                                            'e__13 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.stSem_of_list l) :
                                              'str_items) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "stri") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"stri\") | \"anti\") | \"list\"), _)"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (st :
                                            'str_items) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  ((((("" | "stri") | "anti")
                                                     | "list") as n), s) ->
                                                  ((Ast.StSem
                                                     (_loc, (
                                                      (Ast.StAnt
                                                        (_loc, (
                                                         (mk_anti n
                                                           ~c:"str_item" s)
                                                         ))) ), st)) :
                                                    'str_items)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (((("" | "stri") | "anti")
                                                 | "list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((((\"\" | \"stri\") | \"anti\") | \"list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((((("" | "stri") | "anti")
                                                   | "list") as n), s) ->
                                                ((Ast.StAnt
                                                   (_loc, (
                                                    (mk_anti n ~c:"str_item"
                                                      s) ))) : 'str_items)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
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
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (phrase : 'phrase Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ph :
                                            'phrase) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Some (ph)) : 'top_phrase) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (use_file : 'use_file Gram.Entry.t) ) (
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
                                             | EOI ->
                                                (([] , None ) : 'use_file)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (str_item :
                                               'str_item Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun ((sil, stopped) :
                                            'use_file) ->
                                           fun _ ->
                                            fun (si :
                                              'str_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((( ( si ) :: sil  ), stopped) :
                                                'use_file) )) ));
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
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (dp :
                                             'opt_expr) ->
                                            fun (n :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               (((
                                                 [( (Ast.StDir (_loc, n, dp))
                                                  )] ), ( (stopped_at _loc)
                                                 )) : 'use_file) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend ( (phrase : 'phrase Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (str_item :
                                               'str_item Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (st :
                                             'str_item) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (st : 'phrase) )) ));
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
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (dp :
                                             'opt_expr) ->
                                            fun (n :
                                              'a_LIDENT) ->
                                             fun _ ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.StDir (_loc, n, dp)) :
                                                 'phrase) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend ( (a_INT : 'a_INT Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | INT (_, _) -> (true)
                                            | _ -> (false) ), "INT (_, _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | INT (_, s) -> (s : 'a_INT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "int") | "`int"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"int\") | \"`int\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "int") | "`int") as
                                                  n), s) ->
                                                ((mk_anti n s) : 'a_INT)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_INT32 : 'a_INT32 Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | INT32 (_, _) -> (true)
                                            | _ -> (false) ), "INT32 (_, _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | INT32 (_, s) -> (s : 'a_INT32)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "int32") | "`int32"),
                                                _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"int32\") | \"`int32\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "int32") | "`int32") as
                                                  n), s) ->
                                                ((mk_anti n s) : 'a_INT32)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_INT64 : 'a_INT64 Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | INT64 (_, _) -> (true)
                                            | _ -> (false) ), "INT64 (_, _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | INT64 (_, s) -> (s : 'a_INT64)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "int64") | "`int64"),
                                                _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"int64\") | \"`int64\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "int64") | "`int64") as
                                                  n), s) ->
                                                ((mk_anti n s) : 'a_INT64)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_NATIVEINT : 'a_NATIVEINT Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | NATIVEINT (_, _) -> (true)
                                            | _ -> (false) ),
                                            "NATIVEINT (_, _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | NATIVEINT (_, s) ->
                                                (s : 'a_NATIVEINT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "nativeint")
                                                 | "`nativeint"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"nativeint\") | \"`nativeint\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "nativeint")
                                                   | "`nativeint") as n), s) ->
                                                ((mk_anti n s) :
                                                  'a_NATIVEINT)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_FLOAT : 'a_FLOAT Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | FLOAT (_, _) -> (true)
                                            | _ -> (false) ), "FLOAT (_, _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | FLOAT (_, s) -> (s : 'a_FLOAT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "flo") | "`flo"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"flo\") | \"`flo\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "flo") | "`flo") as
                                                  n), s) ->
                                                ((mk_anti n s) : 'a_FLOAT)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend ( (a_CHAR : 'a_CHAR Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | CHAR (_, _) -> (true)
                                            | _ -> (false) ), "CHAR (_, _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | CHAR (_, s) -> (s : 'a_CHAR)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "chr") | "`chr"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"chr\") | \"`chr\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "chr") | "`chr") as
                                                  n), s) ->
                                                ((mk_anti n s) : 'a_CHAR)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
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
                              (
                              (Gram.extend (
                                (a_LIDENT : 'a_LIDENT Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LIDENT (_) -> (true)
                                            | _ -> (false) ), "LIDENT (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | LIDENT (s) -> (s : 'a_LIDENT)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "lid"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"lid\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("" | "lid") as n), s) ->
                                                ((mk_anti n s) : 'a_LIDENT)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_LABEL : 'a_LABEL Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LABEL (_) -> (true)
                                            | _ -> (false) ), "LABEL (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | LABEL (s) -> (s : 'a_LABEL)
                                             | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("~")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) ); (
                                         (Gram.Skeyword (":")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT (("" as n), s) ->
                                                  ((mk_anti n s) : 'a_LABEL)
                                               | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_OPTLABEL : 'a_OPTLABEL Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | OPTLABEL (_) -> (true)
                                            | _ -> (false) ), "OPTLABEL (_)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | OPTLABEL (s) ->
                                                (s : 'a_OPTLABEL)
                                             | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("?")) ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) ); (
                                         (Gram.Skeyword (":")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT (("" as n), s) ->
                                                  ((mk_anti n s) :
                                                    'a_OPTLABEL)
                                               | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (a_STRING : 'a_STRING Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | STRING (_, _) -> (true)
                                            | _ -> (false) ),
                                            "STRING (_, _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | STRING (_, s) ->
                                                (s : 'a_STRING)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               ((("" | "str") | "`str"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (((\"\" | \"str\") | \"`str\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (((("" | "str") | "`str") as
                                                  n), s) ->
                                                ((mk_anti n s) : 'a_STRING)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (string_list : 'string_list Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | STRING (_, _) -> (true)
                                            | _ -> (false) ),
                                            "STRING (_, _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | STRING (_, x) ->
                                                ((Ast.LCons (x, Ast.LNil )) :
                                                  'string_list)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | STRING (_, _) -> (true)
                                            | _ -> (false) ),
                                            "STRING (_, _)")) ); Gram.Sself ]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (xs :
                                            'string_list) ->
                                           fun (__camlp4_0 :
                                             Gram.Token.t) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | STRING (_, x) ->
                                                 ((Ast.LCons (x, xs)) :
                                                   'string_list)
                                              | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT (("" | "str_list"), _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"\" | \"str_list\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                (("" | "str_list"), s) ->
                                                ((Ast.LAnt
                                                   (mk_anti "str_list" s)) :
                                                  'string_list)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (value_let : 'value_let Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("value")) )] ),
                                        (
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
                                      [(( [( (Gram.Skeyword ("value")) )] ),
                                        (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'value_val) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend ( (semi : 'semi Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword (";")) )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'semi) )) ))] ))] ))) () )
                                ))
                              );
                              (
                              (Gram.extend (
                                (expr_quot : 'expr_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.ExNil (_loc)) : 'expr_quot)
                                          )) ));
                                       ((
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
                                            (e : 'expr_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_expr :
                                               'sem_expr Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'sem_expr) ->
                                           fun _ ->
                                            fun (e1 :
                                              'expr) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.ExSem (_loc, e1, e2)) :
                                                'expr_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (",")) ); (
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
                                              ((Ast.ExCom (_loc, e1, e2)) :
                                                'expr_quot) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (patt_quot : 'patt_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.PaNil (_loc)) : 'patt_quot)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'patt) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'patt_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'patt) ->
                                           fun _ ->
                                            fun (x :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (let i =
                                                (match x with
                                                 | Ast.PaAnt (loc, s) ->
                                                    (Ast.IdAnt (loc, s))
                                                 | p -> (Ast.ident_of_patt p)) in
                                               (Ast.PaEq (_loc, i, y)) :
                                                'patt_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sem_patt :
                                               'sem_patt Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'sem_patt) ->
                                           fun _ ->
                                            fun (x :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaSem (_loc, x, y)) :
                                                'patt_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (",")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_patt :
                                               'comma_patt Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'comma_patt) ->
                                           fun _ ->
                                            fun (x :
                                              'patt) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.PaCom (_loc, x, y)) :
                                                'patt_quot) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (ctyp_quot : 'ctyp_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.TyNil (_loc)) : 'ctyp_quot)
                                          )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'more_ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("and")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAnd (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("&")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (amp_ctyp :
                                               'amp_ctyp Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'amp_ctyp) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyAmp (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("*")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (star_ctyp :
                                               'star_ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'star_ctyp) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TySta (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration_list :
                                               'label_declaration_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (z :
                                            'label_declaration_list) ->
                                           fun _ ->
                                            fun (y :
                                              'more_ctyp) ->
                                             fun _ ->
                                              fun (x :
                                                'more_ctyp) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TySem
                                                   (_loc, (
                                                    (Ast.TyCol (_loc, x, y))
                                                    ), z)) : 'ctyp_quot) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'more_ctyp) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyCol (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Skeyword ("&")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (amp_ctyp :
                                               'amp_ctyp Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword ("|")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (row_field :
                                               'row_field Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (z :
                                            'row_field) ->
                                           fun _ ->
                                            fun (y :
                                              'amp_ctyp) ->
                                             fun _ ->
                                              fun _ ->
                                               fun (x :
                                                 'more_ctyp) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Ast.TyOr
                                                    (_loc, (
                                                     (Ast.TyOfAmp
                                                       (_loc, x, y)) ), z)) :
                                                   'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Skeyword ("&")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (amp_ctyp :
                                               'amp_ctyp Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'amp_ctyp) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (x :
                                               'more_ctyp) ->
                                              fun (_loc :
                                                Gram.Loc.t) ->
                                               ((Ast.TyOfAmp (_loc, x, y)) :
                                                 'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) ); ( (Gram.Skeyword ("|"))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declarations :
                                               'constructor_declarations Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (z :
                                            'constructor_declarations) ->
                                           fun _ ->
                                            fun (y :
                                              'constructor_arg_list) ->
                                             fun _ ->
                                              fun (x :
                                                'more_ctyp) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.TyOr
                                                   (_loc, (
                                                    (Ast.TyOf (_loc, x, y))
                                                    ), z)) : 'ctyp_quot) ))
                                        ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("of")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_arg_list :
                                               'constructor_arg_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'constructor_arg_list) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyOf (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("|")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (constructor_declarations :
                                               'constructor_declarations Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'constructor_declarations) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyOr (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (";")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_declaration_list :
                                               'label_declaration_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'label_declaration_list) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TySem (_loc, x, y)) :
                                                'ctyp_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (more_ctyp :
                                               'more_ctyp Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword (",")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (comma_ctyp :
                                               'comma_ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (y :
                                            'comma_ctyp) ->
                                           fun _ ->
                                            fun (x :
                                              'more_ctyp) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.TyCom (_loc, x, y)) :
                                                'ctyp_quot) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (more_ctyp : 'more_ctyp Gram.Entry.t) ) (
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
                                          fun (x :
                                            'type_parameter) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'more_ctyp) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ctyp : 'ctyp Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'ctyp) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'more_ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyVrn (_loc, x)) :
                                               'more_ctyp) )) ));
                                       ((
                                        [( (Gram.Skeyword ("mutable")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'more_ctyp) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.TyMut (_loc, x)) :
                                               'more_ctyp) )) ))] ))] ))) ()
                                  ) ))
                              );
                              (
                              (Gram.extend (
                                (str_item_quot : 'str_item_quot Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.StNil (_loc)) :
                                             'str_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (str_item :
                                               'str_item Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (st :
                                            'str_item) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (st : 'str_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (str_item :
                                               'str_item Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (st2 :
                                            'str_item_quot) ->
                                           fun _ ->
                                            fun (st1 :
                                              'str_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match st2 with
                                                | Ast.StNil (_) -> st1
                                                | _ ->
                                                   (Ast.StSem
                                                     (_loc, st1, st2))) :
                                                'str_item_quot) )) ));
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
                                               'opt_expr Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (dp :
                                            'opt_expr) ->
                                           fun (n :
                                             'a_LIDENT) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.StDir (_loc, n, dp)) :
                                                'str_item_quot) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (sig_item_quot : 'sig_item_quot Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.SgNil (_loc)) :
                                             'sig_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sig_item :
                                               'sig_item Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (sg :
                                            'sig_item) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (sg : 'sig_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sig_item :
                                               'sig_item Gram.Entry.t) ))) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (sg2 :
                                            'sig_item_quot) ->
                                           fun _ ->
                                            fun (sg1 :
                                              'sig_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match sg2 with
                                                | Ast.SgNil (_) -> sg1
                                                | _ ->
                                                   (Ast.SgSem
                                                     (_loc, sg1, sg2))) :
                                                'sig_item_quot) )) ));
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
                                               'opt_expr Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (dp :
                                            'opt_expr) ->
                                           fun (n :
                                             'a_LIDENT) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.SgDir (_loc, n, dp)) :
                                                'sig_item_quot) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_type_quot :
                                  'module_type_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.MtNil (_loc)) :
                                             'module_type_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'module_type) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'module_type_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_expr_quot :
                                  'module_expr_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.MeNil (_loc)) :
                                             'module_expr_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'module_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'module_expr_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (match_case_quot :
                                  'match_case_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.McNil (_loc)) :
                                             'match_case_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Slist0sep
                                           ((
                                            (Gram.Snterm
                                              (Gram.Entry.obj (
                                                (match_case0 :
                                                  'match_case0 Gram.Entry.t)
                                                ))) ), (
                                            (Gram.Skeyword ("|")) ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'match_case0 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.mcOr_of_list x) :
                                              'match_case_quot) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (binding_quot : 'binding_quot Gram.Entry.t) )
                                (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.BiNil (_loc)) :
                                             'binding_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (binding :
                                               'binding Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'binding) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'binding_quot) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (rec_binding_quot :
                                  'rec_binding_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.RbNil (_loc)) :
                                             'rec_binding_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (label_expr_list :
                                               'label_expr_list Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'label_expr_list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'rec_binding_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (module_binding_quot :
                                  'module_binding_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.MbNil (_loc)) :
                                             'module_binding_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun _ ->
                                              fun (m :
                                                'a_UIDENT) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Ast.MbColEq
                                                   (_loc, m, mt, me)) :
                                                  'module_binding_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_UIDENT :
                                               'a_UIDENT Gram.Entry.t) ))) );
                                         ( (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (m :
                                              'a_UIDENT) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MbCol (_loc, m, mt)) :
                                                'module_binding_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) ); (
                                         (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_expr :
                                               'module_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (me :
                                            'module_expr) ->
                                           fun _ ->
                                            fun (mt :
                                              'module_type) ->
                                             fun _ ->
                                              fun (__camlp4_0 :
                                                Gram.Token.t) ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (match __camlp4_0 with
                                                 | ANTIQUOT (("" as n), m) ->
                                                    ((Ast.MbColEq
                                                       (_loc, ( (mk_anti n m)
                                                        ), mt, me)) :
                                                      'module_binding_quot)
                                                 | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) ); (
                                         (Gram.Skeyword (":")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (module_type :
                                               'module_type Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (mt :
                                            'module_type) ->
                                           fun _ ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT (("" as n), m) ->
                                                  ((Ast.MbCol
                                                     (_loc, ( (mk_anti n m)
                                                      ), mt)) :
                                                    'module_binding_quot)
                                               | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("", _) -> (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"\", _)")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT (("" as n), s) ->
                                                ((Ast.MbAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"module_binding" n
                                                      s) ))) :
                                                  'module_binding_quot)
                                             | _ -> assert false) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT
                                               (("module_binding" | "anti"),
                                                _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT ((\"module_binding\" | \"anti\"), _)"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | ANTIQUOT
                                                ((("module_binding" | "anti") as
                                                  n), s) ->
                                                ((Ast.MbAnt
                                                   (_loc, (
                                                    (mk_anti
                                                      ~c:"module_binding" n
                                                      s) ))) :
                                                  'module_binding_quot)
                                             | _ -> assert false) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (b2 :
                                            'module_binding_quot) ->
                                           fun _ ->
                                            fun (b1 :
                                              'module_binding_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.MbAnd (_loc, b1, b2)) :
                                                'module_binding_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (ident_quot : 'ident_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(( (Some ("apply")) ), None , (
                                      [(( [Gram.Sself ; Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'ident_quot) ->
                                           fun (i :
                                             'ident_quot) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.IdApp (_loc, i, j)) :
                                               'ident_quot) )) ))] ));
                                     (( (Some (".")) ), None , (
                                      [((
                                        [Gram.Sself ; ( (Gram.Skeyword ("."))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (j :
                                            'ident_quot) ->
                                           fun _ ->
                                            fun (i :
                                              'ident_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.IdAcc (_loc, i, j)) :
                                                'ident_quot) )) ))] ));
                                     (( (Some ("simple")) ), None , (
                                      [((
                                        [( (Gram.Skeyword ("(")) );
                                         Gram.Sself ; ( (Gram.Skeyword (")"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (i :
                                             'ident_quot) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (i : 'ident_quot) )) ));
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
                                            'ident_quot) ->
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
                                                    'ident_quot)
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
                                              'ident_quot) )) ));
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
                                              'ident_quot) )) ));
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
                                                    ))) : 'ident_quot)
                                             | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_expr_quot :
                                  'class_expr_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.CeNil (_loc)) :
                                             'class_expr_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_expr :
                                               'class_expr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'class_expr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'class_expr_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("virtual", _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"virtual\", _)")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ident : 'ident Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_comma_ctyp :
                                               'opt_comma_ctyp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ot :
                                            'opt_comma_ctyp) ->
                                           fun (i :
                                             'ident) ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  (("virtual" as n), s) ->
                                                  (let anti =
                                                    (Ast.ViAnt
                                                      (mk_anti
                                                        ~c:"class_expr" n s)) in
                                                   (Ast.CeCon
                                                     (_loc, anti, i, ot)) :
                                                    'class_expr_quot)
                                               | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("virtual")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_name_and_param :
                                               'class_name_and_param Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun ((i, ot) :
                                            'class_name_and_param) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CeCon
                                                (_loc, Ast.ViVirtual , (
                                                 (Ast.IdLid (_loc, i)) ), ot)) :
                                               'class_expr_quot) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("="))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ce2 :
                                            'class_expr_quot) ->
                                           fun _ ->
                                            fun (ce1 :
                                              'class_expr_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CeEq (_loc, ce1, ce2)) :
                                                'class_expr_quot) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ce2 :
                                            'class_expr_quot) ->
                                           fun _ ->
                                            fun (ce1 :
                                              'class_expr_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CeAnd (_loc, ce1, ce2)) :
                                                'class_expr_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_type_quot :
                                  'class_type_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.CtNil (_loc)) :
                                             'class_type_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_type_plus :
                                               'class_type_plus Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'class_type_plus) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'class_type_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Stoken
                                           ((
                                            function
                                            | ANTIQUOT ("virtual", _) ->
                                               (true)
                                            | _ -> (false) ),
                                            "ANTIQUOT (\"virtual\", _)")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (ident : 'ident Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_comma_ctyp :
                                               'opt_comma_ctyp Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (ot :
                                            'opt_comma_ctyp) ->
                                           fun (i :
                                             'ident) ->
                                            fun (__camlp4_0 :
                                              Gram.Token.t) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (match __camlp4_0 with
                                               | ANTIQUOT
                                                  (("virtual" as n), s) ->
                                                  (let anti =
                                                    (Ast.ViAnt
                                                      (mk_anti
                                                        ~c:"class_type" n s)) in
                                                   (Ast.CtCon
                                                     (_loc, anti, i, ot)) :
                                                    'class_type_quot)
                                               | _ -> assert false) )) ));
                                       ((
                                        [( (Gram.Skeyword ("virtual")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_name_and_param :
                                               'class_name_and_param Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun ((i, ot) :
                                            'class_name_and_param) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.CtCon
                                                (_loc, Ast.ViVirtual , (
                                                 (Ast.IdLid (_loc, i)) ), ot)) :
                                               'class_type_quot) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword (":"))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ct2 :
                                            'class_type_quot) ->
                                           fun _ ->
                                            fun (ct1 :
                                              'class_type_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtCol (_loc, ct1, ct2)) :
                                                'class_type_quot) )) ));
                                       ((
                                        [Gram.Sself ; ( (Gram.Skeyword ("="))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ct2 :
                                            'class_type_quot) ->
                                           fun _ ->
                                            fun (ct1 :
                                              'class_type_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtEq (_loc, ct1, ct2)) :
                                                'class_type_quot) )) ));
                                       ((
                                        [Gram.Sself ; (
                                         (Gram.Skeyword ("and")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (ct2 :
                                            'class_type_quot) ->
                                           fun _ ->
                                            fun (ct1 :
                                              'class_type_quot) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((Ast.CtAnd (_loc, ct1, ct2)) :
                                                'class_type_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_str_item_quot :
                                  'class_str_item_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.CrNil (_loc)) :
                                             'class_str_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_str_item :
                                               'class_str_item Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'class_str_item) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'class_str_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_str_item :
                                               'class_str_item Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (x2 :
                                            'class_str_item_quot) ->
                                           fun _ ->
                                            fun (x1 :
                                              'class_str_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match x2 with
                                                | Ast.CrNil (_) -> x1
                                                | _ ->
                                                   (Ast.CrSem (_loc, x1, x2))) :
                                                'class_str_item_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (class_sig_item_quot :
                                  'class_sig_item_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.CgNil (_loc)) :
                                             'class_sig_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_sig_item :
                                               'class_sig_item Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'class_sig_item) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'class_sig_item_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (class_sig_item :
                                               'class_sig_item Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (semi : 'semi Gram.Entry.t) )))
                                         ); Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (x2 :
                                            'class_sig_item_quot) ->
                                           fun _ ->
                                            fun (x1 :
                                              'class_sig_item) ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((match x2 with
                                                | Ast.CgNil (_) -> x1
                                                | _ ->
                                                   (Ast.CgSem (_loc, x1, x2))) :
                                                'class_sig_item_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (with_constr_quot :
                                  'with_constr_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((Ast.WcNil (_loc)) :
                                             'with_constr_quot) )) ));
                                       ((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (with_constr :
                                               'with_constr Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'with_constr) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'with_constr_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (rec_flag_quot : 'rec_flag_quot Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_rec :
                                               'opt_rec Gram.Entry.t) ))) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'opt_rec) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'rec_flag_quot) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (direction_flag_quot :
                                  'direction_flag_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (direction_flag :
                                               'direction_flag Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'direction_flag) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'direction_flag_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (mutable_flag_quot :
                                  'mutable_flag_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_mutable :
                                               'opt_mutable Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'opt_mutable) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'mutable_flag_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (private_flag_quot :
                                  'private_flag_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_private :
                                               'opt_private Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'opt_private) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'private_flag_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (virtual_flag_quot :
                                  'virtual_flag_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_virtual :
                                               'opt_virtual Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'opt_virtual) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'virtual_flag_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (row_var_flag_quot :
                                  'row_var_flag_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_dot_dot :
                                               'opt_dot_dot Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'opt_dot_dot) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'row_var_flag_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (override_flag_quot :
                                  'override_flag_quot Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_override :
                                               'opt_override Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'opt_override) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (x : 'override_flag_quot) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (patt_eoi : 'patt_eoi Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (patt : 'patt Gram.Entry.t) )))
                                         ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | EOI -> (true)
                                            | _ -> (false) ), "EOI")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (x :
                                             'patt) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | EOI -> (x : 'patt_eoi)
                                              | _ -> assert false) )) ))] ))]
                                    ))) () ) ))
                              );
                              (Gram.extend (
                                (expr_eoi : 'expr_eoi Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t) )))
                                         ); (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | EOI -> (true)
                                            | _ -> (false) ), "EOI")) )] ), (
                                        (Gram.Action.mk (
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (x :
                                             'expr) ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (match __camlp4_0 with
                                              | EOI -> (x : 'expr_eoi)
                                              | _ -> assert false) )) ))] ))]
                                    ))) () ) ))

                     end
