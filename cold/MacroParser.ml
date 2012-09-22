open FanSig

module Id =
              struct
               let name = "Camlp4MacroParser"

               let version = Sys.ocaml_version

              end

module MakeMacroParser =
                    functor (Syntax : Camlp4.Sig.Camlp4Syntax) ->
                     struct
                      open Camlp4.Sig

                      include Syntax

                      type 'a item_or_def =
                         SdStr of 'a
                       | SdDef of string * (string list * Ast.expr) option
                       | SdUnd of string
                       | SdITE of bool * 'a item_or_def list *
                          'a item_or_def list
                       | SdLazy of 'a Lazy.t

                      let rec list_remove =
                       fun x ->
                        function
                        | ((y, _) :: l) when (y = x) -> l
                        | (d :: l) -> ( d ) :: (list_remove x l) 
                        | [] -> ([])

                      let defined = (ref [] )

                      let is_defined =
                       fun i -> (List.mem_assoc i ( !defined ))

                      let bad_patt =
                       fun _loc ->
                        (Loc.raise _loc (
                          (Failure
                            ("this macro cannot be used in a pattern (see its definition)"))
                          ))

                      let substp =
                       fun _loc ->
                        fun env ->
                         let rec loop =
                          function
                          | Ast.ExApp (_, e1, e2) ->
                             (Ast.PaApp (_loc, ( (loop e1) ), ( (loop e2) )))
                          | Ast.ExNil (_) -> (Ast.PaNil (_loc))
                          | Ast.ExId (_, Ast.IdLid (_, x)) ->
                             (try (List.assoc x env) with
                              Not_found ->
                               (Ast.PaId (_loc, ( (Ast.IdLid (_loc, x)) ))))
                          | Ast.ExId (_, Ast.IdUid (_, x)) ->
                             (try (List.assoc x env) with
                              Not_found ->
                               (Ast.PaId (_loc, ( (Ast.IdUid (_loc, x)) ))))
                          | Ast.ExInt (_, x) -> (Ast.PaInt (_loc, x))
                          | Ast.ExStr (_, s) -> (Ast.PaStr (_loc, s))
                          | Ast.ExTup (_, x) ->
                             (Ast.PaTup (_loc, ( (loop x) )))
                          | Ast.ExCom (_, x1, x2) ->
                             (Ast.PaCom (_loc, ( (loop x1) ), ( (loop x2) )))
                          | Ast.ExRec (_, bi, Ast.ExNil (_)) ->
                             let rec substbi =
                              function
                              | Ast.RbSem (_, b1, b2) ->
                                 (Ast.PaSem
                                   (_loc, ( (substbi b1) ), ( (substbi b2) )))
                              | Ast.RbEq (_, i, e) ->
                                 (Ast.PaEq (_loc, i, ( (loop e) )))
                              | _ -> (bad_patt _loc) in
                             (Ast.PaRec (_loc, ( (substbi bi) )))
                          | _ -> (bad_patt _loc) in
                         loop

                      class reloc _loc =
                       object
                        inherit Ast.map as super
                        method loc = fun _ -> _loc
                       end

                      class subst _loc env =
                       object
                        inherit (reloc _loc) as super
                        method expr =
                         function
                         | ((Ast.ExId (_, Ast.IdLid (_, x))
                             | Ast.ExId (_, Ast.IdUid (_, x))) as e) ->
                            (try (List.assoc x env) with
                             Not_found -> (super#expr e))
                         | ((Ast.ExApp
                              (_loc,
                               Ast.ExId (_, Ast.IdUid (_, "LOCATION_OF")),
                               Ast.ExId (_, Ast.IdLid (_, x)))
                             | Ast.ExApp
                                (_loc,
                                 Ast.ExId (_, Ast.IdUid (_, "LOCATION_OF")),
                                 Ast.ExId (_, Ast.IdUid (_, x)))) as e) ->
                            (try
                              let loc =
                               (Ast.loc_of_expr ( (List.assoc x env) )) in
                              let (a, b, c, d, e, f, g, h) =
                               (Loc.to_tuple loc) in
                              (Ast.ExApp
                                (_loc, (
                                 (Ast.ExId
                                   (_loc, (
                                    (Ast.IdAcc
                                      (_loc, ( (Ast.IdUid (_loc, "Loc")) ), (
                                       (Ast.IdLid (_loc, "of_tuple")) ))) )))
                                 ), (
                                 (Ast.ExTup
                                   (_loc, (
                                    (Ast.ExCom
                                      (_loc, (
                                       (Ast.ExStr
                                         (_loc, ( (Ast.safe_string_escaped a)
                                          ))) ), (
                                       (Ast.ExCom
                                         (_loc, (
                                          (Ast.ExCom
                                            (_loc, (
                                             (Ast.ExCom
                                               (_loc, (
                                                (Ast.ExCom
                                                  (_loc, (
                                                   (Ast.ExCom
                                                     (_loc, (
                                                      (Ast.ExCom
                                                        (_loc, (
                                                         (Ast.ExInt
                                                           (_loc, (
                                                            (string_of_int b)
                                                            ))) ), (
                                                         (Ast.ExInt
                                                           (_loc, (
                                                            (string_of_int c)
                                                            ))) ))) ), (
                                                      (Ast.ExInt
                                                        (_loc, (
                                                         (string_of_int d) )))
                                                      ))) ), (
                                                   (Ast.ExInt
                                                     (_loc, (
                                                      (string_of_int e) )))
                                                   ))) ), (
                                                (Ast.ExInt
                                                  (_loc, ( (string_of_int f)
                                                   ))) ))) ), (
                                             (Ast.ExInt
                                               (_loc, ( (string_of_int g) )))
                                             ))) ), (
                                          if h then
                                           (
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdUid (_loc, "True")) )))
                                           )
                                          else
                                           (Ast.ExId
                                             (_loc, (
                                              (Ast.IdUid (_loc, "False")) )))
                                          ))) ))) ))) )))
                             with
                             Not_found -> (super#expr e))
                         | e -> (super#expr e)
                        method patt =
                         function
                         | ((Ast.PaId (_, Ast.IdLid (_, x))
                             | Ast.PaId (_, Ast.IdUid (_, x))) as p) ->
                            (try
                              (substp _loc []  ( (List.assoc x env) ))
                             with
                             Not_found -> (super#patt p))
                         | p -> (super#patt p)
                       end

                      let incorrect_number =
                       fun loc ->
                        fun l1 ->
                         fun l2 ->
                          (Loc.raise loc (
                            (Failure
                              (Printf.sprintf
                                "expected %d parameters; found %d" (
                                (List.length l2) ) ( (List.length l1) ))) ))

                      let define =
                       fun eo ->
                        fun x ->
                         (
                         (match eo with
                          | Some ([], e) ->
                             (
                             (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                               ((fun ()
                                   ->
                                  ((
                                   (Some ((FanSig.Grammar.Level ("simple"))))
                                   ), (
                                   [(None , None , (
                                     [((
                                       [(
                                        (Gram.Stoken
                                          ((
                                           function
                                           | UIDENT (camlp4_x) when
                                              (camlp4_x = x) ->
                                              (true)
                                           | _ -> (false) ), "$UIDENT x")) )]
                                       ), (
                                       (Gram.Action.mk (
                                         fun (__camlp4_0 :
                                           Gram.Token.t) ->
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (match __camlp4_0 with
                                            | UIDENT (_) ->
                                               (((((new reloc) _loc)#expr) e) :
                                                 'expr)
                                            | _ -> assert false) )) ))] ))]
                                   ))) () ) ))
                             );
                             (Gram.extend ( (patt : 'patt Gram.Entry.t) ) (
                               ((fun ()
                                   ->
                                  ((
                                   (Some ((FanSig.Grammar.Level ("simple"))))
                                   ), (
                                   [(None , None , (
                                     [((
                                       [(
                                        (Gram.Stoken
                                          ((
                                           function
                                           | UIDENT (camlp4_x) when
                                              (camlp4_x = x) ->
                                              (true)
                                           | _ -> (false) ), "$UIDENT x")) )]
                                       ), (
                                       (Gram.Action.mk (
                                         fun (__camlp4_0 :
                                           Gram.Token.t) ->
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           (match __camlp4_0 with
                                            | UIDENT (_) ->
                                               (let p = (substp _loc []  e) in
                                                ((((new reloc) _loc)#patt) p) :
                                                 'patt)
                                            | _ -> assert false) )) ))] ))]
                                   ))) () ) ))
                          | Some (sl, e) ->
                             (
                             (Gram.extend ( (expr : 'expr Gram.Entry.t) ) (
                               ((fun ()
                                   ->
                                  ((
                                   (Some ((FanSig.Grammar.Level ("apply"))))
                                   ), (
                                   [(None , None , (
                                     [((
                                       [(
                                        (Gram.Stoken
                                          ((
                                           function
                                           | UIDENT (camlp4_x) when
                                              (camlp4_x = x) ->
                                              (true)
                                           | _ -> (false) ), "$UIDENT x")) );
                                        Gram.Sself ] ), (
                                       (Gram.Action.mk (
                                         fun (param :
                                           'expr) ->
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | UIDENT (_) ->
                                                (let el =
                                                  (match param with
                                                   | Ast.ExTup (_, e) ->
                                                      (Ast.list_of_expr e []
                                                        )
                                                   | e -> [e]) in
                                                 if (( (List.length el) ) = (
                                                      (List.length sl) )) then
                                                  (
                                                  let env =
                                                   (List.combine sl el) in
                                                  ((((new subst) _loc env)
                                                    #expr) e)
                                                  )
                                                 else
                                                  (incorrect_number _loc el
                                                    sl) : 'expr)
                                             | _ -> assert false) )) ))] ))]
                                   ))) () ) ))
                             );
                             (Gram.extend ( (patt : 'patt Gram.Entry.t) ) (
                               ((fun ()
                                   ->
                                  ((
                                   (Some ((FanSig.Grammar.Level ("simple"))))
                                   ), (
                                   [(None , None , (
                                     [((
                                       [(
                                        (Gram.Stoken
                                          ((
                                           function
                                           | UIDENT (camlp4_x) when
                                              (camlp4_x = x) ->
                                              (true)
                                           | _ -> (false) ), "$UIDENT x")) );
                                        Gram.Sself ] ), (
                                       (Gram.Action.mk (
                                         fun (param :
                                           'patt) ->
                                          fun (__camlp4_0 :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (match __camlp4_0 with
                                             | UIDENT (_) ->
                                                (let pl =
                                                  (match param with
                                                   | Ast.PaTup (_, p) ->
                                                      (Ast.list_of_patt p []
                                                        )
                                                   | p -> [p]) in
                                                 if (( (List.length pl) ) = (
                                                      (List.length sl) )) then
                                                  (
                                                  let env =
                                                   (List.combine sl pl) in
                                                  let p = (substp _loc env e) in
                                                  ((((new reloc) _loc)#patt)
                                                    p)
                                                  )
                                                 else
                                                  (incorrect_number _loc pl
                                                    sl) : 'patt)
                                             | _ -> assert false) )) ))] ))]
                                   ))) () ) ))
                          | None -> ())
                         );
                         (defined := ( ( (x, eo) ) :: !defined  ))

                      let undef =
                       fun x ->
                        (try
                          (
                         let eo = (List.assoc x ( !defined )) in
                         (match eo with
                          | Some ([], _) ->
                             (
                             (Gram.delete_rule expr (
                               [(
                                (Gram.Stoken
                                  ((
                                   function
                                   | UIDENT (camlp4_x) when (camlp4_x = x) ->
                                      (true)
                                   | _ -> (false) ), "$UIDENT x")) )] ))
                             );
                             (Gram.delete_rule patt (
                               [(
                                (Gram.Stoken
                                  ((
                                   function
                                   | UIDENT (camlp4_x) when (camlp4_x = x) ->
                                      (true)
                                   | _ -> (false) ), "$UIDENT x")) )] ))
                          | Some (_, _) ->
                             (
                             (Gram.delete_rule expr (
                               [(
                                (Gram.Stoken
                                  ((
                                   function
                                   | UIDENT (camlp4_x) when (camlp4_x = x) ->
                                      (true)
                                   | _ -> (false) ), "$UIDENT x")) );
                                Gram.Sself ] ))
                             );
                             (Gram.delete_rule patt (
                               [(
                                (Gram.Stoken
                                  ((
                                   function
                                   | UIDENT (camlp4_x) when (camlp4_x = x) ->
                                      (true)
                                   | _ -> (false) ), "$UIDENT x")) );
                                Gram.Sself ] ))
                          | None -> ())
                         );
                          (defined := ( (list_remove x ( !defined )) ))
                         with
                         Not_found -> ())

                      let parse_def =
                       fun s ->
                        (match
                           (Gram.parse_string expr (
                             (Loc.mk "<command line>") ) s) with
                         | Ast.ExId (_, Ast.IdUid (_, n)) -> (define None  n)
                         | Ast.ExApp
                            (_,
                             Ast.ExApp
                              (_, Ast.ExId (_, Ast.IdLid (_, "=")),
                               Ast.ExId (_, Ast.IdUid (_, n))), e) ->
                            (define ( (Some ([] , e)) ) n)
                         | _ -> (invalid_arg s))

                      let include_dirs = (ref [] )

                      let add_include_dir =
                       fun str ->
                        if (str <> "") then
                         (
                         let str =
                          if ((
                               (String.get str (
                                 (( (String.length str) ) - 1) )) ) = '/') then
                           str
                          else (str ^ "/") in
                         (include_dirs := ( (( !include_dirs ) @ ( [str] ))
                           ))
                         )
                        else ()

                      let parse_include_file =
                       fun rule ->
                        let dir_ok =
                         fun file ->
                          fun dir -> (Sys.file_exists ( (dir ^ file) )) in
                        fun file ->
                         let file =
                          (try
                            ((
                              (List.find ( (dir_ok file) ) (
                                (( !include_dirs ) @ ( ["./"] )) )) ) ^ file)
                           with
                           Not_found -> file) in
                         let ch = (open_in file) in
                         let st = (Stream.of_channel ch) in
                         (Gram.parse rule ( (Loc.mk file) ) st)

                      let rec execute_macro =
                       fun nil ->
                        fun cons ->
                         function
                         | SdStr (i) -> i
                         | SdDef (x, eo) -> ( (define eo x) ); nil
                         | SdUnd (x) -> ( (undef x) ); nil
                         | SdITE (b, l1, l2) ->
                            (execute_macro_list nil cons (
                              if b then l1 else l2 ))
                         | SdLazy (l) -> (Lazy.force l)
                      and execute_macro_list =
                       fun nil ->
                        fun cons ->
                         function
                         | [] -> nil
                         | (hd :: tl) ->
                            let il1 = (execute_macro nil cons hd) in
                            let il2 = (execute_macro_list nil cons tl) in
                            (cons il1 il2)

                      let stack = (Stack.create () )

                      let make_SdITE_result =
                       fun st1 ->
                        fun st2 ->
                         let test = (Stack.pop stack) in
                         (SdITE (test, st1, st2))

                      type branch = Then | Else

                      let execute_macro_if_active_branch =
                       fun _loc ->
                        fun nil ->
                         fun cons ->
                          fun branch ->
                           fun macro_def ->
                            let test = (Stack.top stack) in
                            let item =
                             if (( (test && ( (branch = Then ) )) ) || (
                                  (( (not test) ) && ( (branch = Else )
                                    )) )) then
                              (
                              (execute_macro nil cons macro_def)
                              )
                             else nil in
                            (SdStr (item))

                      let _ = let _ = (expr : 'expr Gram.Entry.t)
                              and _ = (sig_item : 'sig_item Gram.Entry.t)
                              and _ = (str_item : 'str_item Gram.Entry.t)
                              and _ = (patt : 'patt Gram.Entry.t) in
                              let grammar_entry_create = Gram.Entry.mk in
                              let macro_def =
                               ((grammar_entry_create "macro_def") :
                                 'macro_def Gram.Entry.t)
                              and uident =
                               ((grammar_entry_create "uident") :
                                 'uident Gram.Entry.t)
                              and opt_macro_value =
                               ((grammar_entry_create "opt_macro_value") :
                                 'opt_macro_value Gram.Entry.t)
                              and endif =
                               ((grammar_entry_create "endif") :
                                 'endif Gram.Entry.t)
                              and sglist_else =
                               ((grammar_entry_create "sglist_else") :
                                 'sglist_else Gram.Entry.t)
                              and sglist_then =
                               ((grammar_entry_create "sglist_then") :
                                 'sglist_then Gram.Entry.t)
                              and smlist_else =
                               ((grammar_entry_create "smlist_else") :
                                 'smlist_else Gram.Entry.t)
                              and smlist_then =
                               ((grammar_entry_create "smlist_then") :
                                 'smlist_then Gram.Entry.t)
                              and else_expr =
                               ((grammar_entry_create "else_expr") :
                                 'else_expr Gram.Entry.t)
                              and else_macro_def_sig =
                               ((grammar_entry_create
                                  "else_macro_def_sig") :
                                 'else_macro_def_sig Gram.Entry.t)
                              and else_macro_def =
                               ((grammar_entry_create "else_macro_def") :
                                 'else_macro_def Gram.Entry.t)
                              and uident_eval_ifndef =
                               ((grammar_entry_create
                                  "uident_eval_ifndef") :
                                 'uident_eval_ifndef Gram.Entry.t)
                              and uident_eval_ifdef =
                               ((grammar_entry_create
                                  "uident_eval_ifdef") :
                                 'uident_eval_ifdef Gram.Entry.t)
                              and macro_def_sig =
                               ((grammar_entry_create "macro_def_sig") :
                                 'macro_def_sig Gram.Entry.t) in
                              (
                              (Gram.extend (
                                (str_item : 'str_item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (( (Some ((FanSig.Grammar.First))) ),
                                    (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (macro_def :
                                               'macro_def Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'macro_def) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((execute_macro (
                                               (Ast.StNil (_loc)) ) (
                                               fun a ->
                                                fun b ->
                                                 (Ast.StSem (_loc, a, b))
                                               ) x) : 'str_item) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (sig_item : 'sig_item Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (( (Some ((FanSig.Grammar.First))) ),
                                    (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (macro_def_sig :
                                               'macro_def_sig Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (x :
                                            'macro_def_sig) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((execute_macro (
                                               (Ast.SgNil (_loc)) ) (
                                               fun a ->
                                                fun b ->
                                                 (Ast.SgSem (_loc, a, b))
                                               ) x) : 'sig_item) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (macro_def : 'macro_def Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("INCLUDE")) );
                                         (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | STRING (_) -> (true)
                                            | _ -> (false) ), "STRING _"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (fname :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (let fname =
                                               (Gram.Token.extract_string
                                                 fname) in
                                              (SdLazy
                                                (lazy (
                                                  (parse_include_file
                                                    str_items fname) ))) :
                                               'macro_def) )) ));
                                       ((
                                        [( (Gram.Skeyword ("IFNDEF")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident_eval_ifndef :
                                               'uident_eval_ifndef Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("THEN")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (smlist_then :
                                               'smlist_then Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (else_macro_def :
                                               'else_macro_def Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (st2 :
                                            'else_macro_def) ->
                                           fun (st1 :
                                             'smlist_then) ->
                                            fun _ ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((make_SdITE_result st1
                                                   st2) : 'macro_def) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("IFDEF")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident_eval_ifdef :
                                               'uident_eval_ifdef Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("THEN")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (smlist_then :
                                               'smlist_then Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (else_macro_def :
                                               'else_macro_def Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (st2 :
                                            'else_macro_def) ->
                                           fun (st1 :
                                             'smlist_then) ->
                                            fun _ ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((make_SdITE_result st1
                                                   st2) : 'macro_def) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("UNDEF")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'uident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((SdUnd (i)) : 'macro_def)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("DEFINE")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (opt_macro_value :
                                               'opt_macro_value Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (def :
                                            'opt_macro_value) ->
                                           fun (i :
                                             'uident) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              ((SdDef (i, def)) :
                                                'macro_def) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (macro_def_sig :
                                  'macro_def_sig Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("INCLUDE")) );
                                         (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | STRING (_) -> (true)
                                            | _ -> (false) ), "STRING _"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (fname :
                                            Gram.Token.t) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             (let fname =
                                               (Gram.Token.extract_string
                                                 fname) in
                                              (SdLazy
                                                (lazy (
                                                  (parse_include_file
                                                    sig_items fname) ))) :
                                               'macro_def_sig) )) ));
                                       ((
                                        [( (Gram.Skeyword ("IFNDEF")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident_eval_ifndef :
                                               'uident_eval_ifndef Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("THEN")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sglist_then :
                                               'sglist_then Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (else_macro_def_sig :
                                               'else_macro_def_sig Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (sg2 :
                                            'else_macro_def_sig) ->
                                           fun (sg1 :
                                             'sglist_then) ->
                                            fun _ ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((make_SdITE_result sg1
                                                   sg2) : 'macro_def_sig)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("IFDEF")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident_eval_ifdef :
                                               'uident_eval_ifdef Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Skeyword ("THEN")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sglist_then :
                                               'sglist_then Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (else_macro_def_sig :
                                               'else_macro_def_sig Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (sg2 :
                                            'else_macro_def_sig) ->
                                           fun (sg1 :
                                             'sglist_then) ->
                                            fun _ ->
                                             fun _ ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((make_SdITE_result sg1
                                                   sg2) : 'macro_def_sig)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("UNDEF")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'uident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((SdUnd (i)) :
                                               'macro_def_sig) )) ));
                                       ((
                                        [( (Gram.Skeyword ("DEFINE")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'uident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((SdDef (i, None )) :
                                               'macro_def_sig) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (uident_eval_ifdef :
                                  'uident_eval_ifdef Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'uident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Stack.push ( (is_defined i)
                                               ) stack) :
                                              'uident_eval_ifdef) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (uident_eval_ifndef :
                                  'uident_eval_ifndef Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            'uident) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Stack.push (
                                               (not ( (is_defined i) )) )
                                               stack) :
                                              'uident_eval_ifndef) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (else_macro_def :
                                  'else_macro_def Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (([]) : 'else_macro_def) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("ELSE")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (smlist_else :
                                               'smlist_else Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (st :
                                             'smlist_else) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (st : 'else_macro_def) ))
                                        ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (else_macro_def_sig :
                                  'else_macro_def_sig Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (([]) : 'else_macro_def_sig)
                                          )) ));
                                       ((
                                        [( (Gram.Skeyword ("ELSE")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (sglist_else :
                                               'sglist_else Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (st :
                                             'sglist_else) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (st : 'else_macro_def_sig)
                                          )) ))] ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (else_expr : 'else_expr Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            ((Ast.ExId
                                               (_loc, (
                                                (Ast.IdUid (_loc, "()"))
                                                ))) : 'else_expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("ELSE")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t)
                                             ))) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (e :
                                             'expr) ->
                                            fun _ ->
                                             fun (_loc :
                                               Gram.Loc.t) ->
                                              (e : 'else_expr) )) ))] ))]
                                    ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (smlist_then : 'smlist_then Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist1
                                           (Gram.srules smlist_then (
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
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (si :
                                                    'str_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((SdStr (si)) :
                                                      'e__1) )) ));
                                              ((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (macro_def :
                                                      'macro_def Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (d :
                                                    'macro_def) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((execute_macro_if_active_branch
                                                       _loc (
                                                       (Ast.StNil (_loc))
                                                       ) (
                                                       fun a ->
                                                        fun b ->
                                                         (Ast.StSem
                                                           (_loc, a, b))
                                                       ) Then  d) :
                                                      'e__1) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (sml :
                                            'e__1 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (sml : 'smlist_then) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (smlist_else : 'smlist_else Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist1
                                           (Gram.srules smlist_else (
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
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (si :
                                                    'str_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((SdStr (si)) :
                                                      'e__2) )) ));
                                              ((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (macro_def :
                                                      'macro_def Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (d :
                                                    'macro_def) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((execute_macro_if_active_branch
                                                       _loc (
                                                       (Ast.StNil (_loc))
                                                       ) (
                                                       fun a ->
                                                        fun b ->
                                                         (Ast.StSem
                                                           (_loc, a, b))
                                                       ) Else  d) :
                                                      'e__2) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (sml :
                                            'e__2 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (sml : 'smlist_else) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (sglist_then : 'sglist_then Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist1
                                           (Gram.srules sglist_then (
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
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (si :
                                                    'sig_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((SdStr (si)) :
                                                      'e__3) )) ));
                                              ((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (macro_def_sig :
                                                      'macro_def_sig Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (d :
                                                    'macro_def_sig) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((execute_macro_if_active_branch
                                                       _loc (
                                                       (Ast.SgNil (_loc))
                                                       ) (
                                                       fun a ->
                                                        fun b ->
                                                         (Ast.SgSem
                                                           (_loc, a, b))
                                                       ) Then  d) :
                                                      'e__3) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (sgl :
                                            'e__3 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (sgl : 'sglist_then) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (sglist_else : 'sglist_else Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [(
                                         (Gram.Slist1
                                           (Gram.srules sglist_else (
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
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (si :
                                                    'sig_item) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((SdStr (si)) :
                                                      'e__4) )) ));
                                              ((
                                               [(
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (macro_def_sig :
                                                      'macro_def_sig Gram.Entry.t)
                                                    ))) ); (
                                                (Gram.Snterm
                                                  (Gram.Entry.obj (
                                                    (semi :
                                                      'semi Gram.Entry.t)
                                                    ))) )] ), (
                                               (Gram.Action.mk (
                                                 fun _ ->
                                                  fun (d :
                                                    'macro_def_sig) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    ((execute_macro_if_active_branch
                                                       _loc (
                                                       (Ast.SgNil (_loc))
                                                       ) (
                                                       fun a ->
                                                        fun b ->
                                                         (Ast.SgSem
                                                           (_loc, a, b))
                                                       ) Else  d) :
                                                      'e__4) )) ))] )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (sgl :
                                            'e__4 list) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (sgl : 'sglist_else) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (endif : 'endif Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [(( [( (Gram.Skeyword ("ENDIF")) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'endif) )) ));
                                       (( [( (Gram.Skeyword ("END")) )]
                                        ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (() : 'endif) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend (
                                (opt_macro_value :
                                  'opt_macro_value Gram.Entry.t) ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [([] , (
                                        (Gram.Action.mk (
                                          fun (_loc :
                                            Gram.Loc.t) ->
                                           ((None) : 'opt_macro_value) ))
                                        ));
                                       ((
                                        [( (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Some ([] , e)) :
                                               'opt_macro_value) )) ));
                                       ((
                                        [( (Gram.Skeyword ("(")) ); (
                                         (Gram.Slist1sep
                                           ((
                                            (Gram.srules opt_macro_value
                                              (
                                              [((
                                                [(
                                                 (Gram.Stoken
                                                   ((
                                                    function
                                                    | LIDENT (_) ->
                                                       (true)
                                                    | _ -> (false) ),
                                                    "LIDENT _")) )] ), (
                                                (Gram.Action.mk (
                                                  fun (x :
                                                    Gram.Token.t) ->
                                                   fun (_loc :
                                                     Gram.Loc.t) ->
                                                    (let x =
                                                      (Gram.Token.extract_string
                                                        x) in
                                                     x : 'e__5) )) ))] ))
                                            ), ( (Gram.Skeyword (",")) )))
                                         ); ( (Gram.Skeyword (")")) ); (
                                         (Gram.Skeyword ("=")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (expr : 'expr Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e :
                                            'expr) ->
                                           fun _ ->
                                            fun _ ->
                                             fun (pl :
                                               'e__5 list) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                ((Some (pl, e)) :
                                                  'opt_macro_value) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some
                                      ((FanSig.Grammar.Level ("top"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("DEFINE")) );
                                         (
                                         (Gram.Stoken
                                           ((
                                            function
                                            | LIDENT (_) -> (true)
                                            | _ -> (false) ), "LIDENT _"))
                                         ); ( (Gram.Skeyword ("=")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("IN")) );
                                         Gram.Sself ] ), (
                                        (Gram.Action.mk (
                                          fun (body :
                                            'expr) ->
                                           fun _ ->
                                            fun (def :
                                              'expr) ->
                                             fun _ ->
                                              fun (i :
                                                Gram.Token.t) ->
                                               fun _ ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 (let i =
                                                   (Gram.Token.extract_string
                                                     i) in
                                                  ((((new subst) _loc (
                                                      [(i, def)] ))#expr)
                                                    body) : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("IFNDEF")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("THEN")) );
                                         Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (else_expr :
                                               'else_expr Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'else_expr) ->
                                           fun (e1 :
                                             'expr) ->
                                            fun _ ->
                                             fun (i :
                                               'uident) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (if (is_defined i) then
                                                  e2
                                                 else e1 : 'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("IFDEF")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("THEN")) );
                                         Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (else_expr :
                                               'else_expr Gram.Entry.t)
                                             ))) )] ), (
                                        (Gram.Action.mk (
                                          fun (e2 :
                                            'else_expr) ->
                                           fun (e1 :
                                             'expr) ->
                                            fun _ ->
                                             fun (i :
                                               'uident) ->
                                              fun _ ->
                                               fun (_loc :
                                                 Gram.Loc.t) ->
                                                (if (is_defined i) then
                                                  e1
                                                 else e2 : 'expr) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend ( (patt : 'patt Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   (None , (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("IFNDEF")) );
                                         (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("THEN")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("ELSE")) );
                                         Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p2 :
                                             'patt) ->
                                            fun _ ->
                                             fun (p1 :
                                               'patt) ->
                                              fun _ ->
                                               fun (i :
                                                 'uident) ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  (if (is_defined i) then
                                                    p2
                                                   else p1 : 'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("IFDEF")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (uident :
                                               'uident Gram.Entry.t) )))
                                         ); ( (Gram.Skeyword ("THEN")) );
                                         Gram.Sself ; (
                                         (Gram.Skeyword ("ELSE")) );
                                         Gram.Sself ; (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (endif :
                                               'endif Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun _ ->
                                           fun (p2 :
                                             'patt) ->
                                            fun _ ->
                                             fun (p1 :
                                               'patt) ->
                                              fun _ ->
                                               fun (i :
                                                 'uident) ->
                                                fun _ ->
                                                 fun (_loc :
                                                   Gram.Loc.t) ->
                                                  (if (is_defined i) then
                                                    p1
                                                   else p2 : 'patt) )) ))]
                                      ))] ))) () ) ))
                              );
                              (
                              (Gram.extend (
                                (uident : 'uident Gram.Entry.t) ) (
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
                                            | _ -> (false) ), "UIDENT _"))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (i :
                                            Gram.Token.t) ->
                                           fun (_loc :
                                             Gram.Loc.t) ->
                                            (let i =
                                              (Gram.Token.extract_string
                                                i) in
                                             i : 'uident) )) ))] ))] )))
                                  () ) ))
                              );
                              (
                              (Gram.extend ( (expr : 'expr Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some
                                      ((FanSig.Grammar.Before ("simple"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExVrn (_loc, s)) :
                                               'expr) )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.srules expr (
                                           [((
                                             [( (Gram.Skeyword ("IN")) )]
                                             ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [(
                                              (Gram.Skeyword ("DEFINE"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [( (Gram.Skeyword ("ENDIF"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [( (Gram.Skeyword ("END"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [( (Gram.Skeyword ("ELSE"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [( (Gram.Skeyword ("THEN"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [(
                                              (Gram.Skeyword ("IFNDEF"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ));
                                            ((
                                             [( (Gram.Skeyword ("IFDEF"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__6) )) ))] ))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (kwd :
                                            'e__6) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.ExVrn (_loc, kwd)) :
                                               'expr) )) ))] ))] ))) () )
                                ))
                              );
                              (Gram.extend ( (patt : 'patt Gram.Entry.t)
                                ) (
                                ((fun ()
                                    ->
                                   ((
                                    (Some
                                      ((FanSig.Grammar.Before ("simple"))))
                                    ), (
                                    [(None , None , (
                                      [((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.Snterm
                                           (Gram.Entry.obj (
                                             (a_ident :
                                               'a_ident Gram.Entry.t) )))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (s :
                                            'a_ident) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaVrn (_loc, s)) :
                                               'patt) )) ));
                                       ((
                                        [( (Gram.Skeyword ("`")) ); (
                                         (Gram.srules patt (
                                           [((
                                             [( (Gram.Skeyword ("ENDIF"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__7) )) ));
                                            ((
                                             [( (Gram.Skeyword ("END"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__7) )) ));
                                            ((
                                             [( (Gram.Skeyword ("ELSE"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__7) )) ));
                                            ((
                                             [( (Gram.Skeyword ("THEN"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__7) )) ));
                                            ((
                                             [(
                                              (Gram.Skeyword ("IFNDEF"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__7) )) ));
                                            ((
                                             [( (Gram.Skeyword ("IFDEF"))
                                              )] ), (
                                             (Gram.Action.mk (
                                               fun (x :
                                                 Gram.Token.t) ->
                                                fun (_loc :
                                                  Gram.Loc.t) ->
                                                 ((Gram.Token.extract_string
                                                    x) : 'e__7) )) ))] ))
                                         )] ), (
                                        (Gram.Action.mk (
                                          fun (kwd :
                                            'e__7) ->
                                           fun _ ->
                                            fun (_loc :
                                              Gram.Loc.t) ->
                                             ((Ast.PaVrn (_loc, kwd)) :
                                               'patt) )) ))] ))] ))) () )
                                ))

                      let _ = (Options.add "-D" (
                                (Arg.String (parse_def)) )
                                "<string> Define for IFDEF instruction.")

                      let _ = (Options.add "-U" ( (Arg.String (undef)) )
                                "<string> Undefine for IFDEF instruction.")

                      let _ = (Options.add "-I" (
                                (Arg.String (add_include_dir)) )
                                "<string> Add a directory to INCLUDE search path.")

                     end

module MakeNothing =
                           functor (AstFilters : Camlp4.Sig.AstFilters) ->
                            struct
                             open AstFilters

                             open Ast

                             let map_expr =
                              function
                              | (Ast.ExApp
                                  (_, e,
                                   Ast.ExId (_, Ast.IdUid (_, "NOTHING")))
                                 | Ast.ExFun
                                    (_,
                                     Ast.McArr
                                      (_,
                                       Ast.PaId
                                        (_, Ast.IdUid (_, "NOTHING")),
                                       Ast.ExNil (_), e))) ->
                                 e
                              | Ast.ExId
                                 (_loc, Ast.IdLid (_, "__FILE__")) ->
                                 (Ast.ExStr
                                   (_loc, (
                                    (Ast.safe_string_escaped (
                                      (Loc.file_name _loc) )) )))
                              | Ast.ExId
                                 (_loc, Ast.IdLid (_, "__LOCATION__")) ->
                                 let (a, b, c, d, e, f, g, h) =
                                  (Loc.to_tuple _loc) in
                                 (Ast.ExApp
                                   (_loc, (
                                    (Ast.ExId
                                      (_loc, (
                                       (Ast.IdAcc
                                         (_loc, (
                                          (Ast.IdUid (_loc, "Loc")) ), (
                                          (Ast.IdLid (_loc, "of_tuple"))
                                          ))) ))) ), (
                                    (Ast.ExTup
                                      (_loc, (
                                       (Ast.ExCom
                                         (_loc, (
                                          (Ast.ExStr
                                            (_loc, (
                                             (Ast.safe_string_escaped a)
                                             ))) ), (
                                          (Ast.ExCom
                                            (_loc, (
                                             (Ast.ExCom
                                               (_loc, (
                                                (Ast.ExCom
                                                  (_loc, (
                                                   (Ast.ExCom
                                                     (_loc, (
                                                      (Ast.ExCom
                                                        (_loc, (
                                                         (Ast.ExCom
                                                           (_loc, (
                                                            (Ast.ExInt
                                                              (_loc, (
                                                               (string_of_int
                                                                 b) )))
                                                            ), (
                                                            (Ast.ExInt
                                                              (_loc, (
                                                               (string_of_int
                                                                 c) )))
                                                            ))) ), (
                                                         (Ast.ExInt
                                                           (_loc, (
                                                            (string_of_int
                                                              d) ))) )))
                                                      ), (
                                                      (Ast.ExInt
                                                        (_loc, (
                                                         (string_of_int
                                                           e) ))) ))) ),
                                                   (
                                                   (Ast.ExInt
                                                     (_loc, (
                                                      (string_of_int f)
                                                      ))) ))) ), (
                                                (Ast.ExInt
                                                  (_loc, (
                                                   (string_of_int g) )))
                                                ))) ), (
                                             if h then
                                              (
                                              (Ast.ExId
                                                (_loc, (
                                                 (Ast.IdUid
                                                   (_loc, "True")) )))
                                              )
                                             else
                                              (Ast.ExId
                                                (_loc, (
                                                 (Ast.IdUid
                                                   (_loc, "False")) )))
                                             ))) ))) ))) )))
                              | e -> e

                             let _ = (register_str_item_filter (
                                       ((Ast.map_expr map_expr)#str_item)
                                       ))

                            end
