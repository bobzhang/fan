let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let item = Gramf.mk_dynamic g "item"
let dot_namespace = Gramf.mk_dynamic g "dot_namespace"
let items = Gramf.mk_dynamic g "items"
let _ =
  Gramf.unsafe_extend_single (item : 'item Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "default";
           `Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "Str")],
            ("match Ast_quotation.resolve_name ((`Sub []), s) with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n",
              (Gramf.mk_action
                 (fun ~__fan_1  ~__fan_0:_  (_loc : Locf.t)  ->
                    match __fan_1 with
                    | ({ txt = s;_} : Tokenf.txt) ->
                        ((match Ast_quotation.resolve_name ((`Sub []), s)
                          with
                          | None  ->
                              Locf.failf _loc "DDSL `%s' can not be resolved"
                                s
                          | Some x -> Ast_quotation.set_default x) : 
                        'item )))));
         ([`Keyword "import";
          `Nterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))],
           ("Ast_quotation.paths := ((`Absolute xs) :: (!Ast_quotation.paths))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(xs : 'dot_namespace)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (Ast_quotation.paths := ((`Absolute xs) ::
                      (!Ast_quotation.paths)) : 'item )))));
         ([`Keyword "filter";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "Str")],
           ("Ast_filters.use_implem_filter s\n",
             (Gramf.mk_action
                (fun ~__fan_1  ~__fan_0:_  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | ({ txt = s;_} : Tokenf.txt) ->
                       (Ast_filters.use_implem_filter s : 'item )))));
         ([`Keyword "lang_clear"],
           ("Ast_quotation.clear_map (); Ast_quotation.clear_default ()\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   (Ast_quotation.clear_map ();
                    Ast_quotation.clear_default () : 'item )))))]) : 
      Gramf.olevel ));
  Gramf.unsafe_extend_single (dot_namespace : 'dot_namespace Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "Uid");
           `Keyword ".";
           `Self],
            ("i :: xs\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'dot_namespace)  ~__fan_1:_  ~__fan_0 
                    (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = i;_} : Tokenf.txt) -> (i ::
                        xs : 'dot_namespace )))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "Uid")],
           ("[i]\n",
             (Gramf.mk_action
                (fun ~__fan_0  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = i;_} : Tokenf.txt) -> ([i] : 'dot_namespace )))))]) : 
      Gramf.olevel ));
  Gramf.unsafe_extend_single (items : 'items Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (item : 'item Gramf.t )); `Keyword ";"],
            ("()\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (() : 'items )))));
         ([`Nterm (Gramf.obj (item : 'item Gramf.t )); `Keyword ";"; `Self],
           ("()\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (() : 'items )))));
         ([],
           ("()\n",
             (Gramf.mk_action (fun (_loc : Locf.t)  -> (() : 'items )))))]) : 
      Gramf.olevel ))
let () =
  Fdir.register
    ((Tokenf.name_of_string "control"),
      (fun loc  _  c  -> Gramf.parse_string ~loc items c))
