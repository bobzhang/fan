let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let item = Gramf.mk_dynamic g "item"
let dot_namespace = Gramf.mk_dynamic g "dot_namespace"
let items = Gramf.mk_dynamic g "items"
let _ =
  Gramf.unsafe_extend_single (item : 'item Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "default";
          `Stoken
            (((function | `Str (_,_) -> true | _ -> false)), (4153489, `Any),
              "`Str s")],
           ("match Ast_quotation.resolve_name ((`Sub []), s) with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n",
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Str (_,s) ->
                       ((match Ast_quotation.resolve_name ((`Sub []), s) with
                         | None  ->
                             Locf.failf _loc "DDSL `%s' can not be resolved"
                               s
                         | Some x -> Ast_quotation.set_default x) : 'item )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_1))))));
        ([`Skeyword "import";
         `Snterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))],
          ("Ast_quotation.paths := ((`Absolute xs) :: (Ast_quotation.paths.contents))\n",
            (Gramf.mk_action
               (fun (xs : 'dot_namespace)  _  (_loc : Locf.t)  ->
                  (Ast_quotation.paths := ((`Absolute xs) ::
                     (Ast_quotation.paths.contents)) : 'item )))));
        ([`Skeyword "filter";
         `Stoken
           (((function | `Str (_,_) -> true | _ -> false)), (4153489, `Any),
             "`Str s")],
          ("Ast_filters.use_implem_filter s\n",
            (Gramf.mk_action
               (fun (__fan_1 : Tokenf.t)  _  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Str (_,s) -> (Ast_filters.use_implem_filter s : 'item )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_1))))));
        ([`Skeyword "lang_clear"],
          ("Ast_quotation.clear_map (); Ast_quotation.clear_default ()\n",
            (Gramf.mk_action
               (fun _  (_loc : Locf.t)  ->
                  (Ast_quotation.clear_map (); Ast_quotation.clear_default () : 
                  'item )))))]));
  Gramf.unsafe_extend_single (dot_namespace : 'dot_namespace Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)),
               (4250480, `Any), "`Uid i");
          `Skeyword ".";
          `Sself],
           ("i :: xs\n",
             (Gramf.mk_action
                (fun (xs : 'dot_namespace)  _  (__fan_0 : Tokenf.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,i) -> (i :: xs : 'dot_namespace )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Tokenf.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Uid (_,_) -> true | _ -> false)), (4250480, `Any),
              "`Uid i")],
          ("[i]\n",
            (Gramf.mk_action
               (fun (__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid (_,i) -> ([i] : 'dot_namespace )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Tokenf.token_to_string __fan_0))))))]));
  Gramf.unsafe_extend_single (items : 'items Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (item : 'item Gramf.t )); `Skeyword ";"],
           ("()\n",
             (Gramf.mk_action (fun _  _  (_loc : Locf.t)  -> (() : 'items )))));
        ([`Snterm (Gramf.obj (item : 'item Gramf.t )); `Skeyword ";"; `Sself],
          ("()\n",
            (Gramf.mk_action
               (fun _  _  _  (_loc : Locf.t)  -> (() : 'items )))));
        ([],
          ("()\n",
            (Gramf.mk_action (fun (_loc : Locf.t)  -> (() : 'items )))))]))
let () =
  Fdir.register
    ((Tokenf.name_of_string "control"),
      (fun loc  _  c  -> Gramf.parse_string ~loc items c))
