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
            (((function | `Str _ -> true | _ -> false)),
              (`App ((`Vrn "Str"), `Any)), "`Str _")],
           ("match Ast_quotation.resolve_name ((`Sub []), s) with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n",
             (Gramf.mk_action
                (fun (__fan_1 : [> Tokenf.t])  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Str s ->
                       ((match Ast_quotation.resolve_name ((`Sub []), s) with
                         | None  ->
                             Locf.failf _loc "DDSL `%s' can not be resolved"
                               s
                         | Some x -> Ast_quotation.set_default x) : 'item )
                   | _ ->
                       failwith
                         "match Ast_quotation.resolve_name ((`Sub []), s) with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n"))));
        ([`Skeyword "import";
         `Snterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))],
          ("Ast_quotation.paths := ((`Absolute xs) :: (Ast_quotation.paths.contents))\n",
            (Gramf.mk_action
               (fun (xs : 'dot_namespace)  _  (_loc : Locf.t)  ->
                  (Ast_quotation.paths := ((`Absolute xs) ::
                     (Ast_quotation.paths.contents)) : 'item )))));
        ([`Skeyword "filter";
         `Stoken
           (((function | `Str _ -> true | _ -> false)),
             (`App ((`Vrn "Str"), `Any)), "`Str _")],
          ("Ast_filters.use_implem_filter s\n",
            (Gramf.mk_action
               (fun (__fan_1 : [> Tokenf.t])  _  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Str s -> (Ast_filters.use_implem_filter s : 'item )
                  | _ -> failwith "Ast_filters.use_implem_filter s\n"))));
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
             (((function | `Uid _ -> true | _ -> false)),
               (`App ((`Vrn "Uid"), `Any)), "`Uid _");
          `Skeyword ".";
          `Sself],
           ("i :: xs\n",
             (Gramf.mk_action
                (fun (xs : 'dot_namespace)  _  (__fan_0 : [> Tokenf.t]) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid i -> (i :: xs : 'dot_namespace )
                   | _ -> failwith "i :: xs\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("[i]\n",
            (Gramf.mk_action
               (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid i -> ([i] : 'dot_namespace )
                  | _ -> failwith "[i]\n"))))]));
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
