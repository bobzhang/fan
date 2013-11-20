let g = Gramf.create_lexer ~annot:"" ~keywords:[] ()
let fan_quot = Gramf.mk_dynamic g "fan_quot"
let fan_quots = Gramf.mk_dynamic g "fan_quots"
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let id: 'id Gramf.t = grammar_entry_create "id"
  and fan_quot_semi: 'fan_quot_semi Gramf.t =
    grammar_entry_create "fan_quot_semi" in
  Gramf.unsafe_extend_single (fan_quot : 'fan_quot Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "derive";
          `Skeyword "(";
          `Slist1 (`Snterm (Gramf.obj (id : 'id Gramf.t )));
          `Skeyword ")"],
           ("List.iter Typehook.plugin_add plugins\n",
             (Gramf.mk_action
                (fun _  (plugins : 'id list)  _  _  (_loc : Locf.t)  ->
                   (List.iter Typehook.plugin_add plugins : 'fan_quot )))));
        ([`Skeyword "unload";
         `Slist1sep
           ((`Snterm (Gramf.obj (id : 'id Gramf.t ))), (`Skeyword ","))],
          ("List.iter Typehook.plugin_remove plugins\n",
            (Gramf.mk_action
               (fun (plugins : 'id list)  _  (_loc : Locf.t)  ->
                  (List.iter Typehook.plugin_remove plugins : 'fan_quot )))));
        ([`Skeyword "clear"],
          ("FState.reset_current_filters ()\n",
            (Gramf.mk_action
               (fun _  (_loc : Locf.t)  ->
                  (FState.reset_current_filters () : 'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "on"],
          ("FState.keep := true\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (FState.keep := true : 'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "off"],
          ("FState.keep := false\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (FState.keep := false : 'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "on"],
          ("Typehook.show_code := true\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (Typehook.show_code := true : 'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "off"],
          ("Typehook.show_code := false\n",
            (Gramf.mk_action
               (fun _  _  (_loc : Locf.t)  ->
                  (Typehook.show_code := false : 'fan_quot )))))]));
  Gramf.unsafe_extend_single (id : 'id Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("x\n",
             (Gramf.mk_action
                (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid x -> (x : 'id )
                   | _ -> failwith "x\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("x\n",
            (Gramf.mk_action
               (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid x -> (x : 'id )
                  | _ -> failwith "x\n"))))]));
  Gramf.unsafe_extend_single (fan_quot_semi : 'fan_quot_semi Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (fan_quot : 'fan_quot Gramf.t ));
          `Skeyword ";"],
           ("",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  -> (() : 'fan_quot_semi )))))]));
  Gramf.unsafe_extend_single (fan_quots : 'fan_quots Gramf.t )
    (None,
      (None, None,
        [([`Slist1
             (`Snterm (Gramf.obj (fan_quot_semi : 'fan_quot_semi Gramf.t )))],
           ("(`Uid (_loc, \"()\") : Astf.exp )\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ((`Uid (_loc, "()") : Astf.exp ) : 'fan_quots )))))]))
let _ =
  Foptions.add
    ("-keep", (Arg.Set FState.keep), "Keep the included type definitions");
  Foptions.add
    ("-loaded-plugins", (Arg.Unit Typehook.show_modules), "Show plugins")
