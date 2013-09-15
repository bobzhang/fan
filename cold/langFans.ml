let g = Fgram.create_lexer ~annot:"" ~keywords:[] ()
let fan_quot = Fgram.mk_dynamic g "fan_quot"
let fan_quots = Fgram.mk_dynamic g "fan_quots"
let _ =
  let grammar_entry_create x = Fgram.mk_dynamic g x in
  let id: 'id Fgram.t = grammar_entry_create "id"
  and fan_quot_semi: 'fan_quot_semi Fgram.t =
    grammar_entry_create "fan_quot_semi" in
  Fgram.unsafe_extend_single (fan_quot : 'fan_quot Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "derive";
          `Skeyword "(";
          `Slist1 (`Snterm (Fgram.obj (id : 'id Fgram.t )));
          `Skeyword ")"],
           ("List.iter Typehook.plugin_add plugins\n",
             (Fgram.mk_action
                (fun _  (plugins : 'id list)  _  _  (_loc : FLoc.t)  ->
                   (List.iter Typehook.plugin_add plugins : 'fan_quot )))));
        ([`Skeyword "unload";
         `Slist1sep
           ((`Snterm (Fgram.obj (id : 'id Fgram.t ))), (`Skeyword ","))],
          ("List.iter Typehook.plugin_remove plugins\n",
            (Fgram.mk_action
               (fun (plugins : 'id list)  _  (_loc : FLoc.t)  ->
                  (List.iter Typehook.plugin_remove plugins : 'fan_quot )))));
        ([`Skeyword "clear"],
          ("FState.reset_current_filters ()\n",
            (Fgram.mk_action
               (fun _  (_loc : FLoc.t)  ->
                  (FState.reset_current_filters () : 'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "on"],
          ("FState.keep := true\n",
            (Fgram.mk_action
               (fun _  _  (_loc : FLoc.t)  ->
                  (FState.keep := true : 'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "off"],
          ("FState.keep := false\n",
            (Fgram.mk_action
               (fun _  _  (_loc : FLoc.t)  ->
                  (FState.keep := false : 'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "on"],
          ("Typehook.show_code := true\n",
            (Fgram.mk_action
               (fun _  _  (_loc : FLoc.t)  ->
                  (Typehook.show_code := true : 'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "off"],
          ("Typehook.show_code := false\n",
            (Fgram.mk_action
               (fun _  _  (_loc : FLoc.t)  ->
                  (Typehook.show_code := false : 'fan_quot )))))]));
  Fgram.unsafe_extend_single (id : 'id Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("x\n",
             (Fgram.mk_action
                (fun (__fan_0 : [> Ftoken.t])  (_loc : FLoc.t)  ->
                   match __fan_0 with
                   | `Lid x -> (x : 'id )
                   | _ -> failwith "x\n"))));
        ([`Stoken
            (((function | `Uid _ -> true | _ -> false)),
              (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
          ("x\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> Ftoken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Uid x -> (x : 'id )
                  | _ -> failwith "x\n"))))]));
  Fgram.unsafe_extend_single (fan_quot_semi : 'fan_quot_semi Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (fan_quot : 'fan_quot Fgram.t ));
          `Skeyword ";"],
           ("",
             (Fgram.mk_action
                (fun _  _  (_loc : FLoc.t)  -> (() : 'fan_quot_semi )))))]));
  Fgram.unsafe_extend_single (fan_quots : 'fan_quots Fgram.t )
    (None,
      (None, None,
        [([`Slist1
             (`Snterm (Fgram.obj (fan_quot_semi : 'fan_quot_semi Fgram.t )))],
           ("(`Uid (_loc, \"()\") : FAst.exp )\n",
             (Fgram.mk_action
                (fun _  (_loc : FLoc.t)  ->
                   ((`Uid (_loc, "()") : FAst.exp ) : 'fan_quots )))))]))
let _ =
  Foptions.add
    ("-keep", (Arg.Set FState.keep), "Keep the included type definitions");
  Foptions.add
    ("-loaded-plugins", (Arg.Unit Typehook.show_modules), "Show plugins")