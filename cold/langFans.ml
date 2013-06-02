open AstLib

let g = Fgram.create_lexer ~annot:"" ~keywords:[] ()

let fan_quot = Fgram.mk_dynamic g "fan_quot"

let fan_quots = Fgram.mk_dynamic g "fan_quots"

let _ =
  begin
    Fgram.unsafe_extend_single (fan_quot : 'fan_quot Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "derive";
            `Skeyword "(";
            `Slist1
              (Fgram.srules
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    ("x\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Lid x -> (x : 'e__1 )
                            | _ -> failwith "x\n"))));
                 ([`Stoken
                     (((function | `Uid _ -> true | _ -> false)),
                       (`Normal, "`Uid _"))],
                   ("x\n",
                     (Fgram.mk_action
                        (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                           match __fan_0 with
                           | `Uid x -> (x : 'e__1 )
                           | _ -> failwith "x\n"))))]);
            `Skeyword ")"],
             ("begin\n  List.iter Typehook.plugin_add plugins; (`Uid (_loc, \"()\") : FAst.exp )\nend\n",
               (Fgram.mk_action
                  (fun _  (plugins : 'e__1 list)  _  _  (_loc : FLoc.t)  ->
                     (begin
                        List.iter Typehook.plugin_add plugins;
                        (`Uid (_loc, "()") : FAst.exp )
                      end : 'fan_quot )))));
          ([`Skeyword "unload";
           `Slist1sep
             ((Fgram.srules
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    ("x\n",
                      (Fgram.mk_action
                         (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                            match __fan_0 with
                            | `Lid x -> (x : 'e__2 )
                            | _ -> failwith "x\n"))));
                 ([`Stoken
                     (((function | `Uid _ -> true | _ -> false)),
                       (`Normal, "`Uid _"))],
                   ("x\n",
                     (Fgram.mk_action
                        (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                           match __fan_0 with
                           | `Uid x -> (x : 'e__2 )
                           | _ -> failwith "x\n"))))]), (`Skeyword ","))],
            ("begin\n  List.iter Typehook.plugin_remove plugins; (`Uid (_loc, \"()\") : FAst.exp )\nend\n",
              (Fgram.mk_action
                 (fun (plugins : 'e__2 list)  _  (_loc : FLoc.t)  ->
                    (begin
                       List.iter Typehook.plugin_remove plugins;
                       (`Uid (_loc, "()") : FAst.exp )
                     end : 'fan_quot )))));
          ([`Skeyword "clear"],
            ("begin FState.reset_current_filters (); (`Uid (_loc, \"()\") : FAst.exp ) end\n",
              (Fgram.mk_action
                 (fun _  (_loc : FLoc.t)  ->
                    (begin
                       FState.reset_current_filters ();
                       (`Uid (_loc, "()") : FAst.exp )
                     end : 'fan_quot )))));
          ([`Skeyword "keep"; `Skeyword "on"],
            ("begin FState.keep := true; (`Uid (_loc, \"()\") : FAst.exp ) end\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : FLoc.t)  ->
                    (begin
                       FState.keep := true; (`Uid (_loc, "()") : FAst.exp )
                     end : 'fan_quot )))));
          ([`Skeyword "keep"; `Skeyword "off"],
            ("begin FState.keep := false; (`Uid (_loc, \"()\") : FAst.exp ) end\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : FLoc.t)  ->
                    (begin
                       FState.keep := false; (`Uid (_loc, "()") : FAst.exp )
                     end : 'fan_quot )))));
          ([`Skeyword "show_code"; `Skeyword "on"],
            ("begin Typehook.show_code := true; (`Uid (_loc, \"()\") : FAst.exp ) end\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : FLoc.t)  ->
                    (begin
                       Typehook.show_code := true;
                       (`Uid (_loc, "()") : FAst.exp )
                     end : 'fan_quot )))));
          ([`Skeyword "show_code"; `Skeyword "off"],
            ("begin Typehook.show_code := false; (`Uid (_loc, \"()\") : FAst.exp ) end\n",
              (Fgram.mk_action
                 (fun _  _  (_loc : FLoc.t)  ->
                    (begin
                       Typehook.show_code := false;
                       (`Uid (_loc, "()") : FAst.exp )
                     end : 'fan_quot )))))]));
    Fgram.unsafe_extend_single (fan_quots : 'fan_quots Fgram.t )
      (None,
        (None, None,
          [([`Slist1
               (Fgram.srules
                  [([`Snterm (Fgram.obj (fan_quot : 'fan_quot Fgram.t ));
                    `Skeyword ";"],
                     ("x\n",
                       (Fgram.mk_action
                          (fun _  (x : 'fan_quot)  (_loc : FLoc.t)  ->
                             (x : 'e__3 )))))])],
             ("seq_sem xs\n",
               (Fgram.mk_action
                  (fun (xs : 'e__3 list)  (_loc : FLoc.t)  ->
                     (seq_sem xs : 'fan_quots )))))]))
  end

let _ =
  begin
    Foptions.add
      ("-keep", (FArg.Set FState.keep), "Keep the included type definitions");
    Foptions.add
      ("-loaded-plugins", (FArg.Unit Typehook.show_modules), "Show plugins")
  end