open Fsyntax

let macro_def = Fgram.mk "macro_def"

let opt_macro_value = Fgram.mk "opt_macro_value"

let uident = Fgram.mk "uident"

let apply () =
  let grammar_entry_create x = Fgram.mk x in
  let lid: 'lid Fgram.t = grammar_entry_create "lid"
  and kwd: 'kwd Fgram.t = grammar_entry_create "kwd" in
  begin
    Fgram.extend_single (stru : 'stru Fgram.t )
      ((Some `First),
        (None, None,
          [([`Snterm (Fgram.obj (macro_def : 'macro_def Fgram.t ))],
             ("FCMacroGen.execute_macro ~exp ~pat\n  (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru ) x\n",
               (Fgram.mk_action
                  (fun (x : 'macro_def)  (_loc : FLoc.t)  ->
                     (FCMacroGen.execute_macro ~exp ~pat
                        (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru ) x : 
                     'stru )))))]));
    Fgram.extend_single (macro_def : 'macro_def Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "DEFINE";
            `Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`App ((`Vrn "Uid"), `Any)), "`Uid _");
            `Snterm (Fgram.obj (opt_macro_value : 'opt_macro_value Fgram.t ))],
             ("FCMacroGen.Def (i, def)\n",
               (Fgram.mk_action
                  (fun (def : 'opt_macro_value)  (__fan_1 : [> FToken.t])  _ 
                     (_loc : FLoc.t)  ->
                     match __fan_1 with
                     | `Uid i -> (FCMacroGen.Def (i, def) : 'macro_def )
                     | _ -> failwith "FCMacroGen.Def (i, def)\n"))));
          ([`Skeyword "UNDEF";
           `Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
            ("FCMacroGen.Und i\n",
              (Fgram.mk_action
                 (fun (i : 'uident)  _  (_loc : FLoc.t)  ->
                    (FCMacroGen.Und i : 'macro_def )))))]));
    Fgram.extend_single (opt_macro_value : 'opt_macro_value Fgram.t )
      (None,
        (None, None,
          [([`Skeyword "(";
            `Slist1sep
              ((`Snterm (Fgram.obj (lid : 'lid Fgram.t ))), (`Skeyword ","));
            `Skeyword ")";
            `Skeyword "=";
            `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
             ("Some (pl, e)\n",
               (Fgram.mk_action
                  (fun (e : 'exp)  _  _  (pl : 'lid list)  _  (_loc : FLoc.t)
                      -> (Some (pl, e) : 'opt_macro_value )))));
          ([`Skeyword "="; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
            ("Some ([], e)\n",
              (Fgram.mk_action
                 (fun (e : 'exp)  _  (_loc : FLoc.t)  ->
                    (Some ([], e) : 'opt_macro_value )))));
          ([],
            ("None\n",
              (Fgram.mk_action
                 (fun (_loc : FLoc.t)  -> (None : 'opt_macro_value )))))]));
    Fgram.extend_single (lid : 'lid Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function | `Lid _ -> true | _ -> false)),
                 (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
             ("x\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `Lid x -> (x : 'lid )
                     | _ -> failwith "x\n"))))]));
    Fgram.extend_single (exp : 'exp Fgram.t )
      ((Some (`Level "top")),
        (None, None,
          [([`Skeyword "DEFINE";
            `Stoken
              (((function | `Lid _ -> true | _ -> false)),
                (`App ((`Vrn "Lid"), `Any)), "`Lid _");
            `Skeyword "=";
            `Sself;
            `Skeyword "IN";
            `Sself],
             ("((new Exp.subst) _loc [(i, def)])#exp body\n",
               (Fgram.mk_action
                  (fun (body : 'exp)  _  (def : 'exp)  _ 
                     (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                     match __fan_1 with
                     | `Lid i ->
                         (((new Exp.subst) _loc [(i, def)])#exp body : 
                         'exp )
                     | _ ->
                         failwith
                           "((new Exp.subst) _loc [(i, def)])#exp body\n"))))]));
    Fgram.extend_single (kwd : 'kwd Fgram.t )
      (None,
        (None, None,
          [([`Stoken
               (((function
                  | `KEYWORD ("DEFINE"|"UNDEF"|"IN") -> true
                  | _ -> false)),
                 (`App
                    ((`Vrn "KEYWORD"),
                      (`Bar
                         ((`Bar ((`Str "DEFINE"), (`Str "UNDEF"))),
                           (`Str "IN"))))),
                 "`KEYWORD \"DEFINE\"| \"UNDEF\"| \"IN\"")],
             ("x\n",
               (Fgram.mk_action
                  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                     match __fan_0 with
                     | `KEYWORD ("DEFINE"|"UNDEF"|"IN" as x) -> (x : 'kwd )
                     | _ -> failwith "x\n"))))]));
    Fgram.extend_single (exp : 'exp Fgram.t )
      ((Some (`Before "simple")),
        (None, None,
          [([`Skeyword "`"; `Snterm (Fgram.obj (kwd : 'kwd Fgram.t ))],
             ("(`Vrn (_loc, kwd) : FAst.exp )\n",
               (Fgram.mk_action
                  (fun (kwd : 'kwd)  _  (_loc : FLoc.t)  ->
                     ((`Vrn (_loc, kwd) : FAst.exp ) : 'exp )))));
          ([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
            ("(`Vrn (_loc, s) : FAst.exp )\n",
              (Fgram.mk_action
                 (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                    ((`Vrn (_loc, s) : FAst.exp ) : 'exp )))))]));
    Fgram.extend_single (pat : 'pat Fgram.t )
      ((Some (`Before "simple")),
        (None, None,
          [([`Skeyword "`"; `Snterm (Fgram.obj (kwd : 'kwd Fgram.t ))],
             ("(`Vrn (_loc, kwd) : FAst.pat )\n",
               (Fgram.mk_action
                  (fun (kwd : 'kwd)  _  (_loc : FLoc.t)  ->
                     ((`Vrn (_loc, kwd) : FAst.pat ) : 'pat )))));
          ([`Skeyword "`"; `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
            ("(`Vrn (_loc, s) : FAst.pat )\n",
              (Fgram.mk_action
                 (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                    ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))))]))
  end

let _ = AstParsers.register_parser ("macro", apply)