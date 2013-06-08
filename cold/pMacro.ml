open Fsyntax

open FCMacroGen

let macro_def = Fgram.mk "macro_def"

let uident_eval_ifdef = Fgram.mk "uident_eval_ifdef"

let uident_eval_ifndef = Fgram.mk "uident_eval_ifndef"

let else_macro_def = Fgram.mk "else_macro_def"

let else_exp = Fgram.mk "else_exp"

let smlist_then = Fgram.mk "smlist_then"

let smlist_else = Fgram.mk "smlist_else"

let endif = Fgram.mk "endif"

let opt_macro_value = Fgram.mk "opt_macro_value"

let uident = Fgram.mk "uident"

let apply () =
  begin
    (let grammar_entry_create x = Fgram.mk x in
     let macro_semi: 'macro_semi Fgram.t = grammar_entry_create "macro_semi"
     and lid: 'lid Fgram.t = grammar_entry_create "lid"
     and kwd: 'kwd Fgram.t = grammar_entry_create "kwd" in
     begin
       Fgram.extend_single (stru : 'stru Fgram.t )
         ((Some `First),
           (None, None,
             [([`Snterm (Fgram.obj (macro_def : 'macro_def Fgram.t ))],
                ("execute_macro ~exp ~pat (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru )\n  (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru )) x\n",
                  (Fgram.mk_action
                     (fun (x : 'macro_def)  (_loc : FLoc.t)  ->
                        (execute_macro ~exp ~pat
                           (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru )
                           (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru )) x : 
                        'stru )))))]));
       Fgram.extend_single (macro_def : 'macro_def Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "DEFINE";
               `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
               `Snterm
                 (Fgram.obj (opt_macro_value : 'opt_macro_value Fgram.t ))],
                ("Def (i, def)\n",
                  (Fgram.mk_action
                     (fun (def : 'opt_macro_value)  (i : 'uident)  _ 
                        (_loc : FLoc.t)  -> (Def (i, def) : 'macro_def )))));
             ([`Skeyword "UNDEF";
              `Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
               ("Und i\n",
                 (Fgram.mk_action
                    (fun (i : 'uident)  _  (_loc : FLoc.t)  ->
                       (Und i : 'macro_def )))));
             ([`Skeyword "IFDEF";
              `Snterm
                (Fgram.obj (uident_eval_ifdef : 'uident_eval_ifdef Fgram.t ));
              `Skeyword "THEN";
              `Snterm (Fgram.obj (smlist_then : 'smlist_then Fgram.t ));
              `Snterm (Fgram.obj (else_macro_def : 'else_macro_def Fgram.t ))],
               ("make_ITE_result st1 st2\n",
                 (Fgram.mk_action
                    (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _ 
                       _  (_loc : FLoc.t)  ->
                       (make_ITE_result st1 st2 : 'macro_def )))));
             ([`Skeyword "IFNDEF";
              `Snterm
                (Fgram.obj
                   (uident_eval_ifndef : 'uident_eval_ifndef Fgram.t ));
              `Skeyword "THEN";
              `Snterm (Fgram.obj (smlist_then : 'smlist_then Fgram.t ));
              `Snterm (Fgram.obj (else_macro_def : 'else_macro_def Fgram.t ))],
               ("make_ITE_result st1 st2\n",
                 (Fgram.mk_action
                    (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _ 
                       _  (_loc : FLoc.t)  ->
                       (make_ITE_result st1 st2 : 'macro_def )))));
             ([`Skeyword "INCLUDE";
              `Stoken
                (((function | `STR (_,_) -> true | _ -> false)),
                  (`App ((`App ((`Vrn "STR"), `Any)), `Any)), "`STR (_,_)")],
               ("Lazy (lazy (Fgram.parse_include_file strus fname))\n",
                 (Fgram.mk_action
                    (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                       match __fan_1 with
                       | `STR (_,fname) ->
                           (Lazy
                              (lazy (Fgram.parse_include_file strus fname)) : 
                           'macro_def )
                       | _ ->
                           failwith
                             "Lazy (lazy (Fgram.parse_include_file strus fname))\n"))))]));
       Fgram.extend_single (uident_eval_ifdef : 'uident_eval_ifdef Fgram.t )
         (None,
           (None, None,
             [([`Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
                ("Stack.push (is_defined i) stack\n",
                  (Fgram.mk_action
                     (fun (i : 'uident)  (_loc : FLoc.t)  ->
                        (Stack.push (is_defined i) stack : 'uident_eval_ifdef )))))]));
       Fgram.extend_single
         (uident_eval_ifndef : 'uident_eval_ifndef Fgram.t )
         (None,
           (None, None,
             [([`Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
                ("Stack.push (not (is_defined i)) stack\n",
                  (Fgram.mk_action
                     (fun (i : 'uident)  (_loc : FLoc.t)  ->
                        (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef )))))]));
       Fgram.extend_single (else_macro_def : 'else_macro_def Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "ELSE";
               `Snterm (Fgram.obj (smlist_else : 'smlist_else Fgram.t ));
               `Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
                ("st\n",
                  (Fgram.mk_action
                     (fun _  (st : 'smlist_else)  _  (_loc : FLoc.t)  ->
                        (st : 'else_macro_def )))));
             ([`Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
               ("[]\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ([] : 'else_macro_def )))))]));
       Fgram.extend_single (else_exp : 'else_exp Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "ELSE";
               `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
               `Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
                ("e\n",
                  (Fgram.mk_action
                     (fun _  (e : 'exp)  _  (_loc : FLoc.t)  ->
                        (e : 'else_exp )))));
             ([`Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
               ("(`Uid (_loc, \"()\") : FAst.exp )\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  ->
                       ((`Uid (_loc, "()") : FAst.exp ) : 'else_exp )))))]));
       Fgram.extend_single (smlist_then : 'smlist_then Fgram.t )
         (None,
           (None, None,
             [([`Slist1
                  (`Snterm (Fgram.obj (macro_semi : 'macro_semi Fgram.t )))],
                ("sml\n",
                  (Fgram.mk_action
                     (fun (sml : 'macro_semi list)  (_loc : FLoc.t)  ->
                        (sml : 'smlist_then )))))]));
       Fgram.extend_single (macro_semi : 'macro_semi Fgram.t )
         (None,
           (None, None,
             [([`Snterm (Fgram.obj (macro_def : 'macro_def Fgram.t ));
               `Skeyword ";"],
                ("execute_macro_if_active_branch ~exp ~pat _loc\n  (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru )\n  (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru )) Then d\n",
                  (Fgram.mk_action
                     (fun _  (d : 'macro_def)  (_loc : FLoc.t)  ->
                        (execute_macro_if_active_branch ~exp ~pat _loc
                           (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru )
                           (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru ))
                           Then d : 'macro_semi )))));
             ([`Snterm (Fgram.obj (stru : 'stru Fgram.t )); `Skeyword ";"],
               ("Str si\n",
                 (Fgram.mk_action
                    (fun _  (si : 'stru)  (_loc : FLoc.t)  ->
                       (Str si : 'macro_semi )))))]));
       Fgram.extend_single (smlist_else : 'smlist_else Fgram.t )
         (None,
           (None, None,
             [([`Slist1
                  (`Snterm (Fgram.obj (macro_semi : 'macro_semi Fgram.t )))],
                ("sml\n",
                  (Fgram.mk_action
                     (fun (sml : 'macro_semi list)  (_loc : FLoc.t)  ->
                        (sml : 'smlist_else )))))]));
       Fgram.extend_single (endif : 'endif Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "END"],
                ("()\n",
                  (Fgram.mk_action
                     (fun _  (_loc : FLoc.t)  -> (() : 'endif )))));
             ([`Skeyword "ENDIF"],
               ("()\n",
                 (Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (() : 'endif )))))]));
       Fgram.extend_single (opt_macro_value : 'opt_macro_value Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "(";
               `Slist1sep
                 ((`Snterm (Fgram.obj (lid : 'lid Fgram.t ))),
                   (`Skeyword ","));
               `Skeyword ")";
               `Skeyword "=";
               `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
                ("Some (pl, e)\n",
                  (Fgram.mk_action
                     (fun (e : 'exp)  _  _  (pl : 'lid list)  _ 
                        (_loc : FLoc.t)  ->
                        (Some (pl, e) : 'opt_macro_value )))));
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
             [([`Skeyword "IFDEF";
               `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
               `Skeyword "THEN";
               `Sself;
               `Snterm (Fgram.obj (else_exp : 'else_exp Fgram.t ))],
                ("if is_defined i then e1 else e2\n",
                  (Fgram.mk_action
                     (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _ 
                        (_loc : FLoc.t)  ->
                        (if is_defined i then e1 else e2 : 'exp )))));
             ([`Skeyword "IFNDEF";
              `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
              `Skeyword "THEN";
              `Sself;
              `Snterm (Fgram.obj (else_exp : 'else_exp Fgram.t ))],
               ("if is_defined i then e2 else e1\n",
                 (Fgram.mk_action
                    (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _ 
                       (_loc : FLoc.t)  ->
                       (if is_defined i then e2 else e1 : 'exp )))));
             ([`Skeyword "DEFINE";
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
       Fgram.extend_single (pat : 'pat Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "IFDEF";
               `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
               `Skeyword "THEN";
               `Sself;
               `Skeyword "ELSE";
               `Sself;
               `Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
                ("if is_defined i then p1 else p2\n",
                  (Fgram.mk_action
                     (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _
                         (_loc : FLoc.t)  ->
                        (if is_defined i then p1 else p2 : 'pat )))));
             ([`Skeyword "IFNDEF";
              `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
              `Skeyword "THEN";
              `Sself;
              `Skeyword "ELSE";
              `Sself;
              `Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
               ("if is_defined i then p2 else p1\n",
                 (Fgram.mk_action
                    (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _ 
                       (_loc : FLoc.t)  ->
                       (if is_defined i then p2 else p1 : 'pat )))))]));
       Fgram.extend_single (uident : 'uident Fgram.t )
         (None,
           (None, None,
             [([`Stoken
                  (((function | `Uid _ -> true | _ -> false)),
                    (`App ((`Vrn "Uid"), `Any)), "`Uid _")],
                ("i\n",
                  (Fgram.mk_action
                     (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                        match __fan_0 with
                        | `Uid i -> (i : 'uident )
                        | _ -> failwith "i\n"))))]));
       Fgram.extend_single (kwd : 'kwd Fgram.t )
         (None,
           (None, None,
             [([`Skeyword "IFDEF"],
                ("\"IFDEF\"\n",
                  (Fgram.mk_action
                     (fun _  (_loc : FLoc.t)  -> ("IFDEF" : 'kwd )))));
             ([`Skeyword "IFNDEF"],
               ("\"IFNDEF\"\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ("IFNDEF" : 'kwd )))));
             ([`Skeyword "THEN"],
               ("\"THEN\"\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ("THEN" : 'kwd )))));
             ([`Skeyword "ELSE"],
               ("\"ELSE\"\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ("ELSE" : 'kwd )))));
             ([`Skeyword "END"],
               ("\"END\"\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ("END" : 'kwd )))));
             ([`Skeyword "ENDIF"],
               ("\"ENDIF\"\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ("ENDIF" : 'kwd )))));
             ([`Skeyword "DEFINE"],
               ("\"DEFINE\"\n",
                 (Fgram.mk_action
                    (fun _  (_loc : FLoc.t)  -> ("DEFINE" : 'kwd )))));
             ([`Skeyword "IN"],
               ("\"IN\"\n",
                 (Fgram.mk_action (fun _  (_loc : FLoc.t)  -> ("IN" : 'kwd )))))]));
       Fgram.extend_single (exp : 'exp Fgram.t )
         ((Some (`Before "simple")),
           (None, None,
             [([`Skeyword "`"; `Snterm (Fgram.obj (kwd : 'kwd Fgram.t ))],
                ("(`Vrn (_loc, kwd) : FAst.exp )\n",
                  (Fgram.mk_action
                     (fun (kwd : 'kwd)  _  (_loc : FLoc.t)  ->
                        ((`Vrn (_loc, kwd) : FAst.exp ) : 'exp )))));
             ([`Skeyword "`";
              `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
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
             ([`Skeyword "`";
              `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
               ("(`Vrn (_loc, s) : FAst.pat )\n",
                 (Fgram.mk_action
                    (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                       ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))))]))
     end);
    Foptions.add
      ("-D", (FArg.String (parse_def ~exp ~pat)),
        "<string> Define for IFDEF instruction.");
    Foptions.add
      ("-U", (FArg.String (undef ~exp ~pat)),
        "<string> Undefine for IFDEF instruction.");
    Foptions.add
      ("-I", (FArg.String FIncludeDir.add),
        "<string> Add a directory to INCLUDE search path.")
  end

let _ = AstParsers.register_parser ("macro", apply)