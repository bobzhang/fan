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
    begin
      Fgram.extend_single (stru : 'stru Fgram.t )
        ((Some `First),
          (None, None,
            [([`Snterm (Fgram.obj (macro_def : 'macro_def Fgram.t ))],
               ("Fgram.mk_action\n  (fun (x : 'macro_def)  (_loc : FLoc.t)  ->\n     (execute_macro ~exp ~pat\n        (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru )\n        (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru )) x : 'stru ))\n",
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
               ("Fgram.mk_action\n  (fun (def : 'opt_macro_value)  (i : 'uident)  _  (_loc : FLoc.t)  ->\n     (Def (i, def) : 'macro_def ))\n",
                 (Fgram.mk_action
                    (fun (def : 'opt_macro_value)  (i : 'uident)  _ 
                       (_loc : FLoc.t)  -> (Def (i, def) : 'macro_def )))));
            ([`Skeyword "UNDEF";
             `Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
              ("Fgram.mk_action\n  (fun (i : 'uident)  _  (_loc : FLoc.t)  -> (Und i : 'macro_def ))\n",
                (Fgram.mk_action
                   (fun (i : 'uident)  _  (_loc : FLoc.t)  ->
                      (Und i : 'macro_def )))));
            ([`Skeyword "IFDEF";
             `Snterm
               (Fgram.obj (uident_eval_ifdef : 'uident_eval_ifdef Fgram.t ));
             `Skeyword "THEN";
             `Snterm (Fgram.obj (smlist_then : 'smlist_then Fgram.t ));
             `Snterm (Fgram.obj (else_macro_def : 'else_macro_def Fgram.t ))],
              ("Fgram.mk_action\n  (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ \n     (_loc : FLoc.t)  -> (make_ITE_result st1 st2 : 'macro_def ))\n",
                (Fgram.mk_action
                   (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _ 
                      _  (_loc : FLoc.t)  ->
                      (make_ITE_result st1 st2 : 'macro_def )))));
            ([`Skeyword "IFNDEF";
             `Snterm
               (Fgram.obj (uident_eval_ifndef : 'uident_eval_ifndef Fgram.t ));
             `Skeyword "THEN";
             `Snterm (Fgram.obj (smlist_then : 'smlist_then Fgram.t ));
             `Snterm (Fgram.obj (else_macro_def : 'else_macro_def Fgram.t ))],
              ("Fgram.mk_action\n  (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ \n     (_loc : FLoc.t)  -> (make_ITE_result st1 st2 : 'macro_def ))\n",
                (Fgram.mk_action
                   (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _ 
                      _  (_loc : FLoc.t)  ->
                      (make_ITE_result st1 st2 : 'macro_def )))));
            ([`Skeyword "INCLUDE";
             `Stoken
               (((function | `STR (_,_) -> true | _ -> false)),
                 (`Normal, "`STR (_,_)"))],
              ("Fgram.mk_action\n  (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `STR (_,fname) ->\n         (Lazy (lazy (Fgram.parse_include_file strus fname)) : 'macro_def )\n     | _ -> failwith \"Lazy (lazy (Fgram.parse_include_file strus fname))\n\")\n",
                (Fgram.mk_action
                   (fun (__fan_1 : [> FToken.t])  _  (_loc : FLoc.t)  ->
                      match __fan_1 with
                      | `STR (_,fname) ->
                          (Lazy (lazy (Fgram.parse_include_file strus fname)) : 
                          'macro_def )
                      | _ ->
                          failwith
                            "Lazy (lazy (Fgram.parse_include_file strus fname))\n"))))]));
      Fgram.extend_single (uident_eval_ifdef : 'uident_eval_ifdef Fgram.t )
        (None,
          (None, None,
            [([`Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
               ("Fgram.mk_action\n  (fun (i : 'uident)  (_loc : FLoc.t)  ->\n     (Stack.push (is_defined i) stack : 'uident_eval_ifdef ))\n",
                 (Fgram.mk_action
                    (fun (i : 'uident)  (_loc : FLoc.t)  ->
                       (Stack.push (is_defined i) stack : 'uident_eval_ifdef )))))]));
      Fgram.extend_single (uident_eval_ifndef : 'uident_eval_ifndef Fgram.t )
        (None,
          (None, None,
            [([`Snterm (Fgram.obj (uident : 'uident Fgram.t ))],
               ("Fgram.mk_action\n  (fun (i : 'uident)  (_loc : FLoc.t)  ->\n     (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef ))\n",
                 (Fgram.mk_action
                    (fun (i : 'uident)  (_loc : FLoc.t)  ->
                       (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef )))))]));
      Fgram.extend_single (else_macro_def : 'else_macro_def Fgram.t )
        (None,
          (None, None,
            [([`Skeyword "ELSE";
              `Snterm (Fgram.obj (smlist_else : 'smlist_else Fgram.t ));
              `Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
               ("Fgram.mk_action\n  (fun _  (st : 'smlist_else)  _  (_loc : FLoc.t)  -> (st : 'else_macro_def ))\n",
                 (Fgram.mk_action
                    (fun _  (st : 'smlist_else)  _  (_loc : FLoc.t)  ->
                       (st : 'else_macro_def )))));
            ([`Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
              ("Fgram.mk_action (fun _  (_loc : FLoc.t)  -> ([] : 'else_macro_def ))\n",
                (Fgram.mk_action
                   (fun _  (_loc : FLoc.t)  -> ([] : 'else_macro_def )))))]));
      Fgram.extend_single (else_exp : 'else_exp Fgram.t )
        (None,
          (None, None,
            [([`Skeyword "ELSE";
              `Snterm (Fgram.obj (exp : 'exp Fgram.t ));
              `Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
               ("Fgram.mk_action (fun _  (e : 'exp)  _  (_loc : FLoc.t)  -> (e : 'else_exp ))\n",
                 (Fgram.mk_action
                    (fun _  (e : 'exp)  _  (_loc : FLoc.t)  ->
                       (e : 'else_exp )))));
            ([`Snterm (Fgram.obj (endif : 'endif Fgram.t ))],
              ("Fgram.mk_action\n  (fun _  (_loc : FLoc.t)  -> ((`Uid (_loc, \"()\") : FAst.exp ) : 'else_exp ))\n",
                (Fgram.mk_action
                   (fun _  (_loc : FLoc.t)  ->
                      ((`Uid (_loc, "()") : FAst.exp ) : 'else_exp )))))]));
      Fgram.extend_single (smlist_then : 'smlist_then Fgram.t )
        (None,
          (None, None,
            [([`Slist1
                 (Fgram.srules
                    [([`Snterm (Fgram.obj (macro_def : 'macro_def Fgram.t ));
                      `Skeyword ";"],
                       ("Fgram.mk_action\n  (fun _  (d : 'macro_def)  (_loc : FLoc.t)  ->\n     (execute_macro_if_active_branch ~exp ~pat _loc\n        (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru )\n        (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru )) Then d : 'e__1 ))\n",
                         (Fgram.mk_action
                            (fun _  (d : 'macro_def)  (_loc : FLoc.t)  ->
                               (execute_macro_if_active_branch ~exp ~pat _loc
                                  (`StExp (_loc, (`Uid (_loc, "()"))) : 
                                  FAst.stru )
                                  (fun a  b  ->
                                     (`Sem (_loc, a, b) : FAst.stru )) Then d : 
                               'e__1 )))));
                    ([`Snterm (Fgram.obj (stru : 'stru Fgram.t ));
                     `Skeyword ";"],
                      ("Fgram.mk_action (fun _  (si : 'stru)  (_loc : FLoc.t)  -> (Str si : 'e__1 ))\n",
                        (Fgram.mk_action
                           (fun _  (si : 'stru)  (_loc : FLoc.t)  ->
                              (Str si : 'e__1 )))))])],
               ("Fgram.mk_action\n  (fun (sml : 'e__1 list)  (_loc : FLoc.t)  -> (sml : 'smlist_then ))\n",
                 (Fgram.mk_action
                    (fun (sml : 'e__1 list)  (_loc : FLoc.t)  ->
                       (sml : 'smlist_then )))))]));
      Fgram.extend_single (smlist_else : 'smlist_else Fgram.t )
        (None,
          (None, None,
            [([`Slist1
                 (Fgram.srules
                    [([`Snterm (Fgram.obj (macro_def : 'macro_def Fgram.t ));
                      `Skeyword ";"],
                       ("Fgram.mk_action\n  (fun _  (d : 'macro_def)  (_loc : FLoc.t)  ->\n     (execute_macro_if_active_branch ~exp ~pat _loc\n        (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru )\n        (fun a  b  -> (`Sem (_loc, a, b) : FAst.stru )) Else d : 'e__2 ))\n",
                         (Fgram.mk_action
                            (fun _  (d : 'macro_def)  (_loc : FLoc.t)  ->
                               (execute_macro_if_active_branch ~exp ~pat _loc
                                  (`StExp (_loc, (`Uid (_loc, "()"))) : 
                                  FAst.stru )
                                  (fun a  b  ->
                                     (`Sem (_loc, a, b) : FAst.stru )) Else d : 
                               'e__2 )))));
                    ([`Snterm (Fgram.obj (stru : 'stru Fgram.t ));
                     `Skeyword ";"],
                      ("Fgram.mk_action (fun _  (si : 'stru)  (_loc : FLoc.t)  -> (Str si : 'e__2 ))\n",
                        (Fgram.mk_action
                           (fun _  (si : 'stru)  (_loc : FLoc.t)  ->
                              (Str si : 'e__2 )))))])],
               ("Fgram.mk_action\n  (fun (sml : 'e__2 list)  (_loc : FLoc.t)  -> (sml : 'smlist_else ))\n",
                 (Fgram.mk_action
                    (fun (sml : 'e__2 list)  (_loc : FLoc.t)  ->
                       (sml : 'smlist_else )))))]));
      Fgram.extend_single (endif : 'endif Fgram.t )
        (None,
          (None, None,
            [([`Skeyword "END"],
               ("Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (() : 'endif ))\n",
                 (Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (() : 'endif )))));
            ([`Skeyword "ENDIF"],
              ("Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (() : 'endif ))\n",
                (Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (() : 'endif )))))]));
      Fgram.extend_single (opt_macro_value : 'opt_macro_value Fgram.t )
        (None,
          (None, None,
            [([`Skeyword "(";
              `Slist1sep
                ((Fgram.srules
                    [([`Stoken
                         (((function | `Lid _ -> true | _ -> false)),
                           (`Normal, "`Lid _"))],
                       ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Lid x -> (x : 'e__3 ) | _ -> failwith \"x\n\")\n",
                         (Fgram.mk_action
                            (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t) 
                               ->
                               match __fan_0 with
                               | `Lid x -> (x : 'e__3 )
                               | _ -> failwith "x\n"))))]), (`Skeyword ","));
              `Skeyword ")";
              `Skeyword "=";
              `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
               ("Fgram.mk_action\n  (fun (e : 'exp)  _  _  (pl : 'e__3 list)  _  (_loc : FLoc.t)  ->\n     (Some (pl, e) : 'opt_macro_value ))\n",
                 (Fgram.mk_action
                    (fun (e : 'exp)  _  _  (pl : 'e__3 list)  _ 
                       (_loc : FLoc.t)  -> (Some (pl, e) : 'opt_macro_value )))));
            ([`Skeyword "="; `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
              ("Fgram.mk_action\n  (fun (e : 'exp)  _  (_loc : FLoc.t)  -> (Some ([], e) : 'opt_macro_value ))\n",
                (Fgram.mk_action
                   (fun (e : 'exp)  _  (_loc : FLoc.t)  ->
                      (Some ([], e) : 'opt_macro_value )))));
            ([],
              ("Fgram.mk_action (fun (_loc : FLoc.t)  -> (None : 'opt_macro_value ))\n",
                (Fgram.mk_action
                   (fun (_loc : FLoc.t)  -> (None : 'opt_macro_value )))))]));
      Fgram.extend_single (exp : 'exp Fgram.t )
        ((Some (`Level "top")),
          (None, None,
            [([`Skeyword "IFDEF";
              `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
              `Skeyword "THEN";
              `Sself;
              `Snterm (Fgram.obj (else_exp : 'else_exp Fgram.t ))],
               ("Fgram.mk_action\n  (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _  (_loc : FLoc.t) \n     -> (if is_defined i then e1 else e2 : 'exp ))\n",
                 (Fgram.mk_action
                    (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _ 
                       (_loc : FLoc.t)  ->
                       (if is_defined i then e1 else e2 : 'exp )))));
            ([`Skeyword "IFNDEF";
             `Snterm (Fgram.obj (uident : 'uident Fgram.t ));
             `Skeyword "THEN";
             `Sself;
             `Snterm (Fgram.obj (else_exp : 'else_exp Fgram.t ))],
              ("Fgram.mk_action\n  (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _  (_loc : FLoc.t) \n     -> (if is_defined i then e2 else e1 : 'exp ))\n",
                (Fgram.mk_action
                   (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _ 
                      (_loc : FLoc.t)  ->
                      (if is_defined i then e2 else e1 : 'exp )))));
            ([`Skeyword "DEFINE";
             `Stoken
               (((function | `Lid _ -> true | _ -> false)),
                 (`Normal, "`Lid _"));
             `Skeyword "=";
             `Sself;
             `Skeyword "IN";
             `Sself],
              ("Fgram.mk_action\n  (fun (body : 'exp)  _  (def : 'exp)  _  (__fan_1 : [> FToken.t])  _ \n     (_loc : FLoc.t)  ->\n     match __fan_1 with\n     | `Lid i -> (((new Exp.subst) _loc [(i, def)])#exp body : 'exp )\n     | _ -> failwith \"((new Exp.subst) _loc [(i, def)])#exp body\n\")\n",
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
               ("Fgram.mk_action\n  (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _  (_loc : FLoc.t) \n     -> (if is_defined i then p1 else p2 : 'pat ))\n",
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
              ("Fgram.mk_action\n  (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _  (_loc : FLoc.t) \n     -> (if is_defined i then p2 else p1 : 'pat ))\n",
                (Fgram.mk_action
                   (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _ 
                      (_loc : FLoc.t)  ->
                      (if is_defined i then p2 else p1 : 'pat )))))]));
      Fgram.extend_single (uident : 'uident Fgram.t )
        (None,
          (None, None,
            [([`Stoken
                 (((function | `Uid _ -> true | _ -> false)),
                   (`Normal, "`Uid _"))],
               ("Fgram.mk_action\n  (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->\n     match __fan_0 with | `Uid i -> (i : 'uident ) | _ -> failwith \"i\n\")\n",
                 (Fgram.mk_action
                    (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                       match __fan_0 with
                       | `Uid i -> (i : 'uident )
                       | _ -> failwith "i\n"))))]));
      Fgram.extend_single (exp : 'exp Fgram.t )
        ((Some (`Before "simple")),
          (None, None,
            [([`Skeyword "`";
              Fgram.srules
                [([`Skeyword "IFDEF"],
                   ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                     (Fgram.mk_action
                        (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                           (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "IFNDEF"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "THEN"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "ELSE"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "END"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "ENDIF"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "DEFINE"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))));
                ([`Skeyword "IN"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__4 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__4 )))))]],
               ("Fgram.mk_action\n  (fun (kwd : 'e__4)  _  (_loc : FLoc.t)  ->\n     ((`Vrn (_loc, kwd) : FAst.exp ) : 'exp ))\n",
                 (Fgram.mk_action
                    (fun (kwd : 'e__4)  _  (_loc : FLoc.t)  ->
                       ((`Vrn (_loc, kwd) : FAst.exp ) : 'exp )))));
            ([`Skeyword "`";
             `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
              ("Fgram.mk_action\n  (fun (s : 'luident)  _  (_loc : FLoc.t)  ->\n     ((`Vrn (_loc, s) : FAst.exp ) : 'exp ))\n",
                (Fgram.mk_action
                   (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                      ((`Vrn (_loc, s) : FAst.exp ) : 'exp )))))]));
      Fgram.extend_single (pat : 'pat Fgram.t )
        ((Some (`Before "simple")),
          (None, None,
            [([`Skeyword "`";
              Fgram.srules
                [([`Skeyword "IFDEF"],
                   ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__5 ))\n",
                     (Fgram.mk_action
                        (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                           (Fgram.string_of_token x : 'e__5 )))));
                ([`Skeyword "IFNDEF"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__5 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__5 )))));
                ([`Skeyword "THEN"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__5 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__5 )))));
                ([`Skeyword "ELSE"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__5 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__5 )))));
                ([`Skeyword "END"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__5 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__5 )))));
                ([`Skeyword "ENDIF"],
                  ("Fgram.mk_action\n  (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->\n     (Fgram.string_of_token x : 'e__5 ))\n",
                    (Fgram.mk_action
                       (fun (x : [> FToken.t])  (_loc : FLoc.t)  ->
                          (Fgram.string_of_token x : 'e__5 )))))]],
               ("Fgram.mk_action\n  (fun (kwd : 'e__5)  _  (_loc : FLoc.t)  ->\n     ((`Vrn (_loc, kwd) : FAst.pat ) : 'pat ))\n",
                 (Fgram.mk_action
                    (fun (kwd : 'e__5)  _  (_loc : FLoc.t)  ->
                       ((`Vrn (_loc, kwd) : FAst.pat ) : 'pat )))));
            ([`Skeyword "`";
             `Snterm (Fgram.obj (luident : 'luident Fgram.t ))],
              ("Fgram.mk_action\n  (fun (s : 'luident)  _  (_loc : FLoc.t)  ->\n     ((`Vrn (_loc, s) : FAst.pat ) : 'pat ))\n",
                (Fgram.mk_action
                   (fun (s : 'luident)  _  (_loc : FLoc.t)  ->
                      ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))))]))
    end;
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