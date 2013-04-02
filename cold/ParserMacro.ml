open PreCast.Syntax
open FanMacroTools
let macro_def = Gram.mk "macro_def"
let uident_eval_ifdef = Gram.mk "uident_eval_ifdef"
let uident_eval_ifndef = Gram.mk "uident_eval_ifndef"
let else_macro_def = Gram.mk "else_macro_def"
let else_exp = Gram.mk "else_exp"
let smlist_then = Gram.mk "smlist_then"
let smlist_else = Gram.mk "smlist_else"
let endif = Gram.mk "endif"
let opt_macro_value = Gram.mk "opt_macro_value"
let uident = Gram.mk "uident"
let apply () =
  (Gram.extend_single (stru : 'stru Gram.t )
     ((Some `First),
       (None, None,
         [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'macro_def)  (_loc : FanLoc.t)  ->\n     (execute_macro ~exp ~pat\n        (`StExp (_loc, (`Id (_loc, (`Uid (_loc, \"()\"))))))\n        (fun a  b  -> `Sem (_loc, a, b)) x : 'stru ))\n",
              (Gram.mk_action
                 (fun (x : 'macro_def)  (_loc : FanLoc.t)  ->
                    (execute_macro ~exp ~pat
                       (`StExp (_loc, (`Id (_loc, (`Uid (_loc, "()"))))))
                       (fun a  b  -> `Sem (_loc, a, b)) x : 'stru )))))]));
   Gram.extend_single (macro_def : 'macro_def Gram.t )
     (None,
       (None, None,
         [([`Skeyword "DEFINE";
           `Snterm (Gram.obj (uident : 'uident Gram.t ));
           `Snterm (Gram.obj (opt_macro_value : 'opt_macro_value Gram.t ))],
            ("Gram.mk_action\n  (fun (def : 'opt_macro_value)  (i : 'uident)  _  (_loc : FanLoc.t)  ->\n     (Def (i, def) : 'macro_def ))\n",
              (Gram.mk_action
                 (fun (def : 'opt_macro_value)  (i : 'uident)  _ 
                    (_loc : FanLoc.t)  -> (Def (i, def) : 'macro_def )))));
         ([`Skeyword "UNDEF"; `Snterm (Gram.obj (uident : 'uident Gram.t ))],
           ("Gram.mk_action\n  (fun (i : 'uident)  _  (_loc : FanLoc.t)  -> (Und i : 'macro_def ))\n",
             (Gram.mk_action
                (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                   (Und i : 'macro_def )))));
         ([`Skeyword "IFDEF";
          `Snterm (Gram.obj (uident_eval_ifdef : 'uident_eval_ifdef Gram.t ));
          `Skeyword "THEN";
          `Snterm (Gram.obj (smlist_then : 'smlist_then Gram.t ));
          `Snterm (Gram.obj (else_macro_def : 'else_macro_def Gram.t ))],
           ("Gram.mk_action\n  (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ \n     (_loc : FanLoc.t)  -> (make_ITE_result st1 st2 : 'macro_def ))\n",
             (Gram.mk_action
                (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ 
                   (_loc : FanLoc.t)  ->
                   (make_ITE_result st1 st2 : 'macro_def )))));
         ([`Skeyword "IFNDEF";
          `Snterm
            (Gram.obj (uident_eval_ifndef : 'uident_eval_ifndef Gram.t ));
          `Skeyword "THEN";
          `Snterm (Gram.obj (smlist_then : 'smlist_then Gram.t ));
          `Snterm (Gram.obj (else_macro_def : 'else_macro_def Gram.t ))],
           ("Gram.mk_action\n  (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ \n     (_loc : FanLoc.t)  -> (make_ITE_result st1 st2 : 'macro_def ))\n",
             (Gram.mk_action
                (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ 
                   (_loc : FanLoc.t)  ->
                   (make_ITE_result st1 st2 : 'macro_def )))));
         ([`Skeyword "INCLUDE";
          `Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `STR (_,fname) ->\n         (Lazy (lazy (FanBasic.parse_include_file strus fname)) : 'macro_def )\n     | _ ->\n         failwith \"Lazy (lazy (FanBasic.parse_include_file strus fname))\n\")\n",
             (Gram.mk_action
                (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `STR (_,fname) ->
                       (Lazy (lazy (FanBasic.parse_include_file strus fname)) : 
                       'macro_def )
                   | _ ->
                       failwith
                         "Lazy (lazy (FanBasic.parse_include_file strus fname))\n"))))]));
   Gram.extend_single (uident_eval_ifdef : 'uident_eval_ifdef Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (uident : 'uident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'uident)  (_loc : FanLoc.t)  ->\n     (Stack.push (is_defined i) stack : 'uident_eval_ifdef ))\n",
              (Gram.mk_action
                 (fun (i : 'uident)  (_loc : FanLoc.t)  ->
                    (Stack.push (is_defined i) stack : 'uident_eval_ifdef )))))]));
   Gram.extend_single (uident_eval_ifndef : 'uident_eval_ifndef Gram.t )
     (None,
       (None, None,
         [([`Snterm (Gram.obj (uident : 'uident Gram.t ))],
            ("Gram.mk_action\n  (fun (i : 'uident)  (_loc : FanLoc.t)  ->\n     (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef ))\n",
              (Gram.mk_action
                 (fun (i : 'uident)  (_loc : FanLoc.t)  ->
                    (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef )))))]));
   Gram.extend_single (else_macro_def : 'else_macro_def Gram.t )
     (None,
       (None, None,
         [([`Skeyword "ELSE";
           `Snterm (Gram.obj (smlist_else : 'smlist_else Gram.t ));
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            ("Gram.mk_action\n  (fun _  (st : 'smlist_else)  _  (_loc : FanLoc.t)  ->\n     (st : 'else_macro_def ))\n",
              (Gram.mk_action
                 (fun _  (st : 'smlist_else)  _  (_loc : FanLoc.t)  ->
                    (st : 'else_macro_def )))));
         ([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> ([] : 'else_macro_def ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> ([] : 'else_macro_def )))))]));
   Gram.extend_single (else_exp : 'else_exp Gram.t )
     (None,
       (None, None,
         [([`Skeyword "ELSE";
           `Snterm (Gram.obj (exp : 'exp Gram.t ));
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            ("Gram.mk_action (fun _  (e : 'exp)  _  (_loc : FanLoc.t)  -> (e : 'else_exp ))\n",
              (Gram.mk_action
                 (fun _  (e : 'exp)  _  (_loc : FanLoc.t)  ->
                    (e : 'else_exp )))));
         ([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  ->\n     (`Id (_loc, (`Uid (_loc, \"()\"))) : 'else_exp ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (`Id (_loc, (`Uid (_loc, "()"))) : 'else_exp )))))]));
   Gram.extend_single (smlist_then : 'smlist_then Gram.t )
     (None,
       (None, None,
         [([`Slist1
              (Gram.srules
                 [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ));
                   `Skeyword ";"],
                    ("Gram.mk_action\n  (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->\n     (execute_macro_if_active_branch ~exp ~pat _loc\n        (`StExp (_loc, (`Id (_loc, (`Uid (_loc, \"()\"))))))\n        (fun a  b  -> `Sem (_loc, a, b)) Then d : 'e__1 ))\n",
                      (Gram.mk_action
                         (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->
                            (execute_macro_if_active_branch ~exp ~pat _loc
                               (`StExp
                                  (_loc, (`Id (_loc, (`Uid (_loc, "()"))))))
                               (fun a  b  -> `Sem (_loc, a, b)) Then d : 
                            'e__1 )))));
                 ([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";"],
                   ("Gram.mk_action (fun _  (si : 'stru)  (_loc : FanLoc.t)  -> (Str si : 'e__1 ))\n",
                     (Gram.mk_action
                        (fun _  (si : 'stru)  (_loc : FanLoc.t)  ->
                           (Str si : 'e__1 )))))])],
            ("Gram.mk_action\n  (fun (sml : 'e__1 list)  (_loc : FanLoc.t)  -> (sml : 'smlist_then ))\n",
              (Gram.mk_action
                 (fun (sml : 'e__1 list)  (_loc : FanLoc.t)  ->
                    (sml : 'smlist_then )))))]));
   Gram.extend_single (smlist_else : 'smlist_else Gram.t )
     (None,
       (None, None,
         [([`Slist1
              (Gram.srules
                 [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ));
                   `Skeyword ";"],
                    ("Gram.mk_action\n  (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->\n     (execute_macro_if_active_branch ~exp ~pat _loc\n        (`StExp (_loc, (`Id (_loc, (`Uid (_loc, \"()\"))))))\n        (fun a  b  -> `Sem (_loc, a, b)) Else d : 'e__2 ))\n",
                      (Gram.mk_action
                         (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->
                            (execute_macro_if_active_branch ~exp ~pat _loc
                               (`StExp
                                  (_loc, (`Id (_loc, (`Uid (_loc, "()"))))))
                               (fun a  b  -> `Sem (_loc, a, b)) Else d : 
                            'e__2 )))));
                 ([`Snterm (Gram.obj (stru : 'stru Gram.t )); `Skeyword ";"],
                   ("Gram.mk_action (fun _  (si : 'stru)  (_loc : FanLoc.t)  -> (Str si : 'e__2 ))\n",
                     (Gram.mk_action
                        (fun _  (si : 'stru)  (_loc : FanLoc.t)  ->
                           (Str si : 'e__2 )))))])],
            ("Gram.mk_action\n  (fun (sml : 'e__2 list)  (_loc : FanLoc.t)  -> (sml : 'smlist_else ))\n",
              (Gram.mk_action
                 (fun (sml : 'e__2 list)  (_loc : FanLoc.t)  ->
                    (sml : 'smlist_else )))))]));
   Gram.extend_single (endif : 'endif Gram.t )
     (None,
       (None, None,
         [([`Skeyword "END"],
            ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif ))\n",
              (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif )))));
         ([`Skeyword "ENDIF"],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif ))\n",
             (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif )))))]));
   Gram.extend_single (opt_macro_value : 'opt_macro_value Gram.t )
     (None,
       (None, None,
         [([`Skeyword "(";
           `Slist1sep
             ((Gram.srules
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Lid x -> (x : 'e__3 ) | _ -> failwith \"x\n\")\n",
                      (Gram.mk_action
                         (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) 
                            ->
                            match __fan_0 with
                            | `Lid x -> (x : 'e__3 )
                            | _ -> failwith "x\n"))))]), (`Skeyword ","));
           `Skeyword ")";
           `Skeyword "=";
           `Snterm (Gram.obj (exp : 'exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e : 'exp)  _  _  (pl : 'e__3 list)  _  (_loc : FanLoc.t)  ->\n     (Some (pl, e) : 'opt_macro_value ))\n",
              (Gram.mk_action
                 (fun (e : 'exp)  _  _  (pl : 'e__3 list)  _ 
                    (_loc : FanLoc.t)  -> (Some (pl, e) : 'opt_macro_value )))));
         ([`Skeyword "="; `Snterm (Gram.obj (exp : 'exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->\n     (Some ([], e) : 'opt_macro_value ))\n",
             (Gram.mk_action
                (fun (e : 'exp)  _  (_loc : FanLoc.t)  ->
                   (Some ([], e) : 'opt_macro_value )))));
         ([],
           ("Gram.mk_action (fun (_loc : FanLoc.t)  -> (None : 'opt_macro_value ))\n",
             (Gram.mk_action
                (fun (_loc : FanLoc.t)  -> (None : 'opt_macro_value )))))]));
   Gram.extend_single (exp : 'exp Gram.t )
     ((Some (`Level "top")),
       (None, None,
         [([`Skeyword "IFDEF";
           `Snterm (Gram.obj (uident : 'uident Gram.t ));
           `Skeyword "THEN";
           `Sself;
           `Snterm (Gram.obj (else_exp : 'else_exp Gram.t ))],
            ("Gram.mk_action\n  (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _  (_loc : FanLoc.t) \n     -> (if is_defined i then e1 else e2 : 'exp ))\n",
              (Gram.mk_action
                 (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _ 
                    (_loc : FanLoc.t)  ->
                    (if is_defined i then e1 else e2 : 'exp )))));
         ([`Skeyword "IFNDEF";
          `Snterm (Gram.obj (uident : 'uident Gram.t ));
          `Skeyword "THEN";
          `Sself;
          `Snterm (Gram.obj (else_exp : 'else_exp Gram.t ))],
           ("Gram.mk_action\n  (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _  (_loc : FanLoc.t) \n     -> (if is_defined i then e2 else e1 : 'exp ))\n",
             (Gram.mk_action
                (fun (e2 : 'else_exp)  (e1 : 'exp)  _  (i : 'uident)  _ 
                   (_loc : FanLoc.t)  ->
                   (if is_defined i then e2 else e1 : 'exp )))));
         ([`Skeyword "DEFINE";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"));
          `Skeyword "=";
          `Sself;
          `Skeyword "IN";
          `Sself],
           ("Gram.mk_action\n  (fun (body : 'exp)  _  (def : 'exp)  _  (__fan_1 : [> FanToken.t])  _ \n     (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Lid i -> (((new Exp.subst) _loc [(i, def)])#exp body : 'exp )\n     | _ -> failwith \"((new Exp.subst) _loc [(i, def)])#exp body\n\")\n",
             (Gram.mk_action
                (fun (body : 'exp)  _  (def : 'exp)  _ 
                   (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                   match __fan_1 with
                   | `Lid i ->
                       (((new Exp.subst) _loc [(i, def)])#exp body : 
                       'exp )
                   | _ ->
                       failwith
                         "((new Exp.subst) _loc [(i, def)])#exp body\n"))))]));
   Gram.extend_single (pat : 'pat Gram.t )
     (None,
       (None, None,
         [([`Skeyword "IFDEF";
           `Snterm (Gram.obj (uident : 'uident Gram.t ));
           `Skeyword "THEN";
           `Sself;
           `Skeyword "ELSE";
           `Sself;
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            ("Gram.mk_action\n  (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _  (_loc : FanLoc.t)\n      -> (if is_defined i then p1 else p2 : 'pat ))\n",
              (Gram.mk_action
                 (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _ 
                    (_loc : FanLoc.t)  ->
                    (if is_defined i then p1 else p2 : 'pat )))));
         ([`Skeyword "IFNDEF";
          `Snterm (Gram.obj (uident : 'uident Gram.t ));
          `Skeyword "THEN";
          `Sself;
          `Skeyword "ELSE";
          `Sself;
          `Snterm (Gram.obj (endif : 'endif Gram.t ))],
           ("Gram.mk_action\n  (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _  (_loc : FanLoc.t)\n      -> (if is_defined i then p2 else p1 : 'pat ))\n",
             (Gram.mk_action
                (fun _  (p2 : 'pat)  _  (p1 : 'pat)  _  (i : 'uident)  _ 
                   (_loc : FanLoc.t)  ->
                   (if is_defined i then p2 else p1 : 'pat )))))]));
   Gram.extend_single (uident : 'uident Gram.t )
     (None,
       (None, None,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
            ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid i -> (i : 'uident ) | _ -> failwith \"i\n\")\n",
              (Gram.mk_action
                 (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                    match __fan_0 with
                    | `Uid i -> (i : 'uident )
                    | _ -> failwith "i\n"))))]));
   Gram.extend_single (exp : 'exp Gram.t )
     ((Some (`Before "simple")),
       (None, None,
         [([`Skeyword "`";
           Gram.srules
             [([`Skeyword "IFDEF"],
                ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                  (Gram.mk_action
                     (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "IFNDEF"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "THEN"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "ELSE"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "END"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "ENDIF"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "DEFINE"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))));
             ([`Skeyword "IN"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__4 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__4 )))))]],
            ("Gram.mk_action\n  (fun (kwd : 'e__4)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, kwd) : 'exp ))\n",
              (Gram.mk_action
                 (fun (kwd : 'e__4)  _  (_loc : FanLoc.t)  ->
                    (`Vrn (_loc, kwd) : 'exp )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, s) : 'exp ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'exp )))))]));
   Gram.extend_single (pat : 'pat Gram.t )
     ((Some (`Before "simple")),
       (None, None,
         [([`Skeyword "`";
           Gram.srules
             [([`Skeyword "IFDEF"],
                ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__5 ))\n",
                  (Gram.mk_action
                     (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                        (Gram.string_of_token x : 'e__5 )))));
             ([`Skeyword "IFNDEF"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__5 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__5 )))));
             ([`Skeyword "THEN"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__5 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__5 )))));
             ([`Skeyword "ELSE"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__5 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__5 )))));
             ([`Skeyword "END"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__5 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__5 )))));
             ([`Skeyword "ENDIF"],
               ("Gram.mk_action\n  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     (Gram.string_of_token x : 'e__5 ))\n",
                 (Gram.mk_action
                    (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                       (Gram.string_of_token x : 'e__5 )))))]],
            ("Gram.mk_action\n  (fun (kwd : 'e__5)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, kwd) : 'pat ))\n",
              (Gram.mk_action
                 (fun (kwd : 'e__5)  _  (_loc : FanLoc.t)  ->
                    (`Vrn (_loc, kwd) : 'pat )))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           ("Gram.mk_action\n  (fun (s : 'luident)  _  (_loc : FanLoc.t)  -> (`Vrn (_loc, s) : 'pat ))\n",
             (Gram.mk_action
                (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                   (`Vrn (_loc, s) : 'pat )))))])));
  Options.add
    ("-D", (FanArg.String (parse_def ~exp ~pat)),
      "<string> Define for IFDEF instruction.");
  Options.add
    ("-U", (FanArg.String (undef ~exp ~pat)),
      "<string> Undefine for IFDEF instruction.");
  Options.add
    ("-I", (FanArg.String FanBasic.add_include_dir),
      "<string> Add a directory to INCLUDE search path.")
let _ = AstParsers.register_parser ("macro", apply)