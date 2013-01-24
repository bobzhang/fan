open PreCast.Syntax
open FanMacroTools
open Lib
let macro_def = Gram.mk "macro_def"
let macro_def_sig = Gram.mk "macro_def_sig"
let uident_eval_ifdef = Gram.mk "uident_eval_ifdef"
let uident_eval_ifndef = Gram.mk "uident_eval_ifndef"
let else_macro_def = Gram.mk "else_macro_def"
let else_macro_def_sig = Gram.mk "else_macro_def_sig"
let else_expr = Gram.mk "else_expr"
let smlist_then = Gram.mk "smlist_then"
let smlist_else = Gram.mk "smlist_else"
let sglist_then = Gram.mk "sglist_then"
let sglist_else = Gram.mk "sglist_else"
let endif = Gram.mk "endif"
let opt_macro_value = Gram.mk "opt_macro_value"
let uident = Gram.mk "uident"
let apply () =
  (Gram.extend_single (str_item : 'str_item Gram.t )
     ((Some `First),
       ("", `LA,
         [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ))],
            (Gram.mk_action
               (fun (x : 'macro_def)  (_loc : FanLoc.t)  ->
                  (execute_macro ~expr ~patt (`Nil _loc)
                     (fun a  b  -> `Sem (_loc, a, b)) x : 'str_item ))))]));
   Gram.extend_single (sig_item : 'sig_item Gram.t )
     ((Some `First),
       ("", `LA,
         [([`Snterm (Gram.obj (macro_def_sig : 'macro_def_sig Gram.t ))],
            (Gram.mk_action
               (fun (x : 'macro_def_sig)  (_loc : FanLoc.t)  ->
                  (execute_macro ~expr ~patt (`Nil _loc)
                     (fun a  b  -> `Sem (_loc, a, b)) x : 'sig_item ))))]));
   Gram.extend_single (macro_def : 'macro_def Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "DEFINE";
           `Snterm (Gram.obj (uident : 'uident Gram.t ));
           `Snterm (Gram.obj (opt_macro_value : 'opt_macro_value Gram.t ))],
            (Gram.mk_action
               (fun (def : 'opt_macro_value)  (i : 'uident)  _ 
                  (_loc : FanLoc.t)  -> (Def (i, def) : 'macro_def ))));
         ([`Skeyword "UNDEF"; `Snterm (Gram.obj (uident : 'uident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                 (Und i : 'macro_def ))));
         ([`Skeyword "IFDEF";
          `Snterm (Gram.obj (uident_eval_ifdef : 'uident_eval_ifdef Gram.t ));
          `Skeyword "THEN";
          `Snterm (Gram.obj (smlist_then : 'smlist_then Gram.t ));
          `Snterm (Gram.obj (else_macro_def : 'else_macro_def Gram.t ))],
           (Gram.mk_action
              (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ 
                 (_loc : FanLoc.t)  ->
                 (make_ITE_result st1 st2 : 'macro_def ))));
         ([`Skeyword "IFNDEF";
          `Snterm
            (Gram.obj (uident_eval_ifndef : 'uident_eval_ifndef Gram.t ));
          `Skeyword "THEN";
          `Snterm (Gram.obj (smlist_then : 'smlist_then Gram.t ));
          `Snterm (Gram.obj (else_macro_def : 'else_macro_def Gram.t ))],
           (Gram.mk_action
              (fun (st2 : 'else_macro_def)  (st1 : 'smlist_then)  _  _  _ 
                 (_loc : FanLoc.t)  ->
                 (make_ITE_result st1 st2 : 'macro_def ))));
         ([`Skeyword "INCLUDE";
          `Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `STR (_,fname) ->
                     (Lazy (lazy (parse_include_file str_items fname)) : 
                     'macro_def )
                 | _ -> assert false)))]));
   Gram.extend_single (macro_def_sig : 'macro_def_sig Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "DEFINE";
           `Snterm (Gram.obj (uident : 'uident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                  (Def (i, None) : 'macro_def_sig ))));
         ([`Skeyword "UNDEF"; `Snterm (Gram.obj (uident : 'uident Gram.t ))],
           (Gram.mk_action
              (fun (i : 'uident)  _  (_loc : FanLoc.t)  ->
                 (Und i : 'macro_def_sig ))));
         ([`Skeyword "IFDEF";
          `Snterm (Gram.obj (uident_eval_ifdef : 'uident_eval_ifdef Gram.t ));
          `Skeyword "THEN";
          `Snterm (Gram.obj (sglist_then : 'sglist_then Gram.t ));
          `Snterm
            (Gram.obj (else_macro_def_sig : 'else_macro_def_sig Gram.t ))],
           (Gram.mk_action
              (fun (sg2 : 'else_macro_def_sig)  (sg1 : 'sglist_then)  _  _  _
                  (_loc : FanLoc.t)  ->
                 (make_ITE_result sg1 sg2 : 'macro_def_sig ))));
         ([`Skeyword "IFNDEF";
          `Snterm
            (Gram.obj (uident_eval_ifndef : 'uident_eval_ifndef Gram.t ));
          `Skeyword "THEN";
          `Snterm (Gram.obj (sglist_then : 'sglist_then Gram.t ));
          `Snterm
            (Gram.obj (else_macro_def_sig : 'else_macro_def_sig Gram.t ))],
           (Gram.mk_action
              (fun (sg2 : 'else_macro_def_sig)  (sg1 : 'sglist_then)  _  _  _
                  (_loc : FanLoc.t)  ->
                 (make_ITE_result sg1 sg2 : 'macro_def_sig ))));
         ([`Skeyword "INCLUDE";
          `Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `STR (_,fname) ->
                     (Lazy (lazy (parse_include_file sig_items fname)) : 
                     'macro_def_sig )
                 | _ -> assert false)))]));
   Gram.extend_single (uident_eval_ifdef : 'uident_eval_ifdef Gram.t )
     (None,
       ("", `LA,
         [([`Snterm (Gram.obj (uident : 'uident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'uident)  (_loc : FanLoc.t)  ->
                  (Stack.push (is_defined i) stack : 'uident_eval_ifdef ))))]));
   Gram.extend_single (uident_eval_ifndef : 'uident_eval_ifndef Gram.t )
     (None,
       ("", `LA,
         [([`Snterm (Gram.obj (uident : 'uident Gram.t ))],
            (Gram.mk_action
               (fun (i : 'uident)  (_loc : FanLoc.t)  ->
                  (Stack.push (not (is_defined i)) stack : 'uident_eval_ifndef ))))]));
   Gram.extend_single (else_macro_def : 'else_macro_def Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "ELSE";
           `Snterm (Gram.obj (smlist_else : 'smlist_else Gram.t ));
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            (Gram.mk_action
               (fun _  (st : 'smlist_else)  _  (_loc : FanLoc.t)  ->
                  (st : 'else_macro_def ))));
         ([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> ([] : 'else_macro_def ))))]));
   Gram.extend_single (else_macro_def_sig : 'else_macro_def_sig Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "ELSE";
           `Snterm (Gram.obj (sglist_else : 'sglist_else Gram.t ));
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            (Gram.mk_action
               (fun _  (st : 'sglist_else)  _  (_loc : FanLoc.t)  ->
                  (st : 'else_macro_def_sig ))));
         ([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  -> ([] : 'else_macro_def_sig ))))]));
   Gram.extend_single (else_expr : 'else_expr Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "ELSE";
           `Snterm (Gram.obj (expr : 'expr Gram.t ));
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            (Gram.mk_action
               (fun _  (e : 'expr)  _  (_loc : FanLoc.t)  ->
                  (e : 'else_expr ))));
         ([`Snterm (Gram.obj (endif : 'endif Gram.t ))],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  ->
                 (`Id (_loc, (`Uid (_loc, "()"))) : 'else_expr ))))]));
   Gram.extend_single (smlist_then : 'smlist_then Gram.t )
     (None,
       ("", `LA,
         [([`Slist1
              (Gram.srules
                 [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->
                          (execute_macro_if_active_branch ~expr ~patt _loc
                             (`Nil _loc) (fun a  b  -> `Sem (_loc, a, b))
                             Then d : 'e__1 ))));
                 ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   (Gram.mk_action
                      (fun _  (si : 'str_item)  (_loc : FanLoc.t)  ->
                         (Str si : 'e__1 ))))])],
            (Gram.mk_action
               (fun (sml : 'e__1 list)  (_loc : FanLoc.t)  ->
                  (sml : 'smlist_then ))))]));
   Gram.extend_single (smlist_else : 'smlist_else Gram.t )
     (None,
       ("", `LA,
         [([`Slist1
              (Gram.srules
                 [([`Snterm (Gram.obj (macro_def : 'macro_def Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (d : 'macro_def)  (_loc : FanLoc.t)  ->
                          (execute_macro_if_active_branch ~expr ~patt _loc
                             (`Nil _loc) (fun a  b  -> `Sem (_loc, a, b))
                             Else d : 'e__2 ))));
                 ([`Snterm (Gram.obj (str_item : 'str_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   (Gram.mk_action
                      (fun _  (si : 'str_item)  (_loc : FanLoc.t)  ->
                         (Str si : 'e__2 ))))])],
            (Gram.mk_action
               (fun (sml : 'e__2 list)  (_loc : FanLoc.t)  ->
                  (sml : 'smlist_else ))))]));
   Gram.extend_single (sglist_then : 'sglist_then Gram.t )
     (None,
       ("", `LA,
         [([`Slist1
              (Gram.srules
                 [([`Snterm
                      (Gram.obj (macro_def_sig : 'macro_def_sig Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (d : 'macro_def_sig)  (_loc : FanLoc.t)  ->
                          (execute_macro_if_active_branch ~expr ~patt _loc
                             (`Nil _loc) (fun a  b  -> `Sem (_loc, a, b))
                             Then d : 'e__3 ))));
                 ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   (Gram.mk_action
                      (fun _  (si : 'sig_item)  (_loc : FanLoc.t)  ->
                         (Str si : 'e__3 ))))])],
            (Gram.mk_action
               (fun (sgl : 'e__3 list)  (_loc : FanLoc.t)  ->
                  (sgl : 'sglist_then ))))]));
   Gram.extend_single (sglist_else : 'sglist_else Gram.t )
     (None,
       ("", `LA,
         [([`Slist1
              (Gram.srules
                 [([`Snterm
                      (Gram.obj (macro_def_sig : 'macro_def_sig Gram.t ));
                   `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                    (Gram.mk_action
                       (fun _  (d : 'macro_def_sig)  (_loc : FanLoc.t)  ->
                          (execute_macro_if_active_branch ~expr ~patt _loc
                             (`Nil _loc) (fun a  b  -> `Sem (_loc, a, b))
                             Else d : 'e__4 ))));
                 ([`Snterm (Gram.obj (sig_item : 'sig_item Gram.t ));
                  `Snterm (Gram.obj (semi : 'semi Gram.t ))],
                   (Gram.mk_action
                      (fun _  (si : 'sig_item)  (_loc : FanLoc.t)  ->
                         (Str si : 'e__4 ))))])],
            (Gram.mk_action
               (fun (sgl : 'e__4 list)  (_loc : FanLoc.t)  ->
                  (sgl : 'sglist_else ))))]));
   Gram.extend_single (endif : 'endif Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "END"],
            (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif ))));
         ([`Skeyword "ENDIF"],
           (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (() : 'endif ))))]));
   Gram.extend_single (opt_macro_value : 'opt_macro_value Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "(";
           `Slist1sep
             ((Gram.srules
                 [([`Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"))],
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `Lid x -> (x : 'e__5 )
                          | _ -> assert false)))]), (`Skeyword ","));
           `Skeyword ")";
           `Skeyword "=";
           `Snterm (Gram.obj (expr : 'expr Gram.t ))],
            (Gram.mk_action
               (fun (e : 'expr)  _  _  (pl : 'e__5 list)  _ 
                  (_loc : FanLoc.t)  -> (Some (pl, e) : 'opt_macro_value ))));
         ([`Skeyword "="; `Snterm (Gram.obj (expr : 'expr Gram.t ))],
           (Gram.mk_action
              (fun (e : 'expr)  _  (_loc : FanLoc.t)  ->
                 (Some ([], e) : 'opt_macro_value ))));
         ([],
           (Gram.mk_action
              (fun (_loc : FanLoc.t)  -> (None : 'opt_macro_value ))))]));
   Gram.extend_single (expr : 'expr Gram.t )
     ((Some (`Level "top")),
       ("", `LA,
         [([`Skeyword "IFDEF";
           `Snterm (Gram.obj (uident : 'uident Gram.t ));
           `Skeyword "THEN";
           `Sself;
           `Snterm (Gram.obj (else_expr : 'else_expr Gram.t ))],
            (Gram.mk_action
               (fun (e2 : 'else_expr)  (e1 : 'expr)  _  (i : 'uident)  _ 
                  (_loc : FanLoc.t)  ->
                  (if is_defined i then e1 else e2 : 'expr ))));
         ([`Skeyword "IFNDEF";
          `Snterm (Gram.obj (uident : 'uident Gram.t ));
          `Skeyword "THEN";
          `Sself;
          `Snterm (Gram.obj (else_expr : 'else_expr Gram.t ))],
           (Gram.mk_action
              (fun (e2 : 'else_expr)  (e1 : 'expr)  _  (i : 'uident)  _ 
                 (_loc : FanLoc.t)  ->
                 (if is_defined i then e2 else e1 : 'expr ))));
         ([`Skeyword "DEFINE";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"));
          `Skeyword "=";
          `Sself;
          `Skeyword "IN";
          `Sself],
           (Gram.mk_action
              (fun (body : 'expr)  _  (def : 'expr)  _ 
                 (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `Lid i ->
                     (((new Expr.subst) _loc [(i, def)])#expr body : 
                     'expr )
                 | _ -> assert false)))]));
   Gram.extend_single (patt : 'patt Gram.t )
     (None,
       ("", `LA,
         [([`Skeyword "IFDEF";
           `Snterm (Gram.obj (uident : 'uident Gram.t ));
           `Skeyword "THEN";
           `Sself;
           `Skeyword "ELSE";
           `Sself;
           `Snterm (Gram.obj (endif : 'endif Gram.t ))],
            (Gram.mk_action
               (fun _  (p2 : 'patt)  _  (p1 : 'patt)  _  (i : 'uident)  _ 
                  (_loc : FanLoc.t)  ->
                  (if is_defined i then p1 else p2 : 'patt ))));
         ([`Skeyword "IFNDEF";
          `Snterm (Gram.obj (uident : 'uident Gram.t ));
          `Skeyword "THEN";
          `Sself;
          `Skeyword "ELSE";
          `Sself;
          `Snterm (Gram.obj (endif : 'endif Gram.t ))],
           (Gram.mk_action
              (fun _  (p2 : 'patt)  _  (p1 : 'patt)  _  (i : 'uident)  _ 
                 (_loc : FanLoc.t)  ->
                 (if is_defined i then p2 else p1 : 'patt ))))]));
   Gram.extend_single (uident : 'uident Gram.t )
     (None,
       ("", `LA,
         [([`Stoken
              (((function | `Uid _ -> true | _ -> false)),
                (`Normal, "`Uid _"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Uid i -> (i : 'uident )
                  | _ -> assert false)))]));
   Gram.extend_single (expr : 'expr Gram.t )
     ((Some (`Before "simple")),
       ("", `LA,
         [([`Skeyword "`";
           Gram.srules
             [([`Skeyword "IFDEF"],
                (Gram.mk_action
                   (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "IFNDEF"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "THEN"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "ELSE"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "END"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "ENDIF"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "DEFINE"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))));
             ([`Skeyword "IN"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__6 ))))]],
            (Gram.mk_action
               (fun (kwd : 'e__6)  _  (_loc : FanLoc.t)  ->
                  (`Vrn (_loc, kwd) : 'expr ))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           (Gram.mk_action
              (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                 (`Vrn (_loc, s) : 'expr ))))]));
   Gram.extend_single (patt : 'patt Gram.t )
     ((Some (`Before "simple")),
       ("", `LA,
         [([`Skeyword "`";
           Gram.srules
             [([`Skeyword "IFDEF"],
                (Gram.mk_action
                   (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                      (Gram.string_of_token x : 'e__7 ))));
             ([`Skeyword "IFNDEF"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__7 ))));
             ([`Skeyword "THEN"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__7 ))));
             ([`Skeyword "ELSE"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__7 ))));
             ([`Skeyword "END"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__7 ))));
             ([`Skeyword "ENDIF"],
               (Gram.mk_action
                  (fun (x : [> FanToken.t])  (_loc : FanLoc.t)  ->
                     (Gram.string_of_token x : 'e__7 ))))]],
            (Gram.mk_action
               (fun (kwd : 'e__7)  _  (_loc : FanLoc.t)  ->
                  (`Vrn (_loc, kwd) : 'patt ))));
         ([`Skeyword "`"; `Snterm (Gram.obj (luident : 'luident Gram.t ))],
           (Gram.mk_action
              (fun (s : 'luident)  _  (_loc : FanLoc.t)  ->
                 (`Vrn (_loc, s) : 'patt ))))])));
  Options.add
    ("-D", (FanArg.String (parse_def ~expr ~patt)),
      "<string> Define for IFDEF instruction.");
  Options.add
    ("-U", (FanArg.String (undef ~expr ~patt)),
      "<string> Undefine for IFDEF instruction.");
  Options.add
    ("-I", (FanArg.String add_include_dir),
      "<string> Add a directory to INCLUDE search path.")
let _ = AstParsers.register_parser ("macro", apply)