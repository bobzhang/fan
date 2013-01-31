open Lib
open LibUtil
open PreCast.Syntax
let regexp = Gram.mk "regexp"
let chr = Gram.mk "chr"
let ch_class = Gram.mk "ch_class"
let regexps = Gram.mk "regexps"
let lex = Gram.mk "lex"
let declare_regexp = Gram.mk "declare_regexp"
let _ =
  Gram.extend_single (lex : 'lex Gram.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Slist0sep
            ((Gram.srules
                [([`Snterm (Gram.obj (regexp : 'regexp Gram.t ));
                  `Skeyword "->";
                  `Snterm (Gram.obj (sequence : 'sequence Gram.t ))],
                   ("Gram.mk_action\n  (fun (a : 'sequence)  _  (r : 'regexp)  (_loc : FanLoc.t)  ->\n     ((r, (Expr.mksequence a)) : 'e__1 ))\n",
                     (Gram.mk_action
                        (fun (a : 'sequence)  _  (r : 'regexp) 
                           (_loc : FanLoc.t)  ->
                           ((r, (Expr.mksequence a)) : 'e__1 )))))]),
              (`Skeyword "|"))],
           ("Gram.mk_action\n  (fun (l : 'e__1 list)  _  (_loc : FanLoc.t)  ->\n     (FanLexTools.gen_definition _loc l : 'lex ))\n",
             (Gram.mk_action
                (fun (l : 'e__1 list)  _  (_loc : FanLoc.t)  ->
                   (FanLexTools.gen_definition _loc l : 'lex )))))]));
  Gram.extend_single (declare_regexp : 'declare_regexp Gram.t )
    (None,
      (None, None,
        [([`Smeta
             (["FOLD1"; "SEP"],
               [Gram.srules
                  [([`Stoken
                       (((function | `Lid _ -> true | _ -> false)),
                         (`Normal, "`Lid _"));
                    `Skeyword ":";
                    `Snterm (Gram.obj (regexp : 'regexp Gram.t ))],
                     ("Gram.mk_action\n  (fun (r : 'regexp)  _  (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x -> ((x, r) : 'e__2 )\n     | _ -> failwith \"(x, r)\n\")\n",
                       (Gram.mk_action
                          (fun (r : 'regexp)  _  (__fan_0 : [> FanToken.t]) 
                             (_loc : FanLoc.t)  ->
                             match __fan_0 with
                             | `Lid x -> ((x, r) : 'e__2 )
                             | _ -> failwith "(x, r)\n"))))];
               `Skeyword ";"],
               (Gram.Action.mk
                  (Gram.sfold1sep
                     (fun (x,r)  ()  ->
                        if Hashtbl.mem FanLexTools.named_regexps x
                        then
                          Printf.eprintf
                            "pa_ulex (warning): multiple definition of named regexp '%s'\n"
                            x
                        else ();
                        Hashtbl.add FanLexTools.named_regexps x r) () : 
                  (_,'e__2,'e__3) Gram.foldsep )))],
           ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (`Nil _loc : 'declare_regexp ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  -> (`Nil _loc : 'declare_regexp )))))]));
  Gram.extend_single (regexps : 'regexps Gram.t )
    (None,
      (None, None,
        [([`Skeyword "{";
          `Slist1sep
            ((`Snterm (Gram.obj (regexp : 'regexp Gram.t ))),
              (`Skeyword ";"));
          `Skeyword "}"],
           ("Gram.mk_action\n  (fun _  (xs : 'regexp list)  _  (_loc : FanLoc.t)  ->\n     (Array.of_list xs : 'regexps ))\n",
             (Gram.mk_action
                (fun _  (xs : 'regexp list)  _  (_loc : FanLoc.t)  ->
                   (Array.of_list xs : 'regexps )))))]));
  Gram.extend (regexp : 'regexp Gram.t )
    (None,
      [(None, None,
         [([`Sself; `Skeyword "|"; `Sself],
            ("Gram.mk_action\n  (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (FanLexTools.alt r1 r2 : 'regexp ))\n",
              (Gram.mk_action
                 (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t) 
                    -> (FanLexTools.alt r1 r2 : 'regexp )))))]);
      (None, None,
        [([`Sself; `Sself],
           ("Gram.mk_action\n  (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (FanLexTools.seq r1 r2 : 'regexp ))\n",
             (Gram.mk_action
                (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                   (FanLexTools.seq r1 r2 : 'regexp )))))]);
      (None, None,
        [([`Sself; `Skeyword "*"],
           ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (FanLexTools.rep r1 : 'regexp ))\n",
             (Gram.mk_action
                (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                   (FanLexTools.rep r1 : 'regexp )))));
        ([`Sself; `Skeyword "+"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (FanLexTools.plus r1 : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (FanLexTools.plus r1 : 'regexp )))));
        ([`Sself; `Skeyword "?"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (FanLexTools.alt FanLexTools.eps r1 : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (FanLexTools.alt FanLexTools.eps r1 : 'regexp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  _  (_loc : FanLoc.t)  -> (r1 : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  _  (_loc : FanLoc.t)  ->
                  (r1 : 'regexp )))));
        ([`Skeyword "_"],
          ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (FanLexTools.chars LexSet.any : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (FanLexTools.chars LexSet.any : 'regexp )))));
        ([`Snterm (Gram.obj (chr : 'chr Gram.t ))],
          ("Gram.mk_action\n  (fun (c : 'chr)  (_loc : FanLoc.t)  ->\n     (FanLexTools.chars (LexSet.singleton c) : 'regexp ))\n",
            (Gram.mk_action
               (fun (c : 'chr)  (_loc : FanLoc.t)  ->
                  (FanLexTools.chars (LexSet.singleton c) : 'regexp )))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (s,_) -> (FanLexTools.of_string s : 'regexp )\n     | _ -> failwith \"FanLexTools.of_string s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (s,_) -> (FanLexTools.of_string s : 'regexp )
                  | _ -> failwith "FanLexTools.of_string s\n"))));
        ([`Skeyword "[";
         `Snterm (Gram.obj (ch_class : 'ch_class Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->\n     (FanLexTools.chars cc : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->
                  (FanLexTools.chars cc : 'regexp )))));
        ([`Skeyword "[^";
         `Snterm (Gram.obj (ch_class : 'ch_class Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->\n     (FanLexTools.chars (LexSet.difference LexSet.any cc) : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->
                  (FanLexTools.chars (LexSet.difference LexSet.any cc) : 
                  'regexp )))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find FanLexTools.named_regexps x\n           with\n           | Not_found  ->\n               failwithf \"referenced to unbound named  regexp  `%s'\" x) : \n         'regexp )\n     | _ ->\n         failwith\n           \"try Hashtbl.find FanLexTools.named_regexps x\nwith | Not_found  -> failwithf \"referenced to unbound named  regexp  `%s'\" x\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find FanLexTools.named_regexps x
                        with
                        | Not_found  ->
                            failwithf
                              "referenced to unbound named  regexp  `%s'" x) : 
                      'regexp )
                  | _ ->
                      failwith
                        "try Hashtbl.find FanLexTools.named_regexps x\nwith | Not_found  -> failwithf \"referenced to unbound named  regexp  `%s'\" x\n"))))])]);
  Gram.extend_single (chr : 'chr Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (c,_) -> (Char.code c : 'chr )\n     | _ -> failwith \"Char.code c\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `CHAR (c,_) -> (Char.code c : 'chr )
                   | _ -> failwith "Char.code c\n"))));
        ([`Stoken
            (((function | `INT (_,_) -> true | _ -> false)),
              (`Normal, "`INT (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `INT (i,s) ->\n         (if (i >= 0) && (i <= LexSet.max_code)\n          then i\n          else failwithf \"Invalid Unicode code point:%s\" s : 'chr )\n     | _ ->\n         failwith\n           \"if (i >= 0) && (i <= LexSet.max_code)\nthen i\nelse failwithf \"Invalid Unicode code point:%s\" s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `INT (i,s) ->
                      (if (i >= 0) && (i <= LexSet.max_code)
                       then i
                       else failwithf "Invalid Unicode code point:%s" s : 
                      'chr )
                  | _ ->
                      failwith
                        "if (i >= 0) && (i <= LexSet.max_code)\nthen i\nelse failwithf \"Invalid Unicode code point:%s\" s\n"))))]));
  Gram.extend_single (ch_class : 'ch_class Gram.t )
    (None,
      (None, None,
        [([`Snterm (Gram.obj (chr : 'chr Gram.t ));
          `Skeyword "-";
          `Snterm (Gram.obj (chr : 'chr Gram.t ))],
           ("Gram.mk_action\n  (fun (c2 : 'chr)  _  (c1 : 'chr)  (_loc : FanLoc.t)  ->\n     (LexSet.interval c1 c2 : 'ch_class ))\n",
             (Gram.mk_action
                (fun (c2 : 'chr)  _  (c1 : 'chr)  (_loc : FanLoc.t)  ->
                   (LexSet.interval c1 c2 : 'ch_class )))));
        ([`Snterm (Gram.obj (chr : 'chr Gram.t ))],
          ("Gram.mk_action\n  (fun (c : 'chr)  (_loc : FanLoc.t)  -> (LexSet.singleton c : 'ch_class ))\n",
            (Gram.mk_action
               (fun (c : 'chr)  (_loc : FanLoc.t)  ->
                  (LexSet.singleton c : 'ch_class )))));
        ([`Sself; `Sself],
          ("Gram.mk_action\n  (fun (cc2 : 'ch_class)  (cc1 : 'ch_class)  (_loc : FanLoc.t)  ->\n     (LexSet.union cc1 cc2 : 'ch_class ))\n",
            (Gram.mk_action
               (fun (cc2 : 'ch_class)  (cc1 : 'ch_class)  (_loc : FanLoc.t) 
                  -> (LexSet.union cc1 cc2 : 'ch_class )))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (s,_) ->\n         (let c = ref LexSet.empty in\n          (for i = 0 to (String.length s) - 1 do\n             c :=\n               (LexSet.union c.contents\n                  (LexSet.singleton (Char.code (s.[i]))))\n           done;\n           c.contents) : 'ch_class )\n     | _ ->\n         failwith\n           \"let c = ref LexSet.empty in\nfor i = 0 to (String.length s) - 1 do\n  c := (LexSet.union c.contents (LexSet.singleton (Char.code (s.[i]))))\ndone;\nc.contents\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (s,_) ->
                      (let c = ref LexSet.empty in
                       (for i = 0 to (String.length s) - 1 do
                          c :=
                            (LexSet.union c.contents
                               (LexSet.singleton (Char.code (s.[i]))))
                        done;
                        c.contents) : 'ch_class )
                  | _ ->
                      failwith
                        "let c = ref LexSet.empty in\nfor i = 0 to (String.length s) - 1 do\n  c := (LexSet.union c.contents (LexSet.singleton (Char.code (s.[i]))))\ndone;\nc.contents\n"))))]))
let d = `Absolute ["Fan"; "Lang"; "Lex"]
let _ = AstQuotation.of_expr ~name:(d, "lex") ~entry:lex
let _ = AstQuotation.of_str_item ~name:(d, "reg") ~entry:declare_regexp