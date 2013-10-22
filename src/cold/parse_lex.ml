open Translate_lex
let named_regexps: (string,concrete_regexp) Hashtbl.t = Hashtbl.create 13
let _ = Hashtbl.add named_regexps "eof" Eof
exception UnboundRegexp
let g =
  Fgram.create_lexer ~annot:"Lexer's lexer"
    ~keywords:["as";
              "eof";
              "let";
              "#";
              "|";
              "^";
              "<";
              "->";
              "=";
              "_";
              "*";
              "[";
              "]";
              "*";
              "?";
              "+";
              "(";
              ")";
              "-"] ()
let regexp = Fgram.mk_dynamic g "regexp"
let char_class = Fgram.mk_dynamic g "char_class"
let char_class1 = Fgram.mk_dynamic g "char_class1"
let lex = Fgram.mk_dynamic g "lex"
let declare_regexp = Fgram.mk_dynamic g "declare_regexp"
let _ =
  let grammar_entry_create x = Fgram.mk_dynamic g x in
  let case: 'case Fgram.t = grammar_entry_create "case"
  and lid: 'lid Fgram.t = grammar_entry_create "lid" in
  Fgram.extend_single (lex : 'lex Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Slist0sep
            ((`Snterm (Fgram.obj (case : 'case Fgram.t ))), (`Skeyword "|"))],
           ("Compile_lex.output_entry @@\n  (Lexgen.make_single_dfa { shortest = false; clauses = l })\n",
             (Fgram.mk_action
                (fun (l : 'case list)  _  (_loc : Locf.t)  ->
                   (Compile_lex.output_entry @@
                      (Lexgen.make_single_dfa
                         { shortest = false; clauses = l }) : 'lex )))));
        ([`Skeyword "<";
         `Slist0sep
           ((`Snterm (Fgram.obj (case : 'case Fgram.t ))), (`Skeyword "|"))],
          ("Compile_lex.output_entry @@\n  (Lexgen.make_single_dfa { shortest = true; clauses = l })\n",
            (Fgram.mk_action
               (fun (l : 'case list)  _  (_loc : Locf.t)  ->
                  (Compile_lex.output_entry @@
                     (Lexgen.make_single_dfa { shortest = true; clauses = l }) : 
                  'lex )))))]));
  Fgram.extend_single (case : 'case Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (regexp : 'regexp Fgram.t ));
          `Stoken
            (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
              "`Quot _")],
           ("let expander loc _ s = Fgram.parse_string ~loc Syntaxf.exp s in\nlet e = Ftoken.quot_expand expander x in (r, e)\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  (r : 'regexp)  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Quot x ->
                       (let expander loc _ s =
                          Fgram.parse_string ~loc Syntaxf.exp s in
                        let e = Ftoken.quot_expand expander x in (r, e) : 
                       'case )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]));
  Fgram.extend_single (declare_regexp : 'declare_regexp Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "let";
          `Stoken
            (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
              "`Lid x");
          `Skeyword "=";
          `Snterm (Fgram.obj (regexp : 'regexp Fgram.t ))],
           ("if Hashtbl.mem named_regexps x\nthen\n  (Printf.eprintf\n     \"fanlex (warning): multiple definition of named regexp '%s'\n\" x;\n   exit 2)\nelse\n  (Hashtbl.add named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru ))\n",
             (Fgram.mk_action
                (fun (r : 'regexp)  _  (__fan_1 : Ftoken.t)  _ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Lid (_,x) ->
                       (if Hashtbl.mem named_regexps x
                        then
                          (Printf.eprintf
                             "fanlex (warning): multiple definition of named regexp '%s'\n"
                             x;
                           exit 2)
                        else
                          (Hashtbl.add named_regexps x r;
                           (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru )) : 
                       'declare_regexp )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))));
        ([`Sself; `Sself],
          ("x\n",
            (Fgram.mk_action
               (fun (x : 'declare_regexp)  _  (_loc : Locf.t)  ->
                  (x : 'declare_regexp )))))]));
  Fgram.extend_single (lid : 'lid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid x")],
           ("(_loc, y)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,y) -> ((_loc, y) : 'lid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend (regexp : 'regexp Fgram.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Snterm (Fgram.obj (lid : 'lid Fgram.t ))],
            ("Bind (r1, z)\n",
              (Fgram.mk_action
                 (fun (z : 'lid)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Bind (r1, z) : 'regexp )))))]);
      ((Some "#"), None,
        [([`Sself; `Skeyword "#"; `Sself],
           ("let s1 = as_cset r1 in let s2 = as_cset r2 in Characters (Fcset.diff s1 s2)\n",
             (Fgram.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (let s1 = as_cset r1 in
                    let s2 = as_cset r2 in Characters (Fcset.diff s1 s2) : 
                   'regexp )))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           ("Alternative (r1, r2)\n",
             (Fgram.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Alternative (r1, r2) : 'regexp )))))]);
      ((Some "app"), None,
        [([`Sself; `Sself],
           ("Sequence (r1, r2)\n",
             (Fgram.mk_action
                (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Sequence (r1, r2) : 'regexp )))))]);
      ((Some "basic"), None,
        [([`Skeyword "_"],
           ("Characters Fcset.all_chars\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (Characters Fcset.all_chars : 'regexp )))));
        ([`Stoken
            (((function | `Chr _ -> true | _ -> false)), ("Chr", `Any),
              "`Chr _")],
          ("Characters (Fcset.singleton (Char.code @@ (TokenEval.char c)))\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Chr c ->
                      (Characters
                         (Fcset.singleton (Char.code @@ (TokenEval.char c))) : 
                      'regexp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
              "`Str _")],
          ("regexp_for_string @@ (TokenEval.string s)\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Str s ->
                      (regexp_for_string @@ (TokenEval.string s) : 'regexp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "[";
         `Snterm (Fgram.obj (char_class : 'char_class Fgram.t ));
         `Skeyword "]"],
          ("Characters cc\n",
            (Fgram.mk_action
               (fun _  (cc : 'char_class)  _  (_loc : Locf.t)  ->
                  (Characters cc : 'regexp )))));
        ([`Sself; `Skeyword "*"],
          ("Repetition r1\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                  (Repetition r1 : 'regexp )))));
        ([`Sself; `Skeyword "?"],
          ("Alternative (Epsilon, r1)\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                  (Alternative (Epsilon, r1) : 'regexp )))));
        ([`Sself; `Skeyword "+"],
          ("Sequence ((Repetition (remove_as r1)), r1)\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                  (Sequence ((Repetition (remove_as r1)), r1) : 'regexp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("r1\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  _  (_loc : Locf.t)  -> (r1 : 'regexp )))));
        ([`Skeyword "eof"],
          ("Eof\n",
            (Fgram.mk_action (fun _  (_loc : Locf.t)  -> (Eof : 'regexp )))));
        ([`Stoken
            (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
              "`Lid x")],
          ("try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = _loc.loc_start in\n    (Fan_warnings.emitf p \"Reference to unbound regexp name `%s'\" x;\n     raise UnboundRegexp)\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Lid (_,x) ->
                      ((try Hashtbl.find named_regexps x
                        with
                        | Not_found  ->
                            let p = _loc.loc_start in
                            (Fan_warnings.emitf p
                               "Reference to unbound regexp name `%s'" x;
                             raise UnboundRegexp)) : 'regexp )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))])]);
  Fgram.extend_single (char_class : 'char_class Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "^";
          `Snterm (Fgram.obj (char_class1 : 'char_class1 Fgram.t ))],
           ("Fcset.complement r\n",
             (Fgram.mk_action
                (fun (r : 'char_class1)  _  (_loc : Locf.t)  ->
                   (Fcset.complement r : 'char_class )))));
        ([`Snterm (Fgram.obj (char_class1 : 'char_class1 Fgram.t ))],
          ("r\n",
            (Fgram.mk_action
               (fun (r : 'char_class1)  (_loc : Locf.t)  ->
                  (r : 'char_class )))))]));
  Fgram.extend_single (char_class1 : 'char_class1 Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Chr _ -> true | _ -> false)), ("Chr", `Any),
               "`Chr _");
          `Skeyword "-";
          `Stoken
            (((function | `Chr _ -> true | _ -> false)), ("Chr", `Any),
              "`Chr _")],
           ("let c1 = Char.code @@ (TokenEval.char c1) in\nlet c2 = Char.code @@ (TokenEval.char c2) in Fcset.interval c1 c2\n",
             (Fgram.mk_action
                (fun (__fan_2 : Ftoken.t)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`Chr c2,`Chr c1) ->
                       (let c1 = Char.code @@ (TokenEval.char c1) in
                        let c2 = Char.code @@ (TokenEval.char c2) in
                        Fcset.interval c1 c2 : 'char_class1 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s"
                            (Ftoken.token_to_string __fan_2)
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Chr _ -> true | _ -> false)), ("Chr", `Any),
              "`Chr _")],
          ("Fcset.singleton (Char.code @@ (TokenEval.char c1))\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Chr c1 ->
                      (Fcset.singleton (Char.code @@ (TokenEval.char c1)) : 
                      'char_class1 )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Sself; `Sself],
          ("Fcset.union cc1 cc2\n",
            (Fgram.mk_action
               (fun (cc2 : 'char_class1)  (cc1 : 'char_class1) 
                  (_loc : Locf.t)  -> (Fcset.union cc1 cc2 : 'char_class1 )))))]))
let () =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream ~name:(d, "lex") ~entry:lex
    ();
  Ast_quotation.of_stru ~lexer:Lex_lex.from_stream ~name:(d, "regex")
    ~entry:declare_regexp ()