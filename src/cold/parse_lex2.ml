open Translate_lex
let named_regexps: (string,concrete_regexp) Hashtbl.t = Hashtbl.create 13
let _ = Hashtbl.add named_regexps "eof" Eof
exception UnboundRegexp
let g =
  Gramf.create_lexer ~annot:"Lexer's lexer"
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
let regexp = Gramf.mk_dynamic g "regexp"
let char_class = Gramf.mk_dynamic g "char_class"
let char_class1 = Gramf.mk_dynamic g "char_class1"
let lex = Gramf.mk_dynamic g "lex"
let declare_regexp = Gramf.mk_dynamic g "declare_regexp"
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let case: 'case Gramf.t = grammar_entry_create "case"
  and lid: 'lid Gramf.t = grammar_entry_create "lid" in
  Gramf.extend_single (lex : 'lex Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Slist0sep
            ((`Snterm (Gramf.obj (case : 'case Gramf.t ))), (`Skeyword "|"))],
           ("Compile_lex.output_entry @@\n  (Lexgen.make_single_dfa { shortest = false; clauses = l })\n",
             (Gramf.mk_action
                (fun (l : 'case list)  _  (_loc : Locf.t)  ->
                   (Compile_lex.output_entry @@
                      (Lexgen.make_single_dfa
                         { shortest = false; clauses = l }) : 'lex )))));
        ([`Skeyword "<";
         `Slist0sep
           ((`Snterm (Gramf.obj (case : 'case Gramf.t ))), (`Skeyword "|"))],
          ("Compile_lex.output_entry @@\n  (Lexgen.make_single_dfa { shortest = true; clauses = l })\n",
            (Gramf.mk_action
               (fun (l : 'case list)  _  (_loc : Locf.t)  ->
                  (Compile_lex.output_entry @@
                     (Lexgen.make_single_dfa { shortest = true; clauses = l }) : 
                  'lex )))))]));
  Gramf.extend_single (case : 'case Gramf.t )
    (None,
      (None, None,
        [([`Snterm (Gramf.obj (regexp : 'regexp Gramf.t ));
          `Stoken
            (((function | `Quot _ -> true | _ -> false)),
              (`App ((`Vrn "Quot"), `Any)), "`Quot _")],
           ("let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\nlet e = Tokenf.quot_expand expander x in (r, e)\n",
             (Gramf.mk_action
                (fun (__fan_1 : [> Tokenf.t])  (r : 'regexp)  (_loc : Locf.t)
                    ->
                   match __fan_1 with
                   | `Quot x ->
                       (let expander loc _ s =
                          Gramf.parse_string ~loc Syntaxf.exp s in
                        let e = Tokenf.quot_expand expander x in (r, e) : 
                       'case )
                   | _ ->
                       failwith
                         "let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\nlet e = Tokenf.quot_expand expander x in (r, e)\n"))))]));
  Gramf.extend_single (declare_regexp : 'declare_regexp Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "let";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _");
          `Skeyword "=";
          `Snterm (Gramf.obj (regexp : 'regexp Gramf.t ))],
           ("if Hashtbl.mem named_regexps x\nthen\n  (Printf.eprintf\n     \"fanlex (warning): multiple definition of named regexp '%s'\n\" x;\n   exit 2)\nelse\n  (Hashtbl.add named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : Astf.stru ))\n",
             (Gramf.mk_action
                (fun (r : 'regexp)  _  (__fan_1 : [> Tokenf.t])  _ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Lid x ->
                       (if Hashtbl.mem named_regexps x
                        then
                          (Printf.eprintf
                             "fanlex (warning): multiple definition of named regexp '%s'\n"
                             x;
                           exit 2)
                        else
                          (Hashtbl.add named_regexps x r;
                           (`StExp (_loc, (`Uid (_loc, "()"))) : Astf.stru )) : 
                       'declare_regexp )
                   | _ ->
                       failwith
                         "if Hashtbl.mem named_regexps x\nthen\n  (Printf.eprintf\n     \"fanlex (warning): multiple definition of named regexp '%s'\\n\" x;\n   exit 2)\nelse\n  (Hashtbl.add named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : Astf.stru ))\n"))));
        ([`Sself; `Sself],
          ("x\n",
            (Gramf.mk_action
               (fun (x : 'declare_regexp)  _  (_loc : Locf.t)  ->
                  (x : 'declare_regexp )))))]));
  Gramf.extend_single (lid : 'lid Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("(_loc, y)\n",
             (Gramf.mk_action
                (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid y -> ((_loc, y) : 'lid )
                   | _ -> failwith "(_loc, y)\n"))))]));
  Gramf.extend (regexp : 'regexp Gramf.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Snterm (Gramf.obj (lid : 'lid Gramf.t ))],
            ("Bind (r1, z)\n",
              (Gramf.mk_action
                 (fun (z : 'lid)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Bind (r1, z) : 'regexp )))))]);
      ((Some "#"), None,
        [([`Sself; `Skeyword "#"; `Sself],
           ("let s1 = as_cset r1 in let s2 = as_cset r2 in Characters (Fcset.diff s1 s2)\n",
             (Gramf.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (let s1 = as_cset r1 in
                    let s2 = as_cset r2 in Characters (Fcset.diff s1 s2) : 
                   'regexp )))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           ("Alternative (r1, r2)\n",
             (Gramf.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Alternative (r1, r2) : 'regexp )))))]);
      ((Some "app"), None,
        [([`Sself; `Sself],
           ("Sequence (r1, r2)\n",
             (Gramf.mk_action
                (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Sequence (r1, r2) : 'regexp )))))]);
      ((Some "basic"), None,
        [([`Skeyword "_"],
           ("Characters Fcset.all_chars\n",
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (Characters Fcset.all_chars : 'regexp )))));
        ([`Stoken
            (((function | `Chr _ -> true | _ -> false)),
              (`App ((`Vrn "Chr"), `Any)), "`Chr _")],
          ("Characters (Fcset.singleton (Char.code @@ (Escape.char c)))\n",
            (Gramf.mk_action
               (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Chr c ->
                      (Characters
                         (Fcset.singleton (Char.code @@ (Escape.char c))) : 
                      'regexp )
                  | _ ->
                      failwith
                        "Characters (Fcset.singleton (Char.code @@ (Escape.char c)))\n"))));
        ([`Stoken
            (((function | `Str _ -> true | _ -> false)),
              (`App ((`Vrn "Str"), `Any)), "`Str _")],
          ("regexp_for_string @@ (Escape.string s)\n",
            (Gramf.mk_action
               (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Str s ->
                      (regexp_for_string @@ (Escape.string s) : 'regexp )
                  | _ ->
                      failwith "regexp_for_string @@ (Escape.string s)\n"))));
        ([`Skeyword "[";
         `Snterm (Gramf.obj (char_class : 'char_class Gramf.t ));
         `Skeyword "]"],
          ("Characters cc\n",
            (Gramf.mk_action
               (fun _  (cc : 'char_class)  _  (_loc : Locf.t)  ->
                  (Characters cc : 'regexp )))));
        ([`Sself; `Skeyword "*"],
          ("Repetition r1\n",
            (Gramf.mk_action
               (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                  (Repetition r1 : 'regexp )))));
        ([`Sself; `Skeyword "?"],
          ("Alternative (Epsilon, r1)\n",
            (Gramf.mk_action
               (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                  (Alternative (Epsilon, r1) : 'regexp )))));
        ([`Sself; `Skeyword "+"],
          ("Sequence ((Repetition (remove_as r1)), r1)\n",
            (Gramf.mk_action
               (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                  (Sequence ((Repetition (remove_as r1)), r1) : 'regexp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("r1\n",
            (Gramf.mk_action
               (fun _  (r1 : 'regexp)  _  (_loc : Locf.t)  -> (r1 : 'regexp )))));
        ([`Skeyword "eof"],
          ("Eof\n",
            (Gramf.mk_action (fun _  (_loc : Locf.t)  -> (Eof : 'regexp )))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = Locf.start_pos _loc in\n    (Fan_warnings.emitf p \"Reference to unbound regexp name `%s'\" x;\n     raise UnboundRegexp)\n",
            (Gramf.mk_action
               (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find named_regexps x
                        with
                        | Not_found  ->
                            let p = Locf.start_pos _loc in
                            (Fan_warnings.emitf p
                               "Reference to unbound regexp name `%s'" x;
                             raise UnboundRegexp)) : 'regexp )
                  | _ ->
                      failwith
                        "try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = Locf.start_pos _loc in\n    (Fan_warnings.emitf p \"Reference to unbound regexp name `%s'\" x;\n     raise UnboundRegexp)\n"))))])]);
  Gramf.extend_single (char_class : 'char_class Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "^";
          `Snterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))],
           ("Fcset.complement r\n",
             (Gramf.mk_action
                (fun (r : 'char_class1)  _  (_loc : Locf.t)  ->
                   (Fcset.complement r : 'char_class )))));
        ([`Snterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))],
          ("r\n",
            (Gramf.mk_action
               (fun (r : 'char_class1)  (_loc : Locf.t)  ->
                  (r : 'char_class )))))]));
  Gramf.extend_single (char_class1 : 'char_class1 Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Chr _ -> true | _ -> false)),
               (`App ((`Vrn "Chr"), `Any)), "`Chr _");
          `Skeyword "-";
          `Stoken
            (((function | `Chr _ -> true | _ -> false)),
              (`App ((`Vrn "Chr"), `Any)), "`Chr _")],
           ("let c1 = Char.code @@ (Escape.char c1) in\nlet c2 = Char.code @@ (Escape.char c2) in Fcset.interval c1 c2\n",
             (Gramf.mk_action
                (fun (__fan_2 : [> Tokenf.t])  _  (__fan_0 : [> Tokenf.t]) 
                   (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`Chr c2,`Chr c1) ->
                       (let c1 = Char.code @@ (Escape.char c1) in
                        let c2 = Char.code @@ (Escape.char c2) in
                        Fcset.interval c1 c2 : 'char_class1 )
                   | _ ->
                       failwith
                         "let c1 = Char.code @@ (Escape.char c1) in\nlet c2 = Char.code @@ (Escape.char c2) in Fcset.interval c1 c2\n"))));
        ([`Stoken
            (((function | `Chr _ -> true | _ -> false)),
              (`App ((`Vrn "Chr"), `Any)), "`Chr _")],
          ("Fcset.singleton (Char.code @@ (Escape.char c1))\n",
            (Gramf.mk_action
               (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Chr c1 ->
                      (Fcset.singleton (Char.code @@ (Escape.char c1)) : 
                      'char_class1 )
                  | _ ->
                      failwith
                        "Fcset.singleton (Char.code @@ (Escape.char c1))\n"))));
        ([`Sself; `Sself],
          ("Fcset.union cc1 cc2\n",
            (Gramf.mk_action
               (fun (cc2 : 'char_class1)  (cc1 : 'char_class1) 
                  (_loc : Locf.t)  -> (Fcset.union cc1 cc2 : 'char_class1 )))))]))
let d = `Absolute ["Fan"; "Lang"]
let () =
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream ~name:(d, "lex2")
    ~entry:lex ();
  Ast_quotation.of_stru ~lexer:Lex_lex.from_stream ~name:(d, "regex2")
    ~entry:declare_regexp ()
