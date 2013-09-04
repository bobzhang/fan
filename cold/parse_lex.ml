open LibUtil
open LexSyntax
open! Fsyntax
let regexp_for_string s =
  let rec re_string n =
    if n >= (String.length s)
    then Epsilon
    else
      if (succ n) = (String.length s)
      then Characters (Fcset.singleton (Char.code (s.[n])))
      else
        Sequence
          ((Characters (Fcset.singleton (Char.code (s.[n])))),
            (re_string (succ n))) in
  re_string 0
let named_regexps: (string,regular_expression) Hashtbl.t = Hashtbl.create 13
let rec remove_as =
  function
  | Bind (e,_) -> remove_as e
  | Epsilon |Eof |Characters _ as e -> e
  | Sequence (e1,e2) -> Sequence ((remove_as e1), (remove_as e2))
  | Alternative (e1,e2) -> Alternative ((remove_as e1), (remove_as e2))
  | Repetition e -> Repetition (remove_as e)
let as_cset = function | Characters s -> s | _ -> raise Fcset.Bad
let regexp = Fgram.mk "regexp"
let char_class = Fgram.mk "char_class"
let char_class1 = Fgram.mk "char_class1"
let lex = Fgram.mk "lex"
let declare_regexp = Fgram.mk "declare_regexp"
let _ =
  let grammar_entry_create x = Fgram.mk x in
  let case: 'case Fgram.t = grammar_entry_create "case" in
  Fgram.extend_single (lex : 'lex Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "|";
          `Slist0sep
            ((`Snterm (Fgram.obj (case : 'case Fgram.t ))), (`Skeyword "|"))],
           ("Compile_lex.output_entry\n  (Lexgen.make_single_dfa { LexSyntax.shortest = false; clauses = l })\n",
             (Fgram.mk_action
                (fun (l : 'case list)  _  (_loc : FLoc.t)  ->
                   (Compile_lex.output_entry
                      (Lexgen.make_single_dfa
                         { LexSyntax.shortest = false; clauses = l }) : 
                   'lex )))));
        ([`Skeyword "<";
         `Slist0sep
           ((`Snterm (Fgram.obj (case : 'case Fgram.t ))), (`Skeyword "|"))],
          ("Compile_lex.output_entry\n  (Lexgen.make_single_dfa { LexSyntax.shortest = true; clauses = l })\n",
            (Fgram.mk_action
               (fun (l : 'case list)  _  (_loc : FLoc.t)  ->
                  (Compile_lex.output_entry
                     (Lexgen.make_single_dfa
                        { LexSyntax.shortest = true; clauses = l }) : 
                  'lex )))))]));
  Fgram.extend_single (case : 'case Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (regexp : 'regexp Fgram.t ));
          `Skeyword "->";
          `Snterm (Fgram.obj (exp : 'exp Fgram.t ))],
           ("(r, a)\n",
             (Fgram.mk_action
                (fun (a : 'exp)  _  (r : 'regexp)  (_loc : FLoc.t)  ->
                   ((r, a) : 'case )))))]));
  Fgram.extend_single (declare_regexp : 'declare_regexp Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "let";
          `Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _");
          `Skeyword "=";
          `Snterm (Fgram.obj (regexp : 'regexp Fgram.t ))],
           ("if Hashtbl.mem named_regexps x\nthen\n  (Printf.eprintf\n     \"fanlex (warning): multiple definition of named regexp '%s'\n\" x;\n   exit 2)\nelse\n  (Hashtbl.add named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru ))\n",
             (Fgram.mk_action
                (fun (r : 'regexp)  _  (__fan_1 : [> FToken.t])  _ 
                   (_loc : FLoc.t)  ->
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
                           (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru )) : 
                       'declare_regexp )
                   | _ ->
                       failwith
                         "if Hashtbl.mem named_regexps x\nthen\n  (Printf.eprintf\n     \"fanlex (warning): multiple definition of named regexp '%s'\\n\" x;\n   exit 2)\nelse\n  (Hashtbl.add named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru ))\n"))));
        ([`Sself; `Sself],
          ("x\n",
            (Fgram.mk_action
               (fun (x : 'declare_regexp)  _  (_loc : FLoc.t)  ->
                  (x : 'declare_regexp )))))]));
  Fgram.extend (regexp : 'regexp Fgram.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
            ("match x with\n| #FAst.lident as y -> Bind (r1, y)\n| `Ant (_loc,_) -> assert false\n",
              (Fgram.mk_action
                 (fun (x : 'a_lident)  _  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                    (match x with
                     | #FAst.lident as y -> Bind (r1, y)
                     | `Ant (_loc,_) -> assert false : 'regexp )))))]);
      ((Some "#"), None,
        [([`Sself; `Skeyword "#"; `Sself],
           ("let s1 = as_cset r1 in let s2 = as_cset r2 in Characters (Fcset.diff s1 s2)\n",
             (Fgram.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                   (let s1 = as_cset r1 in
                    let s2 = as_cset r2 in Characters (Fcset.diff s1 s2) : 
                   'regexp )))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           ("Alternative (r1, r2)\n",
             (Fgram.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                   (Alternative (r1, r2) : 'regexp )))))]);
      ((Some "app"), None,
        [([`Sself; `Sself],
           ("Sequence (r1, r2)\n",
             (Fgram.mk_action
                (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                   (Sequence (r1, r2) : 'regexp )))))]);
      ((Some "basic"), None,
        [([`Skeyword "_"],
           ("Characters Fcset.all_chars\n",
             (Fgram.mk_action
                (fun _  (_loc : FLoc.t)  ->
                   (Characters Fcset.all_chars : 'regexp )))));
        ([`Skeyword "!"],
          ("Eof\n",
            (Fgram.mk_action (fun _  (_loc : FLoc.t)  -> (Eof : 'regexp )))));
        ([`Stoken
            (((function | `CHAR (_,_) -> true | _ -> false)),
              (`App ((`App ((`Vrn "CHAR"), `Any)), `Any)), "`CHAR (_,_)")],
          ("Characters (Fcset.singleton (Char.code c))\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `CHAR (c,_) ->
                      (Characters (Fcset.singleton (Char.code c)) : 'regexp )
                  | _ ->
                      failwith "Characters (Fcset.singleton (Char.code c))\n"))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`App ((`App ((`Vrn "STR"), `Any)), `Any)), "`STR (_,_)")],
          ("regexp_for_string s\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `STR (s,_) -> (regexp_for_string s : 'regexp )
                  | _ -> failwith "regexp_for_string s\n"))));
        ([`Skeyword "[";
         `Snterm (Fgram.obj (char_class : 'char_class Fgram.t ));
         `Skeyword "]"],
          ("Characters cc\n",
            (Fgram.mk_action
               (fun _  (cc : 'char_class)  _  (_loc : FLoc.t)  ->
                  (Characters cc : 'regexp )))));
        ([`Sself; `Skeyword "*"],
          ("Repetition r1\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                  (Repetition r1 : 'regexp )))));
        ([`Sself; `Skeyword "?"],
          ("Alternative (Epsilon, r1)\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                  (Alternative (Epsilon, r1) : 'regexp )))));
        ([`Sself; `Skeyword "+"],
          ("Sequence ((Repetition (remove_as r1)), r1)\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FLoc.t)  ->
                  (Sequence ((Repetition (remove_as r1)), r1) : 'regexp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("r1\n",
            (Fgram.mk_action
               (fun _  (r1 : 'regexp)  _  (_loc : FLoc.t)  -> (r1 : 'regexp )))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)),
              (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = FLoc.start_pos _loc in\n    (Printf.eprintf\n       \"File \"%s\", line %d, character %d:\nReference to unbound regexp name `%s'.\n\"\n       p.Lexing.pos_fname p.Lexing.pos_lnum\n       (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;\n     exit 2)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find named_regexps x
                        with
                        | Not_found  ->
                            let p = FLoc.start_pos _loc in
                            (Printf.eprintf
                               "File \"%s\", line %d, character %d:\nReference to unbound regexp name `%s'.\n"
                               p.Lexing.pos_fname p.Lexing.pos_lnum
                               (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;
                             exit 2)) : 'regexp )
                  | _ ->
                      failwith
                        "try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = FLoc.start_pos _loc in\n    (Printf.eprintf\n       \"File \\\"%s\\\", line %d, character %d:\\nReference to unbound regexp name `%s'.\\n\"\n       p.Lexing.pos_fname p.Lexing.pos_lnum\n       (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;\n     exit 2)\n"))))])]);
  Fgram.extend_single (char_class : 'char_class Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "!";
          `Snterm (Fgram.obj (char_class1 : 'char_class1 Fgram.t ))],
           ("Fcset.complement r\n",
             (Fgram.mk_action
                (fun (r : 'char_class1)  _  (_loc : FLoc.t)  ->
                   (Fcset.complement r : 'char_class )))));
        ([`Snterm (Fgram.obj (char_class1 : 'char_class1 Fgram.t ))],
          ("r\n",
            (Fgram.mk_action
               (fun (r : 'char_class1)  (_loc : FLoc.t)  ->
                  (r : 'char_class )))))]));
  Fgram.extend_single (char_class1 : 'char_class1 Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`App ((`App ((`Vrn "CHAR"), `Any)), `Any)), "`CHAR (_,_)");
          `Skeyword "-";
          `Stoken
            (((function | `CHAR (_,_) -> true | _ -> false)),
              (`App ((`App ((`Vrn "CHAR"), `Any)), `Any)), "`CHAR (_,_)")],
           ("let c1 = Char.code c1 in let c2 = Char.code c2 in Fcset.interval c1 c2\n",
             (Fgram.mk_action
                (fun (__fan_2 : [> FToken.t])  _  (__fan_0 : [> FToken.t]) 
                   (_loc : FLoc.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`CHAR (c2,_),`CHAR (c1,_)) ->
                       (let c1 = Char.code c1 in
                        let c2 = Char.code c2 in Fcset.interval c1 c2 : 
                       'char_class1 )
                   | _ ->
                       failwith
                         "let c1 = Char.code c1 in let c2 = Char.code c2 in Fcset.interval c1 c2\n"))));
        ([`Stoken
            (((function | `CHAR (_,_) -> true | _ -> false)),
              (`App ((`App ((`Vrn "CHAR"), `Any)), `Any)), "`CHAR (_,_)")],
          ("Fcset.singleton (Char.code c1)\n",
            (Fgram.mk_action
               (fun (__fan_0 : [> FToken.t])  (_loc : FLoc.t)  ->
                  match __fan_0 with
                  | `CHAR (c1,_) ->
                      (Fcset.singleton (Char.code c1) : 'char_class1 )
                  | _ -> failwith "Fcset.singleton (Char.code c1)\n"))));
        ([`Sself; `Sself],
          ("Fcset.union cc1 cc2\n",
            (Fgram.mk_action
               (fun (cc2 : 'char_class1)  (cc1 : 'char_class1) 
                  (_loc : FLoc.t)  -> (Fcset.union cc1 cc2 : 'char_class1 )))))]))
let d = `Absolute ["Fan"; "Lang"]
let _ =
  AstQuotation.of_exp ~name:(d, "lexer") ~entry:lex;
  AstQuotation.of_stru ~name:(d, "regexp") ~entry:declare_regexp