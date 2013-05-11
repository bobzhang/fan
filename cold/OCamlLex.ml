open LibUtil

open LexSyntax

open Syntax

let regexp_for_string s =
  let rec re_string n =
    if n >= (String.length s)
    then Epsilon
    else
      if (succ n) = (String.length s)
      then Characters (Cset.singleton (Char.code (s.[n])))
      else
        Sequence
          ((Characters (Cset.singleton (Char.code (s.[n])))),
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

let as_cset = function | Characters s -> s | _ -> raise Cset.Bad

let regexp = Gram.mk "regexp"

let char_class = Gram.mk "char_class"

let char_class1 = Gram.mk "char_class1"

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
                  `Snterm (Gram.obj (exp : 'exp Gram.t ))],
                   ("Gram.mk_action\n  (fun (a : 'exp)  _  (r : 'regexp)  (_loc : FanLoc.t)  -> ((r, a) : 'e__1 ))\n",
                     (Gram.mk_action
                        (fun (a : 'exp)  _  (r : 'regexp)  (_loc : FanLoc.t) 
                           -> ((r, a) : 'e__1 )))))]), (`Skeyword "|"))],
           ("Gram.mk_action\n  (fun (l : 'e__1 list)  _  (_loc : FanLoc.t)  ->\n     (LexBackend.output_entry\n        (Lexgen.make_single_dfa { LexSyntax.shortest = false; clauses = l }) : \n     'lex ))\n",
             (Gram.mk_action
                (fun (l : 'e__1 list)  _  (_loc : FanLoc.t)  ->
                   (LexBackend.output_entry
                      (Lexgen.make_single_dfa
                         { LexSyntax.shortest = false; clauses = l }) : 
                   'lex )))));
        ([`Skeyword "<";
         `Slist0sep
           ((Gram.srules
               [([`Snterm (Gram.obj (regexp : 'regexp Gram.t ));
                 `Skeyword "->";
                 `Snterm (Gram.obj (exp : 'exp Gram.t ))],
                  ("Gram.mk_action\n  (fun (a : 'exp)  _  (r : 'regexp)  (_loc : FanLoc.t)  -> ((r, a) : 'e__2 ))\n",
                    (Gram.mk_action
                       (fun (a : 'exp)  _  (r : 'regexp)  (_loc : FanLoc.t) 
                          -> ((r, a) : 'e__2 )))))]), (`Skeyword "|"))],
          ("Gram.mk_action\n  (fun (l : 'e__2 list)  _  (_loc : FanLoc.t)  ->\n     (LexBackend.output_entry\n        (Lexgen.make_single_dfa { LexSyntax.shortest = true; clauses = l }) : \n     'lex ))\n",
            (Gram.mk_action
               (fun (l : 'e__2 list)  _  (_loc : FanLoc.t)  ->
                  (LexBackend.output_entry
                     (Lexgen.make_single_dfa
                        { LexSyntax.shortest = true; clauses = l }) : 
                  'lex )))))]));
  Gram.extend_single (declare_regexp : 'declare_regexp Gram.t )
    (None,
      (None, None,
        [([`Smeta
             (["FOLD1"],
               [Gram.srules
                  [([`Skeyword "let";
                    `Stoken
                      (((function | `Lid _ -> true | _ -> false)),
                        (`Normal, "`Lid _"));
                    `Skeyword "=";
                    `Snterm (Gram.obj (regexp : 'regexp Gram.t ))],
                     ("Gram.mk_action\n  (fun (r : 'regexp)  _  (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->\n     match __fan_1 with\n     | `Lid x -> ((x, r) : 'e__3 )\n     | _ -> failwith \"(x, r)\n\")\n",
                       (Gram.mk_action
                          (fun (r : 'regexp)  _  (__fan_1 : [> FanToken.t]) 
                             _  (_loc : FanLoc.t)  ->
                             match __fan_1 with
                             | `Lid x -> ((x, r) : 'e__3 )
                             | _ -> failwith "(x, r)\n"))))]],
               (Gram.Action.mk
                  (Gram.sfold1
                     (fun (x,r)  ()  ->
                        if Hashtbl.mem named_regexps x
                        then
                          Printf.eprintf
                            "fanlex (warning): multiple definition of named regexp '%s'\n"
                            x;
                        Hashtbl.add named_regexps x r) () : (_,'e__3,
                                                              'e__4)
                                                              Gram.fold )))],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  ->\n     ((`StExp (_loc, (`Uid (_loc, \"()\"))) : Ast.stru ) : 'declare_regexp ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   ((`StExp (_loc, (`Uid (_loc, "()"))) : Ast.stru ) : 
                   'declare_regexp )))))]));
  Gram.extend (regexp : 'regexp Gram.t )
    (None,
      [((Some "as"), None,
         [([`Sself;
           `Skeyword "as";
           `Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
            ("Gram.mk_action\n  (fun (x : 'a_lident)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (match x with\n      | #Ast.lident as y -> Bind (r1, y)\n      | `Ant (_loc,_) -> assert false : 'regexp ))\n",
              (Gram.mk_action
                 (fun (x : 'a_lident)  _  (r1 : 'regexp)  (_loc : FanLoc.t) 
                    ->
                    (match x with
                     | #Ast.lident as y -> Bind (r1, y)
                     | `Ant (_loc,_) -> assert false : 'regexp )))))]);
      ((Some "#"), None,
        [([`Sself; `Skeyword "#"; `Sself],
           ("Gram.mk_action\n  (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (let s1 = as_cset r1 in\n      let s2 = as_cset r2 in Characters (Cset.diff s1 s2) : 'regexp ))\n",
             (Gram.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                   (let s1 = as_cset r1 in
                    let s2 = as_cset r2 in Characters (Cset.diff s1 s2) : 
                   'regexp )))))]);
      ((Some "|"), None,
        [([`Sself; `Skeyword "|"; `Sself],
           ("Gram.mk_action\n  (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (Alternative (r1, r2) : 'regexp ))\n",
             (Gram.mk_action
                (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                   (Alternative (r1, r2) : 'regexp )))))]);
      ((Some "app"), None,
        [([`Sself; `Sself],
           ("Gram.mk_action\n  (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (Sequence (r1, r2) : 'regexp ))\n",
             (Gram.mk_action
                (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                   (Sequence (r1, r2) : 'regexp )))))]);
      ((Some "basic"), None,
        [([`Skeyword "_"],
           ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  -> (Characters Cset.all_chars : 'regexp ))\n",
             (Gram.mk_action
                (fun _  (_loc : FanLoc.t)  ->
                   (Characters Cset.all_chars : 'regexp )))));
        ([`Skeyword "!"],
          ("Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (Eof : 'regexp ))\n",
            (Gram.mk_action (fun _  (_loc : FanLoc.t)  -> (Eof : 'regexp )))));
        ([`Stoken
            (((function | `CHAR (_,_) -> true | _ -> false)),
              (`Normal, "`CHAR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (c,_) -> (Characters (Cset.singleton (Char.code c)) : 'regexp )\n     | _ -> failwith \"Characters (Cset.singleton (Char.code c))\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `CHAR (c,_) ->
                      (Characters (Cset.singleton (Char.code c)) : 'regexp )
                  | _ ->
                      failwith "Characters (Cset.singleton (Char.code c))\n"))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (s,_) -> (regexp_for_string s : 'regexp )\n     | _ -> failwith \"regexp_for_string s\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `STR (s,_) -> (regexp_for_string s : 'regexp )
                  | _ -> failwith "regexp_for_string s\n"))));
        ([`Skeyword "[";
         `Snterm (Gram.obj (char_class : 'char_class Gram.t ));
         `Skeyword "]"],
          ("Gram.mk_action\n  (fun _  (cc : 'char_class)  _  (_loc : FanLoc.t)  ->\n     (Characters cc : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (cc : 'char_class)  _  (_loc : FanLoc.t)  ->
                  (Characters cc : 'regexp )))));
        ([`Sself; `Skeyword "*"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  -> (Repetition r1 : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (Repetition r1 : 'regexp )))));
        ([`Sself; `Skeyword "?"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (Alternative (Epsilon, r1) : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (Alternative (Epsilon, r1) : 'regexp )))));
        ([`Sself; `Skeyword "+"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->\n     (Sequence ((Repetition (remove_as r1)), r1) : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (Sequence ((Repetition (remove_as r1)), r1) : 'regexp )))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          ("Gram.mk_action\n  (fun _  (r1 : 'regexp)  _  (_loc : FanLoc.t)  -> (r1 : 'regexp ))\n",
            (Gram.mk_action
               (fun _  (r1 : 'regexp)  _  (_loc : FanLoc.t)  ->
                  (r1 : 'regexp )))));
        ([`Stoken
            (((function | `Lid _ -> true | _ -> false)), (`Normal, "`Lid _"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `Lid x ->\n         ((try Hashtbl.find named_regexps x\n           with\n           | Not_found  ->\n               let p = FanLoc.start_pos _loc in\n               (Printf.eprintf\n                  \"File \"%s\", line %d, character %d:\nReference to unbound regexp name `%s'.\n\"\n                  p.Lexing.pos_fname p.Lexing.pos_lnum\n                  (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;\n                exit 2)) : 'regexp )\n     | _ ->\n         failwith\n           \"try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = FanLoc.start_pos _loc in\n    (Printf.eprintf\n       \"File \\\"%s\\\", line %d, character %d:\\nReference to unbound regexp name `%s'.\\n\"\n       p.Lexing.pos_fname p.Lexing.pos_lnum\n       (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;\n     exit 2)\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `Lid x ->
                      ((try Hashtbl.find named_regexps x
                        with
                        | Not_found  ->
                            let p = FanLoc.start_pos _loc in
                            (Printf.eprintf
                               "File \"%s\", line %d, character %d:\nReference to unbound regexp name `%s'.\n"
                               p.Lexing.pos_fname p.Lexing.pos_lnum
                               (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;
                             exit 2)) : 'regexp )
                  | _ ->
                      failwith
                        "try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    let p = FanLoc.start_pos _loc in\n    (Printf.eprintf\n       \"File \\\"%s\\\", line %d, character %d:\\nReference to unbound regexp name `%s'.\\n\"\n       p.Lexing.pos_fname p.Lexing.pos_lnum\n       (p.Lexing.pos_cnum - p.Lexing.pos_bol) x;\n     exit 2)\n"))))])]);
  Gram.extend_single (char_class : 'char_class Gram.t )
    (None,
      (None, None,
        [([`Skeyword "!";
          `Snterm (Gram.obj (char_class1 : 'char_class1 Gram.t ))],
           ("Gram.mk_action\n  (fun (r : 'char_class1)  _  (_loc : FanLoc.t)  ->\n     (Cset.complement r : 'char_class ))\n",
             (Gram.mk_action
                (fun (r : 'char_class1)  _  (_loc : FanLoc.t)  ->
                   (Cset.complement r : 'char_class )))));
        ([`Snterm (Gram.obj (char_class1 : 'char_class1 Gram.t ))],
          ("Gram.mk_action\n  (fun (r : 'char_class1)  (_loc : FanLoc.t)  -> (r : 'char_class ))\n",
            (Gram.mk_action
               (fun (r : 'char_class1)  (_loc : FanLoc.t)  ->
                  (r : 'char_class )))))]));
  Gram.extend_single (char_class1 : 'char_class1 Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `CHAR (_,_) -> true | _ -> false)),
               (`Normal, "`CHAR (_,_)"));
          `Skeyword "-";
          `Stoken
            (((function | `CHAR (_,_) -> true | _ -> false)),
              (`Normal, "`CHAR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_2 : [> FanToken.t])  _  (__fan_0 : [> FanToken.t]) \n     (_loc : FanLoc.t)  ->\n     match (__fan_2, __fan_0) with\n     | (`CHAR (c2,_),`CHAR (c1,_)) ->\n         (let c1 = Char.code c1 in\n          let c2 = Char.code c2 in Cset.interval c1 c2 : 'char_class1 )\n     | _ ->\n         failwith\n           \"let c1 = Char.code c1 in let c2 = Char.code c2 in Cset.interval c1 c2\n\")\n",
             (Gram.mk_action
                (fun (__fan_2 : [> FanToken.t])  _ 
                   (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`CHAR (c2,_),`CHAR (c1,_)) ->
                       (let c1 = Char.code c1 in
                        let c2 = Char.code c2 in Cset.interval c1 c2 : 
                       'char_class1 )
                   | _ ->
                       failwith
                         "let c1 = Char.code c1 in let c2 = Char.code c2 in Cset.interval c1 c2\n"))));
        ([`Stoken
            (((function | `CHAR (_,_) -> true | _ -> false)),
              (`Normal, "`CHAR (_,_)"))],
          ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `CHAR (c1,_) -> (Cset.singleton (Char.code c1) : 'char_class1 )\n     | _ -> failwith \"Cset.singleton (Char.code c1)\n\")\n",
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `CHAR (c1,_) ->
                      (Cset.singleton (Char.code c1) : 'char_class1 )
                  | _ -> failwith "Cset.singleton (Char.code c1)\n"))));
        ([`Sself; `Sself],
          ("Gram.mk_action\n  (fun (cc2 : 'char_class1)  (cc1 : 'char_class1)  (_loc : FanLoc.t)  ->\n     (Cset.union cc1 cc2 : 'char_class1 ))\n",
            (Gram.mk_action
               (fun (cc2 : 'char_class1)  (cc1 : 'char_class1) 
                  (_loc : FanLoc.t)  -> (Cset.union cc1 cc2 : 'char_class1 )))))]))

let d = `Absolute ["Fan"; "Lang"]

let _ =
  AstQuotation.of_exp ~name:(d, "lexer") ~entry:lex;
  AstQuotation.of_stru ~name:(d, "regexp") ~entry:declare_regexp