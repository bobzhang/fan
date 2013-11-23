let named_regexps: (string,Translate_lex.concrete_regexp) Hashtbl.t =
  Hashtbl.create 13
type desc = 
  {
  quot_opt: Tokenf.quot option;
  tokens_opt: Tokenf.txt list option;
  loc: Locf.t} 
let named_cases:
  (string,desc -> (Translate_lex.concrete_regexp* Astf.exp) list) Hashtbl.t =
  Hashtbl.create 13
let _ =
  let (+>) = Hashtbl.add named_regexps in
  "newline" +>
    (Alternative
       ((Alternative ((Characters [(10, 10)]), (Characters [(13, 13)]))),
         (Sequence ((Characters [(13, 13)]), (Characters [(10, 10)])))));
  "ocaml_blank" +> (Characters [(9, 9); (12, 12); (32, 32)]);
  "lowercase" +> (Characters [(95, 95); (97, 122); (223, 246); (248, 255)]);
  "uppercase" +> (Characters [(65, 90); (192, 214); (216, 222)]);
  "identchar" +>
    (Characters
       [(39, 39);
       (48, 57);
       (65, 90);
       (95, 95);
       (97, 122);
       (192, 214);
       (216, 246);
       (248, 255)]);
  "eof" +> Eof;
  "_" +> (Characters Fcset.all_chars);
  "hexa_char" +> (Characters [(48, 57); (65, 70); (97, 102)]);
  "ident" +>
    (Sequence
       ((Alternative
           ((Characters [(95, 95); (97, 122); (223, 246); (248, 255)]),
             (Characters [(65, 90); (192, 214); (216, 222)]))),
         (Repetition
            (Characters
               [(39, 39);
               (48, 57);
               (65, 90);
               (95, 95);
               (97, 122);
               (192, 214);
               (216, 246);
               (248, 255)]))));
  "ocaml_escaped_char" +>
    (Sequence
       ((Characters [(92, 92)]),
         (Alternative
            ((Alternative
                ((Characters
                    [(32, 32);
                    (34, 34);
                    (39, 39);
                    (92, 92);
                    (98, 98);
                    (110, 110);
                    (114, 114);
                    (116, 116)]),
                  (Sequence
                     ((Sequence
                         ((Characters [(48, 57)]), (Characters [(48, 57)]))),
                       (Characters [(48, 57)]))))),
              (Sequence
                 ((Sequence
                     ((Characters [(120, 120)]),
                       (Characters [(48, 57); (65, 70); (97, 102)]))),
                   (Characters [(48, 57); (65, 70); (97, 102)])))))));
  "ocaml_char" +>
    (Alternative
       ((Characters [(0, 9); (11, 12); (14, 91); (93, 255)]),
         (Sequence
            ((Characters [(92, 92)]),
              (Alternative
                 ((Alternative
                     ((Characters
                         [(32, 32);
                         (34, 34);
                         (39, 39);
                         (92, 92);
                         (98, 98);
                         (110, 110);
                         (114, 114);
                         (116, 116)]),
                       (Sequence
                          ((Sequence
                              ((Characters [(48, 57)]),
                                (Characters [(48, 57)]))),
                            (Characters [(48, 57)]))))),
                   (Sequence
                      ((Sequence
                          ((Characters [(120, 120)]),
                            (Characters [(48, 57); (65, 70); (97, 102)]))),
                        (Characters [(48, 57); (65, 70); (97, 102)])))))))));
  "ocaml_lid" +>
    (Sequence
       ((Characters [(95, 95); (97, 122); (223, 246); (248, 255)]),
         (Repetition
            (Characters
               [(39, 39);
               (48, 57);
               (65, 90);
               (95, 95);
               (97, 122);
               (192, 214);
               (216, 246);
               (248, 255)]))));
  "ocaml_uid" +>
    (Sequence
       ((Characters [(65, 90); (192, 214); (216, 222)]),
         (Repetition
            (Characters
               [(39, 39);
               (48, 57);
               (65, 90);
               (95, 95);
               (97, 122);
               (192, 214);
               (216, 246);
               (248, 255)]))));
  "decimal_literal" +>
    (Sequence
       ((Characters [(48, 57)]),
         (Repetition (Characters [(48, 57); (95, 95)]))));
  "hex_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 48)]), (Characters [(88, 88); (120, 120)]))),
             (Characters [(48, 57); (65, 70); (97, 102)]))),
         (Repetition (Characters [(48, 57); (65, 70); (95, 95); (97, 102)]))));
  "oct_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 48)]), (Characters [(79, 79); (111, 111)]))),
             (Characters [(48, 55)]))),
         (Repetition (Characters [(48, 55); (95, 95)]))));
  "bin_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 48)]), (Characters [(66, 66); (98, 98)]))),
             (Characters [(48, 49)]))),
         (Repetition (Characters [(48, 49); (95, 95)]))));
  "int_literal" +>
    (Alternative
       ((Alternative
           ((Alternative
               ((Sequence
                   ((Characters [(48, 57)]),
                     (Repetition (Characters [(48, 57); (95, 95)])))),
                 (Sequence
                    ((Sequence
                        ((Sequence
                            ((Characters [(48, 48)]),
                              (Characters [(88, 88); (120, 120)]))),
                          (Characters [(48, 57); (65, 70); (97, 102)]))),
                      (Repetition
                         (Characters
                            [(48, 57); (65, 70); (95, 95); (97, 102)])))))),
             (Sequence
                ((Sequence
                    ((Sequence
                        ((Characters [(48, 48)]),
                          (Characters [(79, 79); (111, 111)]))),
                      (Characters [(48, 55)]))),
                  (Repetition (Characters [(48, 55); (95, 95)])))))),
         (Sequence
            ((Sequence
                ((Sequence
                    ((Characters [(48, 48)]),
                      (Characters [(66, 66); (98, 98)]))),
                  (Characters [(48, 49)]))),
              (Repetition (Characters [(48, 49); (95, 95)]))))));
  "float_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 57)]),
                 (Repetition (Characters [(48, 57); (95, 95)])))),
             (Alternative
                (Epsilon,
                  (Sequence
                     ((Characters [(46, 46)]),
                       (Repetition (Characters [(48, 57); (95, 95)])))))))),
         (Alternative
            (Epsilon,
              (Sequence
                 ((Sequence
                     ((Sequence
                         ((Characters [(69, 69); (101, 101)]),
                           (Alternative
                              (Epsilon, (Characters [(43, 43); (45, 45)]))))),
                       (Characters [(48, 57)]))),
                   (Repetition (Characters [(48, 57); (95, 95)]))))))));
  "quotation_name" +>
    (Sequence
       ((Sequence
           ((Alternative (Epsilon, (Characters [(46, 46)]))),
             (Repetition
                (Sequence
                   ((Sequence
                       ((Characters [(65, 90); (192, 214); (216, 222)]),
                         (Repetition
                            (Characters
                               [(39, 39);
                               (48, 57);
                               (65, 90);
                               (95, 95);
                               (97, 122);
                               (192, 214);
                               (216, 246);
                               (248, 255)])))), (Characters [(46, 46)])))))),
         (Sequence
            ((Characters [(95, 95); (97, 122); (223, 246); (248, 255)]),
              (Repetition
                 (Alternative
                    ((Characters
                        [(39, 39);
                        (48, 57);
                        (65, 90);
                        (95, 95);
                        (97, 122);
                        (192, 214);
                        (216, 246);
                        (248, 255)]), (Characters [(45, 45)]))))))));
  "identchars" +>
    (Sequence
       ((Repetition
           (Characters
              [(39, 39);
              (48, 57);
              (65, 90);
              (95, 95);
              (97, 122);
              (192, 214);
              (216, 246);
              (248, 255)])),
         (Characters
            [(39, 39);
            (48, 57);
            (65, 90);
            (95, 95);
            (97, 122);
            (192, 214);
            (216, 246);
            (248, 255)])))
let append_quot (y : Tokenf.quot option) (e : Astf.exp) =
  match y with
  | None  -> e
  | Some y ->
      let a = Parsef.expand_exp y in
      let _loc = y.loc in (`Seq (_loc, (`Sem (_loc, e, a))) : Astf.exp )
let _ =
  Hashtblf.add_list named_cases
    [("ocaml_uid",
       ((fun { tokens_opt = ls; loc = _loc;_}  ->
           [((Bind
                ((Sequence
                    ((Characters [(65, 90); (192, 214); (216, 222)]),
                      (Repetition
                         (Characters
                            [(39, 39);
                            (48, 57);
                            (65, 90);
                            (95, 95);
                            (97, 122);
                            (192, 214);
                            (216, 246);
                            (248, 255)])))),
                  (({
                      loc_start =
                        {
                          pos_fname = "predef_lex.ml";
                          pos_lnum = 63;
                          pos_bol = 2196;
                          pos_cnum = 2221
                        };
                      loc_end =
                        {
                          pos_fname = "predef_lex.ml";
                          pos_lnum = 63;
                          pos_bol = 2196;
                          pos_cnum = 2224
                        };
                      loc_ghost = false
                    } : Locf.t ), "txt"))),
              ((let default: Astf.exp =
                  `App
                    (_loc, (`Vrn (_loc, "Uid")),
                      (`Record
                         (_loc,
                           (`Sem
                              (_loc,
                                (`RecBind
                                   (_loc, (`Lid (_loc, "loc")),
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "loc_start")),
                                                    (`Field
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "lexbuf")),
                                                         (`Lid
                                                            (_loc,
                                                              "lex_start_p")))))),
                                               (`Sem
                                                  (_loc,
                                                    (`RecBind
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "loc_end")),
                                                         (`Field
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "lexbuf")),
                                                              (`Lid
                                                                 (_loc,
                                                                   "lex_curr_p")))))),
                                                    (`RecBind
                                                       (_loc,
                                                         (`Lid
                                                            (_loc,
                                                              "loc_ghost")),
                                                         (`Lid
                                                            (_loc, "false")))))))))))),
                                (`RecBind
                                   (_loc, (`Lid (_loc, "txt")),
                                     (`Lid (_loc, "txt"))))))))) in
                match ls with
                | None  -> default
                | Some x ->
                    let cases =
                      Ast_gen.bar_of_list @@
                        (List.map
                           (fun (x : Tokenf.txt)  ->
                              let v = x.txt in
                              let i = Hashtbl.hash v in
                              (`Case
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`App
                                      (_loc,
                                        (`App
                                           (_loc, (`Lid (_loc, "=")),
                                             (`Lid (_loc, "txt")))),
                                        (`Str (_loc, v))))) : Astf.case )) x) in
                    (`LetIn
                       (_loc, (`Negative _loc),
                         (`Bind
                            (_loc, (`Lid (_loc, "v")),
                              (`App
                                 (_loc,
                                   (`Dot
                                      (_loc, (`Uid (_loc, "Hashtbl")),
                                        (`Lid (_loc, "hash")))),
                                   (`Lid (_loc, "txt")))))),
                         (`IfThenElse
                            (_loc,
                              (`App
                                 (_loc,
                                   (`Fun
                                      (_loc,
                                        (`Bar
                                           (_loc, cases,
                                             (`Case
                                                (_loc, (`Any _loc),
                                                  (`Lid (_loc, "false")))))))),
                                   (`Lid (_loc, "v")))),
                              (`App
                                 (_loc, (`Vrn (_loc, "Key")),
                                   (`Record
                                      (_loc,
                                        (`Sem
                                           (_loc,
                                             (`RecBind
                                                (_loc, (`Lid (_loc, "loc")),
                                                  (`Record
                                                     (_loc,
                                                       (`Sem
                                                          (_loc,
                                                            (`RecBind
                                                               (_loc,
                                                                 (`Lid
                                                                    (_loc,
                                                                    "loc_start")),
                                                                 (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_start_p")))))),
                                                            (`Sem
                                                               (_loc,
                                                                 (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_end")),
                                                                    (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))))),
                                                                 (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_ghost")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "false")))))))))))),
                                             (`RecBind
                                                (_loc, (`Lid (_loc, "txt")),
                                                  (`Lid (_loc, "txt")))))))))),
                              default))) : Astf.exp ))))])));
    ("ocaml_lid",
      ((fun { tokens_opt = ls; loc = _loc;_}  ->
          [((Bind
               ((Sequence
                   ((Characters [(95, 95); (97, 122); (223, 246); (248, 255)]),
                     (Repetition
                        (Characters
                           [(39, 39);
                           (48, 57);
                           (65, 90);
                           (95, 95);
                           (97, 122);
                           (192, 214);
                           (216, 246);
                           (248, 255)])))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 93;
                         pos_bol = 3187;
                         pos_cnum = 3211
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 93;
                         pos_bol = 3187;
                         pos_cnum = 3214
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt"))),
             ((let default: Astf.exp =
                 `App
                   (_loc, (`Vrn (_loc, "Lid")),
                     (`Record
                        (_loc,
                          (`Sem
                             (_loc,
                               (`RecBind
                                  (_loc, (`Lid (_loc, "loc")),
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "loc_start")),
                                                   (`Field
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "lexbuf")),
                                                        (`Lid
                                                           (_loc,
                                                             "lex_start_p")))))),
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "loc_end")),
                                                        (`Field
                                                           (_loc,
                                                             (`Lid
                                                                (_loc,
                                                                  "lexbuf")),
                                                             (`Lid
                                                                (_loc,
                                                                  "lex_curr_p")))))),
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid
                                                           (_loc,
                                                             "loc_ghost")),
                                                        (`Lid (_loc, "false")))))))))))),
                               (`RecBind
                                  (_loc, (`Lid (_loc, "txt")),
                                    (`Lid (_loc, "txt"))))))))) in
               match ls with
               | None  -> default
               | Some x ->
                   let cases =
                     Ast_gen.bar_of_list @@
                       (List.map
                          (fun (x : Tokenf.txt)  ->
                             let v = x.txt in
                             let i = Hashtbl.hash v in
                             (`Case
                                (_loc, (`Int (_loc, (string_of_int i))),
                                  (`App
                                     (_loc,
                                       (`App
                                          (_loc, (`Lid (_loc, "=")),
                                            (`Lid (_loc, "txt")))),
                                       (`Str (_loc, v))))) : Astf.case )) x) in
                   (`LetIn
                      (_loc, (`Negative _loc),
                        (`Bind
                           (_loc, (`Lid (_loc, "v")),
                             (`App
                                (_loc,
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Hashtbl")),
                                       (`Lid (_loc, "hash")))),
                                  (`Lid (_loc, "txt")))))),
                        (`IfThenElse
                           (_loc,
                             (`App
                                (_loc,
                                  (`Fun
                                     (_loc,
                                       (`Bar
                                          (_loc, cases,
                                            (`Case
                                               (_loc, (`Any _loc),
                                                 (`Lid (_loc, "false")))))))),
                                  (`Lid (_loc, "v")))),
                             (`App
                                (_loc, (`Vrn (_loc, "Key")),
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "loc")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "loc_start")),
                                                                (`Field
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_start_p")))))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_end")),
                                                                    (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_ghost")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "false")))))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "txt")),
                                                 (`Lid (_loc, "txt")))))))))),
                             default))) : Astf.exp ))))])));
    ("kwd_symbol",
      ((fun { tokens_opt = ls; loc = _loc;_}  ->
          match ls with
          | Some ls ->
              let regexp =
                Listf.reduce_left_with
                  ~compose:(fun r1  r2  ->
                              (Alternative (r1, r2) : Translate_lex.concrete_regexp ))
                  ~project:(fun (x : Tokenf.txt)  ->
                              Translate_lex.regexp_for_string @@
                                (Escape.string x.txt)) ls in
              [(regexp,
                 (`LetIn
                    (_loc, (`Negative _loc),
                      (`Bind
                         (_loc, (`Lid (_loc, "txt")),
                           (`App
                              (_loc,
                                (`App
                                   (_loc,
                                     (`App
                                        (_loc,
                                          (`Dot
                                             (_loc, (`Uid (_loc, "Lexing")),
                                               (`Lid (_loc, "sub_lexeme")))),
                                          (`Lid (_loc, "lexbuf")))),
                                     (`Field
                                        (_loc, (`Lid (_loc, "lexbuf")),
                                          (`Lid (_loc, "lex_start_pos")))))),
                                (`Field
                                   (_loc, (`Lid (_loc, "lexbuf")),
                                     (`Lid (_loc, "lex_curr_pos")))))))),
                      (`Constraint
                         (_loc,
                           (`App
                              (_loc, (`Vrn (_loc, "Key")),
                                (`Record
                                   (_loc,
                                     (`Sem
                                        (_loc,
                                          (`RecBind
                                             (_loc, (`Lid (_loc, "loc")),
                                               (`Record
                                                  (_loc,
                                                    (`Sem
                                                       (_loc,
                                                         (`RecBind
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "loc_start")),
                                                              (`Field
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "lex_start_p")))))),
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "loc_end")),
                                                                   (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))))),
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "loc_ghost")),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))))),
                                          (`RecBind
                                             (_loc, (`Lid (_loc, "txt")),
                                               (`Lid (_loc, "txt")))))))))),
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "t"))))))) : Astf.exp ))]
          | None  -> Locf.failf _loc "no following strings after kwd_symbol")));
    ("ocaml_int",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Alternative
                   ((Alternative
                       ((Alternative
                           ((Sequence
                               ((Characters [(48, 57)]),
                                 (Repetition
                                    (Characters [(48, 57); (95, 95)])))),
                             (Sequence
                                ((Sequence
                                    ((Sequence
                                        ((Characters [(48, 48)]),
                                          (Characters [(88, 88); (120, 120)]))),
                                      (Characters
                                         [(48, 57); (65, 70); (97, 102)]))),
                                  (Repetition
                                     (Characters
                                        [(48, 57);
                                        (65, 70);
                                        (95, 95);
                                        (97, 102)])))))),
                         (Sequence
                            ((Sequence
                                ((Sequence
                                    ((Characters [(48, 48)]),
                                      (Characters [(79, 79); (111, 111)]))),
                                  (Characters [(48, 55)]))),
                              (Repetition (Characters [(48, 55); (95, 95)])))))),
                     (Sequence
                        ((Sequence
                            ((Sequence
                                ((Characters [(48, 48)]),
                                  (Characters [(66, 66); (98, 98)]))),
                              (Characters [(48, 49)]))),
                          (Repetition (Characters [(48, 49); (95, 95)])))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 146;
                         pos_bol = 4979;
                         pos_cnum = 5005
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 146;
                         pos_bol = 4979;
                         pos_cnum = 5008
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt"))),
             (`App
                (_loc, (`Vrn (_loc, "Int")),
                  (`Record
                     (_loc,
                       (`Sem
                          (_loc,
                            (`RecBind
                               (_loc, (`Lid (_loc, "loc")),
                                 (`App
                                    (_loc,
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Lexing_util")),
                                           (`Lid (_loc, "from_lexbuf")))),
                                      (`Lid (_loc, "lexbuf")))))),
                            (`RecBind
                               (_loc, (`Lid (_loc, "txt")),
                                 (`Lid (_loc, "txt"))))))))) : Astf.exp ))])));
    ("ocaml_num_literal",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Sequence
                   ((Alternative
                       ((Alternative
                           ((Alternative
                               ((Sequence
                                   ((Characters [(48, 57)]),
                                     (Repetition
                                        (Characters [(48, 57); (95, 95)])))),
                                 (Sequence
                                    ((Sequence
                                        ((Sequence
                                            ((Characters [(48, 48)]),
                                              (Characters
                                                 [(88, 88); (120, 120)]))),
                                          (Characters
                                             [(48, 57); (65, 70); (97, 102)]))),
                                      (Repetition
                                         (Characters
                                            [(48, 57);
                                            (65, 70);
                                            (95, 95);
                                            (97, 102)])))))),
                             (Sequence
                                ((Sequence
                                    ((Sequence
                                        ((Characters [(48, 48)]),
                                          (Characters [(79, 79); (111, 111)]))),
                                      (Characters [(48, 55)]))),
                                  (Repetition
                                     (Characters [(48, 55); (95, 95)])))))),
                         (Sequence
                            ((Sequence
                                ((Sequence
                                    ((Characters [(48, 48)]),
                                      (Characters [(66, 66); (98, 98)]))),
                                  (Characters [(48, 49)]))),
                              (Repetition (Characters [(48, 49); (95, 95)])))))),
                     (Alternative
                        (Epsilon,
                          (Bind
                             ((Alternative
                                 ((Alternative
                                     ((Characters [(108, 108)]),
                                       (Characters [(76, 76)]))),
                                   (Characters [(110, 110)]))),
                               (({
                                   loc_start =
                                     {
                                       pos_fname = "predef_lex.ml";
                                       pos_lnum = 151;
                                       pos_bol = 5130;
                                       pos_cnum = 5172
                                     };
                                   loc_end =
                                     {
                                       pos_fname = "predef_lex.ml";
                                       pos_lnum = 151;
                                       pos_bol = 5130;
                                       pos_cnum = 5173
                                     };
                                   loc_ghost = false
                                 } : Locf.t ), "s"))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 151;
                         pos_bol = 5130;
                         pos_cnum = 5182
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 151;
                         pos_bol = 5130;
                         pos_cnum = 5185
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt"))),
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc,
                       (`Constraint
                          (_loc, (`Lid (_loc, "loc")),
                            (`Dot
                               (_loc, (`Uid (_loc, "Locf")),
                                 (`Lid (_loc, "t")))))),
                       (`Record
                          (_loc,
                            (`Sem
                               (_loc,
                                 (`RecBind
                                    (_loc, (`Lid (_loc, "loc_start")),
                                      (`Field
                                         (_loc, (`Lid (_loc, "lexbuf")),
                                           (`Lid (_loc, "lex_start_p")))))),
                                 (`Sem
                                    (_loc,
                                      (`RecBind
                                         (_loc, (`Lid (_loc, "loc_end")),
                                           (`Field
                                              (_loc, (`Lid (_loc, "lexbuf")),
                                                (`Lid (_loc, "lex_curr_p")))))),
                                      (`RecBind
                                         (_loc, (`Lid (_loc, "loc_ghost")),
                                           (`Lid (_loc, "false")))))))))))),
                  (`Match
                     (_loc, (`Lid (_loc, "s")),
                       (`Bar
                          (_loc,
                            (`Case
                               (_loc,
                                 (`App
                                    (_loc, (`Uid (_loc, "Some")),
                                      (`Chr (_loc, "l")))),
                                 (`App
                                    (_loc, (`Vrn (_loc, "Int32")),
                                      (`Record
                                         (_loc,
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "loc")),
                                                     (`Lid (_loc, "loc")))),
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "txt")),
                                                     (`Lid (_loc, "txt")))))))))))),
                            (`Bar
                               (_loc,
                                 (`Case
                                    (_loc,
                                      (`App
                                         (_loc, (`Uid (_loc, "Some")),
                                           (`Chr (_loc, "L")))),
                                      (`App
                                         (_loc, (`Vrn (_loc, "Int64")),
                                           (`Record
                                              (_loc,
                                                (`Sem
                                                   (_loc,
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid (_loc, "loc")),
                                                          (`Lid (_loc, "loc")))),
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid (_loc, "txt")),
                                                          (`Lid (_loc, "txt")))))))))))),
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Uid (_loc, "Some")),
                                                (`Chr (_loc, "n")))),
                                           (`App
                                              (_loc,
                                                (`Vrn (_loc, "Nativeint")),
                                                (`Record
                                                   (_loc,
                                                     (`Sem
                                                        (_loc,
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "loc")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "loc")))),
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "txt")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "txt")))))))))))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`App
                                              (_loc, (`Vrn (_loc, "Int")),
                                                (`Record
                                                   (_loc,
                                                     (`Sem
                                                        (_loc,
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "loc")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "loc")))),
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "txt")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "txt"))))))))))))))))))))) : 
             Astf.exp ))])));
    ("ocaml_char",
      ((fun { loc = _loc;_}  ->
          [((Sequence
               ((Sequence
                   ((Characters [(39, 39)]),
                     (Bind
                        ((Alternative
                            ((Alternative
                                ((Characters [(10, 10)]),
                                  (Characters [(13, 13)]))),
                              (Sequence
                                 ((Characters [(13, 13)]),
                                   (Characters [(10, 10)]))))),
                          (({
                              loc_start =
                                {
                                  pos_fname = "predef_lex.ml";
                                  pos_lnum = 166;
                                  pos_bol = 5576;
                                  pos_cnum = 5603
                                };
                              loc_end =
                                {
                                  pos_fname = "predef_lex.ml";
                                  pos_lnum = 166;
                                  pos_bol = 5576;
                                  pos_cnum = 5606
                                };
                              loc_ghost = false
                            } : Locf.t ), "txt"))))),
                 (Characters [(39, 39)]))),
             (`Seq
                (_loc,
                  (`Sem
                     (_loc,
                       (`LetIn
                          (_loc, (`Negative _loc),
                            (`Bind
                               (_loc, (`Lid (_loc, "pos")),
                                 (`Field
                                    (_loc, (`Lid (_loc, "lexbuf")),
                                      (`Lid (_loc, "lex_curr_p")))))),
                            (`Assign
                               (_loc,
                                 (`Field
                                    (_loc, (`Lid (_loc, "lexbuf")),
                                      (`Lid (_loc, "lex_curr_p")))),
                                 (`RecordWith
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc,
                                                (`Lid (_loc, "pos_lnum")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Lid (_loc, "+")),
                                                          (`Field
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos_lnum")))))),
                                                     (`Int (_loc, "1")))))),
                                           (`RecBind
                                              (_loc,
                                                (`Lid (_loc, "pos_bol")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Lid (_loc, "-")),
                                                          (`Field
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos_cnum")))))),
                                                     (`Int (_loc, "1")))))))),
                                      (`Lid (_loc, "pos")))))))),
                       (`Constraint
                          (_loc,
                            (`App
                               (_loc, (`Vrn (_loc, "Chr")),
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "loc")),
                                                (`Record
                                                   (_loc,
                                                     (`Sem
                                                        (_loc,
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "loc_start")),
                                                               (`Field
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "lex_start_p")))))),
                                                          (`Sem
                                                             (_loc,
                                                               (`RecBind
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "loc_end")),
                                                                    (
                                                                    `Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))))),
                                                               (`RecBind
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "loc_ghost")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "false")))))))))))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "txt")),
                                                (`Lid (_loc, "txt")))))))))),
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "t"))))))))) : Astf.exp ));
          ((Sequence
              ((Sequence
                  ((Characters [(39, 39)]),
                    (Bind
                       ((Alternative
                           ((Characters
                               [(0, 9); (11, 12); (14, 91); (93, 255)]),
                             (Sequence
                                ((Characters [(92, 92)]),
                                  (Alternative
                                     ((Alternative
                                         ((Characters
                                             [(32, 32);
                                             (34, 34);
                                             (39, 39);
                                             (92, 92);
                                             (98, 98);
                                             (110, 110);
                                             (114, 114);
                                             (116, 116)]),
                                           (Sequence
                                              ((Sequence
                                                  ((Characters [(48, 57)]),
                                                    (Characters [(48, 57)]))),
                                                (Characters [(48, 57)]))))),
                                       (Sequence
                                          ((Sequence
                                              ((Characters [(120, 120)]),
                                                (Characters
                                                   [(48, 57);
                                                   (65, 70);
                                                   (97, 102)]))),
                                            (Characters
                                               [(48, 57);
                                               (65, 70);
                                               (97, 102)]))))))))),
                         (({
                             loc_start =
                               {
                                 pos_fname = "predef_lex.ml";
                                 pos_lnum = 179;
                                 pos_bol = 6008;
                                 pos_cnum = 6039
                               };
                             loc_end =
                               {
                                 pos_fname = "predef_lex.ml";
                                 pos_lnum = 179;
                                 pos_bol = 6008;
                                 pos_cnum = 6042
                               };
                             loc_ghost = false
                           } : Locf.t ), "txt"))))), (Characters [(39, 39)]))),
            (`Constraint
               (_loc,
                 (`App
                    (_loc, (`Vrn (_loc, "Chr")),
                      (`Record
                         (_loc,
                           (`Sem
                              (_loc,
                                (`RecBind
                                   (_loc, (`Lid (_loc, "loc")),
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "loc_start")),
                                                    (`Field
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "lexbuf")),
                                                         (`Lid
                                                            (_loc,
                                                              "lex_start_p")))))),
                                               (`Sem
                                                  (_loc,
                                                    (`RecBind
                                                       (_loc,
                                                         (`Lid
                                                            (_loc, "loc_end")),
                                                         (`Field
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "lexbuf")),
                                                              (`Lid
                                                                 (_loc,
                                                                   "lex_curr_p")))))),
                                                    (`RecBind
                                                       (_loc,
                                                         (`Lid
                                                            (_loc,
                                                              "loc_ghost")),
                                                         (`Lid
                                                            (_loc, "false")))))))))))),
                                (`RecBind
                                   (_loc, (`Lid (_loc, "txt")),
                                     (`Lid (_loc, "txt")))))))))),
                 (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))) : 
            Astf.exp ));
          ((Sequence
              ((Sequence ((Characters [(39, 39)]), (Characters [(92, 92)]))),
                (Bind
                   ((Characters [(0, 255)]),
                     (({
                         loc_start =
                           {
                             pos_fname = "predef_lex.ml";
                             pos_lnum = 185;
                             pos_bol = 6231;
                             pos_cnum = 6252
                           };
                         loc_end =
                           {
                             pos_fname = "predef_lex.ml";
                             pos_lnum = 185;
                             pos_bol = 6231;
                             pos_cnum = 6253
                           };
                         loc_ghost = false
                       } : Locf.t ), "c"))))),
            (`App
               (_loc,
                 (`App
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, "Lexing_util")),
                           (`Lid (_loc, "err")))),
                      (`App
                         (_loc, (`Uid (_loc, "Illegal_escape")),
                           (`App
                              (_loc,
                                (`App
                                   (_loc,
                                     (`Dot
                                        (_loc, (`Uid (_loc, "String")),
                                          (`Lid (_loc, "make")))),
                                     (`Int (_loc, "1")))),
                                (`Lid (_loc, "c")))))))),
                 (`Constraint
                    (_loc,
                      (`Record
                         (_loc,
                           (`Sem
                              (_loc,
                                (`RecBind
                                   (_loc, (`Lid (_loc, "loc_start")),
                                     (`Field
                                        (_loc, (`Lid (_loc, "lexbuf")),
                                          (`Lid (_loc, "lex_start_p")))))),
                                (`Sem
                                   (_loc,
                                     (`RecBind
                                        (_loc, (`Lid (_loc, "loc_end")),
                                          (`Field
                                             (_loc, (`Lid (_loc, "lexbuf")),
                                               (`Lid (_loc, "lex_curr_p")))))),
                                     (`RecBind
                                        (_loc, (`Lid (_loc, "loc_ghost")),
                                          (`Lid (_loc, "false")))))))))),
                      (`Dot (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t"))))))) : 
            Astf.exp ))])));
    ("ocaml_float_literal",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Sequence
                   ((Sequence
                       ((Sequence
                           ((Characters [(48, 57)]),
                             (Repetition (Characters [(48, 57); (95, 95)])))),
                         (Alternative
                            (Epsilon,
                              (Sequence
                                 ((Characters [(46, 46)]),
                                   (Repetition
                                      (Characters [(48, 57); (95, 95)])))))))),
                     (Alternative
                        (Epsilon,
                          (Sequence
                             ((Sequence
                                 ((Sequence
                                     ((Characters [(69, 69); (101, 101)]),
                                       (Alternative
                                          (Epsilon,
                                            (Characters [(43, 43); (45, 45)]))))),
                                   (Characters [(48, 57)]))),
                               (Repetition (Characters [(48, 57); (95, 95)])))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 192;
                         pos_bol = 6512;
                         pos_cnum = 6541
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 192;
                         pos_bol = 6512;
                         pos_cnum = 6544
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt"))),
             (`Constraint
                (_loc,
                  (`App
                     (_loc, (`Vrn (_loc, "Flo")),
                       (`Record
                          (_loc,
                            (`Sem
                               (_loc,
                                 (`RecBind
                                    (_loc, (`Lid (_loc, "loc")),
                                      (`Record
                                         (_loc,
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid
                                                        (_loc, "loc_start")),
                                                     (`Field
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "lexbuf")),
                                                          (`Lid
                                                             (_loc,
                                                               "lex_start_p")))))),
                                                (`Sem
                                                   (_loc,
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc,
                                                               "loc_end")),
                                                          (`Field
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "lexbuf")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "lex_curr_p")))))),
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc,
                                                               "loc_ghost")),
                                                          (`Lid
                                                             (_loc, "false")))))))))))),
                                 (`RecBind
                                    (_loc, (`Lid (_loc, "txt")),
                                      (`Lid (_loc, "txt")))))))))),
                  (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))) : 
             Astf.exp ))])));
    ("ocaml_comment",
      ((fun { quot_opt = q; loc = _loc;_}  ->
          [((Sequence
               ((Sequence ((Characters [(40, 40)]), (Characters [(42, 42)]))),
                 (Alternative
                    (Epsilon,
                      (Bind
                         ((Characters [(41, 41)]),
                           (({
                               loc_start =
                                 {
                                   pos_fname = "predef_lex.ml";
                                   pos_lnum = 201;
                                   pos_bol = 6810;
                                   pos_cnum = 6835
                                 };
                               loc_end =
                                 {
                                   pos_fname = "predef_lex.ml";
                                   pos_lnum = 201;
                                   pos_bol = 6810;
                                   pos_cnum = 6836
                                 };
                               loc_ghost = false
                             } : Locf.t ), "x"))))))),
             (append_quot q
                (`LetIn
                   (_loc, (`Negative _loc),
                     (`Bind
                        (_loc, (`Lid (_loc, "c")),
                          (`App
                             (_loc,
                               (`Dot
                                  (_loc, (`Uid (_loc, "Lexing_util")),
                                    (`Lid (_loc, "new_cxt")))),
                               (`Uid (_loc, "()")))))),
                     (`Seq
                        (_loc,
                          (`Sem
                             (_loc,
                               (`IfThen
                                  (_loc,
                                    (`App
                                       (_loc,
                                         (`App
                                            (_loc, (`Lid (_loc, "<>")),
                                              (`Lid (_loc, "x")))),
                                         (`Uid (_loc, "None")))),
                                    (`App
                                       (_loc,
                                         (`App
                                            (_loc,
                                              (`Dot
                                                 (_loc,
                                                   (`Uid
                                                      (_loc, "Lexing_util")),
                                                   (`Lid (_loc, "warn")))),
                                              (`Uid (_loc, "Comment_start")))),
                                         (`App
                                            (_loc,
                                              (`Dot
                                                 (_loc,
                                                   (`Uid
                                                      (_loc, "Lexing_util")),
                                                   (`Lid
                                                      (_loc, "from_lexbuf")))),
                                              (`Lid (_loc, "lexbuf")))))))),
                               (`Sem
                                  (_loc,
                                    (`App
                                       (_loc,
                                         (`App
                                            (_loc,
                                              (`Dot
                                                 (_loc,
                                                   (`Uid
                                                      (_loc, "Lexing_util")),
                                                   (`Lid (_loc, "store")))),
                                              (`Lid (_loc, "c")))),
                                         (`Lid (_loc, "lexbuf")))),
                                    (`Sem
                                       (_loc,
                                         (`App
                                            (_loc,
                                              (`App
                                                 (_loc,
                                                   (`App
                                                      (_loc,
                                                        (`Dot
                                                           (_loc,
                                                             (`Uid
                                                                (_loc,
                                                                  "Lexing_util")),
                                                             (`Lid
                                                                (_loc,
                                                                  "push_loc_cont")))),
                                                        (`Lid (_loc, "c")))),
                                                   (`Lid (_loc, "lexbuf")))),
                                              (`Dot
                                                 (_loc,
                                                   (`Uid
                                                      (_loc, "Lexing_util")),
                                                   (`Lid
                                                      (_loc, "lex_comment")))))),
                                         (`App
                                            (_loc, (`Lid (_loc, "ignore")),
                                              (`App
                                                 (_loc,
                                                   (`Dot
                                                      (_loc,
                                                        (`Uid
                                                           (_loc,
                                                             "Lexing_util")),
                                                        (`Lid
                                                           (_loc,
                                                             "buff_contents")))),
                                                   (`Lid (_loc, "c"))))))))))))))) : 
                Astf.exp )))])));
    ("whitespace",
      ((fun { quot_opt = q; loc = _loc;_}  ->
          [((Sequence
               ((Repetition (Characters [(9, 9); (12, 12); (32, 32)])),
                 (Characters [(9, 9); (12, 12); (32, 32)]))),
             (append_quot q (`Uid (_loc, "()") : Astf.exp )));
          ((Alternative
              ((Alternative
                  ((Characters [(10, 10)]), (Characters [(13, 13)]))),
                (Sequence ((Characters [(13, 13)]), (Characters [(10, 10)]))))),
            (append_quot q
               (`App
                  (_loc,
                    (`Dot
                       (_loc, (`Uid (_loc, "Lexing_util")),
                         (`Lid (_loc, "update_loc")))),
                    (`Lid (_loc, "lexbuf"))) : Astf.exp )))])));
    ("ocaml_string",
      ((fun { loc = _loc;_}  ->
          [((Characters [(34, 34)]),
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc, (`Lid (_loc, "c")),
                       (`App
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, "Lexing_util")),
                                 (`Lid (_loc, "new_cxt")))),
                            (`Uid (_loc, "()")))))),
                  (`LetIn
                     (_loc, (`Negative _loc),
                       (`Bind
                          (_loc, (`Lid (_loc, "old")),
                            (`Field
                               (_loc, (`Lid (_loc, "lexbuf")),
                                 (`Lid (_loc, "lex_start_p")))))),
                       (`Seq
                          (_loc,
                            (`Sem
                               (_loc,
                                 (`App
                                    (_loc,
                                      (`App
                                         (_loc,
                                           (`App
                                              (_loc,
                                                (`Dot
                                                   (_loc,
                                                     (`Uid
                                                        (_loc, "Lexing_util")),
                                                     (`Lid
                                                        (_loc,
                                                          "push_loc_cont")))),
                                                (`Lid (_loc, "c")))),
                                           (`Lid (_loc, "lexbuf")))),
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Lexing_util")),
                                           (`Lid (_loc, "lex_string")))))),
                                 (`App
                                    (_loc, (`Vrn (_loc, "Str")),
                                      (`Record
                                         (_loc,
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "loc")),
                                                     (`App
                                                        (_loc,
                                                          (`App
                                                             (_loc,
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Location_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "--")))),
                                                               (`Lid
                                                                  (_loc,
                                                                    "old")))),
                                                          (`Field
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "lexbuf")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "lex_curr_p")))))))),
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "txt")),
                                                     (`App
                                                        (_loc,
                                                          (`Dot
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc,
                                                                    "Lexing_util")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "buff_contents")))),
                                                          (`Lid (_loc, "c"))))))))))))))))))) : 
             Astf.exp ))])));
    ("default",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Characters [(0, 255)]),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 236;
                         pos_bol = 7969;
                         pos_cnum = 7984
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 236;
                         pos_bol = 7969;
                         pos_cnum = 7985
                       };
                     loc_ghost = false
                   } : Locf.t ), "c"))),
             (`App
                (_loc,
                  (`App
                     (_loc, (`Lid (_loc, "@@")),
                       (`App
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, "Lexing_util")),
                                 (`Lid (_loc, "err")))),
                            (`App
                               (_loc, (`Uid (_loc, "Illegal_character")),
                                 (`Lid (_loc, "c")))))))),
                  (`App
                     (_loc,
                       (`Dot
                          (_loc, (`Uid (_loc, "Lexing_util")),
                            (`Lid (_loc, "from_lexbuf")))),
                       (`Lid (_loc, "lexbuf"))))) : Astf.exp ))])));
    ("ocaml_eof",
      ((fun { loc = _loc;_}  ->
          [(Eof,
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc, (`Lid (_loc, "pos")),
                       (`Field
                          (_loc, (`Lid (_loc, "lexbuf")),
                            (`Lid (_loc, "lex_curr_p")))))),
                  (`Seq
                     (_loc,
                       (`Sem
                          (_loc,
                            (`Assign
                               (_loc,
                                 (`Field
                                    (_loc, (`Lid (_loc, "lexbuf")),
                                      (`Lid (_loc, "lex_curr_p")))),
                                 (`RecordWith
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc,
                                                (`Lid (_loc, "pos_bol")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Lid (_loc, "+")),
                                                          (`Field
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos_bol")))))),
                                                     (`Int (_loc, "1")))))),
                                           (`RecBind
                                              (_loc,
                                                (`Lid (_loc, "pos_cnum")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Lid (_loc, "+")),
                                                          (`Field
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "pos_cnum")))))),
                                                     (`Int (_loc, "1")))))))),
                                      (`Lid (_loc, "pos")))))),
                            (`LetIn
                               (_loc, (`Negative _loc),
                                 (`Bind
                                    (_loc, (`Lid (_loc, "loc")),
                                      (`App
                                         (_loc,
                                           (`Dot
                                              (_loc,
                                                (`Uid (_loc, "Lexing_util")),
                                                (`Lid (_loc, "from_lexbuf")))),
                                           (`Lid (_loc, "lexbuf")))))),
                                 (`Constraint
                                    (_loc,
                                      (`App
                                         (_loc, (`Vrn (_loc, "EOI")),
                                           (`Record
                                              (_loc,
                                                (`Sem
                                                   (_loc,
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid (_loc, "loc")),
                                                          (`Lid (_loc, "loc")))),
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid (_loc, "txt")),
                                                          (`Str (_loc, "")))))))))),
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Tokenf")),
                                           (`Lid (_loc, "t"))))))))))))) : 
             Astf.exp ))])));
    ("ocaml_simple_quotation",
      ((fun { loc = _loc;_}  ->
          [((Sequence ((Characters [(37, 37)]), (Characters [(123, 123)]))),
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc, (`Lid (_loc, "old")),
                       (`Field
                          (_loc, (`Lid (_loc, "lexbuf")),
                            (`Lid (_loc, "lex_start_p")))))),
                  (`LetIn
                     (_loc, (`Negative _loc),
                       (`Bind
                          (_loc, (`Lid (_loc, "c")),
                            (`App
                               (_loc,
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Lexing_util")),
                                      (`Lid (_loc, "new_cxt")))),
                                 (`Uid (_loc, "()")))))),
                       (`Seq
                          (_loc,
                            (`Sem
                               (_loc,
                                 (`App
                                    (_loc,
                                      (`App
                                         (_loc,
                                           (`Dot
                                              (_loc,
                                                (`Uid (_loc, "Lexing_util")),
                                                (`Lid (_loc, "store")))),
                                           (`Lid (_loc, "c")))),
                                      (`Lid (_loc, "lexbuf")))),
                                 (`Sem
                                    (_loc,
                                      (`App
                                         (_loc,
                                           (`App
                                              (_loc,
                                                (`App
                                                   (_loc,
                                                     (`Dot
                                                        (_loc,
                                                          (`Uid
                                                             (_loc,
                                                               "Lexing_util")),
                                                          (`Lid
                                                             (_loc,
                                                               "push_loc_cont")))),
                                                     (`Lid (_loc, "c")))),
                                                (`Lid (_loc, "lexbuf")))),
                                           (`Dot
                                              (_loc,
                                                (`Uid (_loc, "Lexing_util")),
                                                (`Lid (_loc, "lex_quotation")))))),
                                      (`App
                                         (_loc, (`Vrn (_loc, "Quot")),
                                           (`Record
                                              (_loc,
                                                (`Sem
                                                   (_loc,
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "name")),
                                                          (`Dot
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc,
                                                                    "Tokenf")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "empty_name")))))),
                                                     (`Sem
                                                        (_loc,
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "meta")),
                                                               (`Uid
                                                                  (_loc,
                                                                    "None")))),
                                                          (`Sem
                                                             (_loc,
                                                               (`RecBind
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (
                                                                    `App
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "buff_contents")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "c")))))),
                                                               (`Sem
                                                                  (_loc,
                                                                    (
                                                                    `RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")),
                                                                    (`Int
                                                                    (_loc,
                                                                    "2")))),
                                                                    (
                                                                    `Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")),
                                                                    (`Int
                                                                    (_loc,
                                                                    "1")))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Location_util")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "--")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "old")))),
                                                                    (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p"))))))))))))))))))))))))))))))) : 
             Astf.exp ))])));
    ("ocaml_quotation",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Sequence
                   ((Sequence
                       ((Sequence
                           ((Characters [(37, 37)]),
                             (Alternative
                                (Epsilon,
                                  (Bind
                                     ((Sequence
                                         ((Sequence
                                             ((Alternative
                                                 (Epsilon,
                                                   (Characters [(46, 46)]))),
                                               (Repetition
                                                  (Sequence
                                                     ((Sequence
                                                         ((Characters
                                                             [(65, 90);
                                                             (192, 214);
                                                             (216, 222)]),
                                                           (Repetition
                                                              (Characters
                                                                 [(39, 39);
                                                                 (48, 57);
                                                                 (65, 90);
                                                                 (95, 95);
                                                                 (97, 122);
                                                                 (192, 214);
                                                                 (216, 246);
                                                                 (248, 255)])))),
                                                       (Characters [(46, 46)])))))),
                                           (Sequence
                                              ((Characters
                                                  [(95, 95);
                                                  (97, 122);
                                                  (223, 246);
                                                  (248, 255)]),
                                                (Repetition
                                                   (Alternative
                                                      ((Characters
                                                          [(39, 39);
                                                          (48, 57);
                                                          (65, 90);
                                                          (95, 95);
                                                          (97, 122);
                                                          (192, 214);
                                                          (216, 246);
                                                          (248, 255)]),
                                                        (Characters
                                                           [(45, 45)])))))))),
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "predef_lex.ml";
                                               pos_lnum = 270;
                                               pos_bol = 8995;
                                               pos_cnum = 9030
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "predef_lex.ml";
                                               pos_lnum = 270;
                                               pos_bol = 8995;
                                               pos_cnum = 9034
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "name"))))))),
                         (Alternative
                            (Epsilon,
                              (Sequence
                                 ((Characters [(64, 64)]),
                                   (Bind
                                      ((Sequence
                                          ((Alternative
                                              ((Characters
                                                  [(95, 95);
                                                  (97, 122);
                                                  (223, 246);
                                                  (248, 255)]),
                                                (Characters
                                                   [(65, 90);
                                                   (192, 214);
                                                   (216, 222)]))),
                                            (Repetition
                                               (Characters
                                                  [(39, 39);
                                                  (48, 57);
                                                  (65, 90);
                                                  (95, 95);
                                                  (97, 122);
                                                  (192, 214);
                                                  (216, 246);
                                                  (248, 255)])))),
                                        (({
                                            loc_start =
                                              {
                                                pos_fname = "predef_lex.ml";
                                                pos_lnum = 270;
                                                pos_bol = 8995;
                                                pos_cnum = 9053
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "predef_lex.ml";
                                                pos_lnum = 270;
                                                pos_bol = 8995;
                                                pos_cnum = 9057
                                              };
                                            loc_ghost = false
                                          } : Locf.t ), "meta"))))))))),
                     (Characters [(123, 123)]))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 270;
                         pos_bol = 8995;
                         pos_cnum = 9071
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 270;
                         pos_bol = 8995;
                         pos_cnum = 9076
                       };
                     loc_ghost = false
                   } : Locf.t ), "shift"))),
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc, (`Lid (_loc, "c")),
                       (`App
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, "Lexing_util")),
                                 (`Lid (_loc, "new_cxt")))),
                            (`Uid (_loc, "()")))))),
                  (`LetIn
                     (_loc, (`Negative _loc),
                       (`Bind
                          (_loc, (`Lid (_loc, "name")),
                            (`Match
                               (_loc, (`Lid (_loc, "name")),
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Uid (_loc, "Some")),
                                                (`Lid (_loc, "name")))),
                                           (`App
                                              (_loc,
                                                (`Dot
                                                   (_loc,
                                                     (`Uid (_loc, "Tokenf")),
                                                     (`Lid
                                                        (_loc,
                                                          "name_of_string")))),
                                                (`Lid (_loc, "name")))))),
                                      (`Case
                                         (_loc, (`Uid (_loc, "None")),
                                           (`Dot
                                              (_loc, (`Uid (_loc, "Tokenf")),
                                                (`Lid (_loc, "empty_name")))))))))))),
                       (`Seq
                          (_loc,
                            (`LetIn
                               (_loc, (`Negative _loc),
                                 (`Bind
                                    (_loc, (`Lid (_loc, "old")),
                                      (`Field
                                         (_loc, (`Lid (_loc, "lexbuf")),
                                           (`Lid (_loc, "lex_start_p")))))),
                                 (`LetIn
                                    (_loc, (`Negative _loc),
                                      (`Bind
                                         (_loc, (`Lid (_loc, "txt")),
                                           (`Seq
                                              (_loc,
                                                (`Sem
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`App
                                                             (_loc,
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "store")))),
                                                               (`Lid
                                                                  (_loc, "c")))),
                                                          (`Lid
                                                             (_loc, "lexbuf")))),
                                                     (`Sem
                                                        (_loc,
                                                          (`App
                                                             (_loc,
                                                               (`App
                                                                  (_loc,
                                                                    (
                                                                    `App
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "push_loc_cont")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "c")))),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "lexbuf")))),
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "lex_quotation")))))),
                                                          (`App
                                                             (_loc,
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "buff_contents")))),
                                                               (`Lid
                                                                  (_loc, "c")))))))))))),
                                      (`LetIn
                                         (_loc, (`Negative _loc),
                                           (`Bind
                                              (_loc, (`Lid (_loc, "loc")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Dot
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc,
                                                                    "Location_util")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "--")))),
                                                          (`Lid (_loc, "old")))),
                                                     (`Field
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "lexbuf")),
                                                          (`Lid
                                                             (_loc,
                                                               "lex_curr_p")))))))),
                                           (`LetIn
                                              (_loc, (`Negative _loc),
                                                (`Bind
                                                   (_loc,
                                                     (`Lid (_loc, "shift")),
                                                     (`App
                                                        (_loc,
                                                          (`Dot
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc,
                                                                    "String")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "length")))),
                                                          (`Lid
                                                             (_loc, "shift")))))),
                                                (`LetIn
                                                   (_loc, (`Negative _loc),
                                                     (`Bind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc,
                                                               "retract")),
                                                          (`Int (_loc, "1")))),
                                                     (`App
                                                        (_loc,
                                                          (`Vrn
                                                             (_loc, "Quot")),
                                                          (`Record
                                                             (_loc,
                                                               (`Sem
                                                                  (_loc,
                                                                    (
                                                                    `RecBind
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "name")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "name")))),
                                                                    (
                                                                    `Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "meta")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "meta")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract"))))))))))))))))))))))))))))))))) : 
             Astf.exp ))])));
    ("ocaml_double_quotation",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Sequence
                   ((Sequence
                       ((Sequence
                           ((Sequence
                               ((Alternative
                                   (Epsilon,
                                     (Bind
                                        ((Characters [(37, 37)]),
                                          (({
                                              loc_start =
                                                {
                                                  pos_fname = "predef_lex.ml";
                                                  pos_lnum = 293;
                                                  pos_bol = 9800;
                                                  pos_cnum = 9820
                                                };
                                              loc_end =
                                                {
                                                  pos_fname = "predef_lex.ml";
                                                  pos_lnum = 293;
                                                  pos_bol = 9800;
                                                  pos_cnum = 9821
                                                };
                                              loc_ghost = false
                                            } : Locf.t ), "x"))))),
                                 (Characters [(37, 37)]))),
                             (Alternative
                                (Epsilon,
                                  (Bind
                                     ((Sequence
                                         ((Sequence
                                             ((Alternative
                                                 (Epsilon,
                                                   (Characters [(46, 46)]))),
                                               (Repetition
                                                  (Sequence
                                                     ((Sequence
                                                         ((Characters
                                                             [(65, 90);
                                                             (192, 214);
                                                             (216, 222)]),
                                                           (Repetition
                                                              (Characters
                                                                 [(39, 39);
                                                                 (48, 57);
                                                                 (65, 90);
                                                                 (95, 95);
                                                                 (97, 122);
                                                                 (192, 214);
                                                                 (216, 246);
                                                                 (248, 255)])))),
                                                       (Characters [(46, 46)])))))),
                                           (Sequence
                                              ((Characters
                                                  [(95, 95);
                                                  (97, 122);
                                                  (223, 246);
                                                  (248, 255)]),
                                                (Repetition
                                                   (Alternative
                                                      ((Characters
                                                          [(39, 39);
                                                          (48, 57);
                                                          (65, 90);
                                                          (95, 95);
                                                          (97, 122);
                                                          (192, 214);
                                                          (216, 246);
                                                          (248, 255)]),
                                                        (Characters
                                                           [(45, 45)])))))))),
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "predef_lex.ml";
                                               pos_lnum = 293;
                                               pos_bol = 9800;
                                               pos_cnum = 9849
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "predef_lex.ml";
                                               pos_lnum = 293;
                                               pos_bol = 9800;
                                               pos_cnum = 9853
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "name"))))))),
                         (Alternative
                            (Epsilon,
                              (Sequence
                                 ((Characters [(64, 64)]),
                                   (Bind
                                      ((Sequence
                                          ((Alternative
                                              ((Characters
                                                  [(95, 95);
                                                  (97, 122);
                                                  (223, 246);
                                                  (248, 255)]),
                                                (Characters
                                                   [(65, 90);
                                                   (192, 214);
                                                   (216, 222)]))),
                                            (Repetition
                                               (Characters
                                                  [(39, 39);
                                                  (48, 57);
                                                  (65, 90);
                                                  (95, 95);
                                                  (97, 122);
                                                  (192, 214);
                                                  (216, 246);
                                                  (248, 255)])))),
                                        (({
                                            loc_start =
                                              {
                                                pos_fname = "predef_lex.ml";
                                                pos_lnum = 293;
                                                pos_bol = 9800;
                                                pos_cnum = 9872
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "predef_lex.ml";
                                                pos_lnum = 293;
                                                pos_bol = 9800;
                                                pos_cnum = 9876
                                              };
                                            loc_ghost = false
                                          } : Locf.t ), "meta"))))))))),
                     (Characters [(123, 123)]))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 293;
                         pos_bol = 9800;
                         pos_cnum = 9887
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 293;
                         pos_bol = 9800;
                         pos_cnum = 9892
                       };
                     loc_ghost = false
                   } : Locf.t ), "shift"))),
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc, (`Lid (_loc, "c")),
                       (`App
                          (_loc,
                            (`Dot
                               (_loc, (`Uid (_loc, "Lexing_util")),
                                 (`Lid (_loc, "new_cxt")))),
                            (`Uid (_loc, "()")))))),
                  (`LetIn
                     (_loc, (`Negative _loc),
                       (`Bind
                          (_loc, (`Lid (_loc, "name")),
                            (`Match
                               (_loc, (`Lid (_loc, "name")),
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Uid (_loc, "Some")),
                                                (`Lid (_loc, "name")))),
                                           (`App
                                              (_loc,
                                                (`Dot
                                                   (_loc,
                                                     (`Uid (_loc, "Tokenf")),
                                                     (`Lid
                                                        (_loc,
                                                          "name_of_string")))),
                                                (`Lid (_loc, "name")))))),
                                      (`Case
                                         (_loc, (`Uid (_loc, "None")),
                                           (`Dot
                                              (_loc, (`Uid (_loc, "Tokenf")),
                                                (`Lid (_loc, "empty_name")))))))))))),
                       (`Seq
                          (_loc,
                            (`LetIn
                               (_loc, (`Negative _loc),
                                 (`Bind
                                    (_loc, (`Lid (_loc, "old")),
                                      (`Field
                                         (_loc, (`Lid (_loc, "lexbuf")),
                                           (`Lid (_loc, "lex_start_p")))))),
                                 (`LetIn
                                    (_loc, (`Negative _loc),
                                      (`Bind
                                         (_loc, (`Lid (_loc, "txt")),
                                           (`Seq
                                              (_loc,
                                                (`Sem
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`App
                                                             (_loc,
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "store")))),
                                                               (`Lid
                                                                  (_loc, "c")))),
                                                          (`Lid
                                                             (_loc, "lexbuf")))),
                                                     (`Sem
                                                        (_loc,
                                                          (`App
                                                             (_loc,
                                                               (`App
                                                                  (_loc,
                                                                    (
                                                                    `App
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "push_loc_cont")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "c")))),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "lexbuf")))),
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "lex_quotation")))))),
                                                          (`App
                                                             (_loc,
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "buff_contents")))),
                                                               (`Lid
                                                                  (_loc, "c")))))))))))),
                                      (`LetIn
                                         (_loc, (`Negative _loc),
                                           (`Bind
                                              (_loc, (`Lid (_loc, "loc")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Dot
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc,
                                                                    "Location_util")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "--")))),
                                                          (`Lid (_loc, "old")))),
                                                     (`Field
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "lexbuf")),
                                                          (`Lid
                                                             (_loc,
                                                               "lex_curr_p")))))))),
                                           (`LetIn
                                              (_loc, (`Negative _loc),
                                                (`Bind
                                                   (_loc,
                                                     (`Lid (_loc, "shift")),
                                                     (`App
                                                        (_loc,
                                                          (`Dot
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc,
                                                                    "String")),
                                                               (`Lid
                                                                  (_loc,
                                                                    "length")))),
                                                          (`Lid
                                                             (_loc, "shift")))))),
                                                (`LetIn
                                                   (_loc, (`Negative _loc),
                                                     (`Bind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc,
                                                               "retract")),
                                                          (`Int (_loc, "1")))),
                                                     (`IfThenElse
                                                        (_loc,
                                                          (`App
                                                             (_loc,
                                                               (`App
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "=")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "x")))),
                                                               (`Uid
                                                                  (_loc,
                                                                    "None")))),
                                                          (`Constraint
                                                             (_loc,
                                                               (`App
                                                                  (_loc,
                                                                    (
                                                                    `Vrn
                                                                    (_loc,
                                                                    "Quot")),
                                                                    (
                                                                    `Record
                                                                    (_loc,
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "name")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "name")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "meta")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "meta")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")))))))))))))))))),
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "t")))))),
                                                          (`Constraint
                                                             (_loc,
                                                               (`App
                                                                  (_loc,
                                                                    (
                                                                    `Vrn
                                                                    (_loc,
                                                                    "DirQuotation")),
                                                                    (
                                                                    `Record
                                                                    (_loc,
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "name")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "name")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "meta")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "meta")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")))))))))))))))))),
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "t"))))))))))))))))))))))) : 
             Astf.exp ))])));
    ("line_directive",
      ((fun { quot_opt = q; loc = _loc;_}  ->
          [((Sequence
               ((Sequence
                   ((Sequence
                       ((Sequence
                           ((Sequence
                               ((Sequence
                                   ((Characters [(35, 35)]),
                                     (Repetition
                                        (Characters [(9, 9); (32, 32)])))),
                                 (Bind
                                    ((Sequence
                                        ((Repetition (Characters [(48, 57)])),
                                          (Characters [(48, 57)]))),
                                      (({
                                          loc_start =
                                            {
                                              pos_fname = "predef_lex.ml";
                                              pos_lnum = 321;
                                              pos_bol = 10746;
                                              pos_cnum = 10786
                                            };
                                          loc_end =
                                            {
                                              pos_fname = "predef_lex.ml";
                                              pos_lnum = 321;
                                              pos_bol = 10746;
                                              pos_cnum = 10789
                                            };
                                          loc_ghost = false
                                        } : Locf.t ), "num"))))),
                             (Repetition (Characters [(9, 9); (32, 32)])))),
                         (Alternative
                            (Epsilon,
                              (Sequence
                                 ((Sequence
                                     ((Characters [(34, 34)]),
                                       (Bind
                                          ((Repetition
                                              (Characters
                                                 [(0, 9);
                                                 (11, 12);
                                                 (14, 33);
                                                 (35, 255)])),
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "predef_lex.ml";
                                                    pos_lnum = 322;
                                                    pos_bol = 10803;
                                                    pos_cnum = 10845
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "predef_lex.ml";
                                                    pos_lnum = 322;
                                                    pos_bol = 10803;
                                                    pos_cnum = 10849
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "name"))))),
                                   (Characters [(34, 34)]))))))),
                     (Repetition (Characters [(0, 9); (11, 12); (14, 255)])))),
                 (Alternative
                    ((Alternative
                        ((Characters [(10, 10)]), (Characters [(13, 13)]))),
                      (Sequence
                         ((Characters [(13, 13)]), (Characters [(10, 10)]))))))),
             (append_quot q
                (`Seq
                   (_loc,
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc,
                                    (`App
                                       (_loc,
                                         (`Dot
                                            (_loc,
                                              (`Uid (_loc, "Lexing_util")),
                                              (`Lid (_loc, "update_loc")))),
                                         (`Lid (_loc, "lexbuf")))),
                                    (`OptLabl
                                       (_loc, (`Lid (_loc, "file")),
                                         (`Lid (_loc, "name")))))),
                               (`Label
                                  (_loc, (`Lid (_loc, "line")),
                                    (`App
                                       (_loc, (`Lid (_loc, "int_of_string")),
                                         (`Lid (_loc, "num")))))))),
                          (`Label
                             (_loc, (`Lid (_loc, "absolute")),
                               (`Lid (_loc, "true"))))))) : Astf.exp )))])));
    ("ocaml_ant",
      ((fun { loc = _loc;_}  ->
          [((Bind
               ((Sequence
                   ((Sequence
                       ((Characters [(36, 36)]),
                         (Bind
                            ((Sequence
                                ((Characters
                                    [(95, 95);
                                    (97, 122);
                                    (223, 246);
                                    (248, 255)]),
                                  (Repetition
                                     (Characters
                                        [(39, 39);
                                        (48, 57);
                                        (65, 90);
                                        (95, 95);
                                        (97, 122);
                                        (192, 214);
                                        (216, 246);
                                        (248, 255)])))),
                              (({
                                  loc_start =
                                    {
                                      pos_fname = "predef_lex.ml";
                                      pos_lnum = 342;
                                      pos_bol = 11383;
                                      pos_cnum = 11413
                                    };
                                  loc_end =
                                    {
                                      pos_fname = "predef_lex.ml";
                                      pos_lnum = 342;
                                      pos_bol = 11383;
                                      pos_cnum = 11417
                                    };
                                  loc_ghost = false
                                } : Locf.t ), "name"))))),
                     (Alternative
                        (Epsilon,
                          (Bind
                             ((Sequence
                                 ((Characters [(58, 58)]),
                                   (Sequence
                                      ((Repetition
                                          (Characters
                                             [(39, 39);
                                             (48, 57);
                                             (65, 90);
                                             (95, 95);
                                             (97, 122);
                                             (192, 214);
                                             (216, 246);
                                             (248, 255)])),
                                        (Characters
                                           [(39, 39);
                                           (48, 57);
                                           (65, 90);
                                           (95, 95);
                                           (97, 122);
                                           (192, 214);
                                           (216, 246);
                                           (248, 255)]))))),
                               (({
                                   loc_start =
                                     {
                                       pos_fname = "predef_lex.ml";
                                       pos_lnum = 342;
                                       pos_bol = 11383;
                                       pos_cnum = 11439
                                     };
                                   loc_end =
                                     {
                                       pos_fname = "predef_lex.ml";
                                       pos_lnum = 342;
                                       pos_bol = 11383;
                                       pos_cnum = 11445
                                     };
                                   loc_ghost = false
                                 } : Locf.t ), "follow"))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 342;
                         pos_bol = 11383;
                         pos_cnum = 11451
                       };
                     loc_end =
                       {
                         pos_fname = "predef_lex.ml";
                         pos_lnum = 342;
                         pos_bol = 11383;
                         pos_cnum = 11454
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt"))),
             (`LetIn
                (_loc, (`Negative _loc),
                  (`Bind
                     (_loc,
                       (`Par
                          (_loc,
                            (`Com
                               (_loc, (`Lid (_loc, "kind")),
                                 (`Lid (_loc, "shift")))))),
                       (`Match
                          (_loc, (`Lid (_loc, "follow")),
                            (`Bar
                               (_loc,
                                 (`Case
                                    (_loc, (`Uid (_loc, "None")),
                                      (`Par
                                         (_loc,
                                           (`Com
                                              (_loc, (`Str (_loc, "")),
                                                (`Int (_loc, "1")))))))),
                                 (`Case
                                    (_loc,
                                      (`App
                                         (_loc, (`Uid (_loc, "Some")),
                                           (`Any _loc))),
                                      (`Par
                                         (_loc,
                                           (`Com
                                              (_loc, (`Lid (_loc, "name")),
                                                (`App
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Lid (_loc, "+")),
                                                          (`App
                                                             (_loc,
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "String")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "length")))),
                                                               (`Lid
                                                                  (_loc,
                                                                    "name")))))),
                                                     (`Int (_loc, "2")))))))))))))))),
                  (`Constraint
                     (_loc,
                       (`App
                          (_loc, (`Vrn (_loc, "Ant")),
                            (`Record
                               (_loc,
                                 (`Sem
                                    (_loc,
                                      (`RecBind
                                         (_loc, (`Lid (_loc, "loc")),
                                           (`App
                                              (_loc,
                                                (`Dot
                                                   (_loc,
                                                     (`Uid
                                                        (_loc, "Lexing_util")),
                                                     (`Lid
                                                        (_loc, "from_lexbuf")))),
                                                (`Lid (_loc, "lexbuf")))))),
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "kind")),
                                                (`Lid (_loc, "kind")))),
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "txt")),
                                                     (`Lid (_loc, "txt")))),
                                                (`Sem
                                                   (_loc,
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "shift")),
                                                          (`Lid
                                                             (_loc, "shift")))),
                                                     (`Sem
                                                        (_loc,
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "retract")),
                                                               (`Int
                                                                  (_loc, "0")))),
                                                          (`RecBind
                                                             (_loc,
                                                               (`Lid
                                                                  (_loc,
                                                                    "cxt")),
                                                               (`Uid
                                                                  (_loc,
                                                                    "None")))))))))))))))))),
                       (`Dot
                          (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))))) : 
             Astf.exp ));
          ((Bind
              ((Sequence
                  ((Sequence
                      ((Characters [(36, 36)]),
                        (Alternative
                           (Epsilon,
                             (Bind
                                ((Sequence
                                    ((Characters
                                        [(95, 95);
                                        (97, 122);
                                        (223, 246);
                                        (248, 255)]),
                                      (Repetition
                                         (Characters
                                            [(39, 39);
                                            (48, 57);
                                            (65, 90);
                                            (95, 95);
                                            (97, 122);
                                            (192, 214);
                                            (216, 246);
                                            (248, 255)])))),
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "predef_lex.ml";
                                          pos_lnum = 353;
                                          pos_bol = 11754;
                                          pos_cnum = 11784
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "predef_lex.ml";
                                          pos_lnum = 353;
                                          pos_bol = 11754;
                                          pos_cnum = 11788
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "name"))))))),
                    (Characters [(123, 123)]))),
                (({
                    loc_start =
                      {
                        pos_fname = "predef_lex.ml";
                        pos_lnum = 353;
                        pos_bol = 11754;
                        pos_cnum = 11799
                      };
                    loc_end =
                      {
                        pos_fname = "predef_lex.ml";
                        pos_lnum = 353;
                        pos_bol = 11754;
                        pos_cnum = 11802
                      };
                    loc_ghost = false
                  } : Locf.t ), "txt"))),
            (`LetIn
               (_loc, (`Negative _loc),
                 (`Bind
                    (_loc, (`Lid (_loc, "old")),
                      (`Field
                         (_loc, (`Lid (_loc, "lexbuf")),
                           (`Lid (_loc, "lex_start_p")))))),
                 (`LetIn
                    (_loc, (`Negative _loc),
                      (`Bind
                         (_loc, (`Lid (_loc, "c")),
                           (`App
                              (_loc,
                                (`Dot
                                   (_loc, (`Uid (_loc, "Lexing_util")),
                                     (`Lid (_loc, "new_cxt")))),
                                (`Uid (_loc, "()")))))),
                      (`Seq
                         (_loc,
                           (`Sem
                              (_loc,
                                (`App
                                   (_loc,
                                     (`App
                                        (_loc,
                                          (`Dot
                                             (_loc,
                                               (`Uid (_loc, "Lexing_util")),
                                               (`Lid (_loc, "store")))),
                                          (`Lid (_loc, "c")))),
                                     (`Lid (_loc, "lexbuf")))),
                                (`Sem
                                   (_loc,
                                     (`App
                                        (_loc,
                                          (`App
                                             (_loc,
                                               (`App
                                                  (_loc,
                                                    (`Dot
                                                       (_loc,
                                                         (`Uid
                                                            (_loc,
                                                              "Lexing_util")),
                                                         (`Lid
                                                            (_loc,
                                                              "push_loc_cont")))),
                                                    (`Lid (_loc, "c")))),
                                               (`Lid (_loc, "lexbuf")))),
                                          (`Dot
                                             (_loc,
                                               (`Uid (_loc, "Lexing_util")),
                                               (`Lid (_loc, "lex_quotation")))))),
                                     (`App
                                        (_loc, (`Vrn (_loc, "Ant")),
                                          (`Record
                                             (_loc,
                                               (`Sem
                                                  (_loc,
                                                    (`RecBind
                                                       (_loc,
                                                         (`Lid (_loc, "loc")),
                                                         (`Record
                                                            (_loc,
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_start")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "old")))),
                                                                   (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_end")),
                                                                    (`Field
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lexbuf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "lex_curr_p")))))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc_ghost")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "false")))))))))))),
                                                    (`Sem
                                                       (_loc,
                                                         (`RecBind
                                                            (_loc,
                                                              (`Lid
                                                                 (_loc,
                                                                   "kind")),
                                                              (`Match
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "name")),
                                                                   (`Bar
                                                                    (_loc,
                                                                    (`Case
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Some")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "n")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "n")))),
                                                                    (`Case
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "None")),
                                                                    (`Str
                                                                    (_loc,
                                                                    "")))))))))),
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                   (`App
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Lexing_util")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "buff_contents")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "c")))))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "shift")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "String")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "length")))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))),
                                                                   (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "retract")),
                                                                    (`Int
                                                                    (_loc,
                                                                    "1")))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "cxt")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "None"))))))))))))))))))))))))))) : 
            Astf.exp ));
          ((Sequence
              ((Characters [(36, 36)]),
                (Bind
                   ((Characters [(0, 255)]),
                     (({
                         loc_start =
                           {
                             pos_fname = "predef_lex.ml";
                             pos_lnum = 369;
                             pos_bol = 12340;
                             pos_cnum = 12359
                           };
                         loc_end =
                           {
                             pos_fname = "predef_lex.ml";
                             pos_lnum = 369;
                             pos_bol = 12340;
                             pos_cnum = 12360
                           };
                         loc_ghost = false
                       } : Locf.t ), "c"))))),
            (`App
               (_loc,
                 (`App
                    (_loc, (`Lid (_loc, "@@")),
                      (`App
                         (_loc,
                           (`Dot
                              (_loc, (`Uid (_loc, "Lexing_util")),
                                (`Lid (_loc, "err")))),
                           (`App
                              (_loc, (`Uid (_loc, "Illegal_character")),
                                (`Lid (_loc, "c")))))))),
                 (`App
                    (_loc,
                      (`Dot
                         (_loc, (`Uid (_loc, "Lexing_util")),
                           (`Lid (_loc, "from_lexbuf")))),
                      (`Lid (_loc, "lexbuf"))))) : Astf.exp ))])))]
