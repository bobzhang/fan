let as_cset = Translate_lex.as_cset
let regexp_for_string = Translate_lex.regexp_for_string
let remove_as = Translate_lex.remove_as
let named_regexps: (string,Translate_lex.concrete_regexp) Hashtbl.t =
  Hashtbl.create 13
let named_cases:
  (string,Tokenf.txt list option ->
            Tokenf.quot option ->
              Locf.t -> (Translate_lex.concrete_regexp* FAst.exp) list)
    Hashtbl.t
  = Hashtbl.create 13
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
let append_quot (y : Tokenf.quot option) (e : FAst.exp) =
  match y with
  | None  -> e
  | Some y ->
      let a = Parsef.expand_exp y in
      let _loc = y.loc in (`Seq (_loc, (`Sem (_loc, e, a))) : FAst.exp )
let _ =
  Hashtblf.add_list named_cases
    [("ocaml_uid",
       ((fun ls  _  _loc  ->
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
                          pos_fname = "parse_lex.ml";
                          pos_lnum = 65;
                          pos_bol = 2154;
                          pos_cnum = 2179
                        };
                      loc_end =
                        {
                          pos_fname = "parse_lex.ml";
                          pos_lnum = 65;
                          pos_bol = 2154;
                          pos_cnum = 2182
                        };
                      loc_ghost = false
                    } : Locf.t ), "txt"))),
              ((let default: FAst.exp =
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
                                        (`Str (_loc, v))))) : FAst.case )) x) in
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
                              default))) : FAst.exp ))))])));
    ("ocaml_lid",
      ((fun ls  _  _loc  ->
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
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 95;
                         pos_bol = 3122;
                         pos_cnum = 3146
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 95;
                         pos_bol = 3122;
                         pos_cnum = 3149
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt"))),
             ((let default: FAst.exp =
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
                                       (`Str (_loc, v))))) : FAst.case )) x) in
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
                             default))) : FAst.exp ))))])));
    ("kwd_symbol",
      ((fun ls  _  _loc  ->
          match ls with
          | Some ls ->
              let regexp =
                Listf.reduce_left_with
                  ~compose:(fun r1  r2  ->
                              (Alternative (r1, r2) : Translate_lex.concrete_regexp ))
                  ~project:(fun (x : Tokenf.txt)  ->
                              regexp_for_string @@ (TokenEval.string x.txt))
                  ls in
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
                                (`Lid (_loc, "t"))))))) : FAst.exp ))]
          | None  -> Locf.failf _loc "no following strings after kwd_symbol")));
    ("ocaml_int",
      ((fun _  _  _loc  ->
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
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 148;
                         pos_bol = 4873;
                         pos_cnum = 4899
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 148;
                         pos_bol = 4873;
                         pos_cnum = 4902
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
                                 (`Lid (_loc, "txt"))))))))) : FAst.exp ))])));
    ("ocaml_num_literal",
      ((fun _  _  _loc  ->
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
                                       pos_fname = "parse_lex.ml";
                                       pos_lnum = 153;
                                       pos_bol = 5017;
                                       pos_cnum = 5059
                                     };
                                   loc_end =
                                     {
                                       pos_fname = "parse_lex.ml";
                                       pos_lnum = 153;
                                       pos_bol = 5017;
                                       pos_cnum = 5060
                                     };
                                   loc_ghost = false
                                 } : Locf.t ), "s"))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 153;
                         pos_bol = 5017;
                         pos_cnum = 5069
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 153;
                         pos_bol = 5017;
                         pos_cnum = 5072
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
             FAst.exp ))])));
    ("ocaml_char",
      ((fun _  _  _loc  ->
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
                                  pos_fname = "parse_lex.ml";
                                  pos_lnum = 168;
                                  pos_bol = 5456;
                                  pos_cnum = 5483
                                };
                              loc_end =
                                {
                                  pos_fname = "parse_lex.ml";
                                  pos_lnum = 168;
                                  pos_bol = 5456;
                                  pos_cnum = 5486
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
                                 (`Lid (_loc, "t"))))))))) : FAst.exp ));
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
                                 pos_fname = "parse_lex.ml";
                                 pos_lnum = 181;
                                 pos_bol = 5888;
                                 pos_cnum = 5919
                               };
                             loc_end =
                               {
                                 pos_fname = "parse_lex.ml";
                                 pos_lnum = 181;
                                 pos_bol = 5888;
                                 pos_cnum = 5922
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
            FAst.exp ));
          ((Sequence
              ((Sequence ((Characters [(39, 39)]), (Characters [(92, 92)]))),
                (Bind
                   ((Characters [(0, 255)]),
                     (({
                         loc_start =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 187;
                             pos_bol = 6111;
                             pos_cnum = 6132
                           };
                         loc_end =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 187;
                             pos_bol = 6111;
                             pos_cnum = 6133
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
            FAst.exp ))])));
    ("ocaml_float_literal",
      ((fun _  _  _loc  ->
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
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 194;
                         pos_bol = 6385;
                         pos_cnum = 6414
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 194;
                         pos_bol = 6385;
                         pos_cnum = 6417
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
             FAst.exp ))])));
    ("ocaml_comment",
      ((fun _  q  _loc  ->
          [((Sequence
               ((Sequence ((Characters [(40, 40)]), (Characters [(42, 42)]))),
                 (Alternative
                    (Epsilon,
                      (Bind
                         ((Characters [(41, 41)]),
                           (({
                               loc_start =
                                 {
                                   pos_fname = "parse_lex.ml";
                                   pos_lnum = 203;
                                   pos_bol = 6659;
                                   pos_cnum = 6684
                                 };
                               loc_end =
                                 {
                                   pos_fname = "parse_lex.ml";
                                   pos_lnum = 203;
                                   pos_bol = 6659;
                                   pos_cnum = 6685
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
                FAst.exp )))])));
    ("whitespace",
      ((fun _  q  _loc  ->
          [((Sequence
               ((Repetition (Characters [(9, 9); (12, 12); (32, 32)])),
                 (Characters [(9, 9); (12, 12); (32, 32)]))),
             (append_quot q (`Uid (_loc, "()") : FAst.exp )));
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
                    (`Lid (_loc, "lexbuf"))) : FAst.exp )))])));
    ("ocaml_string",
      ((fun _  _  _loc  ->
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
             FAst.exp ))])));
    ("default",
      ((fun _  _  _loc  ->
          [((Bind
               ((Characters [(0, 255)]),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 238;
                         pos_bol = 7782;
                         pos_cnum = 7797
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 238;
                         pos_bol = 7782;
                         pos_cnum = 7798
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
                       (`Lid (_loc, "lexbuf"))))) : FAst.exp ))])));
    ("ocaml_eof",
      ((fun _  _  _loc  ->
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
             FAst.exp ))])));
    ("ocaml_simple_quotation",
      ((fun _  _  _loc  ->
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
             FAst.exp ))])));
    ("ocaml_quotation",
      ((fun _  _  _loc  ->
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
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 272;
                                               pos_bol = 8787;
                                               pos_cnum = 8822
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 272;
                                               pos_bol = 8787;
                                               pos_cnum = 8826
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
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 272;
                                                pos_bol = 8787;
                                                pos_cnum = 8845
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 272;
                                                pos_bol = 8787;
                                                pos_cnum = 8849
                                              };
                                            loc_ghost = false
                                          } : Locf.t ), "meta"))))))))),
                     (Characters [(123, 123)]))),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 272;
                         pos_bol = 8787;
                         pos_cnum = 8863
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 272;
                         pos_bol = 8787;
                         pos_cnum = 8868
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
             FAst.exp ))])));
    ("ocaml_double_quotation",
      ((fun _  _  _loc  ->
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
                                                  pos_fname = "parse_lex.ml";
                                                  pos_lnum = 295;
                                                  pos_bol = 9585;
                                                  pos_cnum = 9605
                                                };
                                              loc_end =
                                                {
                                                  pos_fname = "parse_lex.ml";
                                                  pos_lnum = 295;
                                                  pos_bol = 9585;
                                                  pos_cnum = 9606
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
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 295;
                                               pos_bol = 9585;
                                               pos_cnum = 9634
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 295;
                                               pos_bol = 9585;
                                               pos_cnum = 9638
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
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 295;
                                                pos_bol = 9585;
                                                pos_cnum = 9657
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 295;
                                                pos_bol = 9585;
                                                pos_cnum = 9661
                                              };
                                            loc_ghost = false
                                          } : Locf.t ), "meta"))))))))),
                     (Characters [(123, 123)]))),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 295;
                         pos_bol = 9585;
                         pos_cnum = 9672
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 295;
                         pos_bol = 9585;
                         pos_cnum = 9677
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
             FAst.exp ))])));
    ("line_directive",
      ((fun _  q  _loc  ->
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
                                              pos_fname = "parse_lex.ml";
                                              pos_lnum = 323;
                                              pos_bol = 10509;
                                              pos_cnum = 10549
                                            };
                                          loc_end =
                                            {
                                              pos_fname = "parse_lex.ml";
                                              pos_lnum = 323;
                                              pos_bol = 10509;
                                              pos_cnum = 10552
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
                                                      "parse_lex.ml";
                                                    pos_lnum = 324;
                                                    pos_bol = 10566;
                                                    pos_cnum = 10608
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 324;
                                                    pos_bol = 10566;
                                                    pos_cnum = 10612
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
                               (`Lid (_loc, "true"))))))) : FAst.exp )))])));
    ("ocaml_ant",
      ((fun _  _  _loc  ->
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
                                      pos_fname = "parse_lex.ml";
                                      pos_lnum = 344;
                                      pos_bol = 11137;
                                      pos_cnum = 11167
                                    };
                                  loc_end =
                                    {
                                      pos_fname = "parse_lex.ml";
                                      pos_lnum = 344;
                                      pos_bol = 11137;
                                      pos_cnum = 11171
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
                                       pos_fname = "parse_lex.ml";
                                       pos_lnum = 344;
                                       pos_bol = 11137;
                                       pos_cnum = 11193
                                     };
                                   loc_end =
                                     {
                                       pos_fname = "parse_lex.ml";
                                       pos_lnum = 344;
                                       pos_bol = 11137;
                                       pos_cnum = 11199
                                     };
                                   loc_ghost = false
                                 } : Locf.t ), "follow"))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 344;
                         pos_bol = 11137;
                         pos_cnum = 11205
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 344;
                         pos_bol = 11137;
                         pos_cnum = 11208
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
             FAst.exp ));
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
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 355;
                                          pos_bol = 11508;
                                          pos_cnum = 11538
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 355;
                                          pos_bol = 11508;
                                          pos_cnum = 11542
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "name"))))))),
                    (Characters [(123, 123)]))),
                (({
                    loc_start =
                      {
                        pos_fname = "parse_lex.ml";
                        pos_lnum = 355;
                        pos_bol = 11508;
                        pos_cnum = 11553
                      };
                    loc_end =
                      {
                        pos_fname = "parse_lex.ml";
                        pos_lnum = 355;
                        pos_bol = 11508;
                        pos_cnum = 11556
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
            FAst.exp ));
          ((Sequence
              ((Characters [(36, 36)]),
                (Bind
                   ((Characters [(0, 255)]),
                     (({
                         loc_start =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 371;
                             pos_bol = 12094;
                             pos_cnum = 12113
                           };
                         loc_end =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 371;
                             pos_bol = 12094;
                             pos_cnum = 12114
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
                      (`Lid (_loc, "lexbuf"))))) : FAst.exp ))])))]
let meta_cset _loc (x : Fcset.t) =
  Fan_ops.meta_list
    (fun _loc  (a,b)  ->
       (`Par
          (_loc,
            (`Com
               (_loc, (`Int (_loc, (string_of_int a))),
                 (`Int (_loc, (string_of_int b)))))) : FAst.ep )) _loc x
let rec meta_concrete_regexp _loc (x : Translate_lex.concrete_regexp) =
  match x with
  | Epsilon  -> (`Uid (_loc, "Epsilon") : FAst.ep )
  | Eof  -> (`Uid (_loc, "Eof") : FAst.ep )
  | Characters a ->
      (`App (_loc, (`Uid (_loc, "Characters")), (meta_cset _loc a)) : 
      FAst.ep )
  | Sequence (a0,a1) ->
      (`App
         (_loc,
           (`App
              (_loc, (`Uid (_loc, "Sequence")),
                (meta_concrete_regexp _loc a0))),
           (meta_concrete_regexp _loc a1)) : FAst.ep )
  | Alternative (a0,a1) ->
      (`App
         (_loc,
           (`App
              (_loc, (`Uid (_loc, "Alternative")),
                (meta_concrete_regexp _loc a0))),
           (meta_concrete_regexp _loc a1)) : FAst.ep )
  | Repetition a ->
      (`App
         (_loc, (`Uid (_loc, "Repetition")), (meta_concrete_regexp _loc a)) : 
      FAst.ep )
  | Bind (a,(loc,s)) ->
      (`App
         (_loc, (`Uid (_loc, "Bind")),
           (`Par
              (_loc,
                (`Com
                   (_loc, (meta_concrete_regexp _loc a),
                     (`Par
                        (_loc,
                          (`Com
                             (_loc, (Ast_gen.meta_here _loc loc),
                               (`Str (loc, (String.escaped s)) : FAst.ep )))))))))) : 
      FAst.ep )
let _ = Hashtbl.add named_regexps "eof" Eof
exception UnboundRegexp
exception UnboundCase
let regexp = Gramf.mk "regexp"
let char_class = Gramf.mk "char_class"
let char_class1 = Gramf.mk "char_class1"
let lex = Gramf.mk "lex"
let declare_regexp = Gramf.mk "declare_regexp"
let lex_fan = Gramf.mk "lex_fan"
let case = Gramf.mk "case"
let make_automata shortest l =
  Compile_lex.output_entry @@
    (Lexgen.make_single_dfa { shortest; clauses = (Listf.concat l) })
let make_lex nt a b =
  Gramf.extend_single (nt : 'nt Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" }
                  } : Tokenf.pattern );
              List0sep
                ((Nterm (Gramf.obj (case : 'case Gramf.t ))),
                  (Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern )))];
            annot = "";
            fn =
              (Gramf.mk_action
                 (a : 'case list -> Tokenf.txt -> Locf.t -> 'nt ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "<"); tag_name = "Key" } } : 
                Tokenf.pattern );
             List0sep
               ((Nterm (Gramf.obj (case : 'case Gramf.t ))),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )))];
           annot = "";
           fn =
             (Gramf.mk_action
                (b : 'case list -> Tokenf.txt -> Locf.t -> 'nt ))
         }]
     } : Gramf.olevel )
let _ =
  make_lex lex (fun l  _  _  -> make_automata false l)
    (fun l  _  _  -> make_automata true l);
  make_lex lex_fan
    (fun l  _  _loc  ->
       let e = make_automata false l in
       (`Constraint
          (_loc, e,
            (`Arrow
               (_loc,
                 (`Dot
                    (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lexbuf")))),
                 (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))))) : 
         FAst.exp ))
    (fun l  _  _loc  ->
       let e = make_automata true l in
       (`Constraint
          (_loc, e,
            (`Arrow
               (_loc,
                 (`Dot
                    (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lexbuf")))),
                 (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))))) : 
         FAst.exp ))
let _ =
  Gramf.extend_single (case : 'case Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (regexp : 'regexp Gramf.t ));
              Token
                ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
                Tokenf.pattern )];
            annot = "[(r, (Parsef.expand_exp x))]\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_1 : Tokenf.quot)  (r : 'regexp)  (_loc : Locf.t)
                     ->
                    let x = __fan_1 in
                    ([(r, (Parsef.expand_exp x))] : 'case ) : Tokenf.quot ->
                                                                'regexp ->
                                                                  Locf.t ->
                                                                    'case ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres None y xloc\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = None in
                   (let res =
                      try Hashtbl.find named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res None y xloc : 'case ) : Tokenf.txt ->
                                                  Tokenf.txt ->
                                                    Locf.t -> 'case ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres None y xloc\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.quot)  (__fan_1 : Tokenf.txt)  _ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = __fan_2 in
                   let y = Some y in
                   (let res =
                      try Hashtbl.find named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res None y xloc : 'case ) : Tokenf.quot ->
                                                  Tokenf.txt ->
                                                    Tokenf.txt ->
                                                      Locf.t -> 'case ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             List1sep
               ((Token
                   ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                   Tokenf.pattern )),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres (Some l) y xloc\n";
           fn =
             (Gramf.mk_action
                (fun _  (l : Tokenf.txt list)  _  (__fan_1 : Tokenf.txt)  _ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = None in
                   (let res =
                      try Hashtbl.find named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res (Some l) y xloc : 'case ) : Tokenf.txt ->
                                                      Tokenf.txt list ->
                                                        Tokenf.txt ->
                                                          Tokenf.txt ->
                                                            Tokenf.txt ->
                                                              Locf.t -> 'case ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             List1sep
               ((Token
                   ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                   Tokenf.pattern )),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres (Some l) y xloc\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_5 : Tokenf.quot)  _  (l : Tokenf.txt list)  _ 
                   (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = __fan_5 in
                   let y = Some y in
                   (let res =
                      try Hashtbl.find named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res (Some l) y xloc : 'case ) : Tokenf.quot ->
                                                      Tokenf.txt ->
                                                        Tokenf.txt list ->
                                                          Tokenf.txt ->
                                                            Tokenf.txt ->
                                                              Tokenf.txt ->
                                                                Locf.t ->
                                                                  'case ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (declare_regexp : 'declare_regexp Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Key; word = (A "let"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
                Tokenf.pattern );
              Nterm (Gramf.obj (regexp : 'regexp Gramf.t ))];
            annot =
              "if Hashtbl.mem named_regexps x\nthen\n  (Fan_warnings.emitf xloc.loc_start\n     \"fanlex (warning): multiple definition of named regexp '%s'\n\" x;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru ))\nelse\n  (Hashtbl.add named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : FAst.stru ))\n";
            fn =
              (Gramf.mk_action
                 (fun (r : 'regexp)  _  (__fan_1 : Tokenf.txt)  _ 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (if Hashtbl.mem named_regexps x
                     then
                       (Fan_warnings.emitf xloc.loc_start
                          "fanlex (warning): multiple definition of named regexp '%s'\n"
                          x;
                        (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru ))
                     else
                       (Hashtbl.add named_regexps x r;
                        (`StExp (_loc, (`Uid (_loc, "()"))) : FAst.stru )) : 
                      'declare_regexp ) : 'regexp ->
                                            Tokenf.txt ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Locf.t -> 'declare_regexp ))
          };
         {
           symbols = [Self; Self];
           annot = "x\n";
           fn =
             (Gramf.mk_action
                (fun (x : 'declare_regexp)  _  (_loc : Locf.t)  ->
                   (x : 'declare_regexp ) : 'declare_regexp ->
                                              'declare_regexp ->
                                                Locf.t -> 'declare_regexp ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 10);
       lassoc = true;
       productions =
         [{
            symbols =
              [Self;
              Token
                ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" }
                 } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern )];
            annot = "Bind (r1, (xloc, y))\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_2 : Tokenf.txt)  _  (r1 : 'regexp) 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_2.loc in
                    let y = __fan_2.txt in (Bind (r1, (xloc, y)) : 'regexp ) : 
                 Tokenf.txt -> Tokenf.txt -> 'regexp -> Locf.t -> 'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 20);
       lassoc = true;
       productions =
         [{
            symbols =
              [Self;
              Token
                ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot =
              "let s1 = as_cset r1 in let s2 = as_cset r2 in Characters (Fcset.diff s1 s2)\n";
            fn =
              (Gramf.mk_action
                 (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (let s1 = as_cset r1 in
                     let s2 = as_cset r2 in Characters (Fcset.diff s1 s2) : 
                    'regexp ) : 'regexp ->
                                  Tokenf.txt -> 'regexp -> Locf.t -> 'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 30);
       lassoc = true;
       productions =
         [{
            symbols =
              [Self;
              Token
                ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot = "Alternative (r1, r2)\n";
            fn =
              (Gramf.mk_action
                 (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Alternative (r1, r2) : 'regexp ) : 'regexp ->
                                                          Tokenf.txt ->
                                                            'regexp ->
                                                              Locf.t ->
                                                                'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 40);
       lassoc = true;
       productions =
         [{
            symbols = [Self; Self];
            annot = "Sequence (r1, r2)\n";
            fn =
              (Gramf.mk_action
                 (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Sequence (r1, r2) : 'regexp ) : 'regexp ->
                                                       'regexp ->
                                                         Locf.t -> 'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 50);
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" }
                  } : Tokenf.pattern )];
            annot = "Characters Fcset.all_chars\n";
            fn =
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  ->
                    (Characters Fcset.all_chars : 'regexp ) : Tokenf.txt ->
                                                                Locf.t ->
                                                                  'regexp ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                Tokenf.pattern )];
           annot =
             "Characters (Fcset.singleton (Char.code @@ (TokenEval.char c)))\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let c = __fan_0.txt in
                   (Characters
                      (Fcset.singleton (Char.code @@ (TokenEval.char c))) : 
                     'regexp ) : Tokenf.txt -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
           annot = "regexp_for_string @@ (TokenEval.string s)\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   (regexp_for_string @@ (TokenEval.string s) : 'regexp ) : 
                Tokenf.txt -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
                Tokenf.pattern );
             Nterm (Gramf.obj (char_class : 'char_class Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Characters cc\n";
           fn =
             (Gramf.mk_action
                (fun _  (cc : 'char_class)  _  (_loc : Locf.t)  ->
                   (Characters cc : 'regexp ) : Tokenf.txt ->
                                                  'char_class ->
                                                    Tokenf.txt ->
                                                      Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Self;
             Token
               ({ descr = { tag = `Key; word = (A "*"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Repetition r1\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Repetition r1 : 'regexp ) : Tokenf.txt ->
                                                  'regexp ->
                                                    Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Self;
             Token
               ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Alternative (Epsilon, r1)\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Alternative (Epsilon, r1) : 'regexp ) : Tokenf.txt ->
                                                              'regexp ->
                                                                Locf.t ->
                                                                  'regexp ))
         };
         {
           symbols =
             [Self;
             Token
               ({ descr = { tag = `Key; word = (A "+"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Sequence ((Repetition (remove_as r1)), r1)\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Sequence ((Repetition (remove_as r1)), r1) : 'regexp ) : 
                Tokenf.txt -> 'regexp -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
                Tokenf.pattern );
             Self;
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "r1\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  _  (_loc : Locf.t)  ->
                   (r1 : 'regexp ) : Tokenf.txt ->
                                       'regexp ->
                                         Tokenf.txt -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "eof"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot = "Eof\n";
           fn =
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  -> (Eof : 'regexp ) : Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern )];
           annot =
             "try Hashtbl.find named_regexps x\nwith\n| Not_found  ->\n    (Fan_warnings.emitf xloc.loc_start\n       \"Reference to unbound regexp name `%s'\" x;\n     raise UnboundRegexp)\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let xloc = __fan_0.loc in
                   let x = __fan_0.txt in
                   (try Hashtbl.find named_regexps x
                    with
                    | Not_found  ->
                        (Fan_warnings.emitf xloc.loc_start
                           "Reference to unbound regexp name `%s'" x;
                         raise UnboundRegexp) : 'regexp ) : Tokenf.txt ->
                                                              Locf.t ->
                                                                'regexp ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (char_class : 'char_class Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "^"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Nterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))];
            annot = "Fcset.complement r\n";
            fn =
              (Gramf.mk_action
                 (fun (r : 'char_class1)  _  (_loc : Locf.t)  ->
                    (Fcset.complement r : 'char_class ) : 'char_class1 ->
                                                            Tokenf.txt ->
                                                              Locf.t ->
                                                                'char_class ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))];
           annot = "r\n";
           fn =
             (Gramf.mk_action
                (fun (r : 'char_class1)  (_loc : Locf.t)  ->
                   (r : 'char_class ) : 'char_class1 -> Locf.t -> 'char_class ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (char_class1 : 'char_class1 Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Token
                ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                Tokenf.pattern )];
            annot =
              "let c1 = Char.code @@ (TokenEval.char c1) in\nlet c2 = Char.code @@ (TokenEval.char c2) in Fcset.interval c1 c2\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let c1 = __fan_0.txt in
                    let c2 = __fan_2.txt in
                    (let c1 = Char.code @@ (TokenEval.char c1) in
                     let c2 = Char.code @@ (TokenEval.char c2) in
                     Fcset.interval c1 c2 : 'char_class1 ) : Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'char_class1 ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                Tokenf.pattern )];
           annot = "Fcset.singleton (Char.code @@ (TokenEval.char c1))\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let c1 = __fan_0.txt in
                   (Fcset.singleton (Char.code @@ (TokenEval.char c1)) : 
                     'char_class1 ) : Tokenf.txt -> Locf.t -> 'char_class1 ))
         };
         {
           symbols = [Self; Self];
           annot = "Fcset.union cc1 cc2\n";
           fn =
             (Gramf.mk_action
                (fun (cc2 : 'char_class1)  (cc1 : 'char_class1) 
                   (_loc : Locf.t)  -> (Fcset.union cc1 cc2 : 'char_class1 ) : 
                'char_class1 -> 'char_class1 -> Locf.t -> 'char_class1 ))
         }]
     } : Gramf.olevel )
let () =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream ~name:(d, "lex") ~entry:lex
    ();
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream ~name:(d, "lex_fan")
    ~entry:lex_fan ();
  Ast_quotation.of_stru ~lexer:Lex_lex.from_stream ~name:(d, "regex")
    ~entry:declare_regexp ();
  Ast_quotation.add_quotation ~lexer:Lex_lex.from_stream (d, "re") regexp
    ~mexp:meta_concrete_regexp ~mpat:meta_concrete_regexp
    ~exp_filter:(fun x  -> (x : FAst.ep  :>FAst.exp))
    ~pat_filter:(fun x  -> (x : FAst.ep  :>FAst.pat))
