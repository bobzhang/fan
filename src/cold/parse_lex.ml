let as_cset = Translate_lex.as_cset
let regexp_for_string = Translate_lex.regexp_for_string
let remove_as = Translate_lex.remove_as
let named_regexps: (string,Translate_lex.concrete_regexp) Hashtbl.t =
  Hashtbl.create 13
let named_cases:
  (string,(Translate_lex.concrete_regexp* FAstN.exp) list) Hashtbl.t =
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
let _ =
  Hashtblf.add_list named_cases
    [("ocaml_uid",
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
                      pos_lnum = 53;
                      pos_bol = 1785;
                      pos_cnum = 1820
                    };
                  loc_end =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 53;
                      pos_bol = 1785;
                      pos_cnum = 1823
                    };
                  loc_ghost = false
                } : Locf.t ), "txt"))),
          (`App
             ((`Vrn "Uid"),
               (`Record
                  (`Sem
                     ((`RecBind
                         ((`Lid "loc"),
                           (`Record
                              (`Sem
                                 ((`RecBind
                                     ((`Lid "loc_start"),
                                       (`Field
                                          ((`Lid "lexbuf"),
                                            (`Lid "lex_start_p"))))),
                                   (`Sem
                                      ((`RecBind
                                          ((`Lid "loc_end"),
                                            (`Field
                                               ((`Lid "lexbuf"),
                                                 (`Lid "lex_curr_p"))))),
                                        (`RecBind
                                           ((`Lid "loc_ghost"),
                                             (`Lid "false")))))))))),
                       (`RecBind ((`Lid "txt"), (`Lid "txt"))))))) : 
          FAstN.exp ))]);
    ("ocaml_lid",
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
                     pos_lnum = 60;
                     pos_bol = 1951;
                     pos_cnum = 1986
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 60;
                     pos_bol = 1951;
                     pos_cnum = 1989
                   };
                 loc_ghost = false
               } : Locf.t ), "txt"))),
         (`App
            ((`Vrn "Lid"),
              (`Record
                 (`Sem
                    ((`RecBind
                        ((`Lid "loc"),
                          (`Record
                             (`Sem
                                ((`RecBind
                                    ((`Lid "loc_start"),
                                      (`Field
                                         ((`Lid "lexbuf"),
                                           (`Lid "lex_start_p"))))),
                                  (`Sem
                                     ((`RecBind
                                         ((`Lid "loc_end"),
                                           (`Field
                                              ((`Lid "lexbuf"),
                                                (`Lid "lex_curr_p"))))),
                                       (`RecBind
                                          ((`Lid "loc_ghost"),
                                            (`Lid "false")))))))))),
                      (`RecBind ((`Lid "txt"), (`Lid "txt"))))))) : FAstN.exp ))]);
    ("ocaml_int_literal",
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
                                   pos_lnum = 70;
                                   pos_bol = 2145;
                                   pos_cnum = 2184
                                 };
                               loc_end =
                                 {
                                   pos_fname = "parse_lex.ml";
                                   pos_lnum = 70;
                                   pos_bol = 2145;
                                   pos_cnum = 2185
                                 };
                               loc_ghost = false
                             } : Locf.t ), "s"))))))),
             (({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 70;
                     pos_bol = 2145;
                     pos_cnum = 2194
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 70;
                     pos_bol = 2145;
                     pos_cnum = 2197
                   };
                 loc_ghost = false
               } : Locf.t ), "txt"))),
         (`LetIn
            (`Negative,
              (`Bind
                 ((`Constraint
                     ((`Lid "loc"), (`Dot ((`Uid "Locf"), (`Lid "t"))))),
                   (`Record
                      (`Sem
                         ((`RecBind
                             ((`Lid "loc_start"),
                               (`Field
                                  ((`Lid "lexbuf"), (`Lid "lex_start_p"))))),
                           (`Sem
                              ((`RecBind
                                  ((`Lid "loc_end"),
                                    (`Field
                                       ((`Lid "lexbuf"), (`Lid "lex_curr_p"))))),
                                (`RecBind
                                   ((`Lid "loc_ghost"), (`Lid "false")))))))))),
              (`Match
                 ((`Lid "s"),
                   (`Bar
                      ((`Case
                          ((`App ((`Uid "Some"), (`Chr "l"))),
                            (`App
                               ((`Vrn "Int32"),
                                 (`Record
                                    (`Sem
                                       ((`RecBind
                                           ((`Lid "loc"), (`Lid "loc"))),
                                         (`RecBind
                                            ((`Lid "txt"), (`Lid "txt")))))))))),
                        (`Bar
                           ((`Case
                               ((`App ((`Uid "Some"), (`Chr "L"))),
                                 (`App
                                    ((`Vrn "Int64"),
                                      (`Record
                                         (`Sem
                                            ((`RecBind
                                                ((`Lid "loc"), (`Lid "loc"))),
                                              (`RecBind
                                                 ((`Lid "txt"), (`Lid "txt")))))))))),
                             (`Bar
                                ((`Case
                                    ((`App ((`Uid "Some"), (`Chr "n"))),
                                      (`App
                                         ((`Vrn "Nativeint"),
                                           (`Record
                                              (`Sem
                                                 ((`RecBind
                                                     ((`Lid "loc"),
                                                       (`Lid "loc"))),
                                                   (`RecBind
                                                      ((`Lid "txt"),
                                                        (`Lid "txt")))))))))),
                                  (`Case
                                     (`Any,
                                       (`App
                                          ((`Vrn "Int"),
                                            (`Record
                                               (`Sem
                                                  ((`RecBind
                                                      ((`Lid "loc"),
                                                        (`Lid "loc"))),
                                                    (`RecBind
                                                       ((`Lid "txt"),
                                                         (`Lid "txt"))))))))))))))))))) : 
         FAstN.exp ))]);
    ("ocaml_char",
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
                              pos_lnum = 84;
                              pos_bol = 2495;
                              pos_cnum = 2518
                            };
                          loc_end =
                            {
                              pos_fname = "parse_lex.ml";
                              pos_lnum = 84;
                              pos_bol = 2495;
                              pos_cnum = 2521
                            };
                          loc_ghost = false
                        } : Locf.t ), "txt"))))), (Characters [(39, 39)]))),
         (`Seq
            (`Sem
               ((`App
                   ((`App
                       ((`Dot ((`Uid "Lexing_util"), (`Lid "update_loc"))),
                         (`Lid "lexbuf"))),
                     (`Label ((`Lid "retract"), (`Int "1"))))),
                 (`App
                    ((`Vrn "Chr"),
                      (`Record
                         (`Sem
                            ((`RecBind
                                ((`Lid "loc"),
                                  (`App ((`Lid "!!"), (`Lid "lexbuf"))))),
                              (`RecBind ((`Lid "txt"), (`Lid "txt")))))))))) : 
         FAstN.exp ));
      ((Sequence
          ((Sequence
              ((Characters [(39, 39)]),
                (Bind
                   ((Alternative
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
                                            (Characters
                                               [(48, 57);
                                               (65, 70);
                                               (97, 102)]))),
                                        (Characters
                                           [(48, 57); (65, 70); (97, 102)]))))))))),
                     (({
                         loc_start =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 90;
                             pos_bol = 2661;
                             pos_cnum = 2689
                           };
                         loc_end =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 90;
                             pos_bol = 2661;
                             pos_cnum = 2692
                           };
                         loc_ghost = false
                       } : Locf.t ), "txt"))))), (Characters [(39, 39)]))),
        (`App
           ((`Vrn "Chr"),
             (`Record
                (`Sem
                   ((`RecBind
                       ((`Lid "loc"), (`App ((`Lid "!!"), (`Lid "lexbuf"))))),
                     (`RecBind ((`Lid "txt"), (`Lid "txt"))))))) : FAstN.exp ));
      ((Sequence
          ((Sequence ((Characters [(39, 39)]), (Characters [(92, 92)]))),
            (Bind
               ((Characters [(0, 255)]),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 92;
                         pos_bol = 2747;
                         pos_cnum = 2768
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 92;
                         pos_bol = 2747;
                         pos_cnum = 2769
                       };
                     loc_ghost = false
                   } : Locf.t ), "c"))))),
        (`App
           ((`App
               ((`Lid "@@"),
                 (`App
                    ((`Dot ((`Uid "Lexing_util"), (`Lid "err"))),
                      (`App
                         ((`Uid "Illegal_escape"),
                           (`App
                              ((`App
                                  ((`Dot ((`Uid "String"), (`Lid "make"))),
                                    (`Int "1"))), (`Lid "c"))))))))),
             (`App ((`Lid "!!"), (`Lid "lexbuf")))) : FAstN.exp ))]);
    ("ocaml_float_literal",
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
                               (Repetition (Characters [(48, 57); (95, 95)])))))))),
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
                     pos_lnum = 96;
                     pos_bol = 2879;
                     pos_cnum = 2906
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 96;
                     pos_bol = 2879;
                     pos_cnum = 2909
                   };
                 loc_ghost = false
               } : Locf.t ), "txt"))),
         (`App
            ((`Vrn "Flo"),
              (`Record
                 (`Sem
                    ((`RecBind
                        ((`Lid "loc"),
                          (`Record
                             (`Sem
                                ((`RecBind
                                    ((`Lid "loc_start"),
                                      (`Field
                                         ((`Lid "lexbuf"),
                                           (`Lid "lex_start_p"))))),
                                  (`Sem
                                     ((`RecBind
                                         ((`Lid "loc_end"),
                                           (`Field
                                              ((`Lid "lexbuf"),
                                                (`Lid "lex_curr_p"))))),
                                       (`RecBind
                                          ((`Lid "loc_ghost"),
                                            (`Lid "false")))))))))),
                      (`RecBind ((`Lid "txt"), (`Lid "txt"))))))) : FAstN.exp ))]);
    ("ocaml_comment",
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
                               pos_lnum = 105;
                               pos_bol = 3059;
                               pos_cnum = 3084
                             };
                           loc_end =
                             {
                               pos_fname = "parse_lex.ml";
                               pos_lnum = 105;
                               pos_bol = 3059;
                               pos_cnum = 3085
                             };
                           loc_ghost = false
                         } : Locf.t ), "x"))))))),
         (`LetIn
            (`Negative,
              (`Bind
                 ((`Lid "c"),
                   (`App
                      ((`Dot ((`Uid "Lexing_util"), (`Lid "new_cxt"))),
                        (`Uid "()"))))),
              (`Seq
                 (`Sem
                    ((`IfThen
                        ((`App
                            ((`App ((`Lid "<>"), (`Lid "x"))), (`Uid "None"))),
                          (`App
                             ((`App
                                 ((`Dot ((`Uid "Lexing_util"), (`Lid "warn"))),
                                   (`Uid "Comment_start"))),
                               (`App ((`Lid "!!"), (`Lid "lexbuf"))))))),
                      (`Sem
                         ((`App
                             ((`App
                                 ((`Dot
                                     ((`Uid "Lexing_util"), (`Lid "store"))),
                                   (`Lid "c"))), (`Lid "lexbuf"))),
                           (`Sem
                              ((`App
                                  ((`App
                                      ((`App
                                          ((`Dot
                                              ((`Uid "Lexing_util"),
                                                (`Lid "push_loc_cont"))),
                                            (`Lid "c"))), (`Lid "lexbuf"))),
                                    (`Lid "lex_comment"))),
                                (`App
                                   ((`Lid "ignore"),
                                     (`App
                                        ((`Dot
                                            ((`Uid "Lexing_util"),
                                              (`Lid "buff_contents"))),
                                          (`Lid "c"))))))))))))) : FAstN.exp ))]);
    ("whitespace",
      [((Sequence
           ((Repetition (Characters [(9, 9); (12, 12); (32, 32)])),
             (Characters [(9, 9); (12, 12); (32, 32)]))),
         (`Uid "()" : FAstN.exp ));
      ((Alternative
          ((Alternative ((Characters [(10, 10)]), (Characters [(13, 13)]))),
            (Sequence ((Characters [(13, 13)]), (Characters [(10, 10)]))))),
        (`App ((`Lid "update_loc"), (`Lid "lexbuf")) : FAstN.exp ))]);
    ("ocaml_string",
      [((Characters [(34, 34)]),
         (`LetIn
            (`Negative,
              (`Bind
                 ((`Lid "c"),
                   (`App
                      ((`Dot ((`Uid "Lexing_util"), (`Lid "new_cxt"))),
                        (`Uid "()"))))),
              (`LetIn
                 (`Negative,
                   (`Bind
                      ((`Lid "old"),
                        (`Field ((`Lid "lexbuf"), (`Lid "lex_start_p"))))),
                   (`Seq
                      (`Sem
                         ((`App
                             ((`App
                                 ((`App
                                     ((`Dot
                                         ((`Uid "Lexing_util"),
                                           (`Lid "push_loc_cont"))),
                                       (`Lid "c"))), (`Lid "lexbuf"))),
                               (`Lid "lex_string"))),
                           (`LetIn
                              (`Negative,
                                (`Bind
                                   ((`Lid "loc"),
                                     (`App
                                        ((`App ((`Lid "--"), (`Lid "old"))),
                                          (`Field
                                             ((`Lid "lexbuf"),
                                               (`Lid "lex_curr_p"))))))),
                                (`App
                                   ((`Vrn "Str"),
                                     (`Record
                                        (`Sem
                                           ((`RecBind
                                               ((`Lid "loc"), (`Lid "loc"))),
                                             (`RecBind
                                                ((`Lid "txt"),
                                                  (`App
                                                     ((`Lid "buff_contents"),
                                                       (`Lid "c")))))))))))))))))) : 
         FAstN.exp ))]);
    ("default",
      [((Bind
           ((Characters [(0, 255)]),
             (({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 138;
                     pos_bol = 3961;
                     pos_cnum = 3973
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 138;
                     pos_bol = 3961;
                     pos_cnum = 3974
                   };
                 loc_ghost = false
               } : Locf.t ), "c"))),
         (`App
            ((`App
                ((`Lid "@@"),
                  (`App
                     ((`Lid "err"),
                       (`App ((`Uid "Illegal_character"), (`Lid "c"))))))),
              (`App ((`Lid "!!"), (`Lid "lexbuf")))) : FAstN.exp ))]);
    ("ocaml_eof",
      [(Eof,
         (`LetIn
            (`Negative,
              (`Bind
                 ((`Lid "pos"),
                   (`Field ((`Lid "lexbuf"), (`Lid "lex_curr_p"))))),
              (`Seq
                 (`Sem
                    ((`Assign
                        ((`Field ((`Lid "lexbuf"), (`Lid "lex_curr_p"))),
                          (`RecordWith
                             ((`Sem
                                 ((`RecBind
                                     ((`Lid "pos_bol"),
                                       (`App
                                          ((`App
                                              ((`Lid "+"),
                                                (`Field
                                                   ((`Lid "pos"),
                                                     (`Lid "pos_bol"))))),
                                            (`Int "1"))))),
                                   (`RecBind
                                      ((`Lid "pos_cnum"),
                                        (`App
                                           ((`App
                                               ((`Lid "+"),
                                                 (`Field
                                                    ((`Lid "pos"),
                                                      (`Lid "pos_cnum"))))),
                                             (`Int "1"))))))), (`Lid "pos"))))),
                      (`LetIn
                         (`Negative,
                           (`Bind
                              ((`Lid "loc"),
                                (`App ((`Lid "!!"), (`Lid "lexbuf"))))),
                           (`App
                              ((`Vrn "EOI"),
                                (`Record
                                   (`Sem
                                      ((`RecBind ((`Lid "loc"), (`Lid "loc"))),
                                        (`RecBind ((`Lid "txt"), (`Str "")))))))))))))) : 
         FAstN.exp ))]);
    ("ocaml_simple_quotation",
      [((Sequence ((Characters [(37, 37)]), (Characters [(123, 123)]))),
         (`LetIn
            (`Negative,
              (`Bind
                 ((`Lid "old"),
                   (`Field ((`Lid "lexbuf"), (`Lid "lex_start_p"))))),
              (`LetIn
                 (`Negative,
                   (`Bind
                      ((`Lid "c"), (`App ((`Lid "new_cxt"), (`Uid "()"))))),
                   (`Seq
                      (`Sem
                         ((`App
                             ((`App ((`Lid "store"), (`Lid "c"))),
                               (`Lid "lexbuf"))),
                           (`Sem
                              ((`App
                                  ((`App
                                      ((`App
                                          ((`Lid "push_loc_cont"),
                                            (`Lid "c"))), (`Lid "lexbuf"))),
                                    (`Lid "lex_quotation"))),
                                (`LetIn
                                   (`Negative,
                                     (`Bind
                                        ((`Lid "loc"),
                                          (`App
                                             ((`App
                                                 ((`Lid "--"), (`Lid "old"))),
                                               (`Field
                                                  ((`Lid "lexbuf"),
                                                    (`Lid "lex_curr_p"))))))),
                                     (`App
                                        ((`Vrn "Quot"),
                                          (`Record
                                             (`Sem
                                                ((`RecBind
                                                    ((`Lid "name"),
                                                      (`Dot
                                                         ((`Uid "Tokenf"),
                                                           (`Lid "empty_name"))))),
                                                  (`Sem
                                                     ((`RecBind
                                                         ((`Lid "meta"),
                                                           (`Uid "None"))),
                                                       (`Sem
                                                          ((`RecBind
                                                              ((`Lid "txt"),
                                                                (`App
                                                                   ((`Lid
                                                                    "buff_contents"),
                                                                    (`Lid "c"))))),
                                                            (`Sem
                                                               ((`RecBind
                                                                   ((`Lid
                                                                    "shift"),
                                                                    (`Int "2"))),
                                                                 (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "retract"),
                                                                    (`Int "1"))),
                                                                    (`RecBind
                                                                    ((`Lid
                                                                    "loc"),
                                                                    (`Lid
                                                                    "loc")))))))))))))))))))))))))) : 
         FAstN.exp ))]);
    ("ocaml_quotation",
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
                                                    (Characters [(45, 45)])))))))),
                                   (({
                                       loc_start =
                                         {
                                           pos_fname = "parse_lex.ml";
                                           pos_lnum = 171;
                                           pos_bol = 4727;
                                           pos_cnum = 4758
                                         };
                                       loc_end =
                                         {
                                           pos_fname = "parse_lex.ml";
                                           pos_lnum = 171;
                                           pos_bol = 4727;
                                           pos_cnum = 4762
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
                                            pos_lnum = 171;
                                            pos_bol = 4727;
                                            pos_cnum = 4781
                                          };
                                        loc_end =
                                          {
                                            pos_fname = "parse_lex.ml";
                                            pos_lnum = 171;
                                            pos_bol = 4727;
                                            pos_cnum = 4785
                                          };
                                        loc_ghost = false
                                      } : Locf.t ), "meta"))))))))),
                 (Characters [(123, 123)]))),
             (({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 171;
                     pos_bol = 4727;
                     pos_cnum = 4799
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 171;
                     pos_bol = 4727;
                     pos_cnum = 4804
                   };
                 loc_ghost = false
               } : Locf.t ), "shift"))),
         (`LetIn
            (`Negative,
              (`Bind ((`Lid "c"), (`App ((`Lid "new_cxt"), (`Uid "()"))))),
              (`LetIn
                 (`Negative,
                   (`Bind
                      ((`Lid "name"),
                        (`Match
                           ((`Lid "name"),
                             (`Bar
                                ((`Case
                                    ((`App ((`Uid "Some"), (`Lid "name"))),
                                      (`App
                                         ((`Dot
                                             ((`Uid "Tokenf"),
                                               (`Lid "name_of_string"))),
                                           (`Lid "name"))))),
                                  (`Case
                                     ((`Uid "None"),
                                       (`Dot
                                          ((`Uid "Tokenf"),
                                            (`Lid "empty_name"))))))))))),
                   (`Seq
                      (`LetIn
                         (`Negative,
                           (`Bind
                              ((`Lid "old"),
                                (`Field
                                   ((`Lid "lexbuf"), (`Lid "lex_start_p"))))),
                           (`LetIn
                              (`Negative,
                                (`Bind
                                   ((`Lid "txt"),
                                     (`Seq
                                        (`Sem
                                           ((`App
                                               ((`App
                                                   ((`Lid "store"),
                                                     (`Lid "c"))),
                                                 (`Lid "lexbuf"))),
                                             (`Sem
                                                ((`App
                                                    ((`App
                                                        ((`App
                                                            ((`Lid
                                                                "push_loc_cont"),
                                                              (`Lid "c"))),
                                                          (`Lid "lexbuf"))),
                                                      (`Lid "lex_quotation"))),
                                                  (`App
                                                     ((`Lid "buff_contents"),
                                                       (`Lid "c")))))))))),
                                (`LetIn
                                   (`Negative,
                                     (`Bind
                                        ((`Lid "loc"),
                                          (`App
                                             ((`App
                                                 ((`Lid "--"), (`Lid "old"))),
                                               (`Field
                                                  ((`Lid "lexbuf"),
                                                    (`Lid "lex_curr_p"))))))),
                                     (`LetIn
                                        (`Negative,
                                          (`Bind
                                             ((`Lid "shift"),
                                               (`App
                                                  ((`Dot
                                                      ((`Uid "String"),
                                                        (`Lid "length"))),
                                                    (`Lid "shift"))))),
                                          (`LetIn
                                             (`Negative,
                                               (`Bind
                                                  ((`Lid "retract"),
                                                    (`Int "1"))),
                                               (`App
                                                  ((`Vrn "Quot"),
                                                    (`Record
                                                       (`Sem
                                                          ((`RecBind
                                                              ((`Dot
                                                                  ((`Uid
                                                                    "Tokenf"),
                                                                    (
                                                                    `Lid
                                                                    "name"))),
                                                                (`Lid "name"))),
                                                            (`Sem
                                                               ((`RecBind
                                                                   ((`Lid
                                                                    "meta"),
                                                                    (`Lid
                                                                    "meta"))),
                                                                 (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "shift"),
                                                                    (`Lid
                                                                    "shift"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "txt"),
                                                                    (`Lid
                                                                    "txt"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "loc"),
                                                                    (`Lid
                                                                    "loc"))),
                                                                    (`RecBind
                                                                    ((`Lid
                                                                    "retract"),
                                                                    (`Lid
                                                                    "retract")))))))))))))))))))))))))))))) : 
         FAstN.exp ))]);
    ("ocaml_double_quotation",
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
                                              pos_lnum = 193;
                                              pos_bol = 5397;
                                              pos_cnum = 5417
                                            };
                                          loc_end =
                                            {
                                              pos_fname = "parse_lex.ml";
                                              pos_lnum = 193;
                                              pos_bol = 5397;
                                              pos_cnum = 5418
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
                                                    (Characters [(45, 45)])))))))),
                                   (({
                                       loc_start =
                                         {
                                           pos_fname = "parse_lex.ml";
                                           pos_lnum = 193;
                                           pos_bol = 5397;
                                           pos_cnum = 5446
                                         };
                                       loc_end =
                                         {
                                           pos_fname = "parse_lex.ml";
                                           pos_lnum = 193;
                                           pos_bol = 5397;
                                           pos_cnum = 5450
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
                                            pos_lnum = 193;
                                            pos_bol = 5397;
                                            pos_cnum = 5469
                                          };
                                        loc_end =
                                          {
                                            pos_fname = "parse_lex.ml";
                                            pos_lnum = 193;
                                            pos_bol = 5397;
                                            pos_cnum = 5473
                                          };
                                        loc_ghost = false
                                      } : Locf.t ), "meta"))))))))),
                 (Characters [(123, 123)]))),
             (({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 193;
                     pos_bol = 5397;
                     pos_cnum = 5484
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 193;
                     pos_bol = 5397;
                     pos_cnum = 5489
                   };
                 loc_ghost = false
               } : Locf.t ), "shift"))),
         (`LetIn
            (`Negative,
              (`Bind ((`Lid "c"), (`App ((`Lid "new_cxt"), (`Uid "()"))))),
              (`LetIn
                 (`Negative,
                   (`Bind
                      ((`Lid "name"),
                        (`Match
                           ((`Lid "name"),
                             (`Bar
                                ((`Case
                                    ((`App ((`Uid "Some"), (`Lid "name"))),
                                      (`App
                                         ((`Dot
                                             ((`Uid "Tokenf"),
                                               (`Lid "name_of_string"))),
                                           (`Lid "name"))))),
                                  (`Case
                                     ((`Uid "None"),
                                       (`Dot
                                          ((`Uid "Tokenf"),
                                            (`Lid "empty_name"))))))))))),
                   (`Seq
                      (`LetIn
                         (`Negative,
                           (`Bind
                              ((`Lid "old"),
                                (`Field
                                   ((`Lid "lexbuf"), (`Lid "lex_start_p"))))),
                           (`LetIn
                              (`Negative,
                                (`Bind
                                   ((`Lid "txt"),
                                     (`Seq
                                        (`Sem
                                           ((`App
                                               ((`App
                                                   ((`Lid "store"),
                                                     (`Lid "c"))),
                                                 (`Lid "lexbuf"))),
                                             (`Sem
                                                ((`App
                                                    ((`App
                                                        ((`App
                                                            ((`Lid
                                                                "push_loc_cont"),
                                                              (`Lid "c"))),
                                                          (`Lid "lexbuf"))),
                                                      (`Lid "lex_quotation"))),
                                                  (`App
                                                     ((`Lid "buff_contents"),
                                                       (`Lid "c")))))))))),
                                (`LetIn
                                   (`Negative,
                                     (`Bind
                                        ((`Lid "loc"),
                                          (`App
                                             ((`App
                                                 ((`Lid "--"), (`Lid "old"))),
                                               (`Field
                                                  ((`Lid "lexbuf"),
                                                    (`Lid "lex_curr_p"))))))),
                                     (`LetIn
                                        (`Negative,
                                          (`Bind
                                             ((`Lid "shift"),
                                               (`App
                                                  ((`Dot
                                                      ((`Uid "String"),
                                                        (`Lid "length"))),
                                                    (`Lid "shift"))))),
                                          (`LetIn
                                             (`Negative,
                                               (`Bind
                                                  ((`Lid "retract"),
                                                    (`Int "1"))),
                                               (`IfThenElse
                                                  ((`App
                                                      ((`App
                                                          ((`Lid "="),
                                                            (`Lid "x"))),
                                                        (`Uid "None"))),
                                                    (`App
                                                       ((`Vrn "Quot"),
                                                         (`Record
                                                            (`Sem
                                                               ((`RecBind
                                                                   ((`Lid
                                                                    "name"),
                                                                    (`Lid
                                                                    "name"))),
                                                                 (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "meta"),
                                                                    (`Lid
                                                                    "meta"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "shift"),
                                                                    (`Lid
                                                                    "shift"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "txt"),
                                                                    (`Lid
                                                                    "txt"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "loc"),
                                                                    (`Lid
                                                                    "loc"))),
                                                                    (`RecBind
                                                                    ((`Lid
                                                                    "retract"),
                                                                    (`Lid
                                                                    "retract")))))))))))))))),
                                                    (`App
                                                       ((`Vrn "DirQuotation"),
                                                         (`Record
                                                            (`Sem
                                                               ((`RecBind
                                                                   ((`Lid
                                                                    "name"),
                                                                    (`Lid
                                                                    "name"))),
                                                                 (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "meta"),
                                                                    (`Lid
                                                                    "meta"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "shift"),
                                                                    (`Lid
                                                                    "shift"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "txt"),
                                                                    (`Lid
                                                                    "txt"))),
                                                                    (`Sem
                                                                    ((`RecBind
                                                                    ((`Lid
                                                                    "loc"),
                                                                    (`Lid
                                                                    "loc"))),
                                                                    (`RecBind
                                                                    ((`Lid
                                                                    "retract"),
                                                                    (`Lid
                                                                    "retract")))))))))))))))))))))))))))))))) : 
         FAstN.exp ))]);
    ("line_directive",
      [((Sequence
           ((Sequence
               ((Sequence
                   ((Sequence
                       ((Sequence
                           ((Sequence
                               ((Characters [(35, 35)]),
                                 (Repetition (Characters [(9, 9); (32, 32)])))),
                             (Bind
                                ((Sequence
                                    ((Repetition (Characters [(48, 57)])),
                                      (Characters [(48, 57)]))),
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 218;
                                          pos_bol = 6180;
                                          pos_cnum = 6220
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 218;
                                          pos_bol = 6180;
                                          pos_cnum = 6223
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
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 219;
                                                pos_bol = 6237;
                                                pos_cnum = 6279
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 219;
                                                pos_bol = 6237;
                                                pos_cnum = 6283
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
         (`Seq
            (`Sem
               ((`App
                   ((`App
                       ((`App
                           ((`App ((`Lid "update_loc"), (`Lid "lexbuf"))),
                             (`OptLabl ((`Lid "file"), (`Lid "name"))))),
                         (`Label
                            ((`Lid "line"),
                              (`App ((`Lid "int_of_string"), (`Lid "num"))))))),
                     (`Label ((`Lid "absolute"), (`Lid "true"))))),
                 (`App ((`Lid "token"), (`Lid "lexbuf"))))) : FAstN.exp ))]);
    ("ocaml_ant",
      [((Bind
           ((Sequence
               ((Sequence
                   ((Characters [(36, 36)]),
                     (Bind
                        ((Sequence
                            ((Characters
                                [(95, 95); (97, 122); (223, 246); (248, 255)]),
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
                                  pos_lnum = 236;
                                  pos_bol = 6765;
                                  pos_cnum = 6795
                                };
                              loc_end =
                                {
                                  pos_fname = "parse_lex.ml";
                                  pos_lnum = 236;
                                  pos_bol = 6765;
                                  pos_cnum = 6799
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
                                   pos_lnum = 236;
                                   pos_bol = 6765;
                                   pos_cnum = 6821
                                 };
                               loc_end =
                                 {
                                   pos_fname = "parse_lex.ml";
                                   pos_lnum = 236;
                                   pos_bol = 6765;
                                   pos_cnum = 6827
                                 };
                               loc_ghost = false
                             } : Locf.t ), "follow"))))))),
             (({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 236;
                     pos_bol = 6765;
                     pos_cnum = 6833
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 236;
                     pos_bol = 6765;
                     pos_cnum = 6836
                   };
                 loc_ghost = false
               } : Locf.t ), "txt"))),
         (`LetIn
            (`Negative,
              (`Bind
                 ((`Par (`Com ((`Lid "kind"), (`Lid "shift")))),
                   (`Match
                      ((`Lid "follow"),
                        (`Bar
                           ((`Case
                               ((`Uid "None"),
                                 (`Par (`Com ((`Str ""), (`Int "1")))))),
                             (`Case
                                ((`App ((`Uid "Some"), `Any)),
                                  (`Par
                                     (`Com
                                        ((`Lid "name"),
                                          (`App
                                             ((`App
                                                 ((`Lid "+"),
                                                   (`App
                                                      ((`Dot
                                                          ((`Uid "String"),
                                                            (`Lid "length"))),
                                                        (`Lid "name"))))),
                                               (`Int "2")))))))))))))),
              (`App
                 ((`Vrn "Ant"),
                   (`Record
                      (`Sem
                         ((`RecBind
                             ((`Lid "loc"),
                               (`App ((`Lid "!!"), (`Lid "lexbuf"))))),
                           (`Sem
                              ((`RecBind ((`Lid "kind"), (`Lid "kind"))),
                                (`Sem
                                   ((`RecBind ((`Lid "txt"), (`Lid "txt"))),
                                     (`Sem
                                        ((`RecBind
                                            ((`Lid "shift"), (`Lid "shift"))),
                                          (`Sem
                                             ((`RecBind
                                                 ((`Lid "retract"),
                                                   (`Int "0"))),
                                               (`RecBind
                                                  ((`Lid "cxt"),
                                                    (`Uid "None"))))))))))))))))) : 
         FAstN.exp ));
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
                                      pos_lnum = 247;
                                      pos_bol = 7101;
                                      pos_cnum = 7131
                                    };
                                  loc_end =
                                    {
                                      pos_fname = "parse_lex.ml";
                                      pos_lnum = 247;
                                      pos_bol = 7101;
                                      pos_cnum = 7135
                                    };
                                  loc_ghost = false
                                } : Locf.t ), "name"))))))),
                (Characters [(123, 123)]))),
            (({
                loc_start =
                  {
                    pos_fname = "parse_lex.ml";
                    pos_lnum = 247;
                    pos_bol = 7101;
                    pos_cnum = 7146
                  };
                loc_end =
                  {
                    pos_fname = "parse_lex.ml";
                    pos_lnum = 247;
                    pos_bol = 7101;
                    pos_cnum = 7149
                  };
                loc_ghost = false
              } : Locf.t ), "txt"))),
        (`LetIn
           (`Negative,
             (`Bind
                ((`Lid "old"),
                  (`Field ((`Lid "lexbuf"), (`Lid "lex_start_p"))))),
             (`LetIn
                (`Negative,
                  (`Bind ((`Lid "c"), (`App ((`Lid "new_cxt"), (`Uid "()"))))),
                  (`Seq
                     (`Sem
                        ((`App
                            ((`App ((`Lid "store"), (`Lid "c"))),
                              (`Lid "lexbuf"))),
                          (`Sem
                             ((`App
                                 ((`App
                                     ((`App
                                         ((`Lid "push_loc_cont"), (`Lid "c"))),
                                       (`Lid "lexbuf"))),
                                   (`Lid "lex_quotation"))),
                               (`App
                                  ((`Vrn "Ant"),
                                    (`Record
                                       (`Sem
                                          ((`RecBind
                                              ((`Lid "loc"),
                                                (`Record
                                                   (`Sem
                                                      ((`RecBind
                                                          ((`Lid "loc_start"),
                                                            (`Lid "old"))),
                                                        (`Sem
                                                           ((`RecBind
                                                               ((`Lid
                                                                   "loc_end"),
                                                                 (`Field
                                                                    ((`Lid
                                                                    "lexbuf"),
                                                                    (`Lid
                                                                    "lex_curr_p"))))),
                                                             (`RecBind
                                                                ((`Lid
                                                                    "loc_ghost"),
                                                                  (`Lid
                                                                    "false")))))))))),
                                            (`Sem
                                               ((`RecBind
                                                   ((`Lid "kind"),
                                                     (`Match
                                                        ((`Lid "name"),
                                                          (`Bar
                                                             ((`Case
                                                                 ((`App
                                                                    ((`Uid
                                                                    "Some"),
                                                                    (`Lid "n"))),
                                                                   (`Lid "n"))),
                                                               (`Case
                                                                  ((`Uid
                                                                    "None"),
                                                                    (
                                                                    `Str ""))))))))),
                                                 (`Sem
                                                    ((`RecBind
                                                        ((`Lid "txt"),
                                                          (`App
                                                             ((`Lid
                                                                 "buff_contents"),
                                                               (`Lid "c"))))),
                                                      (`Sem
                                                         ((`RecBind
                                                             ((`Lid "shift"),
                                                               (`App
                                                                  ((`Dot
                                                                    ((`Uid
                                                                    "String"),
                                                                    (`Lid
                                                                    "length"))),
                                                                    (
                                                                    `Lid
                                                                    "txt"))))),
                                                           (`Sem
                                                              ((`RecBind
                                                                  ((`Lid
                                                                    "retract"),
                                                                    (
                                                                    `Int "1"))),
                                                                (`RecBind
                                                                   ((`Lid
                                                                    "cxt"),
                                                                    (`Uid
                                                                    "None")))))))))))))))))))))))) : 
        FAstN.exp ));
      ((Sequence
          ((Characters [(36, 36)]),
            (Bind
               ((Characters [(0, 255)]),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 263;
                         pos_bol = 7628;
                         pos_cnum = 7647
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 263;
                         pos_bol = 7628;
                         pos_cnum = 7648
                       };
                     loc_ghost = false
                   } : Locf.t ), "c"))))),
        (`App
           ((`App
               ((`Lid "err"),
                 (`App ((`Uid "Illegal_character"), (`Lid "c"))))),
             (`App ((`Lid "!!"), (`Lid "lexbuf")))) : FAstN.exp ))])]
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
              "-";
              "@"] ()
let regexp = Gramf.mk_dynamic g "regexp"
let char_class = Gramf.mk_dynamic g "char_class"
let char_class1 = Gramf.mk_dynamic g "char_class1"
let lex = Gramf.mk_dynamic g "lex"
let declare_regexp = Gramf.mk_dynamic g "declare_regexp"
let lex_fan = Gramf.mk_dynamic g "lex_fan"
let make_automata shortest l =
  Compile_lex.output_entry @@
    (Lexgen.make_single_dfa { shortest; clauses = (Listf.concat l) })
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let case: 'case Gramf.t = grammar_entry_create "case" in
  Gramf.extend_single (lex : 'lex Gramf.t )
    (None,
      ((None, None,
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
            annot = "make_automata false l\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (make_automata false l : 'lex )))
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
           annot = "make_automata true l\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (make_automata true l : 'lex )))
         }]) : Gramf.olevel ));
  Gramf.extend_single (lex_fan : 'lex_fan Gramf.t )
    (None,
      ((None, None,
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
            annot =
              "let e = make_automata false l in\n(`Constraint\n   (_loc, e,\n     (`Arrow\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Lexing\")), (`Lid (_loc, \"lexbuf\")))),\n          (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"t\"))))))) : \n  FAst.exp )\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                    ->
                    (let e = make_automata false l in
                     (`Constraint
                        (_loc, e,
                          (`Arrow
                             (_loc,
                               (`Dot
                                  (_loc, (`Uid (_loc, "Lexing")),
                                    (`Lid (_loc, "lexbuf")))),
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "t"))))))) : FAst.exp ) : 
                    'lex_fan )))
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
           annot =
             "let e = make_automata true l in\n(`Constraint\n   (_loc, e,\n     (`Arrow\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Lexing\")), (`Lid (_loc, \"lexbuf\")))),\n          (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"t\"))))))) : \n  FAst.exp )\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                   ->
                   (let e = make_automata true l in
                    (`Constraint
                       (_loc, e,
                         (`Arrow
                            (_loc,
                              (`Dot
                                 (_loc, (`Uid (_loc, "Lexing")),
                                   (`Lid (_loc, "lexbuf")))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "t"))))))) : FAst.exp ) : 
                   'lex_fan )))
         }]) : Gramf.olevel ));
  Gramf.extend_single (case : 'case Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Nterm (Gramf.obj (regexp : 'regexp Gramf.t ));
              Token
                ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
                Tokenf.pattern )];
            annot =
              "let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\n[(r, (Tokenf.quot_expand expander x))]\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.quot) 
                    ~__fan_0:(r : 'regexp)  (_loc : Locf.t)  ->
                    let x = __fan_1 in
                    (let expander loc _ s =
                       Gramf.parse_string ~loc Syntaxf.exp s in
                     [(r, (Tokenf.quot_expand expander x))] : 'case )))
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
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nmatch y with\n| None  -> List.map (fun (x,v)  -> (x, (FanAstN.fill_exp xloc v))) res\n| Some y ->\n    let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\n    let e = Tokenf.quot_expand expander y in\n    List.map\n      (fun (x,v)  ->\n         let v = FanAstN.fill_exp xloc v in\n         let _loc = Ast_gen.loc_of e in\n         (x, (`Seq (_loc, (`Sem (_loc, v, e))) : FAst.exp ))) res\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
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
                    match y with
                    | None  ->
                        List.map
                          (fun (x,v)  -> (x, (FanAstN.fill_exp xloc v))) res
                    | Some y ->
                        let expander loc _ s =
                          Gramf.parse_string ~loc Syntaxf.exp s in
                        let e = Tokenf.quot_expand expander y in
                        List.map
                          (fun (x,v)  ->
                             let v = FanAstN.fill_exp xloc v in
                             let _loc = Ast_gen.loc_of e in
                             (x,
                               (`Seq (_loc, (`Sem (_loc, v, e))) : FAst.exp )))
                          res : 'case )))
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
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nmatch y with\n| None  -> List.map (fun (x,v)  -> (x, (FanAstN.fill_exp xloc v))) res\n| Some y ->\n    let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\n    let e = Tokenf.quot_expand expander y in\n    List.map\n      (fun (x,v)  ->\n         let v = FanAstN.fill_exp xloc v in\n         let _loc = Ast_gen.loc_of e in\n         (x, (`Seq (_loc, (`Sem (_loc, v, e))) : FAst.exp ))) res\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.quot) 
                   ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
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
                    match y with
                    | None  ->
                        List.map
                          (fun (x,v)  -> (x, (FanAstN.fill_exp xloc v))) res
                    | Some y ->
                        let expander loc _ s =
                          Gramf.parse_string ~loc Syntaxf.exp s in
                        let e = Tokenf.quot_expand expander y in
                        List.map
                          (fun (x,v)  ->
                             let v = FanAstN.fill_exp xloc v in
                             let _loc = Ast_gen.loc_of e in
                             (x,
                               (`Seq (_loc, (`Sem (_loc, v, e))) : FAst.exp )))
                          res : 'case )))
         }]) : Gramf.olevel ));
  Gramf.extend_single (declare_regexp : 'declare_regexp Gramf.t )
    (None,
      ((None, None,
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
                 (fun ~__fan_3:(r : 'regexp)  ~__fan_2:_ 
                    ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
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
                      'declare_regexp )))
          };
         {
           symbols = [Self; Self];
           annot = "x\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(x : 'declare_regexp)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (x : 'declare_regexp )))
         }]) : Gramf.olevel ));
  Gramf.extend (regexp : 'regexp Gramf.t )
    (None,
      ([((Some "as"), None,
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
                  (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                     ~__fan_0:(r1 : 'regexp)  (_loc : Locf.t)  ->
                     let xloc = __fan_2.loc in
                     let y = __fan_2.txt in (Bind (r1, (xloc, y)) : 'regexp )))
           }]);
       ((Some "#"), None,
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
                 (fun ~__fan_2:(r2 : 'regexp)  ~__fan_1:_ 
                    ~__fan_0:(r1 : 'regexp)  (_loc : Locf.t)  ->
                    (let s1 = as_cset r1 in
                     let s2 = as_cset r2 in Characters (Fcset.diff s1 s2) : 
                    'regexp )))
          }]);
       ((Some "|"), None,
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
                 (fun ~__fan_2:(r2 : 'regexp)  ~__fan_1:_ 
                    ~__fan_0:(r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Alternative (r1, r2) : 'regexp )))
          }]);
       ((Some "app"), None,
         [{
            symbols = [Self; Self];
            annot = "Sequence (r1, r2)\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(r2 : 'regexp)  ~__fan_0:(r1 : 'regexp) 
                    (_loc : Locf.t)  -> (Sequence (r1, r2) : 'regexp )))
          }]);
       ((Some "basic"), None,
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" }
                  } : Tokenf.pattern )];
            annot = "Characters Fcset.all_chars\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  ->
                    (Characters Fcset.all_chars : 'regexp )))
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
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let c = __fan_0.txt in
                   (Characters
                      (Fcset.singleton (Char.code @@ (TokenEval.char c))) : 
                     'regexp )))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
           annot = "regexp_for_string @@ (TokenEval.string s)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   (regexp_for_string @@ (TokenEval.string s) : 'regexp )))
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
                (fun ~__fan_2:_  ~__fan_1:(cc : 'char_class)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (Characters cc : 'regexp )))
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
                (fun ~__fan_1:_  ~__fan_0:(r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Repetition r1 : 'regexp )))
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
                (fun ~__fan_1:_  ~__fan_0:(r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Alternative (Epsilon, r1) : 'regexp )))
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
                (fun ~__fan_1:_  ~__fan_0:(r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Sequence ((Repetition (remove_as r1)), r1) : 'regexp )))
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
                (fun ~__fan_2:_  ~__fan_1:(r1 : 'regexp)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (r1 : 'regexp )))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "eof"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot = "Eof\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (Eof : 'regexp )))
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
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let xloc = __fan_0.loc in
                   let x = __fan_0.txt in
                   (try Hashtbl.find named_regexps x
                    with
                    | Not_found  ->
                        (Fan_warnings.emitf xloc.loc_start
                           "Reference to unbound regexp name `%s'" x;
                         raise UnboundRegexp) : 'regexp )))
         }])] : Gramf.olevel list ));
  Gramf.extend_single (char_class : 'char_class Gramf.t )
    (None,
      ((None, None,
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "^"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Nterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))];
            annot = "Fcset.complement r\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(r : 'char_class1)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (Fcset.complement r : 'char_class )))
          };
         {
           symbols =
             [Nterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))];
           annot = "r\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(r : 'char_class1)  (_loc : Locf.t)  ->
                   (r : 'char_class )))
         }]) : Gramf.olevel ));
  Gramf.extend_single (char_class1 : 'char_class1 Gramf.t )
    (None,
      ((None, None,
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
                 (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let c1 = __fan_0.txt in
                    let c2 = __fan_2.txt in
                    (let c1 = Char.code @@ (TokenEval.char c1) in
                     let c2 = Char.code @@ (TokenEval.char c2) in
                     Fcset.interval c1 c2 : 'char_class1 )))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                Tokenf.pattern )];
           annot = "Fcset.singleton (Char.code @@ (TokenEval.char c1))\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let c1 = __fan_0.txt in
                   (Fcset.singleton (Char.code @@ (TokenEval.char c1)) : 
                     'char_class1 )))
         };
         {
           symbols = [Self; Self];
           annot = "Fcset.union cc1 cc2\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(cc2 : 'char_class1) 
                   ~__fan_0:(cc1 : 'char_class1)  (_loc : Locf.t)  ->
                   (Fcset.union cc1 cc2 : 'char_class1 )))
         }]) : Gramf.olevel ))
let () =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream ~name:(d, "lex") ~entry:lex
    ();
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream ~name:(d, "lex_fan")
    ~entry:lex_fan ();
  Ast_quotation.of_stru ~lexer:Lex_lex.from_stream ~name:(d, "regex")
    ~entry:declare_regexp ();
  Ast_quotation.add_quotation (d, "re") regexp ~mexp:meta_concrete_regexp
    ~mpat:meta_concrete_regexp
    ~exp_filter:(fun x  -> (x : FAst.ep  :>FAst.exp))
    ~pat_filter:(fun x  -> (x : FAst.ep  :>FAst.pat))
