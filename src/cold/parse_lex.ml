let as_cset = Translate_lex.as_cset
let regexp_for_string = Translate_lex.regexp_for_string
let remove_as = Translate_lex.remove_as
let named_regexps: (string,Translate_lex.concrete_regexp) Hashtbl.t =
  Hashtbl.create 13
let named_cases:
  (string,(Translate_lex.concrete_regexp* FAst.exp) list) Hashtbl.t =
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
                   (Repetition (Characters [(48, 57); (95, 95)]))))))))
let _ =
  let _loc = Locf.ghost in
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
                      pos_lnum = 51;
                      pos_bol = 1693;
                      pos_cnum = 1728
                    };
                  loc_end =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 51;
                      pos_bol = 1693;
                      pos_cnum = 1731
                    };
                  loc_ghost = false
                } : Locf.t ), "txt"))),
          (`App
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
                                           (_loc, (`Lid (_loc, "loc_start")),
                                             (`Field
                                                (_loc,
                                                  (`Lid (_loc, "lexbuf")),
                                                  (`Lid (_loc, "lex_start_p")))))),
                                        (`Sem
                                           (_loc,
                                             (`RecBind
                                                (_loc,
                                                  (`Lid (_loc, "loc_end")),
                                                  (`Field
                                                     (_loc,
                                                       (`Lid (_loc, "lexbuf")),
                                                       (`Lid
                                                          (_loc,
                                                            "lex_curr_p")))))),
                                             (`RecBind
                                                (_loc,
                                                  (`Lid (_loc, "loc_ghost")),
                                                  (`Lid (_loc, "false")))))))))))),
                         (`RecBind
                            (_loc, (`Lid (_loc, "txt")),
                              (`Lid (_loc, "txt"))))))))) : FAst.exp ))]);
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
                                   pos_lnum = 59;
                                   pos_bol = 1884;
                                   pos_cnum = 1923
                                 };
                               loc_end =
                                 {
                                   pos_fname = "parse_lex.ml";
                                   pos_lnum = 59;
                                   pos_bol = 1884;
                                   pos_cnum = 1924
                                 };
                               loc_ghost = false
                             } : Locf.t ), "s"))))))),
             (({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 59;
                     pos_bol = 1884;
                     pos_cnum = 1933
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 59;
                     pos_bol = 1884;
                     pos_cnum = 1936
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
                           (_loc, (`Uid (_loc, "Locf")), (`Lid (_loc, "t")))))),
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
                                               (_loc, (`Lid (_loc, "loc")),
                                                 (`Lid (_loc, "loc")))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "txt")),
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
                                          (_loc, (`Vrn (_loc, "Nativeint")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "loc")),
                                                           (`Lid
                                                              (_loc, "loc")))),
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "txt")),
                                                           (`Lid
                                                              (_loc, "txt")))))))))))),
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
                                                              (_loc, "loc")),
                                                           (`Lid
                                                              (_loc, "loc")))),
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "txt")),
                                                           (`Lid
                                                              (_loc, "txt"))))))))))))))))))))) : 
         FAst.exp ))]);
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
                              pos_lnum = 73;
                              pos_bol = 2233;
                              pos_cnum = 2256
                            };
                          loc_end =
                            {
                              pos_fname = "parse_lex.ml";
                              pos_lnum = 73;
                              pos_bol = 2233;
                              pos_cnum = 2259
                            };
                          loc_ghost = false
                        } : Locf.t ), "txt"))))), (Characters [(39, 39)]))),
         (`Seq
            (_loc,
              (`Sem
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`Dot
                                (_loc, (`Uid (_loc, "Lexing_util")),
                                  (`Lid (_loc, "update_loc")))),
                             (`Lid (_loc, "lexbuf")))),
                        (`Label
                           (_loc, (`Lid (_loc, "retract")),
                             (`Int (_loc, "1")))))),
                   (`App
                      (_loc, (`Vrn (_loc, "Chr")),
                        (`Record
                           (_loc,
                             (`Sem
                                (_loc,
                                  (`RecBind
                                     (_loc, (`Lid (_loc, "loc")),
                                       (`App
                                          (_loc, (`Lid (_loc, "!!")),
                                            (`Lid (_loc, "lexbuf")))))),
                                  (`RecBind
                                     (_loc, (`Lid (_loc, "txt")),
                                       (`Lid (_loc, "txt"))))))))))))) : 
         FAst.exp ));
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
                             pos_lnum = 79;
                             pos_bol = 2398;
                             pos_cnum = 2426
                           };
                         loc_end =
                           {
                             pos_fname = "parse_lex.ml";
                             pos_lnum = 79;
                             pos_bol = 2398;
                             pos_cnum = 2429
                           };
                         loc_ghost = false
                       } : Locf.t ), "txt"))))), (Characters [(39, 39)]))),
        (`App
           (_loc, (`Vrn (_loc, "Chr")),
             (`Record
                (_loc,
                  (`Sem
                     (_loc,
                       (`RecBind
                          (_loc, (`Lid (_loc, "loc")),
                            (`App
                               (_loc, (`Lid (_loc, "!!")),
                                 (`Lid (_loc, "lexbuf")))))),
                       (`RecBind
                          (_loc, (`Lid (_loc, "txt")), (`Lid (_loc, "txt"))))))))) : 
        FAst.exp ));
      ((Sequence
          ((Sequence ((Characters [(39, 39)]), (Characters [(92, 92)]))),
            (Bind
               ((Characters [(0, 255)]),
                 (({
                     loc_start =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 81;
                         pos_bol = 2483;
                         pos_cnum = 2504
                       };
                     loc_end =
                       {
                         pos_fname = "parse_lex.ml";
                         pos_lnum = 81;
                         pos_bol = 2483;
                         pos_cnum = 2505
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
                          (_loc, (`Uid (_loc, "Illegal_escape")),
                            (`App
                               (_loc,
                                 (`App
                                    (_loc,
                                      (`Dot
                                         (_loc, (`Uid (_loc, "String")),
                                           (`Lid (_loc, "make")))),
                                      (`Int (_loc, "1")))),
                                 (`Lid (_loc, "c")))))))))),
             (`App (_loc, (`Lid (_loc, "!!")), (`Lid (_loc, "lexbuf"))))) : 
        FAst.exp ))]);
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
                     pos_lnum = 85;
                     pos_bol = 2614;
                     pos_cnum = 2641
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 85;
                     pos_bol = 2614;
                     pos_cnum = 2644
                   };
                 loc_ghost = false
               } : Locf.t ), "txt"))),
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
                                          (_loc, (`Lid (_loc, "loc_start")),
                                            (`Field
                                               (_loc,
                                                 (`Lid (_loc, "lexbuf")),
                                                 (`Lid (_loc, "lex_start_p")))))),
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc,
                                                 (`Lid (_loc, "loc_end")),
                                                 (`Field
                                                    (_loc,
                                                      (`Lid (_loc, "lexbuf")),
                                                      (`Lid
                                                         (_loc, "lex_curr_p")))))),
                                            (`RecBind
                                               (_loc,
                                                 (`Lid (_loc, "loc_ghost")),
                                                 (`Lid (_loc, "false")))))))))))),
                        (`RecBind
                           (_loc, (`Lid (_loc, "txt")), (`Lid (_loc, "txt"))))))))) : 
         FAst.exp ))]);
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
                               pos_lnum = 94;
                               pos_bol = 2793;
                               pos_cnum = 2818
                             };
                           loc_end =
                             {
                               pos_fname = "parse_lex.ml";
                               pos_lnum = 94;
                               pos_bol = 2793;
                               pos_cnum = 2819
                             };
                           loc_ghost = false
                         } : Locf.t ), "x"))))))),
         (`LetIn
            (_loc, (`Negative _loc),
              (`Bind
                 (_loc, (`Lid (_loc, "c")),
                   (`App
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, "Lexing_util")),
                             (`Lid (_loc, "new_cxt")))), (`Uid (_loc, "()")))))),
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
                                            (`Uid (_loc, "Lexing_util")),
                                            (`Lid (_loc, "warn")))),
                                       (`Uid (_loc, "Comment_start")))),
                                  (`App
                                     (_loc, (`Lid (_loc, "!!")),
                                       (`Lid (_loc, "lexbuf")))))))),
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
                                       (`Lid (_loc, "lex_comment")))),
                                  (`App
                                     (_loc, (`Lid (_loc, "ignore")),
                                       (`App
                                          (_loc,
                                            (`Dot
                                               (_loc,
                                                 (`Uid (_loc, "Lexing_util")),
                                                 (`Lid
                                                    (_loc, "buff_contents")))),
                                            (`Lid (_loc, "c"))))))))))))))) : 
         FAst.exp ))]);
    ("whitespace",
      [((Sequence
           ((Repetition (Characters [(9, 9); (12, 12); (32, 32)])),
             (Characters [(9, 9); (12, 12); (32, 32)]))),
         (`Uid (_loc, "()") : FAst.exp ));
      ((Alternative
          ((Alternative ((Characters [(10, 10)]), (Characters [(13, 13)]))),
            (Sequence ((Characters [(13, 13)]), (Characters [(10, 10)]))))),
        (`App (_loc, (`Lid (_loc, "update_loc")), (`Lid (_loc, "lexbuf"))) : 
        FAst.exp ))]);
    ("ocaml_string",
      [((Characters [(34, 34)]),
         (`LetIn
            (_loc, (`Negative _loc),
              (`Bind
                 (_loc, (`Lid (_loc, "c")),
                   (`App
                      (_loc,
                        (`Dot
                           (_loc, (`Uid (_loc, "Lexing_util")),
                             (`Lid (_loc, "new_cxt")))), (`Uid (_loc, "()")))))),
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
                                                 (`Uid (_loc, "Lexing_util")),
                                                 (`Lid
                                                    (_loc, "push_loc_cont")))),
                                            (`Lid (_loc, "c")))),
                                       (`Lid (_loc, "lexbuf")))),
                                  (`Lid (_loc, "lex_string")))),
                             (`LetIn
                                (_loc, (`Negative _loc),
                                  (`Bind
                                     (_loc, (`Lid (_loc, "loc")),
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Lid (_loc, "--")),
                                                 (`Lid (_loc, "old")))),
                                            (`Field
                                               (_loc,
                                                 (`Lid (_loc, "lexbuf")),
                                                 (`Lid (_loc, "lex_curr_p")))))))),
                                  (`App
                                     (_loc, (`Vrn (_loc, "Str")),
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
                                                      (`App
                                                         (_loc,
                                                           (`Lid
                                                              (_loc,
                                                                "buff_contents")),
                                                           (`Lid (_loc, "c"))))))))))))))))))))) : 
         FAst.exp ))]);
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
                                          pos_lnum = 154;
                                          pos_bol = 4628;
                                          pos_cnum = 4668
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 154;
                                          pos_bol = 4628;
                                          pos_cnum = 4671
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
                                                pos_lnum = 155;
                                                pos_bol = 4685;
                                                pos_cnum = 4727
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "parse_lex.ml";
                                                pos_lnum = 155;
                                                pos_bol = 4685;
                                                pos_cnum = 4731
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
            (_loc,
              (`Sem
                 (_loc,
                   (`App
                      (_loc,
                        (`App
                           (_loc,
                             (`App
                                (_loc,
                                  (`App
                                     (_loc, (`Lid (_loc, "update_loc")),
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
                             (`Lid (_loc, "true")))))),
                   (`App
                      (_loc, (`Lid (_loc, "token")), (`Lid (_loc, "lexbuf"))))))) : 
         FAst.exp ))])]
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
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nmatch y with\n| None  -> res\n| Some y ->\n    let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\n    let e = Tokenf.quot_expand expander y in\n    List.map\n      (fun (x,v)  -> (x, (`Seq (_loc, (`Sem (_loc, v, e))) : FAst.exp ))) res\n";
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
                    | None  -> res
                    | Some y ->
                        let expander loc _ s =
                          Gramf.parse_string ~loc Syntaxf.exp s in
                        let e = Tokenf.quot_expand expander y in
                        List.map
                          (fun (x,v)  ->
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
             "let res =\n  try Hashtbl.find named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nmatch y with\n| None  -> res\n| Some y ->\n    let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\n    let e = Tokenf.quot_expand expander y in\n    List.map\n      (fun (x,v)  -> (x, (`Seq (_loc, (`Sem (_loc, v, e))) : FAst.exp ))) res\n";
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
                    | None  -> res
                    | Some y ->
                        let expander loc _ s =
                          Gramf.parse_string ~loc Syntaxf.exp s in
                        let e = Tokenf.quot_expand expander y in
                        List.map
                          (fun (x,v)  ->
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
