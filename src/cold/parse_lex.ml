let as_cset = Translate_lex.as_cset
let regexp_for_string = Translate_lex.regexp_for_string
let remove_as = Translate_lex.remove_as
let named_regexps: (string,Translate_lex.concrete_regexp) Hashtbl.t =
  Hashtbl.create 13
let named_cases: (string,(Translate_lex.concrete_regexp* FAst.exp)) Hashtbl.t
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
                   (Repetition (Characters [(48, 57); (95, 95)]))))))))
let _ =
  let (+>) = Hashtbl.add named_cases in
  "ocaml_uid" +>
    ((Bind
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
                  pos_lnum = 49;
                  pos_bol = 1667;
                  pos_cnum = 1702
                };
              loc_end =
                {
                  pos_fname = "parse_lex.ml";
                  pos_lnum = 49;
                  pos_bol = 1667;
                  pos_cnum = 1705
                };
              loc_ghost = false
            } : Locf.t ), "txt"))),
      (`App
         (({
             loc_start =
               {
                 pos_fname = "parse_lex.ml";
                 pos_lnum = 52;
                 pos_bol = 1725;
                 pos_cnum = 1727
               };
             loc_end =
               {
                 pos_fname = "parse_lex.ml";
                 pos_lnum = 55;
                 pos_bol = 1805;
                 pos_cnum = 1833
               };
             loc_ghost = false
           } : Locf.t ),
           (`Vrn
              (({
                  loc_start =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 52;
                      pos_bol = 1725;
                      pos_cnum = 1727
                    };
                  loc_end =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 52;
                      pos_bol = 1725;
                      pos_cnum = 1731
                    };
                  loc_ghost = false
                } : Locf.t ), "Uid")),
           (`Record
              (({
                  loc_start =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 52;
                      pos_bol = 1725;
                      pos_cnum = 1731
                    };
                  loc_end =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 55;
                      pos_bol = 1805;
                      pos_cnum = 1833
                    };
                  loc_ghost = false
                } : Locf.t ),
                (`Sem
                   (({
                       loc_start =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 52;
                           pos_bol = 1725;
                           pos_cnum = 1732
                         };
                       loc_end =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 55;
                           pos_bol = 1805;
                           pos_cnum = 1831
                         };
                       loc_ghost = false
                     } : Locf.t ),
                     (`RecBind
                        (({
                            loc_start =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 52;
                                pos_bol = 1725;
                                pos_cnum = 1732
                              };
                            loc_end =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 55;
                                pos_bol = 1805;
                                pos_cnum = 1825
                              };
                            loc_ghost = false
                          } : Locf.t ),
                          (`Lid
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 52;
                                     pos_bol = 1725;
                                     pos_cnum = 1732
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 52;
                                     pos_bol = 1725;
                                     pos_cnum = 1735
                                   };
                                 loc_ghost = false
                               } : Locf.t ), "loc")),
                          (`Record
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 53;
                                     pos_bol = 1738;
                                     pos_cnum = 1740
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 55;
                                     pos_bol = 1805;
                                     pos_cnum = 1825
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`Sem
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 53;
                                          pos_bol = 1738;
                                          pos_cnum = 1741
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 55;
                                          pos_bol = 1805;
                                          pos_cnum = 1824
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`RecBind
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 53;
                                               pos_bol = 1738;
                                               pos_cnum = 1741
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 53;
                                               pos_bol = 1738;
                                               pos_cnum = 1771
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`Lid
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 53;
                                                    pos_bol = 1738;
                                                    pos_cnum = 1741
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 53;
                                                    pos_bol = 1738;
                                                    pos_cnum = 1750
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "loc_start")),
                                         (`Field
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 53;
                                                    pos_bol = 1738;
                                                    pos_cnum = 1753
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 53;
                                                    pos_bol = 1738;
                                                    pos_cnum = 1771
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 53;
                                                         pos_bol = 1738;
                                                         pos_cnum = 1753
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 53;
                                                         pos_bol = 1738;
                                                         pos_cnum = 1759
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "lexbuf")),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 53;
                                                         pos_bol = 1738;
                                                         pos_cnum = 1760
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 53;
                                                         pos_bol = 1738;
                                                         pos_cnum = 1771
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ),
                                                   "lex_start_p")))))),
                                    (`Sem
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 54;
                                               pos_bol = 1773;
                                               pos_cnum = 1776
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 55;
                                               pos_bol = 1805;
                                               pos_cnum = 1824
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`RecBind
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 54;
                                                    pos_bol = 1773;
                                                    pos_cnum = 1776
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 54;
                                                    pos_bol = 1773;
                                                    pos_cnum = 1803
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 54;
                                                         pos_bol = 1773;
                                                         pos_cnum = 1776
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 54;
                                                         pos_bol = 1773;
                                                         pos_cnum = 1783
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "loc_end")),
                                              (`Field
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 54;
                                                         pos_bol = 1773;
                                                         pos_cnum = 1786
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 54;
                                                         pos_bol = 1773;
                                                         pos_cnum = 1803
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ),
                                                   (`Lid
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 54;
                                                              pos_bol = 1773;
                                                              pos_cnum = 1786
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 54;
                                                              pos_bol = 1773;
                                                              pos_cnum = 1792
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ),
                                                        "lexbuf")),
                                                   (`Lid
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 54;
                                                              pos_bol = 1773;
                                                              pos_cnum = 1793
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 54;
                                                              pos_bol = 1773;
                                                              pos_cnum = 1803
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ),
                                                        "lex_curr_p")))))),
                                         (`RecBind
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 55;
                                                    pos_bol = 1805;
                                                    pos_cnum = 1807
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 55;
                                                    pos_bol = 1805;
                                                    pos_cnum = 1824
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 55;
                                                         pos_bol = 1805;
                                                         pos_cnum = 1807
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 55;
                                                         pos_bol = 1805;
                                                         pos_cnum = 1816
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "loc_ghost")),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 55;
                                                         pos_bol = 1805;
                                                         pos_cnum = 1819
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 55;
                                                         pos_bol = 1805;
                                                         pos_cnum = 1824
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "false")))))))))))),
                     (`RecBind
                        (({
                            loc_start =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 55;
                                pos_bol = 1805;
                                pos_cnum = 1828
                              };
                            loc_end =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 55;
                                pos_bol = 1805;
                                pos_cnum = 1831
                              };
                            loc_ghost = false
                          } : Locf.t ),
                          (`Lid
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 55;
                                     pos_bol = 1805;
                                     pos_cnum = 1828
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 55;
                                     pos_bol = 1805;
                                     pos_cnum = 1831
                                   };
                                 loc_ghost = false
                               } : Locf.t ), "txt")),
                          (`Lid
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 55;
                                     pos_bol = 1805;
                                     pos_cnum = 1828
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 55;
                                     pos_bol = 1805;
                                     pos_cnum = 1831
                                   };
                                 loc_ghost = false
                               } : Locf.t ), "txt"))))))))) : FAst.exp ));
  "ocaml_int_literal" +>
    ((Bind
        ((Sequence
            ((Alternative
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
                                pos_lnum = 56;
                                pos_bol = 1838;
                                pos_cnum = 1899
                              };
                            loc_end =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 56;
                                pos_bol = 1838;
                                pos_cnum = 1900
                              };
                            loc_ghost = false
                          } : Locf.t ), "s"))))))),
          (({
              loc_start =
                {
                  pos_fname = "parse_lex.ml";
                  pos_lnum = 56;
                  pos_bol = 1838;
                  pos_cnum = 1909
                };
              loc_end =
                {
                  pos_fname = "parse_lex.ml";
                  pos_lnum = 56;
                  pos_bol = 1838;
                  pos_cnum = 1912
                };
              loc_ghost = false
            } : Locf.t ), "txt"))),
      (`LetIn
         (({
             loc_start =
               {
                 pos_fname = "parse_lex.ml";
                 pos_lnum = 58;
                 pos_bol = 1929;
                 pos_cnum = 1931
               };
             loc_end =
               {
                 pos_fname = "parse_lex.ml";
                 pos_lnum = 66;
                 pos_bol = 2151;
                 pos_cnum = 2174
               };
             loc_ghost = false
           } : Locf.t ),
           (`Negative
              ({
                 loc_start =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 58;
                     pos_bol = 1929;
                     pos_cnum = 1935
                   };
                 loc_end =
                   {
                     pos_fname = "parse_lex.ml";
                     pos_lnum = 58;
                     pos_bol = 1929;
                     pos_cnum = 1935
                   };
                 loc_ghost = false
               } : Locf.t )),
           (`Bind
              (({
                  loc_start =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 58;
                      pos_bol = 1929;
                      pos_cnum = 1935
                    };
                  loc_end =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 61;
                      pos_bol = 2009;
                      pos_cnum = 2029
                    };
                  loc_ghost = false
                } : Locf.t ),
                (`Lid
                   (({
                       loc_start =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 58;
                           pos_bol = 1929;
                           pos_cnum = 1935
                         };
                       loc_end =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 58;
                           pos_bol = 1929;
                           pos_cnum = 1938
                         };
                       loc_ghost = false
                     } : Locf.t ), "loc")),
                (`Record
                   (({
                       loc_start =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 59;
                           pos_bol = 1941;
                           pos_cnum = 1943
                         };
                       loc_end =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 61;
                           pos_bol = 2009;
                           pos_cnum = 2029
                         };
                       loc_ghost = false
                     } : Locf.t ),
                     (`Sem
                        (({
                            loc_start =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 59;
                                pos_bol = 1941;
                                pos_cnum = 1944
                              };
                            loc_end =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 61;
                                pos_bol = 2009;
                                pos_cnum = 2028
                              };
                            loc_ghost = false
                          } : Locf.t ),
                          (`RecBind
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 59;
                                     pos_bol = 1941;
                                     pos_cnum = 1944
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 59;
                                     pos_bol = 1941;
                                     pos_cnum = 1974
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`Lid
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 59;
                                          pos_bol = 1941;
                                          pos_cnum = 1944
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 59;
                                          pos_bol = 1941;
                                          pos_cnum = 1953
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "loc_start")),
                               (`Field
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 59;
                                          pos_bol = 1941;
                                          pos_cnum = 1956
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 59;
                                          pos_bol = 1941;
                                          pos_cnum = 1974
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Lid
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 59;
                                               pos_bol = 1941;
                                               pos_cnum = 1956
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 59;
                                               pos_bol = 1941;
                                               pos_cnum = 1962
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "lexbuf")),
                                    (`Lid
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 59;
                                               pos_bol = 1941;
                                               pos_cnum = 1963
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 59;
                                               pos_bol = 1941;
                                               pos_cnum = 1974
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "lex_start_p")))))),
                          (`Sem
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 60;
                                     pos_bol = 1976;
                                     pos_cnum = 1980
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 61;
                                     pos_bol = 2009;
                                     pos_cnum = 2028
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`RecBind
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 60;
                                          pos_bol = 1976;
                                          pos_cnum = 1980
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 60;
                                          pos_bol = 1976;
                                          pos_cnum = 2007
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Lid
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 60;
                                               pos_bol = 1976;
                                               pos_cnum = 1980
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 60;
                                               pos_bol = 1976;
                                               pos_cnum = 1987
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "loc_end")),
                                    (`Field
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 60;
                                               pos_bol = 1976;
                                               pos_cnum = 1990
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 60;
                                               pos_bol = 1976;
                                               pos_cnum = 2007
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`Lid
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 60;
                                                    pos_bol = 1976;
                                                    pos_cnum = 1990
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 60;
                                                    pos_bol = 1976;
                                                    pos_cnum = 1996
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "lexbuf")),
                                         (`Lid
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 60;
                                                    pos_bol = 1976;
                                                    pos_cnum = 1997
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 60;
                                                    pos_bol = 1976;
                                                    pos_cnum = 2007
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "lex_curr_p")))))),
                               (`RecBind
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 61;
                                          pos_bol = 2009;
                                          pos_cnum = 2011
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 61;
                                          pos_bol = 2009;
                                          pos_cnum = 2028
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Lid
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 61;
                                               pos_bol = 2009;
                                               pos_cnum = 2011
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 61;
                                               pos_bol = 2009;
                                               pos_cnum = 2020
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "loc_ghost")),
                                    (`Lid
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 61;
                                               pos_bol = 2009;
                                               pos_cnum = 2023
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 61;
                                               pos_bol = 2009;
                                               pos_cnum = 2028
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "false")))))))))))),
           (`Match
              (({
                  loc_start =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 62;
                      pos_bol = 2033;
                      pos_cnum = 2035
                    };
                  loc_end =
                    {
                      pos_fname = "parse_lex.ml";
                      pos_lnum = 66;
                      pos_bol = 2151;
                      pos_cnum = 2174
                    };
                  loc_ghost = false
                } : Locf.t ),
                (`Lid
                   (({
                       loc_start =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 62;
                           pos_bol = 2033;
                           pos_cnum = 2041
                         };
                       loc_end =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 62;
                           pos_bol = 2033;
                           pos_cnum = 2042
                         };
                       loc_ghost = false
                     } : Locf.t ), "s")),
                (`Bar
                   (({
                       loc_start =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 63;
                           pos_bol = 2048;
                           pos_cnum = 2052
                         };
                       loc_end =
                         {
                           pos_fname = "parse_lex.ml";
                           pos_lnum = 66;
                           pos_bol = 2151;
                           pos_cnum = 2174
                         };
                       loc_ghost = false
                     } : Locf.t ),
                     (`Case
                        (({
                            loc_start =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 63;
                                pos_bol = 2048;
                                pos_cnum = 2052
                              };
                            loc_end =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 63;
                                pos_bol = 2048;
                                pos_cnum = 2080
                              };
                            loc_ghost = false
                          } : Locf.t ),
                          (`App
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 63;
                                     pos_bol = 2048;
                                     pos_cnum = 2052
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 63;
                                     pos_bol = 2048;
                                     pos_cnum = 2060
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`Uid
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2052
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2056
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "Some")),
                               (`Chr
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2057
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2060
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "l")))),
                          (`App
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 63;
                                     pos_bol = 2048;
                                     pos_cnum = 2064
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 63;
                                     pos_bol = 2048;
                                     pos_cnum = 2080
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`Vrn
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2064
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2070
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "Int32")),
                               (`Record
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2071
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 63;
                                          pos_bol = 2048;
                                          pos_cnum = 2080
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Sem
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 63;
                                               pos_bol = 2048;
                                               pos_cnum = 2072
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 63;
                                               pos_bol = 2048;
                                               pos_cnum = 2079
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`RecBind
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 63;
                                                    pos_bol = 2048;
                                                    pos_cnum = 2072
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 63;
                                                    pos_bol = 2048;
                                                    pos_cnum = 2075
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2072
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2075
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "loc")),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2072
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2075
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "loc")))),
                                         (`RecBind
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 63;
                                                    pos_bol = 2048;
                                                    pos_cnum = 2076
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 63;
                                                    pos_bol = 2048;
                                                    pos_cnum = 2079
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2076
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2079
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "txt")),
                                              (`Lid
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2076
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 63;
                                                         pos_bol = 2048;
                                                         pos_cnum = 2079
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ), "txt")))))))))))),
                     (`Bar
                        (({
                            loc_start =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 64;
                                pos_bol = 2081;
                                pos_cnum = 2085
                              };
                            loc_end =
                              {
                                pos_fname = "parse_lex.ml";
                                pos_lnum = 66;
                                pos_bol = 2151;
                                pos_cnum = 2174
                              };
                            loc_ghost = false
                          } : Locf.t ),
                          (`Case
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 64;
                                     pos_bol = 2081;
                                     pos_cnum = 2085
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 64;
                                     pos_bol = 2081;
                                     pos_cnum = 2113
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`App
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 64;
                                          pos_bol = 2081;
                                          pos_cnum = 2085
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 64;
                                          pos_bol = 2081;
                                          pos_cnum = 2093
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Uid
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2085
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2089
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "Some")),
                                    (`Chr
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2090
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2093
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "L")))),
                               (`App
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 64;
                                          pos_bol = 2081;
                                          pos_cnum = 2097
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 64;
                                          pos_bol = 2081;
                                          pos_cnum = 2113
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Vrn
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2097
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2103
                                             };
                                           loc_ghost = false
                                         } : Locf.t ), "Int64")),
                                    (`Record
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2104
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 64;
                                               pos_bol = 2081;
                                               pos_cnum = 2113
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`Sem
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 64;
                                                    pos_bol = 2081;
                                                    pos_cnum = 2105
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 64;
                                                    pos_bol = 2081;
                                                    pos_cnum = 2112
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`RecBind
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 64;
                                                         pos_bol = 2081;
                                                         pos_cnum = 2105
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 64;
                                                         pos_bol = 2081;
                                                         pos_cnum = 2108
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ),
                                                   (`Lid
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2105
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2108
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ), "loc")),
                                                   (`Lid
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2105
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2108
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ), "loc")))),
                                              (`RecBind
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 64;
                                                         pos_bol = 2081;
                                                         pos_cnum = 2109
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 64;
                                                         pos_bol = 2081;
                                                         pos_cnum = 2112
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ),
                                                   (`Lid
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2109
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2112
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ), "txt")),
                                                   (`Lid
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2109
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 64;
                                                              pos_bol = 2081;
                                                              pos_cnum = 2112
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ), "txt")))))))))))),
                          (`Bar
                             (({
                                 loc_start =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 65;
                                     pos_bol = 2114;
                                     pos_cnum = 2118
                                   };
                                 loc_end =
                                   {
                                     pos_fname = "parse_lex.ml";
                                     pos_lnum = 66;
                                     pos_bol = 2151;
                                     pos_cnum = 2174
                                   };
                                 loc_ghost = false
                               } : Locf.t ),
                               (`Case
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 65;
                                          pos_bol = 2114;
                                          pos_cnum = 2118
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 65;
                                          pos_bol = 2114;
                                          pos_cnum = 2150
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`App
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 65;
                                               pos_bol = 2114;
                                               pos_cnum = 2118
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 65;
                                               pos_bol = 2114;
                                               pos_cnum = 2126
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`Uid
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2118
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2122
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "Some")),
                                         (`Chr
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2123
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2126
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "n")))),
                                    (`App
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 65;
                                               pos_bol = 2114;
                                               pos_cnum = 2130
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 65;
                                               pos_bol = 2114;
                                               pos_cnum = 2150
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`Vrn
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2130
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2140
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "Nativeint")),
                                         (`Record
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2141
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 65;
                                                    pos_bol = 2114;
                                                    pos_cnum = 2150
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Sem
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 65;
                                                         pos_bol = 2114;
                                                         pos_cnum = 2142
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 65;
                                                         pos_bol = 2114;
                                                         pos_cnum = 2149
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ),
                                                   (`RecBind
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 65;
                                                              pos_bol = 2114;
                                                              pos_cnum = 2142
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 65;
                                                              pos_bol = 2114;
                                                              pos_cnum = 2145
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2142
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2145
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "loc")),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2142
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2145
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "loc")))),
                                                   (`RecBind
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 65;
                                                              pos_bol = 2114;
                                                              pos_cnum = 2146
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 65;
                                                              pos_bol = 2114;
                                                              pos_cnum = 2149
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2146
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2149
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "txt")),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2146
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    65;
                                                                   pos_bol =
                                                                    2114;
                                                                   pos_cnum =
                                                                    2149
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "txt")))))))))))),
                               (`Case
                                  (({
                                      loc_start =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 66;
                                          pos_bol = 2151;
                                          pos_cnum = 2155
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "parse_lex.ml";
                                          pos_lnum = 66;
                                          pos_bol = 2151;
                                          pos_cnum = 2174
                                        };
                                      loc_ghost = false
                                    } : Locf.t ),
                                    (`Any
                                       ({
                                          loc_start =
                                            {
                                              pos_fname = "parse_lex.ml";
                                              pos_lnum = 66;
                                              pos_bol = 2151;
                                              pos_cnum = 2155
                                            };
                                          loc_end =
                                            {
                                              pos_fname = "parse_lex.ml";
                                              pos_lnum = 66;
                                              pos_bol = 2151;
                                              pos_cnum = 2156
                                            };
                                          loc_ghost = false
                                        } : Locf.t )),
                                    (`App
                                       (({
                                           loc_start =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 66;
                                               pos_bol = 2151;
                                               pos_cnum = 2160
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "parse_lex.ml";
                                               pos_lnum = 66;
                                               pos_bol = 2151;
                                               pos_cnum = 2174
                                             };
                                           loc_ghost = false
                                         } : Locf.t ),
                                         (`Vrn
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 66;
                                                    pos_bol = 2151;
                                                    pos_cnum = 2160
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 66;
                                                    pos_bol = 2151;
                                                    pos_cnum = 2164
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "Int")),
                                         (`Record
                                            (({
                                                loc_start =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 66;
                                                    pos_bol = 2151;
                                                    pos_cnum = 2165
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "parse_lex.ml";
                                                    pos_lnum = 66;
                                                    pos_bol = 2151;
                                                    pos_cnum = 2174
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ),
                                              (`Sem
                                                 (({
                                                     loc_start =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 66;
                                                         pos_bol = 2151;
                                                         pos_cnum = 2166
                                                       };
                                                     loc_end =
                                                       {
                                                         pos_fname =
                                                           "parse_lex.ml";
                                                         pos_lnum = 66;
                                                         pos_bol = 2151;
                                                         pos_cnum = 2173
                                                       };
                                                     loc_ghost = false
                                                   } : Locf.t ),
                                                   (`RecBind
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 66;
                                                              pos_bol = 2151;
                                                              pos_cnum = 2166
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 66;
                                                              pos_bol = 2151;
                                                              pos_cnum = 2169
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2166
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2169
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "loc")),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2166
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2169
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "loc")))),
                                                   (`RecBind
                                                      (({
                                                          loc_start =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 66;
                                                              pos_bol = 2151;
                                                              pos_cnum = 2170
                                                            };
                                                          loc_end =
                                                            {
                                                              pos_fname =
                                                                "parse_lex.ml";
                                                              pos_lnum = 66;
                                                              pos_bol = 2151;
                                                              pos_cnum = 2173
                                                            };
                                                          loc_ghost = false
                                                        } : Locf.t ),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2170
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2173
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "txt")),
                                                        (`Lid
                                                           (({
                                                               loc_start =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2170
                                                                 };
                                                               loc_end =
                                                                 {
                                                                   pos_fname
                                                                    =
                                                                    "parse_lex.ml";
                                                                   pos_lnum =
                                                                    66;
                                                                   pos_bol =
                                                                    2151;
                                                                   pos_cnum =
                                                                    2173
                                                                 };
                                                               loc_ghost =
                                                                 false
                                                             } : Locf.t ),
                                                             "txt"))))))))))))))))))))) : 
      FAst.exp ))
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
            annot =
              "Compile_lex.output_entry @@\n  (Lexgen.make_single_dfa { shortest = false; clauses = l })\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                    ->
                    (Compile_lex.output_entry @@
                       (Lexgen.make_single_dfa
                          { shortest = false; clauses = l }) : 'lex )))
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
             "Compile_lex.output_entry @@\n  (Lexgen.make_single_dfa { shortest = true; clauses = l })\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                   ->
                   (Compile_lex.output_entry @@
                      (Lexgen.make_single_dfa
                         { shortest = true; clauses = l }) : 'lex )))
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
              "let e =\n  Compile_lex.output_entry @@\n    (Lexgen.make_single_dfa { shortest = false; clauses = l }) in\n(`Constraint\n   (_loc, e,\n     (`Arrow\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Lexing\")), (`Lid (_loc, \"lexbuf\")))),\n          (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"t\"))))))) : \n  FAst.exp )\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                    ->
                    (let e =
                       Compile_lex.output_entry @@
                         (Lexgen.make_single_dfa
                            { shortest = false; clauses = l }) in
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
             "let e =\n  Compile_lex.output_entry @@\n    (Lexgen.make_single_dfa { shortest = true; clauses = l }) in\n(`Constraint\n   (_loc, e,\n     (`Arrow\n        (_loc,\n          (`Dot (_loc, (`Uid (_loc, \"Lexing\")), (`Lid (_loc, \"lexbuf\")))),\n          (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"t\"))))))) : \n  FAst.exp )\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(l : 'case list)  ~__fan_0:_  (_loc : Locf.t) 
                   ->
                   (let e =
                      Compile_lex.output_entry @@
                        (Lexgen.make_single_dfa
                           { shortest = true; clauses = l }) in
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
              "let expander loc _ s = Gramf.parse_string ~loc Syntaxf.exp s in\nlet e = Tokenf.quot_expand expander x in (r, e)\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.quot) 
                    ~__fan_0:(r : 'regexp)  (_loc : Locf.t)  ->
                    let x = __fan_1 in
                    (let expander loc _ s =
                       Gramf.parse_string ~loc Syntaxf.exp s in
                     let e = Tokenf.quot_expand expander x in (r, e) : 
                      'case )))
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
             "try Hashtbl.find named_cases x\nwith\n| Not_found  ->\n    (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\" x;\n     raise UnboundCase)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   (try Hashtbl.find named_cases x
                    with
                    | Not_found  ->
                        (Fan_warnings.emitf xloc.loc_start
                           "Reference to unbound case name %s" x;
                         raise UnboundCase) : 'case )))
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
