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
         (Sequence ((Characters [(13, 13)]), (Characters [(10, 10)])))) : 
    Translate_lex.concrete_regexp );
  "ocaml_blank" +>
    (Characters [(9, 9); (12, 12); (32, 32)] : Translate_lex.concrete_regexp );
  "lowercase" +>
    (Characters [(95, 95); (97, 122); (223, 246); (248, 255)] : Translate_lex.concrete_regexp );
  "uppercase" +>
    (Characters [(65, 90); (192, 214); (216, 222)] : Translate_lex.concrete_regexp );
  "identchar" +>
    (Characters
       [(39, 39);
       (48, 57);
       (65, 90);
       (95, 95);
       (97, 122);
       (192, 214);
       (216, 246);
       (248, 255)] : Translate_lex.concrete_regexp );
  "eof" +> Eof;
  "_" +> (Characters Fcset.all_chars);
  "hexa_char" +>
    (Characters [(48, 57); (65, 70); (97, 102)] : Translate_lex.concrete_regexp );
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
               (248, 255)]))) : Translate_lex.concrete_regexp );
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
                   (Characters [(48, 57); (65, 70); (97, 102)])))))) : 
    Translate_lex.concrete_regexp );
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
                        (Characters [(48, 57); (65, 70); (97, 102)])))))))) : 
    Translate_lex.concrete_regexp );
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
               (248, 255)]))) : Translate_lex.concrete_regexp );
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
               (248, 255)]))) : Translate_lex.concrete_regexp );
  "decimal_literal" +>
    (Sequence
       ((Characters [(48, 57)]),
         (Repetition (Characters [(48, 57); (95, 95)]))) : Translate_lex.concrete_regexp );
  "hex_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 48)]), (Characters [(88, 88); (120, 120)]))),
             (Characters [(48, 57); (65, 70); (97, 102)]))),
         (Repetition (Characters [(48, 57); (65, 70); (95, 95); (97, 102)]))) : 
    Translate_lex.concrete_regexp );
  "oct_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 48)]), (Characters [(79, 79); (111, 111)]))),
             (Characters [(48, 55)]))),
         (Repetition (Characters [(48, 55); (95, 95)]))) : Translate_lex.concrete_regexp );
  "bin_literal" +>
    (Sequence
       ((Sequence
           ((Sequence
               ((Characters [(48, 48)]), (Characters [(66, 66); (98, 98)]))),
             (Characters [(48, 49)]))),
         (Repetition (Characters [(48, 49); (95, 95)]))) : Translate_lex.concrete_regexp );
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
              (Repetition (Characters [(48, 49); (95, 95)]))))) : Translate_lex.concrete_regexp );
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
                   (Repetition (Characters [(48, 57); (95, 95)]))))))) : 
    Translate_lex.concrete_regexp );
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
                        (248, 255)]), (Characters [(45, 45)]))))))) : 
    Translate_lex.concrete_regexp );
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
            (248, 255)])) : Translate_lex.concrete_regexp )
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
                          pos_fname = "lex_predef.ml";
                          pos_lnum = 64;
                          pos_bol = 2201;
                          pos_cnum = 2226
                        };
                      loc_end =
                        {
                          pos_fname = "lex_predef.ml";
                          pos_lnum = 64;
                          pos_bol = 2201;
                          pos_cnum = 2229
                        };
                      loc_ghost = false
                    } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                                         (`Bool (_loc, false)))))))))))),
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
                                                  (`Bool (_loc, false)))))))),
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
                                                                    (`Bool
                                                                    (_loc,
                                                                    false)))))))))))),
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
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 94;
                         pos_bol = 3192;
                         pos_cnum = 3216
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 94;
                         pos_bol = 3192;
                         pos_cnum = 3219
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                                        (`Bool (_loc, false)))))))))))),
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
                                                 (`Bool (_loc, false)))))))),
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
                                                                    (`Bool
                                                                    (_loc,
                                                                    false)))))))))))),
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
                                                                   (`Bool
                                                                    (_loc,
                                                                    false)))))))))))),
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
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 147;
                         pos_bol = 4984;
                         pos_cnum = 5010
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 147;
                         pos_bol = 4984;
                         pos_cnum = 5013
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                       pos_fname = "lex_predef.ml";
                                       pos_lnum = 152;
                                       pos_bol = 5135;
                                       pos_cnum = 5177
                                     };
                                   loc_end =
                                     {
                                       pos_fname = "lex_predef.ml";
                                       pos_lnum = 152;
                                       pos_bol = 5135;
                                       pos_cnum = 5178
                                     };
                                   loc_ghost = false
                                 } : Locf.t ), "s"))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 152;
                         pos_bol = 5135;
                         pos_cnum = 5187
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 152;
                         pos_bol = 5135;
                         pos_cnum = 5190
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                           (`Bool (_loc, false)))))))))))),
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
                                  pos_fname = "lex_predef.ml";
                                  pos_lnum = 167;
                                  pos_bol = 5581;
                                  pos_cnum = 5608
                                };
                              loc_end =
                                {
                                  pos_fname = "lex_predef.ml";
                                  pos_lnum = 167;
                                  pos_bol = 5581;
                                  pos_cnum = 5611
                                };
                              loc_ghost = false
                            } : Locf.t ), "txt"))))),
                 (Characters [(39, 39)])) : Translate_lex.concrete_regexp ),
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
                                                                    `Bool
                                                                    (_loc,
                                                                    false)))))))))))),
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
                                 pos_fname = "lex_predef.ml";
                                 pos_lnum = 180;
                                 pos_bol = 6013;
                                 pos_cnum = 6044
                               };
                             loc_end =
                               {
                                 pos_fname = "lex_predef.ml";
                                 pos_lnum = 180;
                                 pos_bol = 6013;
                                 pos_cnum = 6047
                               };
                             loc_ghost = false
                           } : Locf.t ), "txt"))))), (Characters [(39, 39)])) : 
            Translate_lex.concrete_regexp ),
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
                                                         (`Bool (_loc, false)))))))))))),
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
                             pos_fname = "lex_predef.ml";
                             pos_lnum = 186;
                             pos_bol = 6236;
                             pos_cnum = 6257
                           };
                         loc_end =
                           {
                             pos_fname = "lex_predef.ml";
                             pos_lnum = 186;
                             pos_bol = 6236;
                             pos_cnum = 6258
                           };
                         loc_ghost = false
                       } : Locf.t ), "c")))) : Translate_lex.concrete_regexp ),
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
                                          (`Bool (_loc, false)))))))))),
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
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 193;
                         pos_bol = 6517;
                         pos_cnum = 6546
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 193;
                         pos_bol = 6517;
                         pos_cnum = 6549
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                                          (`Bool
                                                             (_loc, false)))))))))))),
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
                                   pos_fname = "lex_predef.ml";
                                   pos_lnum = 202;
                                   pos_bol = 6815;
                                   pos_cnum = 6840
                                 };
                               loc_end =
                                 {
                                   pos_fname = "lex_predef.ml";
                                   pos_lnum = 202;
                                   pos_bol = 6815;
                                   pos_cnum = 6841
                                 };
                               loc_ghost = false
                             } : Locf.t ), "x")))))) : Translate_lex.concrete_regexp ),
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
                 (Characters [(9, 9); (12, 12); (32, 32)])) : Translate_lex.concrete_regexp ),
             (append_quot q (`Uid (_loc, "()") : Astf.exp )));
          ((Alternative
              ((Alternative
                  ((Characters [(10, 10)]), (Characters [(13, 13)]))),
                (Sequence ((Characters [(13, 13)]), (Characters [(10, 10)])))) : 
            Translate_lex.concrete_regexp ),
            (append_quot q
               (`App
                  (_loc,
                    (`Dot
                       (_loc, (`Uid (_loc, "Lexing_util")),
                         (`Lid (_loc, "update_loc")))),
                    (`Lid (_loc, "lexbuf"))) : Astf.exp )))])));
    ("ocaml_string",
      ((fun { loc = _loc;_}  ->
          [((Characters [(34, 34)] : Translate_lex.concrete_regexp ),
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
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 237;
                         pos_bol = 7974;
                         pos_cnum = 7989
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 237;
                         pos_bol = 7974;
                         pos_cnum = 7990
                       };
                     loc_ghost = false
                   } : Locf.t ), "c")) : Translate_lex.concrete_regexp ),
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
          [((Eof : Translate_lex.concrete_regexp ),
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
          [((Sequence ((Characters [(37, 37)]), (Characters [(123, 123)])) : 
             Translate_lex.concrete_regexp ),
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
                                               pos_fname = "lex_predef.ml";
                                               pos_lnum = 271;
                                               pos_bol = 9000;
                                               pos_cnum = 9035
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "lex_predef.ml";
                                               pos_lnum = 271;
                                               pos_bol = 9000;
                                               pos_cnum = 9039
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
                                                pos_fname = "lex_predef.ml";
                                                pos_lnum = 271;
                                                pos_bol = 9000;
                                                pos_cnum = 9058
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "lex_predef.ml";
                                                pos_lnum = 271;
                                                pos_bol = 9000;
                                                pos_cnum = 9062
                                              };
                                            loc_ghost = false
                                          } : Locf.t ), "meta"))))))))),
                     (Characters [(123, 123)]))),
                 (({
                     loc_start =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 271;
                         pos_bol = 9000;
                         pos_cnum = 9076
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 271;
                         pos_bol = 9000;
                         pos_cnum = 9081
                       };
                     loc_ghost = false
                   } : Locf.t ), "shift")) : Translate_lex.concrete_regexp ),
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
                                                  pos_fname = "lex_predef.ml";
                                                  pos_lnum = 294;
                                                  pos_bol = 9805;
                                                  pos_cnum = 9825
                                                };
                                              loc_end =
                                                {
                                                  pos_fname = "lex_predef.ml";
                                                  pos_lnum = 294;
                                                  pos_bol = 9805;
                                                  pos_cnum = 9826
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
                                               pos_fname = "lex_predef.ml";
                                               pos_lnum = 294;
                                               pos_bol = 9805;
                                               pos_cnum = 9854
                                             };
                                           loc_end =
                                             {
                                               pos_fname = "lex_predef.ml";
                                               pos_lnum = 294;
                                               pos_bol = 9805;
                                               pos_cnum = 9858
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
                                                pos_fname = "lex_predef.ml";
                                                pos_lnum = 294;
                                                pos_bol = 9805;
                                                pos_cnum = 9877
                                              };
                                            loc_end =
                                              {
                                                pos_fname = "lex_predef.ml";
                                                pos_lnum = 294;
                                                pos_bol = 9805;
                                                pos_cnum = 9881
                                              };
                                            loc_ghost = false
                                          } : Locf.t ), "meta"))))))))),
                     (Characters [(123, 123)]))),
                 (({
                     loc_start =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 294;
                         pos_bol = 9805;
                         pos_cnum = 9892
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 294;
                         pos_bol = 9805;
                         pos_cnum = 9897
                       };
                     loc_ghost = false
                   } : Locf.t ), "shift")) : Translate_lex.concrete_regexp ),
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
                                              pos_fname = "lex_predef.ml";
                                              pos_lnum = 322;
                                              pos_bol = 10751;
                                              pos_cnum = 10791
                                            };
                                          loc_end =
                                            {
                                              pos_fname = "lex_predef.ml";
                                              pos_lnum = 322;
                                              pos_bol = 10751;
                                              pos_cnum = 10794
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
                                                      "lex_predef.ml";
                                                    pos_lnum = 323;
                                                    pos_bol = 10808;
                                                    pos_cnum = 10850
                                                  };
                                                loc_end =
                                                  {
                                                    pos_fname =
                                                      "lex_predef.ml";
                                                    pos_lnum = 323;
                                                    pos_bol = 10808;
                                                    pos_cnum = 10854
                                                  };
                                                loc_ghost = false
                                              } : Locf.t ), "name"))))),
                                   (Characters [(34, 34)]))))))),
                     (Repetition (Characters [(0, 9); (11, 12); (14, 255)])))),
                 (Alternative
                    ((Alternative
                        ((Characters [(10, 10)]), (Characters [(13, 13)]))),
                      (Sequence
                         ((Characters [(13, 13)]), (Characters [(10, 10)])))))) : 
             Translate_lex.concrete_regexp ),
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
                               (`Bool (_loc, true))))))) : Astf.exp )))])));
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
                                      pos_fname = "lex_predef.ml";
                                      pos_lnum = 343;
                                      pos_bol = 11388;
                                      pos_cnum = 11418
                                    };
                                  loc_end =
                                    {
                                      pos_fname = "lex_predef.ml";
                                      pos_lnum = 343;
                                      pos_bol = 11388;
                                      pos_cnum = 11422
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
                                       pos_fname = "lex_predef.ml";
                                       pos_lnum = 343;
                                       pos_bol = 11388;
                                       pos_cnum = 11444
                                     };
                                   loc_end =
                                     {
                                       pos_fname = "lex_predef.ml";
                                       pos_lnum = 343;
                                       pos_bol = 11388;
                                       pos_cnum = 11450
                                     };
                                   loc_ghost = false
                                 } : Locf.t ), "follow"))))))),
                 (({
                     loc_start =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 343;
                         pos_bol = 11388;
                         pos_cnum = 11456
                       };
                     loc_end =
                       {
                         pos_fname = "lex_predef.ml";
                         pos_lnum = 343;
                         pos_bol = 11388;
                         pos_cnum = 11459
                       };
                     loc_ghost = false
                   } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                          pos_fname = "lex_predef.ml";
                                          pos_lnum = 354;
                                          pos_bol = 11759;
                                          pos_cnum = 11789
                                        };
                                      loc_end =
                                        {
                                          pos_fname = "lex_predef.ml";
                                          pos_lnum = 354;
                                          pos_bol = 11759;
                                          pos_cnum = 11793
                                        };
                                      loc_ghost = false
                                    } : Locf.t ), "name"))))))),
                    (Characters [(123, 123)]))),
                (({
                    loc_start =
                      {
                        pos_fname = "lex_predef.ml";
                        pos_lnum = 354;
                        pos_bol = 11759;
                        pos_cnum = 11804
                      };
                    loc_end =
                      {
                        pos_fname = "lex_predef.ml";
                        pos_lnum = 354;
                        pos_bol = 11759;
                        pos_cnum = 11807
                      };
                    loc_ghost = false
                  } : Locf.t ), "txt")) : Translate_lex.concrete_regexp ),
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
                                                                    (`Bool
                                                                    (_loc,
                                                                    false)))))))))))),
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
                             pos_fname = "lex_predef.ml";
                             pos_lnum = 370;
                             pos_bol = 12345;
                             pos_cnum = 12364
                           };
                         loc_end =
                           {
                             pos_fname = "lex_predef.ml";
                             pos_lnum = 370;
                             pos_bol = 12345;
                             pos_cnum = 12365
                           };
                         loc_ghost = false
                       } : Locf.t ), "c")))) : Translate_lex.concrete_regexp ),
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
