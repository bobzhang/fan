let register = Gramf.mk "register"
let compile _loc pairs =
  let tbl = Hashtbl.create 30 in
  let () =
    List.iter (fun ((k,kloc),(v,vloc))  -> Hashtbl.add tbl k (kloc, v, vloc))
      pairs in
  (try
     let (_,entry,loc) = Hashtbl.find tbl "entry" in
     fun ()  ->
       let e: Astf.exp = `Lid (loc, entry) in
       (try
          let (_,name,loc) = Hashtbl.find tbl "name" in
          fun ()  ->
            let n: Astf.exp = `Str (loc, name) in
            (try
               let (_,pos,loc) = Hashtblf.find tbl "position" in
               fun ()  ->
                 let p = `Lid (loc, ("of_" ^ pos)) in
                 let lexer = Hashtblf.find_opt tbl "lexer" in
                 match lexer with
                 | None  ->
                     (`App
                        (_loc,
                          (`App
                             (_loc,
                               (`App
                                  (_loc,
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Ast_quotation")),
                                         p)),
                                    (`Label
                                       (_loc, (`Lid (_loc, "name")),
                                         (`Record
                                            (_loc,
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid
                                                           (_loc, "domain")),
                                                        (`Dot
                                                           (_loc,
                                                             (`Uid
                                                                (_loc, "Ns")),
                                                             (`Lid
                                                                (_loc,
                                                                  "lang")))))),
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "name")),
                                                        n)))))))))),
                               (`Label (_loc, (`Lid (_loc, "entry")), e)))),
                          (`Unit _loc)) : Astf.exp )
                 | Some (_,l,loc) ->
                     let l: Astf.exp =
                       `Label (loc, (`Lid (loc, "lexer")), (`Lid (loc, l))) in
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
                                              (`Uid (_loc, "Ast_quotation")),
                                              p)),
                                         (`Label
                                            (_loc, (`Lid (_loc, "name")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc,
                                                                  "domain")),
                                                             (`Dot
                                                                (_loc,
                                                                  (`Uid
                                                                    (_loc,
                                                                    "Ns")),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "lang")))))),
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc,
                                                                  "name")),
                                                             n)))))))))),
                                    (`Label (_loc, (`Lid (_loc, "entry")), e)))),
                               l)), (`Unit _loc)) : Astf.exp )
             with
             | Not_found  ->
                 (fun ()  ->
                    Locf.failf _loc "`position' attribute is required")) ()
        with
        | Not_found  ->
            (fun ()  -> Locf.failf _loc "`name attribute is required")) ()
   with
   | Not_found  ->
       (fun ()  -> Locf.failf _loc "`entry' attribute is required")) ()
let rec token: Lexing.lexbuf -> Tokenf.t =
  fun (lexbuf : Lexing.lexbuf)  ->
    let rec __ocaml_lex_next_char () =
      if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len
      then
        (if lexbuf.lex_eof_reached
         then 256
         else (lexbuf.refill_buff lexbuf; __ocaml_lex_next_char ()))
      else
        (let i = lexbuf.lex_curr_pos in
         lexbuf.lex_curr_pos <- i + 1; Char.code ((lexbuf.lex_buffer).[i]))
    and __ocaml_lex_state0 () =
      match __ocaml_lex_next_char () with
      | 13 -> __ocaml_lex_state6 ()
      | 40 -> __ocaml_lex_state3 ()
      | 58|59 -> __ocaml_lex_state4 ()
      | 9|12|32 -> __ocaml_lex_state8 ()
      | 95
        |97
         |98
          |99
           |100
            |101
             |102
              |103
               |104
                |105
                 |106
                  |107
                   |108
                    |109
                     |110
                      |111
                       |112
                        |113
                         |114
                          |115
                           |116
                            |117
                             |118
                              |119
                               |120
                                |121
                                 |122
                                  |223
                                   |224
                                    |225
                                     |226
                                      |227
                                       |228
                                        |229
                                         |230
                                          |231
                                           |232
                                            |233
                                             |234
                                              |235
                                               |236
                                                |237
                                                 |238
                                                  |239
                                                   |240
                                                    |241
                                                     |242
                                                      |243
                                                       |244
                                                        |245
                                                         |246
                                                          |248
                                                           |249
                                                            |250
                                                             |251
                                                              |252
                                                               |253|254|255
          -> __ocaml_lex_state5 ()
      | 10 -> __ocaml_lex_state7 ()
      | 256 -> __ocaml_lex_state2 ()
      | _ -> __ocaml_lex_state1 ()
    and __ocaml_lex_state1 () = 6
    and __ocaml_lex_state2 () = 5
    and __ocaml_lex_state3 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 6;
      (match __ocaml_lex_next_char () with
       | 42 ->
           ((lexbuf.lex_mem).(1) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state9 ())
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state4 () = 3
    and __ocaml_lex_state5 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 2;
      (match __ocaml_lex_next_char () with
       | 39
         |48
          |49
           |50
            |51
             |52
              |53
               |54
                |55
                 |56
                  |57
                   |65
                    |66
                     |67
                      |68
                       |69
                        |70
                         |71
                          |72
                           |73
                            |74
                             |75
                              |76
                               |77
                                |78
                                 |79
                                  |80
                                   |81
                                    |82
                                     |83
                                      |84
                                       |85
                                        |86
                                         |87
                                          |88
                                           |89
                                            |90
                                             |95
                                              |97
                                               |98
                                                |99
                                                 |100
                                                  |101
                                                   |102
                                                    |103
                                                     |104
                                                      |105
                                                       |106
                                                        |107
                                                         |108
                                                          |109
                                                           |110
                                                            |111
                                                             |112
                                                              |113
                                                               |114
                                                                |115
                                                                 |116
                                                                  |117
                                                                   |118
                                                                    |
                                                                    119
                                                                    |
                                                                    120
                                                                    |
                                                                    121
                                                                    |
                                                                    122
                                                                    |
                                                                    192
                                                                    |
                                                                    193
                                                                    |
                                                                    194
                                                                    |
                                                                    195
                                                                    |
                                                                    196
                                                                    |
                                                                    197
                                                                    |
                                                                    198
                                                                    |
                                                                    199
                                                                    |
                                                                    200
                                                                    |
                                                                    201
                                                                    |
                                                                    202
                                                                    |
                                                                    203
                                                                    |
                                                                    204
                                                                    |
                                                                    205
                                                                    |
                                                                    206
                                                                    |
                                                                    207
                                                                    |
                                                                    208
                                                                    |
                                                                    209
                                                                    |
                                                                    210
                                                                    |
                                                                    211
                                                                    |
                                                                    212
                                                                    |
                                                                    213
                                                                    |
                                                                    214
                                                                    |
                                                                    216
                                                                    |
                                                                    217
                                                                    |
                                                                    218
                                                                    |
                                                                    219
                                                                    |
                                                                    220
                                                                    |
                                                                    221
                                                                    |
                                                                    222
                                                                    |
                                                                    223
                                                                    |
                                                                    224
                                                                    |
                                                                    225
                                                                    |
                                                                    226
                                                                    |
                                                                    227
                                                                    |
                                                                    228
                                                                    |
                                                                    229
                                                                    |
                                                                    230
                                                                    |
                                                                    231
                                                                    |
                                                                    232
                                                                    |
                                                                    233
                                                                    |
                                                                    234
                                                                    |
                                                                    235
                                                                    |
                                                                    236
                                                                    |
                                                                    237
                                                                    |
                                                                    238
                                                                    |
                                                                    239
                                                                    |
                                                                    240
                                                                    |
                                                                    241
                                                                    |
                                                                    242
                                                                    |
                                                                    243
                                                                    |
                                                                    244
                                                                    |
                                                                    245
                                                                    |
                                                                    246
                                                                    |
                                                                    248
                                                                    |
                                                                    249
                                                                    |
                                                                    250
                                                                    |
                                                                    251
                                                                    |
                                                                    252
                                                                    |
                                                                    253
                                                                    |
                                                                    254|255
           -> __ocaml_lex_state5 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state6 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 1;
      (match __ocaml_lex_next_char () with
       | 10 -> __ocaml_lex_state7 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state7 () = 1
    and __ocaml_lex_state8 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 0;
      (match __ocaml_lex_next_char () with
       | 9|12|32 -> __ocaml_lex_state8 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state9 () =
      (lexbuf.lex_mem).(0) <- (-1);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 4;
      (match __ocaml_lex_next_char () with
       | 41 -> __ocaml_lex_state10 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state10 () =
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(1); 4 in
    ((let pos = lexbuf.lex_curr_pos in
      lexbuf.lex_start_pos <- pos;
      lexbuf.lex_last_pos <- pos;
      lexbuf.lex_last_action <- (-1));
     lexbuf.lex_mem <- Array.create 2 (-1));
    (let __ocaml_lex_result = __ocaml_lex_state0 () in
     lexbuf.lex_start_p <- lexbuf.lex_curr_p;
     lexbuf.lex_curr_p <-
       {
         (lexbuf.lex_curr_p) with
         pos_cnum = (lexbuf.lex_abs_pos + lexbuf.lex_curr_pos)
       };
     (match __ocaml_lex_result with
      | 0 -> ((); token lexbuf)
      | 1 -> (Lexing_util.update_loc lexbuf; token lexbuf)
      | 2 ->
          let txt =
            Lexing.sub_lexeme lexbuf lexbuf.lex_start_pos lexbuf.lex_curr_pos in
          `Lid
            {
              loc =
                {
                  loc_start = (lexbuf.lex_start_p);
                  loc_end = (lexbuf.lex_curr_p);
                  loc_ghost = false
                };
              txt
            }
      | 3 ->
          let txt =
            Lexing.sub_lexeme lexbuf lexbuf.lex_start_pos lexbuf.lex_curr_pos in
          (`Key
             {
               loc =
                 {
                   loc_start = (lexbuf.lex_start_p);
                   loc_end = (lexbuf.lex_curr_p);
                   loc_ghost = false
                 };
               txt
             } : Tokenf.t )
      | 4 ->
          let x = Lexing.sub_lexeme_char_opt lexbuf ((lexbuf.lex_mem).(0)) in
          ((let c = Lexing_util.new_cxt () in
            if x <> None
            then
              Lexing_util.warn Comment_start (Lexing_util.from_lexbuf lexbuf);
            Lexing_util.store c lexbuf;
            Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_comment;
            ignore (Lexing_util.buff_contents c));
           token lexbuf)
      | 5 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = Lexing_util.from_lexbuf lexbuf in
            (`EOI { loc; txt = "" } : Tokenf.t )))
      | 6 ->
          let c = Lexing.sub_lexeme_char lexbuf lexbuf.lex_start_pos in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ ->
          failwith
            ("Lang_register" ^ ("." ^ ("token" ^ " lexing: empty token")))))
let make_register register compile =
  let pair: 'pair Gramf.t = Gramf.mk "pair" in
  Gramf.extend_single
    ({
       entry = (register : 'register Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [List1sep
                      ((Nterm (Gramf.obj (pair : 'pair Gramf.t ))),
                        (Token
                           ({
                              descr =
                                {
                                  tag = `Key;
                                  word = (A ";");
                                  tag_name = "Key"
                                }
                            } : Tokenf.pattern )))];
                 annot = "compile _loc pairs\n";
                 fn =
                   (Gramf.mk_action
                      (fun (pairs : 'pair list)  (_loc : Locf.t)  ->
                         (compile _loc pairs : 'register ) : 'pair list ->
                                                               Locf.t ->
                                                                 'register ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pair : 'pair Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                      } : Tokenf.pattern )];
                 annot = "((x, xloc), (y, yloc))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let xloc = __fan_0.loc in
                         let x = __fan_0.txt in
                         let yloc = __fan_2.loc in
                         let y = __fan_2.txt in
                         (((x, xloc), (y, yloc)) : 'pair ) : Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'pair ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let from_stream = Lexing_util.adapt_to_stream token
let () =
  make_register register compile;
  Ast_quotation.of_exp ~name:{ domain = Ns.lang; name = "register" }
    ~entry:register ~lexer:from_stream ()
