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
      | 46|59 -> __ocaml_lex_state3 ()
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
          -> __ocaml_lex_state6 ()
      | 10 -> __ocaml_lex_state9 ()
      | 65
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
                                 |192
                                  |193
                                   |194
                                    |195
                                     |196
                                      |197
                                       |198
                                        |199
                                         |200
                                          |201
                                           |202
                                            |203
                                             |204
                                              |205
                                               |206
                                                |207
                                                 |208
                                                  |209
                                                   |210
                                                    |211
                                                     |212
                                                      |213
                                                       |214
                                                        |216
                                                         |217
                                                          |218
                                                           |219|220|221|222
          -> __ocaml_lex_state4 ()
      | 13 -> __ocaml_lex_state8 ()
      | 34 -> __ocaml_lex_state5 ()
      | 40 -> __ocaml_lex_state7 ()
      | 9|12|32 -> __ocaml_lex_state10 ()
      | 256 -> __ocaml_lex_state2 ()
      | _ -> __ocaml_lex_state1 ()
    and __ocaml_lex_state1 () = 8
    and __ocaml_lex_state2 () = 7
    and __ocaml_lex_state3 () = 6
    and __ocaml_lex_state4 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
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
           -> __ocaml_lex_state4 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state5 () = 4
    and __ocaml_lex_state6 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 3;
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
           -> __ocaml_lex_state6 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state7 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 8;
      (match __ocaml_lex_next_char () with
       | 42 ->
           ((lexbuf.lex_mem).(1) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state11 ())
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state8 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 1;
      (match __ocaml_lex_next_char () with
       | 10 -> __ocaml_lex_state9 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state9 () = 1
    and __ocaml_lex_state10 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 0;
      (match __ocaml_lex_next_char () with
       | 9|12|32 -> __ocaml_lex_state10 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state11 () =
      (lexbuf.lex_mem).(0) <- (-1);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 2;
      (match __ocaml_lex_next_char () with
       | 41 -> __ocaml_lex_state12 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state12 () =
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(1); 2 in
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
          let x = Lexing.sub_lexeme_char_opt lexbuf ((lexbuf.lex_mem).(0)) in
          ((let c = Lexing_util.new_cxt () in
            if x <> None
            then
              Lexing_util.warn Comment_start (Lexing_util.from_lexbuf lexbuf);
            Lexing_util.store c lexbuf;
            Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_comment;
            ignore (Lexing_util.buff_contents c));
           token lexbuf)
      | 3 ->
          let txt =
            Lexing.sub_lexeme lexbuf lexbuf.lex_start_pos lexbuf.lex_curr_pos in
          let v = Hashtbl.hash txt in
          if
            ((function
              | 729319575 -> txt = "default"
              | 245492841 -> txt = "import"
              | 249977308 -> txt = "filter"
              | 674600472 -> txt = "lang_clear"
              | 143481130 -> txt = "require"
              | _ -> false)) v
          then
            `Key
              {
                loc =
                  {
                    loc_start = (lexbuf.lex_start_p);
                    loc_end = (lexbuf.lex_curr_p);
                    loc_ghost = false
                  };
                txt
              }
          else
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
      | 4 ->
          let c = Lexing_util.new_cxt () in
          let old = lexbuf.lex_start_p in
          (Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_string;
           `Str
             {
               loc = (Location_util.( -- )  old lexbuf.lex_curr_p);
               txt = (Lexing_util.buff_contents c)
             })
      | 5 ->
          let txt =
            Lexing.sub_lexeme lexbuf lexbuf.lex_start_pos lexbuf.lex_curr_pos in
          `Uid
            {
              loc =
                {
                  loc_start = (lexbuf.lex_start_p);
                  loc_end = (lexbuf.lex_curr_p);
                  loc_ghost = false
                };
              txt
            }
      | 6 ->
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
      | 7 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = Lexing_util.from_lexbuf lexbuf in
            (`EOI { loc; txt = "" } : Tokenf.t )))
      | 8 ->
          let c = Lexing.sub_lexeme_char lexbuf lexbuf.lex_start_pos in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ ->
          failwith ("Control" ^ ("." ^ ("token" ^ " lexing: empty token")))))
let item = Gramf.mk "item"
let dot_namespace = Gramf.mk "dot_namespace"
let items = Gramf.mk "items"
let _ =
  Gramf.extend_single
    ({
       entry = (item : 'item Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "default");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                      } : Tokenf.pattern )];
                 annot =
                   "match Ast_quotation.resolve_name { domain = (`Sub []); name = s } with\n| None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" s\n| Some x -> Ast_quotation.set_default x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                         let s = __fan_1.txt in
                         (match Ast_quotation.resolve_name
                                  { domain = (`Sub []); name = s }
                          with
                          | None  ->
                              Locf.failf _loc "DDSL `%s' can not be resolved"
                                s
                          | Some x -> Ast_quotation.set_default x : 'item ) : 
                      Tokenf.txt -> Tokenf.txt -> Locf.t -> 'item ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "import"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))];
                annot =
                  "Ast_quotation.paths := ((`Absolute xs) :: (!Ast_quotation.paths))\n";
                fn =
                  (Gramf.mk_action
                     (fun (xs : 'dot_namespace)  _  (_loc : Locf.t)  ->
                        (Ast_quotation.paths := ((`Absolute xs) ::
                           (!Ast_quotation.paths)) : 'item ) : 'dot_namespace
                                                                 ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'item ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "filter"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                    Tokenf.pattern )];
                annot = "Ast_filters.use_implem_filter s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (Ast_filters.use_implem_filter s : 'item ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'item ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "lang_clear");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern )];
                annot =
                  "Ast_quotation.clear_map (); Ast_quotation.clear_default ()\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  ->
                        (Ast_quotation.clear_map ();
                         Ast_quotation.clear_default () : 'item ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'item ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (dot_namespace : 'dot_namespace Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "i :: xs\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'dot_namespace)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (i :: xs : 'dot_namespace ) : 
                      'dot_namespace ->
                        Tokenf.txt -> Tokenf.txt -> Locf.t -> 'dot_namespace ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                      } : Tokenf.pattern )];
                annot = "[i]\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let i = __fan_0.txt in ([i] : 'dot_namespace ) : 
                     Tokenf.txt -> Locf.t -> 'dot_namespace ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (items : 'items Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (item : 'item Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "()\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  -> (() : 'items ) : 
                      Tokenf.txt -> 'item -> Locf.t -> 'items ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (item : 'item Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "()\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  _  (_loc : Locf.t)  -> (() : 'items ) : 
                     'items -> Tokenf.txt -> 'item -> Locf.t -> 'items ))
              };
              {
                symbols = [];
                annot = "()\n";
                fn =
                  (Gramf.mk_action
                     (fun (_loc : Locf.t)  -> (() : 'items ) : Locf.t ->
                                                                 'items ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let lexer = Lexing_util.adapt_to_stream token
let () =
  Ast_quotation.register_unit_parser ~lexer
    ((Tokenf.name_of_string "control"), items)
