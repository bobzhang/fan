let rec token: Lexing.lexbuf -> Tokenf.t =
  fun (lexbuf : Lexing.lexbuf)  ->
    let rec __ocaml_lex_init_lexbuf (lexbuf : Lexing.lexbuf) mem_size =
      let pos = lexbuf.Lexing.lex_curr_pos in
      lexbuf.lex_mem <- Array.create mem_size (-1);
      lexbuf.lex_start_pos <- pos;
      lexbuf.lex_last_pos <- pos;
      lexbuf.lex_last_action <- (-1)
    and __ocaml_lex_next_char (lexbuf : Lexing.lexbuf) =
      if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len
      then
        (if lexbuf.lex_eof_reached
         then 256
         else (lexbuf.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
      else
        (let i = lexbuf.lex_curr_pos in
         let c = (lexbuf.lex_buffer).[i] in
         lexbuf.lex_curr_pos <- i + 1; Char.code c)
    and __ocaml_lex_state0 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 46|59 -> __ocaml_lex_state3 lexbuf
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
          -> __ocaml_lex_state6 lexbuf
      | 10 -> __ocaml_lex_state9 lexbuf
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
          -> __ocaml_lex_state4 lexbuf
      | 13 -> __ocaml_lex_state8 lexbuf
      | 34 -> __ocaml_lex_state5 lexbuf
      | 40 -> __ocaml_lex_state7 lexbuf
      | 9|12|32 -> __ocaml_lex_state10 lexbuf
      | 256 -> __ocaml_lex_state2 lexbuf
      | _ -> __ocaml_lex_state1 lexbuf
    and __ocaml_lex_state1 (lexbuf : Lexing.lexbuf) = 8
    and __ocaml_lex_state2 (lexbuf : Lexing.lexbuf) = 7
    and __ocaml_lex_state3 (lexbuf : Lexing.lexbuf) = 6
    and __ocaml_lex_state4 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
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
           -> __ocaml_lex_state4 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state5 (lexbuf : Lexing.lexbuf) = 4
    and __ocaml_lex_state6 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 3;
      (match __ocaml_lex_next_char lexbuf with
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
           -> __ocaml_lex_state6 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state7 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 8;
      (match __ocaml_lex_next_char lexbuf with
       | 42 ->
           ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state11 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state8 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 1;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state9 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state9 (lexbuf : Lexing.lexbuf) = 1
    and __ocaml_lex_state10 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 0;
      (match __ocaml_lex_next_char lexbuf with
       | 9|12|32 -> __ocaml_lex_state10 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state11 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 2;
      (match __ocaml_lex_next_char lexbuf with
       | 41 -> __ocaml_lex_state12 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state12 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1); 2 in
    __ocaml_lex_init_lexbuf lexbuf 2;
    (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
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
          let x =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
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
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
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
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
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
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ -> failwith "lexing: empty token"))
let item = Gramf.mk "item"
let dot_namespace = Gramf.mk "dot_namespace"
let items = Gramf.mk "items"
let _ =
  Gramf.extend_single (item : 'item Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Key; word = (A "default"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
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
                         Locf.failf _loc "DDSL `%s' can not be resolved" s
                     | Some x -> Ast_quotation.set_default x : 'item ) : 
                 Tokenf.txt -> Tokenf.txt -> Locf.t -> 'item ))
          };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "import"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (dot_namespace : 'dot_namespace Gramf.t ))];
           annot =
             "Ast_quotation.paths := ((`Absolute xs) :: (!Ast_quotation.paths))\n";
           fn =
             (Gramf.mk_action
                (fun (xs : 'dot_namespace)  _  (_loc : Locf.t)  ->
                   (Ast_quotation.paths := ((`Absolute xs) ::
                      (!Ast_quotation.paths)) : 'item ) : 'dot_namespace ->
                                                            Tokenf.txt ->
                                                              Locf.t -> 'item ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "filter"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot = "Ast_filters.use_implem_filter s\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (Ast_filters.use_implem_filter s : 'item ) : Tokenf.txt ->
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
                     { tag = `Key; word = (A "lang_clear"); tag_name = "Key"
                     }
                 } : Tokenf.pattern )];
           annot =
             "Ast_quotation.clear_map (); Ast_quotation.clear_default ()\n";
           fn =
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   (Ast_quotation.clear_map ();
                    Ast_quotation.clear_default () : 'item ) : Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'item ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (dot_namespace : 'dot_namespace Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
                Tokenf.pattern );
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
                ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                Tokenf.pattern )];
           annot = "[i]\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in ([i] : 'dot_namespace ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'dot_namespace ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (items : 'items Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (item : 'item Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
                Tokenf.pattern )];
            annot = "()\n";
            fn =
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  -> (() : 'items ) : Tokenf.txt
                                                                   ->
                                                                   'item ->
                                                                    Locf.t ->
                                                                    'items ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (item : 'item Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Self];
           annot = "()\n";
           fn =
             (Gramf.mk_action
                (fun _  _  _  (_loc : Locf.t)  -> (() : 'items ) : 'items ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'item ->
                                                                    Locf.t ->
                                                                    'items ))
         };
         {
           symbols = [];
           annot = "()\n";
           fn =
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (() : 'items ) : Locf.t -> 'items ))
         }]
     } : Gramf.olevel )
let lexer = Lexing_util.adapt_to_stream token
let () =
  Ast_quotation.register_unit_parser ~lexer
    ((Tokenf.name_of_string "control"), items)
