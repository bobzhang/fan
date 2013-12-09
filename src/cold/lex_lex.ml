let rec token: Lexing.lexbuf -> Tokenf.t =
  fun (lexbuf : Lexing.lexbuf)  ->
    let rec __ocaml_lex_init_lexbuf mem_size =
      let pos = lexbuf.lex_curr_pos in
      lexbuf.lex_mem <- Array.create mem_size (-1);
      lexbuf.lex_start_pos <- pos;
      lexbuf.lex_last_pos <- pos;
      lexbuf.lex_last_action <- (-1)
    and __ocaml_lex_next_char () =
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
      | 37 ->
          ((lexbuf.lex_mem).(4) <- lexbuf.lex_curr_pos; __ocaml_lex_state3 ())
      | 45 -> __ocaml_lex_state6 ()
      | 10 -> __ocaml_lex_state11 ()
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
          -> __ocaml_lex_state9 ()
      | 35|41|42|43|60|61|63|64|91|93|94|124 -> __ocaml_lex_state4 ()
      | 39 -> __ocaml_lex_state8 ()
      | 40 -> __ocaml_lex_state5 ()
      | 34 -> __ocaml_lex_state7 ()
      | 13 -> __ocaml_lex_state10 ()
      | 9|12|32 -> __ocaml_lex_state12 ()
      | 256 -> __ocaml_lex_state2 ()
      | _ -> __ocaml_lex_state1 ()
    and __ocaml_lex_state1 () = 11
    and __ocaml_lex_state2 () = 10
    and __ocaml_lex_state3 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 11;
      (match __ocaml_lex_next_char () with
       | 64 ->
           ((lexbuf.lex_mem).(6) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state28 ())
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
           ->
           ((lexbuf.lex_mem).(7) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state29 ())
       | 123 -> __ocaml_lex_state27 ()
       | 46 -> __ocaml_lex_state31 ()
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
           -> __ocaml_lex_state30 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state4 () = 7
    and __ocaml_lex_state5 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 7;
      (match __ocaml_lex_next_char () with
       | 42 ->
           ((lexbuf.lex_mem).(5) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state25 ())
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state6 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 7;
      (match __ocaml_lex_next_char () with
       | 62 -> __ocaml_lex_state4 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state7 () = 6
    and __ocaml_lex_state8 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 11;
      (match __ocaml_lex_next_char () with
       | 92 -> __ocaml_lex_state13 ()
       | 13 -> __ocaml_lex_state15 ()
       | 256 ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action)
       | 10 -> __ocaml_lex_state16 ()
       | _ -> __ocaml_lex_state14 ())
    and __ocaml_lex_state9 () =
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
           -> __ocaml_lex_state9 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state10 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 1;
      (match __ocaml_lex_next_char () with
       | 10 -> __ocaml_lex_state11 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state11 () = 1
    and __ocaml_lex_state12 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 0;
      (match __ocaml_lex_next_char () with
       | 9|12|32 -> __ocaml_lex_state12 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state13 () =
      match __ocaml_lex_next_char () with
      | 120 -> __ocaml_lex_state20 ()
      | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state22 ()
      | 256 ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state21 ()
      | _ -> __ocaml_lex_state19 ()
    and __ocaml_lex_state14 () =
      match __ocaml_lex_next_char () with
      | 39 -> __ocaml_lex_state18 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state15 () =
      match __ocaml_lex_next_char () with
      | 39 -> __ocaml_lex_state17 ()
      | 10 -> __ocaml_lex_state16 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state16 () =
      match __ocaml_lex_next_char () with
      | 39 -> __ocaml_lex_state17 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state17 () = 3
    and __ocaml_lex_state18 () = 4
    and __ocaml_lex_state19 () = 5
    and __ocaml_lex_state20 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
           -> __ocaml_lex_state24 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state21 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state23 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state22 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 39 -> __ocaml_lex_state18 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state23 () =
      match __ocaml_lex_next_char () with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state14 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state24 () =
      match __ocaml_lex_next_char () with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          -> __ocaml_lex_state14 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state25 () =
      (lexbuf.lex_mem).(0) <- (-1);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 8;
      (match __ocaml_lex_next_char () with
       | 41 -> __ocaml_lex_state26 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state26 () =
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(5); 8
    and __ocaml_lex_state27 () =
      (lexbuf.lex_mem).(3) <- (-1); (lexbuf.lex_mem).(1) <- (-1); 9
    and __ocaml_lex_state28 () =
      match __ocaml_lex_next_char () with
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
                                                        |119
                                                         |120
                                                          |121
                                                           |122
                                                            |192
                                                             |193
                                                              |194
                                                               |195
                                                                |196
                                                                 |197
                                                                  |198
                                                                   |199
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
          ->
          ((lexbuf.lex_mem).(8) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state36 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state29 () =
      match __ocaml_lex_next_char () with
      | 123 -> __ocaml_lex_state32 ()
      | 64 ->
          ((lexbuf.lex_mem).(6) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state33 ())
      | 39
        |45
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
          ->
          ((lexbuf.lex_mem).(7) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state29 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state30 () =
      match __ocaml_lex_next_char () with
      | 46 -> __ocaml_lex_state31 ()
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
                                                                   |119
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
          -> __ocaml_lex_state30 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state31 () =
      match __ocaml_lex_next_char () with
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
          ->
          ((lexbuf.lex_mem).(7) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state29 ())
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
          -> __ocaml_lex_state30 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state32 () =
      (lexbuf.lex_mem).(1) <- (-1);
      (lexbuf.lex_mem).(3) <- (lexbuf.lex_mem).(4);
      (lexbuf.lex_mem).(2) <- (lexbuf.lex_mem).(7);
      9
    and __ocaml_lex_state33 () =
      match __ocaml_lex_next_char () with
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
                                                        |119
                                                         |120
                                                          |121
                                                           |122
                                                            |192
                                                             |193
                                                              |194
                                                               |195
                                                                |196
                                                                 |197
                                                                  |198
                                                                   |199
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
          ->
          ((lexbuf.lex_mem).(8) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state34 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state34 () =
      match __ocaml_lex_next_char () with
      | 123 -> __ocaml_lex_state35 ()
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
                                                                   |119
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
          ->
          ((lexbuf.lex_mem).(8) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state34 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state35 () =
      (lexbuf.lex_mem).(3) <- (lexbuf.lex_mem).(4);
      (lexbuf.lex_mem).(2) <- (lexbuf.lex_mem).(7);
      (lexbuf.lex_mem).(1) <- (lexbuf.lex_mem).(6);
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(8);
      9
    and __ocaml_lex_state36 () =
      match __ocaml_lex_next_char () with
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
                                                                   |119
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
          ->
          ((lexbuf.lex_mem).(8) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state36 ())
      | 123 -> __ocaml_lex_state37 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state37 () =
      (lexbuf.lex_mem).(3) <- (-1);
      (lexbuf.lex_mem).(1) <- (lexbuf.lex_mem).(6);
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(8);
      9 in
    __ocaml_lex_init_lexbuf 9;
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
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 0)
              (lexbuf.lex_curr_pos + 0) in
          let v = Hashtbl.hash txt in
          if
            ((function
              | 916095096 -> txt = "as"
              | 1050473980 -> txt = "let"
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
      | 3 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 1)
              (lexbuf.lex_curr_pos + (-1)) in
          ((let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <-
              {
                pos with
                pos_lnum = (pos.pos_lnum + 1);
                pos_bol = (pos.pos_cnum - 1)
              });
           (`Chr
              {
                loc =
                  {
                    loc_start = (lexbuf.lex_start_p);
                    loc_end = (lexbuf.lex_curr_p);
                    loc_ghost = false
                  };
                txt
              } : Tokenf.t ))
      | 4 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 1)
              (lexbuf.lex_curr_pos + (-1)) in
          (`Chr
             {
               loc =
                 {
                   loc_start = (lexbuf.lex_start_p);
                   loc_end = (lexbuf.lex_curr_p);
                   loc_ghost = false
                 };
               txt
             } : Tokenf.t )
      | 5 ->
          let c = Lexing.sub_lexeme_char lexbuf (lexbuf.lex_start_pos + 2) in
          Lexing_util.err (Illegal_escape (String.make 1 c))
            ({
               loc_start = (lexbuf.lex_start_p);
               loc_end = (lexbuf.lex_curr_p);
               loc_ghost = false
             } : Locf.t )
      | 6 ->
          let c = Lexing_util.new_cxt () in
          let old = lexbuf.lex_start_p in
          (Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_string;
           `Str
             {
               loc = (Location_util.( -- )  old lexbuf.lex_curr_p);
               txt = (Lexing_util.buff_contents c)
             })
      | 7 ->
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
      | 8 ->
          let x =
            Lexing.sub_lexeme_char_opt lexbuf (((lexbuf.lex_mem).(0)) + 0) in
          ((let c = Lexing_util.new_cxt () in
            if x <> None
            then
              Lexing_util.warn Comment_start (Lexing_util.from_lexbuf lexbuf);
            Lexing_util.store c lexbuf;
            Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_comment;
            ignore (Lexing_util.buff_contents c));
           token lexbuf)
      | 9 ->
          let name =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.lex_mem).(3)) + 0)
              (((lexbuf.lex_mem).(2)) + 0)
          and meta =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.lex_mem).(1)) + 0)
              (((lexbuf.lex_mem).(0)) + 0)
          and shift =
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 0)
              (lexbuf.lex_curr_pos + 0) in
          let c = Lexing_util.new_cxt () in
          let name =
            match name with
            | Some name -> Tokenf.name_of_string name
            | None  -> Tokenf.empty_name in
          let old = lexbuf.lex_start_p in
          let txt =
            Lexing_util.store c lexbuf;
            Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_quotation;
            Lexing_util.buff_contents c in
          let loc = Location_util.( -- )  old lexbuf.lex_curr_p in
          let shift = String.length shift in
          let retract = 1 in
          `Quot { Tokenf.name = name; meta; shift; txt; loc; retract }
      | 10 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = Lexing_util.from_lexbuf lexbuf in
            (`EOI { loc; txt = "" } : Tokenf.t )))
      | 11 ->
          let c = Lexing.sub_lexeme_char lexbuf (lexbuf.lex_start_pos + 0) in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ -> failwith ("Lex_lex" ^ ("." ^ ("." ^ "lexing: empty token")))))
let from_lexbuf lb = Streamf.from (fun _  -> Some (token lb))
let from_stream = Lexing_util.adapt_to_stream token
