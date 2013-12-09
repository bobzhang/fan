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
         let c = (lexbuf.lex_buffer).[i] in
         lexbuf.lex_curr_pos <- i + 1; Char.code c)
    and __ocaml_lex_state0 () =
      match __ocaml_lex_next_char () with
      | 37 ->
          ((lexbuf.lex_mem).(4) <- lexbuf.lex_curr_pos; __ocaml_lex_state3 ())
      | 45 -> __ocaml_lex_state6 ()
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
          -> __ocaml_lex_state13 ()
      | 34 -> __ocaml_lex_state11 ()
      | 49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state9 ()
      | 35|41|42|43|44|46|58|59|60|61|63|64|91|93|94|123|124|125 ->
          __ocaml_lex_state4 ()
      | 10 -> __ocaml_lex_state15 ()
      | 39 -> __ocaml_lex_state8 ()
      | 40 -> __ocaml_lex_state5 ()
      | 36 ->
          ((lexbuf.lex_mem).(5) <- lexbuf.lex_curr_pos; __ocaml_lex_state7 ())
      | 9|12|32 -> __ocaml_lex_state16 ()
      | 48 -> __ocaml_lex_state10 ()
      | 13 -> __ocaml_lex_state14 ()
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
          -> __ocaml_lex_state12 ()
      | 256 -> __ocaml_lex_state2 ()
      | _ -> __ocaml_lex_state1 ()
    and __ocaml_lex_state1 () = 16
    and __ocaml_lex_state2 () = 15
    and __ocaml_lex_state3 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 16;
      (match __ocaml_lex_next_char () with
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
           -> __ocaml_lex_state46 ()
       | 64 ->
           ((lexbuf.lex_mem).(11) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state44 ())
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
           ((lexbuf.lex_mem).(12) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state45 ())
       | 46 -> __ocaml_lex_state47 ()
       | 123 -> __ocaml_lex_state43 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state4 () = 12
    and __ocaml_lex_state5 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 12;
      (match __ocaml_lex_next_char () with
       | 42 ->
           ((lexbuf.lex_mem).(10) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state41 ())
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state6 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 12;
      (match __ocaml_lex_next_char () with
       | 62 -> __ocaml_lex_state4 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state7 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 16;
      (match __ocaml_lex_next_char () with
       | 123 -> __ocaml_lex_state36 ()
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
           ((lexbuf.lex_mem).(8) <- lexbuf.lex_curr_pos;
            (lexbuf.lex_mem).(7) <- lexbuf.lex_curr_pos;
            (lexbuf.lex_mem).(6) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state37 ())
       | 256 ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action)
       | _ -> __ocaml_lex_state35 ())
    and __ocaml_lex_state8 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 16;
      (match __ocaml_lex_next_char () with
       | 10 -> __ocaml_lex_state26 ()
       | 92 -> __ocaml_lex_state23 ()
       | 256 ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action)
       | 13 -> __ocaml_lex_state25 ()
       | _ -> __ocaml_lex_state24 ())
    and __ocaml_lex_state9 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state9 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state10 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state9 ()
       | 66|98 -> __ocaml_lex_state17 ()
       | 79|111 -> __ocaml_lex_state18 ()
       | 88|120 -> __ocaml_lex_state19 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state11 () = 4
    and __ocaml_lex_state12 () =
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
           -> __ocaml_lex_state12 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state13 () =
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
           -> __ocaml_lex_state13 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state14 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 1;
      (match __ocaml_lex_next_char () with
       | 10 -> __ocaml_lex_state15 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state15 () = 1
    and __ocaml_lex_state16 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 0;
      (match __ocaml_lex_next_char () with
       | 9|12|32 -> __ocaml_lex_state16 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state17 () =
      match __ocaml_lex_next_char () with
      | 48|49 -> __ocaml_lex_state22 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state18 () =
      match __ocaml_lex_next_char () with
      | 48|49|50|51|52|53|54|55 -> __ocaml_lex_state21 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state19 () =
      match __ocaml_lex_next_char () with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          -> __ocaml_lex_state20 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state20 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48
         |49
          |50|51|52|53|54|55|56|57|65|66|67|68|69|70|95|97|98|99|100|101|102
           -> __ocaml_lex_state20 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state21 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|95 -> __ocaml_lex_state21 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state22 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 48|49|95 -> __ocaml_lex_state22 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state23 () =
      match __ocaml_lex_next_char () with
      | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state32 ()
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state31 ()
      | 256 ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
      | 120 -> __ocaml_lex_state30 ()
      | _ -> __ocaml_lex_state29 ()
    and __ocaml_lex_state24 () =
      match __ocaml_lex_next_char () with
      | 39 -> __ocaml_lex_state28 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state25 () =
      match __ocaml_lex_next_char () with
      | 10 -> __ocaml_lex_state26 ()
      | 39 -> __ocaml_lex_state27 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state26 () =
      match __ocaml_lex_next_char () with
      | 39 -> __ocaml_lex_state27 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state27 () = 6
    and __ocaml_lex_state28 () = 7
    and __ocaml_lex_state29 () = 8
    and __ocaml_lex_state30 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 8;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
           -> __ocaml_lex_state34 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state31 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 8;
      (match __ocaml_lex_next_char () with
       | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state33 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state32 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 8;
      (match __ocaml_lex_next_char () with
       | 39 -> __ocaml_lex_state28 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state33 () =
      match __ocaml_lex_next_char () with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state24 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state34 () =
      match __ocaml_lex_next_char () with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          -> __ocaml_lex_state24 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state35 () = 11
    and __ocaml_lex_state36 () = (lexbuf.lex_mem).(1) <- (-1); 10
    and __ocaml_lex_state37 () =
      (lexbuf.lex_mem).(2) <- (-1);
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(6);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 9;
      (match __ocaml_lex_next_char () with
       | 123 -> __ocaml_lex_state38 ()
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
           ->
           ((lexbuf.lex_mem).(8) <- lexbuf.lex_curr_pos;
            (lexbuf.lex_mem).(6) <- lexbuf.lex_curr_pos;
            (lexbuf.lex_mem).(7) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state37 ())
       | 58 -> __ocaml_lex_state39 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state38 () =
      (lexbuf.lex_mem).(1) <- (lexbuf.lex_mem).(5);
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(8);
      10
    and __ocaml_lex_state39 () =
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
          ((lexbuf.lex_mem).(9) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state40 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state40 () =
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(6);
      (lexbuf.lex_mem).(2) <- (lexbuf.lex_mem).(7);
      (lexbuf.lex_mem).(1) <- (lexbuf.lex_mem).(9);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 9;
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
           ->
           ((lexbuf.lex_mem).(9) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state40 ())
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state41 () =
      (lexbuf.lex_mem).(0) <- (-1);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 13;
      (match __ocaml_lex_next_char () with
       | 41 -> __ocaml_lex_state42 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state42 () =
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(10); 13
    and __ocaml_lex_state43 () =
      (lexbuf.lex_mem).(3) <- (-1); (lexbuf.lex_mem).(1) <- (-1); 14
    and __ocaml_lex_state44 () =
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
          ((lexbuf.lex_mem).(13) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state52 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state45 () =
      match __ocaml_lex_next_char () with
      | 123 -> __ocaml_lex_state48 ()
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
          ((lexbuf.lex_mem).(12) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state45 ())
      | 64 ->
          ((lexbuf.lex_mem).(11) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state49 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state46 () =
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
          -> __ocaml_lex_state46 ()
      | 46 -> __ocaml_lex_state47 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state47 () =
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
          -> __ocaml_lex_state46 ()
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
          ((lexbuf.lex_mem).(12) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state45 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state48 () =
      (lexbuf.lex_mem).(1) <- (-1);
      (lexbuf.lex_mem).(3) <- (lexbuf.lex_mem).(4);
      (lexbuf.lex_mem).(2) <- (lexbuf.lex_mem).(12);
      14
    and __ocaml_lex_state49 () =
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
          ((lexbuf.lex_mem).(13) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state50 ())
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state50 () =
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
          ((lexbuf.lex_mem).(13) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state50 ())
      | 123 -> __ocaml_lex_state51 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state51 () =
      (lexbuf.lex_mem).(3) <- (lexbuf.lex_mem).(4);
      (lexbuf.lex_mem).(2) <- (lexbuf.lex_mem).(12);
      (lexbuf.lex_mem).(1) <- (lexbuf.lex_mem).(11);
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(13);
      14
    and __ocaml_lex_state52 () =
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
          ((lexbuf.lex_mem).(13) <- lexbuf.lex_curr_pos;
           __ocaml_lex_state52 ())
      | 123 -> __ocaml_lex_state53 ()
      | _ ->
          (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
    and __ocaml_lex_state53 () =
      (lexbuf.lex_mem).(3) <- (-1);
      (lexbuf.lex_mem).(1) <- (lexbuf.lex_mem).(11);
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(13);
      14 in
    __ocaml_lex_init_lexbuf 14;
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
              | 888000370 -> txt = "true"
              | 677673548 -> txt = "false"
              | 916095096 -> txt = "as"
              | 479219308 -> txt = "_"
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
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 0)
              (lexbuf.lex_curr_pos + 0) in
          let v = Hashtbl.hash txt in
          if
            ((function
              | 349676278 -> txt = "SEP"
              | 95082182 -> txt = "LEVEL"
              | 536346917 -> txt = "S"
              | 366801699 -> txt = "EOI"
              | 409300411 -> txt = "Lid"
              | 989568174 -> txt = "Uid"
              | 676354871 -> txt = "Ant"
              | 961072975 -> txt = "Quot"
              | 203213615 -> txt = "DirQuotation"
              | 768447600 -> txt = "Str"
              | 572318773 -> txt = "Label"
              | 786753605 -> txt = "Optlabel"
              | 577869539 -> txt = "Chr"
              | 486432153 -> txt = "Int"
              | 848726687 -> txt = "Int32"
              | 568910272 -> txt = "Int64"
              | 75913172 -> txt = "Nativeint"
              | 529459121 -> txt = "Flo"
              | 250075149 -> txt = "Pre"
              | 507098033 -> txt = "Inf"
              | 923010078 -> txt = "TRY"
              | 731387332 -> txt = "PEEK"
              | 314441017 -> txt = "L0"
              | 308945647 -> txt = "L1"
              | 265114720 -> txt = "First"
              | 765123559 -> txt = "Last"
              | 290885330 -> txt = "Before"
              | 138761684 -> txt = "After"
              | 806054151 -> txt = "Level"
              | 100070179 -> txt = "RA"
              | 830446310 -> txt = "Inline"
              | 669476035 -> txt = "Local"
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
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 0)
              (lexbuf.lex_curr_pos + 0) in
          `Int { loc = (Lexing_util.from_lexbuf lexbuf); txt }
      | 6 ->
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
      | 7 ->
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
      | 8 ->
          let c = Lexing.sub_lexeme_char lexbuf (lexbuf.lex_start_pos + 2) in
          Lexing_util.err (Illegal_escape (String.make 1 c))
            ({
               loc_start = (lexbuf.lex_start_p);
               loc_end = (lexbuf.lex_curr_p);
               loc_ghost = false
             } : Locf.t )
      | 9 ->
          let name =
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 1)
              (((lexbuf.lex_mem).(0)) + 0)
          and follow =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.lex_mem).(2)) + 0)
              (((lexbuf.lex_mem).(1)) + 0)
          and txt =
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 0)
              (lexbuf.lex_curr_pos + 0) in
          let (kind,shift) =
            match follow with
            | None  -> ("", 1)
            | Some _ -> (name, ((String.length name) + 2)) in
          (`Ant
             {
               loc = (Lexing_util.from_lexbuf lexbuf);
               kind;
               txt;
               shift;
               retract = 0;
               cxt = None
             } : Tokenf.t )
      | 10 ->
          let name =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.lex_mem).(1)) + 0)
              (((lexbuf.lex_mem).(0)) + 0)
          and txt =
            Lexing.sub_lexeme lexbuf (lexbuf.lex_start_pos + 0)
              (lexbuf.lex_curr_pos + 0) in
          let old = lexbuf.lex_start_p in
          let c = Lexing_util.new_cxt () in
          (Lexing_util.store c lexbuf;
           Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_quotation;
           `Ant
             {
               loc =
                 {
                   loc_start = old;
                   loc_end = (lexbuf.lex_curr_p);
                   loc_ghost = false
                 };
               kind = ((match name with | Some n -> n | None  -> ""));
               txt = (Lexing_util.buff_contents c);
               shift = (String.length txt);
               retract = 1;
               cxt = None
             })
      | 11 ->
          let c = Lexing.sub_lexeme_char lexbuf (lexbuf.lex_start_pos + 1) in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | 12 ->
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
      | 13 ->
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
      | 14 ->
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
      | 15 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = Lexing_util.from_lexbuf lexbuf in
            (`EOI { loc; txt = "" } : Tokenf.t )))
      | 16 ->
          let c = Lexing.sub_lexeme_char lexbuf (lexbuf.lex_start_pos + 0) in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ -> failwith "lexing: empty token"))
let from_stream = Lexing_util.adapt_to_stream token
