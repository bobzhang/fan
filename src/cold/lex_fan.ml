let (++) = Buffer.add_string
let (+>) = Buffer.add_char
let (!!) = Location_util.from_lexbuf
let update_loc = Lexing_util.update_loc
let new_cxt = Lexing_util.new_cxt
let push_loc_cont = Lexing_util.push_loc_cont
let pop_loc = Lexing_util.pop_loc
let lex_string = Lexing_util.lex_string
let lex_comment = Lexing_util.lex_comment
let lex_quotation = Lexing_util.lex_quotation
let lex_antiquot = Lexing_util.lex_antiquot
let buff_contents = Lexing_util.buff_contents
let err = Lexing_util.err
let warn = Lexing_util.warn
let move_curr_p = Lexing_util.move_curr_p
let store = Lexing_util.store
let (--) = Location_util.( -- ) 
let token: Lexing.lexbuf -> (Ftoken.t* Locf.t) =
  fun (lexbuf : Lexing.lexbuf)  ->
    let rec __ocaml_lex_init_lexbuf lexbuf mem_size =
      let pos = lexbuf.Lexing.lex_curr_pos in
      lexbuf.Lexing.lex_mem <- Array.create mem_size (-1);
      lexbuf.Lexing.lex_start_pos <- pos;
      lexbuf.Lexing.lex_last_pos <- pos;
      lexbuf.Lexing.lex_last_action <- (-1)
    and __ocaml_lex_next_char lexbuf =
      if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len
      then
        (if lexbuf.Lexing.lex_eof_reached
         then 256
         else
           (lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
      else
        (let i = lexbuf.Lexing.lex_curr_pos in
         let c = (lexbuf.Lexing.lex_buffer).[i] in
         lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
    and __ocaml_lex_state0 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 36 -> __ocaml_lex_state3 lexbuf
      | 37 ->
          ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state6 lexbuf)
      | 49|50|51|52|53|54|55|56|57 ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state20 lexbuf)
      | 59 -> __ocaml_lex_state13 lexbuf
      | 13 -> __ocaml_lex_state26 lexbuf
      | 62 -> __ocaml_lex_state11 lexbuf
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
          -> __ocaml_lex_state22 lexbuf
      | 124 -> __ocaml_lex_state9 lexbuf
      | 9|12|32 -> __ocaml_lex_state4 lexbuf
      | 41|44|93|96|125 -> __ocaml_lex_state8 lexbuf
      | 46 -> __ocaml_lex_state15 lexbuf
      | 40 ->
          ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state17 lexbuf)
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
          -> __ocaml_lex_state23 lexbuf
      | 10 -> __ocaml_lex_state27 lexbuf
      | 33|38|43|45|47|60|61|64|92|94 -> __ocaml_lex_state5 lexbuf
      | 39 -> __ocaml_lex_state18 lexbuf
      | 63 -> __ocaml_lex_state24 lexbuf
      | 34 -> __ocaml_lex_state19 lexbuf
      | 42 -> __ocaml_lex_state7 lexbuf
      | 35 ->
          ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state16 lexbuf)
      | 126 -> __ocaml_lex_state25 lexbuf
      | 48 ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state21 lexbuf)
      | 91 -> __ocaml_lex_state10 lexbuf
      | 58 -> __ocaml_lex_state14 lexbuf
      | 123 -> __ocaml_lex_state12 lexbuf
      | 256 -> __ocaml_lex_state2 lexbuf
      | _ -> __ocaml_lex_state1 lexbuf
    and __ocaml_lex_state1 lexbuf = 22
    and __ocaml_lex_state2 lexbuf = 21
    and __ocaml_lex_state3 lexbuf = 20
    and __ocaml_lex_state4 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 16;
      (match __ocaml_lex_next_char lexbuf with
       | 9|12|32 -> __ocaml_lex_state4 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state5 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state6 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
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
           -> __ocaml_lex_state91 lexbuf
       | 64 ->
           ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state92 lexbuf)
       | 46 -> __ocaml_lex_state93 lexbuf
       | 33|38|42|43|45|47|58|60|61|62|63|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | 123 -> __ocaml_lex_state89 lexbuf
       | 37 ->
           ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state94 lexbuf)
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
           ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state90 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state7 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | 41 -> __ocaml_lex_state88 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state8 lexbuf = 14
    and __ocaml_lex_state9 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 93 -> __ocaml_lex_state8 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state10 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 60|61|62|124 -> __ocaml_lex_state8 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state11 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 93|125 -> __ocaml_lex_state8 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state12 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 60 -> __ocaml_lex_state8 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state13 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 59 -> __ocaml_lex_state8 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state14 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 58|61|62 -> __ocaml_lex_state8 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state15 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 46 -> __ocaml_lex_state8 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state16 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57 ->
           ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state78 lexbuf)
       | 9|32 ->
           ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state79 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state17 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 97 -> __ocaml_lex_state56 lexbuf
       | 33|37|38|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state61 lexbuf)
       | 108 -> __ocaml_lex_state57 lexbuf
       | 109 -> __ocaml_lex_state58 lexbuf
       | 42 ->
           ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state55 lexbuf)
       | 111 -> __ocaml_lex_state59 lexbuf
       | 9|12|32 ->
           ((lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
            (lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state60 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state18 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state46 lexbuf
       | 13 -> __ocaml_lex_state45 lexbuf
       | 256 ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action)
       | 92 -> __ocaml_lex_state43 lexbuf
       | _ -> __ocaml_lex_state44 lexbuf)
    and __ocaml_lex_state19 lexbuf = 7
    and __ocaml_lex_state20 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|95 ->
           ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state20 lexbuf)
       | 69|101 -> __ocaml_lex_state32 lexbuf
       | 46 -> __ocaml_lex_state33 lexbuf
       | 76|108|110 -> __ocaml_lex_state34 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state21 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|95 ->
           ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state20 lexbuf)
       | 69|101 -> __ocaml_lex_state32 lexbuf
       | 79|111 -> __ocaml_lex_state36 lexbuf
       | 46 -> __ocaml_lex_state33 lexbuf
       | 66|98 -> __ocaml_lex_state35 lexbuf
       | 88|120 -> __ocaml_lex_state37 lexbuf
       | 76|108|110 -> __ocaml_lex_state34 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state22 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 4;
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
           -> __ocaml_lex_state22 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state23 lexbuf =
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
           -> __ocaml_lex_state23 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state24 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
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
           -> __ocaml_lex_state30 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state25 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
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
           -> __ocaml_lex_state28 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state26 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 0;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state27 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state27 lexbuf = 0
    and __ocaml_lex_state28 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          -> __ocaml_lex_state28 lexbuf
      | 58 -> __ocaml_lex_state29 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state29 lexbuf = 1
    and __ocaml_lex_state30 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 58 -> __ocaml_lex_state31 lexbuf
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
          -> __ocaml_lex_state30 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state31 lexbuf = 2
    and __ocaml_lex_state32 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 43|45 -> __ocaml_lex_state42 lexbuf
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state41 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state33 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 6;
      (match __ocaml_lex_next_char lexbuf with
       | 69|101 -> __ocaml_lex_state32 lexbuf
       | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state33 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state34 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(9); 5
    and __ocaml_lex_state35 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49 ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state40 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state36 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55 ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state39 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state37 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state38 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state38 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48
         |49
          |50|51|52|53|54|55|56|57|65|66|67|68|69|70|95|97|98|99|100|101|102
           ->
           ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state38 lexbuf)
       | 76|108|110 -> __ocaml_lex_state34 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state39 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 76|108|110 -> __ocaml_lex_state34 lexbuf
       | 48|49|50|51|52|53|54|55|95 ->
           ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state39 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state40 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|95 ->
           ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state40 lexbuf)
       | 76|108|110 -> __ocaml_lex_state34 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state41 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 6;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state41 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state42 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state41 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state43 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state52 lexbuf
      | 120 -> __ocaml_lex_state50 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state51 lexbuf
      | _ -> __ocaml_lex_state49 lexbuf
    and __ocaml_lex_state44 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state48 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state45 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 10 -> __ocaml_lex_state46 lexbuf
      | 39 -> __ocaml_lex_state47 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state46 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state47 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state47 lexbuf = 8
    and __ocaml_lex_state48 lexbuf = 9
    and __ocaml_lex_state49 lexbuf = 10
    and __ocaml_lex_state50 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 10;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
           -> __ocaml_lex_state54 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state51 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 10;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state53 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state52 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 10;
      (match __ocaml_lex_next_char lexbuf with
       | 39 -> __ocaml_lex_state48 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state53 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state44 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state54 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          -> __ocaml_lex_state44 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state55 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
      (match __ocaml_lex_next_char lexbuf with
       | 41 -> __ocaml_lex_state77 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state56 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 115 -> __ocaml_lex_state76 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state57 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 97 -> __ocaml_lex_state73 lexbuf
      | 120 -> __ocaml_lex_state71 lexbuf
      | 115 -> __ocaml_lex_state70 lexbuf
      | 111 -> __ocaml_lex_state72 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state58 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 111 -> __ocaml_lex_state69 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state59 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state60 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 97 -> __ocaml_lex_state56 lexbuf
      | 108 -> __ocaml_lex_state57 lexbuf
      | 109 -> __ocaml_lex_state58 lexbuf
      | 111 -> __ocaml_lex_state59 lexbuf
      | 9|12|32 ->
          ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
           (lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state60 lexbuf)
      | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
          ((lexbuf.Lexing.lex_mem).(13) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state64 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state61 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
          ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state61 lexbuf)
      | 41 -> __ocaml_lex_state62 lexbuf
      | 9|12|32 -> __ocaml_lex_state63 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state62 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(12); 11
    and __ocaml_lex_state63 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 41 -> __ocaml_lex_state62 lexbuf
      | 9|12|32 -> __ocaml_lex_state63 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state64 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 9|12|32 -> __ocaml_lex_state66 lexbuf
      | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
          ((lexbuf.Lexing.lex_mem).(13) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state64 lexbuf)
      | 41 -> __ocaml_lex_state65 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state65 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(11);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(13);
      12
    and __ocaml_lex_state66 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 9|12|32 -> __ocaml_lex_state66 lexbuf
      | 41 -> __ocaml_lex_state65 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state67 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 9|12|32 -> __ocaml_lex_state67 lexbuf
      | 41 -> __ocaml_lex_state68 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state68 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(8);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(14);
      13
    and __ocaml_lex_state69 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 100 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state70 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 108|114 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state71 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 111 -> __ocaml_lex_state75 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state72 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state73 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 110 -> __ocaml_lex_state74 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state74 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 100 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state75 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state76 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state67 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state77 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(10); 17
    and __ocaml_lex_state78 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 10 -> __ocaml_lex_state81 lexbuf
      | 48|49|50|51|52|53|54|55|56|57 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state78 lexbuf)
      | 34 ->
          ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
           (lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state83 lexbuf)
      | 13 -> __ocaml_lex_state80 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 9|32 -> __ocaml_lex_state84 lexbuf
      | _ -> __ocaml_lex_state82 lexbuf
    and __ocaml_lex_state79 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state78 lexbuf)
      | 9|32 ->
          ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state79 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state80 lexbuf =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(15);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 19;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state81 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state81 lexbuf =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(15);
      19
    and __ocaml_lex_state82 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 10 -> __ocaml_lex_state81 lexbuf
      | 13 -> __ocaml_lex_state80 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | _ -> __ocaml_lex_state82 lexbuf
    and __ocaml_lex_state83 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 10 -> __ocaml_lex_state81 lexbuf
      | 34 -> __ocaml_lex_state85 lexbuf
      | 13 -> __ocaml_lex_state80 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | _ ->
          ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state83 lexbuf)
    and __ocaml_lex_state84 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 10 -> __ocaml_lex_state81 lexbuf
      | 34 ->
          ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
           (lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state83 lexbuf)
      | 13 -> __ocaml_lex_state80 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 9|32 -> __ocaml_lex_state84 lexbuf
      | _ -> __ocaml_lex_state82 lexbuf
    and __ocaml_lex_state85 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 10 -> __ocaml_lex_state87 lexbuf
      | 13 -> __ocaml_lex_state86 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | _ -> __ocaml_lex_state85 lexbuf
    and __ocaml_lex_state86 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(15);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(16);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(17);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 19;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state87 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state87 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(15);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(16);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(17);
      19
    and __ocaml_lex_state88 lexbuf = 15
    and __ocaml_lex_state89 lexbuf =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      18
    and __ocaml_lex_state90 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 64 ->
          ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state111 lexbuf)
      | 123 -> __ocaml_lex_state110 lexbuf
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state90 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state91 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          -> __ocaml_lex_state91 lexbuf
      | 46 -> __ocaml_lex_state109 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state92 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
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
           ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state107 lexbuf)
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state93 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
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
           -> __ocaml_lex_state91 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
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
           ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state90 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state94 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 46 -> __ocaml_lex_state99 lexbuf
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
           ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state96 lexbuf)
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
           -> __ocaml_lex_state97 lexbuf
       | 33|37|38|42|43|45|47|58|60|61|62|63|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | 64 ->
           ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state98 lexbuf)
       | 123 -> __ocaml_lex_state95 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state95 lexbuf =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      18
    and __ocaml_lex_state96 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state96 lexbuf)
      | 64 ->
          ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state104 lexbuf)
      | 123 -> __ocaml_lex_state103 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state97 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          -> __ocaml_lex_state97 lexbuf
      | 46 -> __ocaml_lex_state102 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state98 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
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
           ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state100 lexbuf)
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state99 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
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
           ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state96 lexbuf)
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
           -> __ocaml_lex_state97 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state100 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 123 -> __ocaml_lex_state101 lexbuf
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
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state100 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state101 lexbuf =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(20);
      18
    and __ocaml_lex_state102 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state96 lexbuf)
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
          -> __ocaml_lex_state97 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state103 lexbuf =
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(6);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(18);
      18
    and __ocaml_lex_state104 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state105 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state105 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state105 lexbuf)
      | 123 -> __ocaml_lex_state106 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state106 lexbuf =
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(6);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(18);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(20);
      18
    and __ocaml_lex_state107 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 123 -> __ocaml_lex_state108 lexbuf
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
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state107 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state108 lexbuf =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(20);
      18
    and __ocaml_lex_state109 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          -> __ocaml_lex_state91 lexbuf
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state90 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state110 lexbuf =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(6);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(18);
      18
    and __ocaml_lex_state111 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state112 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state112 lexbuf =
      match __ocaml_lex_next_char lexbuf with
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
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state112 lexbuf)
      | 123 -> __ocaml_lex_state113 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state113 lexbuf =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(6);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(18);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(20);
      18 in
    (__ocaml_lex_init_lexbuf lexbuf 21;
     (lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos);
    (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
     lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
     lexbuf.Lexing.lex_curr_p <-
       {
         (lexbuf.Lexing.lex_curr_p) with
         Lexing.pos_cnum =
           (lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos)
       };
     (match __ocaml_lex_result with
      | 0 -> (update_loc lexbuf; (`NEWLINE, (!! lexbuf)))
      | 1 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          ((`Label x), (!! lexbuf))
      | 2 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          ((`Optlabel x), (!! lexbuf))
      | 3 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let loc = !! lexbuf in ((`Lid (loc, x)), loc)
      | 4 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          ((`Uid x), (!! lexbuf))
      | 5 ->
          let s =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0)
          and x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let x =
            match s with
            | Some 'l' -> `Int32 x
            | Some 'L' -> `Int64 x
            | Some 'n' -> `Nativeint x
            | _ -> `Int x in
          (x, (!! lexbuf))
      | 6 ->
          let f =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          ((`Flo f), (!! lexbuf))
      | 7 ->
          let c = new_cxt () in
          let old = lexbuf.lex_start_p in
          (push_loc_cont c lexbuf lex_string;
           ((`Str (buff_contents c)), (old -- lexbuf.lex_curr_p)))
      | 8 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          (update_loc lexbuf ~retract:1; ((`Chr x), (!! lexbuf)))
      | 9 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          ((`Chr x), (!! lexbuf))
      | 10 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
          (err (Illegal_escape (String.make 1 c))) @@ (!! lexbuf)
      | 11 ->
          let op =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
          ((`Eident op), (!! lexbuf))
      | 12 ->
          let op =
            Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0) in
          ((`Eident op), (!! lexbuf))
      | 13 ->
          let op =
            Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0) in
          ((`Eident op), (!! lexbuf))
      | 14 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          ((`Sym x), (!! lexbuf))
      | 15 ->
          (warn Comment_not_end (!! lexbuf);
           move_curr_p (-1) lexbuf;
           ((`Sym "*"), (!! lexbuf)))
      | 16 ->
          let x =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          ((`Blank x), (!! lexbuf))
      | 17 ->
          let x =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
          let c = new_cxt () in
          let old = lexbuf.lex_start_p in
          (if x <> None then warn Comment_start (!! lexbuf);
           store c lexbuf;
           push_loc_cont c lexbuf lex_comment;
           ((`Comment (buff_contents c)), (old -- lexbuf.lex_curr_p)))
      | 18 ->
          let x =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(4)) + 0)
          and name =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(3)) + 0)
              (((lexbuf.Lexing.lex_mem).(2)) + 0)
          and meta =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(1)) + 0)
              (((lexbuf.Lexing.lex_mem).(0)) + 0)
          and shift =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let c = new_cxt () in
          let name =
            match name with
            | Some name -> Ftoken.name_of_string name
            | None  -> Ftoken.empty_name in
          let old = lexbuf.lex_start_p in
          let content =
            store c lexbuf;
            push_loc_cont c lexbuf lex_quotation;
            buff_contents c in
          let loc = old -- lexbuf.lex_curr_p in
          let shift = String.length shift in
          let retract = 1 in
          ((if x = None
            then
              `Quot
                { Ftoken.name = name; meta; shift; content; loc; retract }
            else
              `DirQuotation
                { Ftoken.name = name; meta; shift; content; loc; retract }),
            loc)
      | 19 ->
          let num =
            Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0)
          and name =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(3)) + 0)
              (((lexbuf.Lexing.lex_mem).(2)) + 0) in
          let line = int_of_string num in
          (update_loc lexbuf ?file:name ~line ~absolute:true;
           ((`LINE_DIRECTIVE (line, name)), (!! lexbuf)))
      | 20 ->
          let dollar (c : Lexing_util.context) (lexbuf : Lexing.lexbuf) =
            let rec __ocaml_lex_init_lexbuf lexbuf mem_size =
              let pos = lexbuf.Lexing.lex_curr_pos in
              lexbuf.Lexing.lex_mem <- Array.create mem_size (-1);
              lexbuf.Lexing.lex_start_pos <- pos;
              lexbuf.Lexing.lex_last_pos <- pos;
              lexbuf.Lexing.lex_last_action <- (-1)
            and __ocaml_lex_next_char lexbuf =
              if lexbuf.Lexing.lex_curr_pos >= lexbuf.Lexing.lex_buffer_len
              then
                (if lexbuf.Lexing.lex_eof_reached
                 then 256
                 else
                   (lexbuf.Lexing.refill_buff lexbuf;
                    __ocaml_lex_next_char lexbuf))
              else
                (let i = lexbuf.Lexing.lex_curr_pos in
                 let c = (lexbuf.Lexing.lex_buffer).[i] in
                 lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
            and __ocaml_lex_state0 lexbuf =
              match __ocaml_lex_next_char lexbuf with
              | 58 -> __ocaml_lex_state3 lexbuf
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
                  ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                   __ocaml_lex_state6 lexbuf)
              | 33|46 ->
                  ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                   __ocaml_lex_state4 lexbuf)
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
                                                                    221|222
                  ->
                  ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                   __ocaml_lex_state5 lexbuf)
              | 256 ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action)
              | 96 ->
                  ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                   __ocaml_lex_state7 lexbuf)
              | 40 -> __ocaml_lex_state2 lexbuf
              | _ -> __ocaml_lex_state1 lexbuf
            and __ocaml_lex_state1 lexbuf = 4
            and __ocaml_lex_state2 lexbuf =
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 3;
              (match __ocaml_lex_next_char lexbuf with
               | 33|46 -> __ocaml_lex_state13 lexbuf
               | 96 -> __ocaml_lex_state15 lexbuf
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
                                                                    |
                                                                    111
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                   -> __ocaml_lex_state14 lexbuf
               | 58 -> __ocaml_lex_state12 lexbuf
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state3 lexbuf =
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 4;
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
                                                                    |
                                                                    111
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                   -> __ocaml_lex_state11 lexbuf
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state4 lexbuf =
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 4;
              (match __ocaml_lex_next_char lexbuf with
               | 33|46 ->
                   ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                    __ocaml_lex_state9 lexbuf)
               | 58 -> __ocaml_lex_state8 lexbuf
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state5 lexbuf =
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 4;
              (match __ocaml_lex_next_char lexbuf with
               | 58 -> __ocaml_lex_state8 lexbuf
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
                                                                    |
                                                                    111
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                   ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                    __ocaml_lex_state10 lexbuf)
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state6 lexbuf =
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 1;
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
                                                                    |
                                                                    111
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                   ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                    __ocaml_lex_state6 lexbuf)
               | 58 -> __ocaml_lex_state8 lexbuf
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state7 lexbuf =
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 4;
              (match __ocaml_lex_next_char lexbuf with
               | 33|46 ->
                   ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                    __ocaml_lex_state9 lexbuf)
               | 58 -> __ocaml_lex_state8 lexbuf
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
                                                                    |
                                                                    111
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                   ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                    __ocaml_lex_state10 lexbuf)
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state8 lexbuf =
              match __ocaml_lex_next_char lexbuf with
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
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                  -> __ocaml_lex_state11 lexbuf
              | _ ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action)
            and __ocaml_lex_state9 lexbuf =
              match __ocaml_lex_next_char lexbuf with
              | 33|46 ->
                  ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                   __ocaml_lex_state9 lexbuf)
              | 58 -> __ocaml_lex_state8 lexbuf
              | _ ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action)
            and __ocaml_lex_state10 lexbuf =
              match __ocaml_lex_next_char lexbuf with
              | 58 -> __ocaml_lex_state8 lexbuf
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
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                  ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
                   __ocaml_lex_state10 lexbuf)
              | _ ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action)
            and __ocaml_lex_state11 lexbuf =
              (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1);
              lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
              lexbuf.Lexing.lex_last_action <- 0;
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
                                                                    |
                                                                    111
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                   -> __ocaml_lex_state11 lexbuf
               | _ ->
                   (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                    lexbuf.Lexing.lex_last_action))
            and __ocaml_lex_state12 lexbuf = 2
            and __ocaml_lex_state13 lexbuf =
              match __ocaml_lex_next_char lexbuf with
              | 33|46 -> __ocaml_lex_state13 lexbuf
              | 58 -> __ocaml_lex_state12 lexbuf
              | _ ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action)
            and __ocaml_lex_state14 lexbuf =
              match __ocaml_lex_next_char lexbuf with
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
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                  -> __ocaml_lex_state14 lexbuf
              | 58 -> __ocaml_lex_state12 lexbuf
              | _ ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action)
            and __ocaml_lex_state15 lexbuf =
              match __ocaml_lex_next_char lexbuf with
              | 33|46 -> __ocaml_lex_state13 lexbuf
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
                                                                    |
                                                                    112
                                                                    |
                                                                    113
                                                                    |
                                                                    114
                                                                    |
                                                                    115
                                                                    |
                                                                    116
                                                                    |
                                                                    117
                                                                    |
                                                                    118
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
                  -> __ocaml_lex_state14 lexbuf
              | 58 -> __ocaml_lex_state12 lexbuf
              | _ ->
                  (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
                   lexbuf.Lexing.lex_last_action) in
            (__ocaml_lex_init_lexbuf lexbuf 2;
             (lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos);
            (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
             lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
             lexbuf.Lexing.lex_curr_p <-
               {
                 (lexbuf.Lexing.lex_curr_p) with
                 Lexing.pos_cnum =
                   (lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos)
               };
             (match __ocaml_lex_result with
              | 0 ->
                  let name =
                    Lexing.sub_lexeme lexbuf
                      (lexbuf.Lexing.lex_start_pos + 0)
                      (((lexbuf.Lexing.lex_mem).(0)) + 0)
                  and x =
                    Lexing.sub_lexeme lexbuf
                      (((lexbuf.Lexing.lex_mem).(0)) + 1)
                      (lexbuf.Lexing.lex_curr_pos + 0) in
                  let old =
                    let v = lexbuf.lex_start_p in
                    {
                      v with
                      pos_cnum = ((v.pos_cnum + (String.length name)) + 1)
                    } in
                  ((`Ant (name, x)), (old -- lexbuf.lex_curr_p))
              | 1 ->
                  let x =
                    Lexing.sub_lexeme lexbuf
                      (lexbuf.Lexing.lex_start_pos + 0)
                      (lexbuf.Lexing.lex_curr_pos + 0) in
                  ((`Ant ("", x)), (!! lexbuf))
              | 2 ->
                  let name =
                    Lexing.sub_lexeme lexbuf
                      (lexbuf.Lexing.lex_start_pos + 1)
                      (lexbuf.Lexing.lex_curr_pos + (-1)) in
                  let old =
                    let v = List.hd c.loc in
                    {
                      v with
                      pos_cnum =
                        (((((v.pos_cnum + 1) + 1) + 1) + (String.length name))
                           - 1)
                    } in
                  (c.buffer +> '(';
                   push_loc_cont c lexbuf lex_antiquot;
                   ((`Ant (name, (buff_contents c))),
                     (old -- (Lexing.lexeme_end_p lexbuf))))
              | 3 ->
                  let old =
                    let v = List.hd c.loc in
                    { v with pos_cnum = (((v.pos_cnum + 1) + 1) - 1) } in
                  (c.buffer +> '(';
                   push_loc_cont c lexbuf lex_antiquot;
                   ((`Ant ("", (buff_contents c))),
                     (old -- (Lexing.lexeme_end_p lexbuf))))
              | 4 ->
                  let c =
                    Lexing.sub_lexeme_char lexbuf
                      (lexbuf.Lexing.lex_start_pos + 0) in
                  err (Illegal_character c) (!! lexbuf)
              | _ -> failwith "lexing: empty token")) in
          let c = new_cxt () in
          if Configf.antiquotations.contents
          then push_loc_cont c lexbuf dollar
          else err Illegal_antiquote (!! lexbuf)
      | 21 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (`EOI, (!! lexbuf)))
      | 22 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
          (err (Illegal_character c)) @@ (!! lexbuf)
      | _ -> failwith "lexing: empty token"))
let from_lexbuf lb =
  (let next _ = Some (token lb) in Fstream.from next : (Ftoken.t* Locf.t)
                                                         Fstream.t )