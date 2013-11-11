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
let buff_contents = Lexing_util.buff_contents
let err = Lexing_util.err
let warn = Lexing_util.warn
let move_curr_p = Lexing_util.move_curr_p
let store = Lexing_util.store
let (--) = Location_util.( -- ) 
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
      | 36 ->
          ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state3 lexbuf)
      | 91 -> __ocaml_lex_state6 lexbuf
      | 33 -> __ocaml_lex_state20 lexbuf
      | 124 -> __ocaml_lex_state13 lexbuf
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
          -> __ocaml_lex_state26 lexbuf
      | 35 ->
          ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state11 lexbuf)
      | 39 -> __ocaml_lex_state22 lexbuf
      | 58 -> __ocaml_lex_state9 lexbuf
      | 9|12|32 -> __ocaml_lex_state4 lexbuf
      | 59 -> __ocaml_lex_state8 lexbuf
      | 64|94 -> __ocaml_lex_state15 lexbuf
      | 34 -> __ocaml_lex_state23 lexbuf
      | 47 -> __ocaml_lex_state17 lexbuf
      | 63 -> __ocaml_lex_state28 lexbuf
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
          -> __ocaml_lex_state27 lexbuf
      | 126 -> __ocaml_lex_state29 lexbuf
      | 41|44|93|96|125 -> __ocaml_lex_state5 lexbuf
      | 10 -> __ocaml_lex_state31 lexbuf
      | 37 ->
          ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state18 lexbuf)
      | 49|50|51|52|53|54|55|56|57 ->
          ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state24 lexbuf)
      | 42 -> __ocaml_lex_state19 lexbuf
      | 13 -> __ocaml_lex_state30 lexbuf
      | 123 -> __ocaml_lex_state7 lexbuf
      | 43|45 -> __ocaml_lex_state16 lexbuf
      | 48 ->
          ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state25 lexbuf)
      | 40 ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state21 lexbuf)
      | 46 -> __ocaml_lex_state10 lexbuf
      | 38|60|61 -> __ocaml_lex_state12 lexbuf
      | 62 -> __ocaml_lex_state14 lexbuf
      | 256 -> __ocaml_lex_state2 lexbuf
      | _ -> __ocaml_lex_state1 lexbuf
    and __ocaml_lex_state1 (lexbuf : Lexing.lexbuf) = 31
    and __ocaml_lex_state2 (lexbuf : Lexing.lexbuf) = 30
    and __ocaml_lex_state3 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 31;
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
           ((lexbuf.Lexing.lex_mem).(24) <- lexbuf.Lexing.lex_curr_pos;
            (lexbuf.Lexing.lex_mem).(23) <- lexbuf.Lexing.lex_curr_pos;
            (lexbuf.Lexing.lex_mem).(22) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state123 lexbuf)
       | 123 -> __ocaml_lex_state122 lexbuf
       | 256 ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action)
       | _ -> __ocaml_lex_state121 lexbuf)
    and __ocaml_lex_state4 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 23;
      (match __ocaml_lex_next_char lexbuf with
       | 9|12|32 -> __ocaml_lex_state4 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state5 (lexbuf : Lexing.lexbuf) = 21
    and __ocaml_lex_state6 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 60|61|62|124 -> __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state7 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 60 -> __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state8 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 59 -> __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state9 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 58|61|62 -> __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state10 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 46 -> __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state11 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 9|32 ->
           ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state112 lexbuf)
       | 48|49|50|51|52|53|54|55|56|57 ->
           ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state111 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state12 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 20;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state12 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state13 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 20;
      (match __ocaml_lex_next_char lexbuf with
       | 93 -> __ocaml_lex_state5 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state12 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state14 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 20;
      (match __ocaml_lex_next_char lexbuf with
       | 93|125 -> __ocaml_lex_state5 lexbuf
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state12 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state15 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 19;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state15 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state16 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 18;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state16 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state17 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state18 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
      (match __ocaml_lex_next_char lexbuf with
       | 37 ->
           ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state91 lexbuf)
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
           ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state87 lexbuf)
       | 33|38|42|43|45|47|58|60|61|62|63|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
       | 64 ->
           ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state89 lexbuf)
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
           -> __ocaml_lex_state88 lexbuf
       | 123 -> __ocaml_lex_state86 lexbuf
       | 46 -> __ocaml_lex_state90 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state19 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
      (match __ocaml_lex_next_char lexbuf with
       | 42 -> __ocaml_lex_state85 lexbuf
       | 33|37|38|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
       | 41 -> __ocaml_lex_state84 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state20 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state83 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state21 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 97 -> __ocaml_lex_state61 lexbuf
       | 108 -> __ocaml_lex_state62 lexbuf
       | 109 -> __ocaml_lex_state63 lexbuf
       | 33|37|38|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           ((lexbuf.Lexing.lex_mem).(13) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state66 lexbuf)
       | 42 ->
           ((lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state60 lexbuf)
       | 9|12|32 ->
           ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
            (lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state65 lexbuf)
       | 111 -> __ocaml_lex_state64 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state22 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 13 -> __ocaml_lex_state50 lexbuf
       | 92 -> __ocaml_lex_state48 lexbuf
       | 256 ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action)
       | 10 -> __ocaml_lex_state51 lexbuf
       | _ -> __ocaml_lex_state49 lexbuf)
    and __ocaml_lex_state23 (lexbuf : Lexing.lexbuf) = 7
    and __ocaml_lex_state24 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 46 -> __ocaml_lex_state38 lexbuf
       | 48|49|50|51|52|53|54|55|56|57|95 ->
           ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state24 lexbuf)
       | 69|101 -> __ocaml_lex_state37 lexbuf
       | 76|108|110 -> __ocaml_lex_state39 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state25 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 66|98 -> __ocaml_lex_state40 lexbuf
       | 46 -> __ocaml_lex_state38 lexbuf
       | 48|49|50|51|52|53|54|55|56|57|95 ->
           ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state24 lexbuf)
       | 69|101 -> __ocaml_lex_state37 lexbuf
       | 88|120 -> __ocaml_lex_state42 lexbuf
       | 79|111 -> __ocaml_lex_state41 lexbuf
       | 76|108|110 -> __ocaml_lex_state39 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state26 (lexbuf : Lexing.lexbuf) =
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
           -> __ocaml_lex_state26 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state27 (lexbuf : Lexing.lexbuf) =
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
           -> __ocaml_lex_state27 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state28 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state32 lexbuf
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
           -> __ocaml_lex_state35 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state29 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 21;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state32 lexbuf
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
           -> __ocaml_lex_state33 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state30 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 0;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state31 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state31 (lexbuf : Lexing.lexbuf) = 0
    and __ocaml_lex_state32 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 15;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state32 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state33 (lexbuf : Lexing.lexbuf) =
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
          -> __ocaml_lex_state33 lexbuf
      | 58 -> __ocaml_lex_state34 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state34 (lexbuf : Lexing.lexbuf) = 1
    and __ocaml_lex_state35 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 58 -> __ocaml_lex_state36 lexbuf
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
          -> __ocaml_lex_state35 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state36 (lexbuf : Lexing.lexbuf) = 2
    and __ocaml_lex_state37 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state46 lexbuf
      | 43|45 -> __ocaml_lex_state47 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state38 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 6;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state38 lexbuf
       | 69|101 -> __ocaml_lex_state37 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state39 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(10); 5
    and __ocaml_lex_state40 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49 ->
          ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state45 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state41 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55 ->
          ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state44 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state42 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          ->
          ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state43 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state43 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48
         |49
          |50|51|52|53|54|55|56|57|65|66|67|68|69|70|95|97|98|99|100|101|102
           ->
           ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state43 lexbuf)
       | 76|108|110 -> __ocaml_lex_state39 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state44 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|95 ->
           ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state44 lexbuf)
       | 76|108|110 -> __ocaml_lex_state39 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state45 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|95 ->
           ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state45 lexbuf)
       | 76|108|110 -> __ocaml_lex_state39 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state46 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 6;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state46 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state47 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state46 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state48 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state56 lexbuf
      | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state57 lexbuf
      | 120 -> __ocaml_lex_state55 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | _ -> __ocaml_lex_state54 lexbuf
    and __ocaml_lex_state49 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state53 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state50 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state52 lexbuf
      | 10 -> __ocaml_lex_state51 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state51 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state52 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state52 (lexbuf : Lexing.lexbuf) = 8
    and __ocaml_lex_state53 (lexbuf : Lexing.lexbuf) = 9
    and __ocaml_lex_state54 (lexbuf : Lexing.lexbuf) = 10
    and __ocaml_lex_state55 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 10;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
           -> __ocaml_lex_state59 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state56 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 10;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state58 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state57 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 10;
      (match __ocaml_lex_next_char lexbuf with
       | 39 -> __ocaml_lex_state53 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state58 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state49 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state59 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          -> __ocaml_lex_state49 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state60 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 24;
      (match __ocaml_lex_next_char lexbuf with
       | 41 -> __ocaml_lex_state82 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state61 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 115 -> __ocaml_lex_state81 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state62 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 97 -> __ocaml_lex_state78 lexbuf
      | 111 -> __ocaml_lex_state77 lexbuf
      | 115 -> __ocaml_lex_state75 lexbuf
      | 120 -> __ocaml_lex_state76 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state63 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 111 -> __ocaml_lex_state74 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state64 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state65 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 97 -> __ocaml_lex_state61 lexbuf
      | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state69 lexbuf)
      | 108 -> __ocaml_lex_state62 lexbuf
      | 109 -> __ocaml_lex_state63 lexbuf
      | 9|12|32 ->
          ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
           (lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state65 lexbuf)
      | 111 -> __ocaml_lex_state64 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state66 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 41 -> __ocaml_lex_state67 lexbuf
      | 9|12|32 -> __ocaml_lex_state68 lexbuf
      | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
          ((lexbuf.Lexing.lex_mem).(13) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state66 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state67 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(13); 11
    and __ocaml_lex_state68 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 41 -> __ocaml_lex_state67 lexbuf
      | 9|12|32 -> __ocaml_lex_state68 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state69 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
          ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state69 lexbuf)
      | 9|12|32 -> __ocaml_lex_state71 lexbuf
      | 41 -> __ocaml_lex_state70 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state70 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(12);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(14);
      12
    and __ocaml_lex_state71 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 9|12|32 -> __ocaml_lex_state71 lexbuf
      | 41 -> __ocaml_lex_state70 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state72 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 41 -> __ocaml_lex_state73 lexbuf
      | 9|12|32 -> __ocaml_lex_state72 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state73 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(9);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(15);
      13
    and __ocaml_lex_state74 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 100 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state75 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 108|114 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state76 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 111 -> __ocaml_lex_state80 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state77 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state78 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 110 -> __ocaml_lex_state79 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state79 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 100 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state80 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state81 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 114 ->
          ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state72 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state82 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(11); 24
    and __ocaml_lex_state83 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 14;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state83 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state84 (lexbuf : Lexing.lexbuf) = 22
    and __ocaml_lex_state85 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 16;
      (match __ocaml_lex_next_char lexbuf with
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state85 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state86 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      25
    and __ocaml_lex_state87 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 64 ->
          ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state108 lexbuf)
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
          ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state87 lexbuf)
      | 123 -> __ocaml_lex_state107 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state88 (lexbuf : Lexing.lexbuf) =
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
          -> __ocaml_lex_state88 lexbuf
      | 46 -> __ocaml_lex_state106 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state89 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
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
           ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state104 lexbuf)
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state90 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
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
           ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state87 lexbuf)
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
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
           -> __ocaml_lex_state88 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state91 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
      (match __ocaml_lex_next_char lexbuf with
       | 123 -> __ocaml_lex_state92 lexbuf
       | 46 -> __ocaml_lex_state96 lexbuf
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
           ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state93 lexbuf)
       | 33|37|38|42|43|45|47|58|60|61|62|63|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
       | 64 ->
           ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state95 lexbuf)
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
           -> __ocaml_lex_state94 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state92 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      25
    and __ocaml_lex_state93 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state93 lexbuf)
      | 64 ->
          ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state101 lexbuf)
      | 123 -> __ocaml_lex_state100 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state94 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 46 -> __ocaml_lex_state99 lexbuf
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
          -> __ocaml_lex_state94 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state95 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
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
           ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state97 lexbuf)
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state96 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 17;
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
           ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state93 lexbuf)
       | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
           __ocaml_lex_state17 lexbuf
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
           -> __ocaml_lex_state94 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state97 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state97 lexbuf)
      | 123 -> __ocaml_lex_state98 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state98 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(17);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(18);
      25
    and __ocaml_lex_state99 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state93 lexbuf)
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
          -> __ocaml_lex_state94 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state100 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(8);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(16);
      25
    and __ocaml_lex_state101 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state102 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state102 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state102 lexbuf)
      | 123 -> __ocaml_lex_state103 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state103 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(4) <- (lexbuf.Lexing.lex_mem).(5);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(8);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(16);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(17);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(18);
      25
    and __ocaml_lex_state104 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state104 lexbuf)
      | 123 -> __ocaml_lex_state105 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state105 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(17);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(18);
      25
    and __ocaml_lex_state106 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state87 lexbuf)
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
          -> __ocaml_lex_state88 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state107 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(1) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(8);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(16);
      25
    and __ocaml_lex_state108 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state109 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state109 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state109 lexbuf)
      | 123 -> __ocaml_lex_state110 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state110 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(4) <- (-1);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(8);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(16);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(17);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(18);
      25
    and __ocaml_lex_state111 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 34 ->
          ((lexbuf.Lexing.lex_mem).(21) <- lexbuf.Lexing.lex_curr_pos;
           (lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state116 lexbuf)
      | 48|49|50|51|52|53|54|55|56|57 ->
          ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state111 lexbuf)
      | 13 -> __ocaml_lex_state113 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 10 -> __ocaml_lex_state114 lexbuf
      | 9|32 -> __ocaml_lex_state117 lexbuf
      | _ -> __ocaml_lex_state115 lexbuf
    and __ocaml_lex_state112 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 9|32 ->
          ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state112 lexbuf)
      | 48|49|50|51|52|53|54|55|56|57 ->
          ((lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state111 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state113 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 26;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state114 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state114 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(3) <- (-1);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      26
    and __ocaml_lex_state115 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 13 -> __ocaml_lex_state113 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 10 -> __ocaml_lex_state114 lexbuf
      | _ -> __ocaml_lex_state115 lexbuf
    and __ocaml_lex_state116 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 34 -> __ocaml_lex_state118 lexbuf
      | 13 -> __ocaml_lex_state113 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 10 -> __ocaml_lex_state114 lexbuf
      | _ ->
          ((lexbuf.Lexing.lex_mem).(21) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state116 lexbuf)
    and __ocaml_lex_state117 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 34 ->
          ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
           (lexbuf.Lexing.lex_mem).(21) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state116 lexbuf)
      | 13 -> __ocaml_lex_state113 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 10 -> __ocaml_lex_state114 lexbuf
      | 9|32 -> __ocaml_lex_state117 lexbuf
      | _ -> __ocaml_lex_state115 lexbuf
    and __ocaml_lex_state118 (lexbuf : Lexing.lexbuf) =
      match __ocaml_lex_next_char lexbuf with
      | 13 -> __ocaml_lex_state119 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 10 -> __ocaml_lex_state120 lexbuf
      | _ -> __ocaml_lex_state118 lexbuf
    and __ocaml_lex_state119 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(20);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(21);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 26;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state120 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state120 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(19);
      (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(20);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(21);
      26
    and __ocaml_lex_state121 (lexbuf : Lexing.lexbuf) = 29
    and __ocaml_lex_state122 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(1) <- (-1); 28
    and __ocaml_lex_state123 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(2) <- (-1);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(22);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 27;
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
           ->
           ((lexbuf.Lexing.lex_mem).(24) <- lexbuf.Lexing.lex_curr_pos;
            (lexbuf.Lexing.lex_mem).(22) <- lexbuf.Lexing.lex_curr_pos;
            (lexbuf.Lexing.lex_mem).(23) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state123 lexbuf)
       | 58 -> __ocaml_lex_state125 lexbuf
       | 123 -> __ocaml_lex_state124 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state124 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(6);
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(24);
      28
    and __ocaml_lex_state125 (lexbuf : Lexing.lexbuf) =
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
          ((lexbuf.Lexing.lex_mem).(25) <- lexbuf.Lexing.lex_curr_pos;
           __ocaml_lex_state126 lexbuf)
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state126 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(22);
      (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(23);
      (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(25);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 27;
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
           ->
           ((lexbuf.Lexing.lex_mem).(25) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state126 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action)) in
    (__ocaml_lex_init_lexbuf lexbuf 26;
     (lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos);
    (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
     lexbuf.lex_start_p <- lexbuf.lex_curr_p;
     lexbuf.lex_curr_p <-
       {
         (lexbuf.lex_curr_p) with
         pos_cnum = (lexbuf.lex_abs_pos + lexbuf.lex_curr_pos)
       };
     (match __ocaml_lex_result with
      | 0 -> (update_loc lexbuf; token lexbuf)
      | 1 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          `Label { loc = (!! lexbuf); txt }
      | 2 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          `Optlabel { loc = (!! lexbuf); txt }
      | 3 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          if
            ((function | "mod"|"land"|"lor"|"lxor" -> true | _ -> false)) txt
          then `Inf { loc = (!! lexbuf); txt; level = 3 }
          else
            if ((function | "lsl"|"lsr"|"asr" -> true | _ -> false)) txt
            then `Inf { loc = (!! lexbuf); txt; level = 4 }
            else `Lid { loc = (!! lexbuf); txt }
      | 4 ->
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
      | 5 ->
          let s =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0)
          and txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let (loc :Locf.t)=
            {
              loc_start = (lexbuf.lex_start_p);
              loc_end = (lexbuf.lex_curr_p);
              loc_ghost = false
            } in
          (match s with
           | Some 'l' -> `Int32 { loc; txt }
           | Some 'L' -> `Int64 { loc; txt }
           | Some 'n' -> `Nativeint { loc; txt }
           | _ -> `Int { loc; txt })
      | 6 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Flo
            {
              loc =
                {
                  loc_start = (lexbuf.lex_start_p);
                  loc_end = (lexbuf.lex_curr_p);
                  loc_ghost = false
                };
              txt
            }
      | 7 ->
          let c = Lexing_util.new_cxt () in
          let old = lexbuf.lex_start_p in
          (Lexing_util.push_loc_cont c lexbuf lex_string;
           (let loc = old -- lexbuf.lex_curr_p in
            `Str { loc; txt = (buff_contents c) }))
      | 8 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          (Lexing_util.update_loc lexbuf ~retract:1;
           `Chr { loc = (!! lexbuf); txt })
      | 9 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          `Chr { loc = (!! lexbuf); txt }
      | 10 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
          (Lexing_util.err (Illegal_escape (String.make 1 c))) @@ (!! lexbuf)
      | 11 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
          `Eident { loc = (!! lexbuf); txt }
      | 12 ->
          let txt =
            Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0) in
          `Eident { loc = (!! lexbuf); txt }
      | 13 ->
          let txt =
            Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0) in
          `Eident { loc = (!! lexbuf); txt }
      | 14 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Pre { loc = (!! lexbuf); txt }
      | 15 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Pre { loc = (!! lexbuf); txt }
      | 16 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Inf { loc = (!! lexbuf); txt; level = 4 }
      | 17 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Inf { loc = (!! lexbuf); txt; level = 3 }
      | 18 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Inf { loc = (!! lexbuf); txt; level = 2 }
      | 19 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Inf { loc = (!! lexbuf); txt; level = 1 }
      | 20 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Inf { loc = (!! lexbuf); txt; level = 0 }
      | 21 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          `Sym { loc = (!! lexbuf); txt }
      | 22 ->
          (warn Comment_not_end (!! lexbuf);
           move_curr_p (-1) lexbuf;
           (let loc = !! lexbuf in `Sym { loc; txt = "*" }))
      | 23 -> token lexbuf
      | 24 ->
          let x =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
          ((let c = Lexing_util.new_cxt () in
            if x <> None then Lexing_util.warn Comment_start (!! lexbuf);
            Lexing_util.store c lexbuf;
            Lexing_util.push_loc_cont c lexbuf lex_comment;
            ignore (Lexing_util.buff_contents c));
           token lexbuf)
      | 25 ->
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
            | Some name -> Tokenf.name_of_string name
            | None  -> Tokenf.empty_name in
          let old = lexbuf.lex_start_p in
          let txt =
            store c lexbuf;
            push_loc_cont c lexbuf lex_quotation;
            buff_contents c in
          let loc = old -- lexbuf.lex_curr_p in
          let shift = String.length shift in
          let retract = 1 in
          if x = None
          then `Quot { name; meta; shift; txt; loc; retract }
          else `DirQuotation { name; meta; shift; txt; loc; retract }
      | 26 ->
          let num =
            Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0)
          and name =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(3)) + 0)
              (((lexbuf.Lexing.lex_mem).(2)) + 0) in
          (update_loc lexbuf ?file:name ~line:(int_of_string num)
             ~absolute:true;
           token lexbuf)
      | 27 ->
          let name =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (((lexbuf.Lexing.lex_mem).(0)) + 0)
          and follow =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(2)) + 0)
              (((lexbuf.Lexing.lex_mem).(1)) + 0)
          and txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let (kind,shift) =
            match follow with
            | None  -> ("", 1)
            | Some _ -> (name, ((String.length name) + 2)) in
          `Ant
            { loc = (!! lexbuf); kind; txt; shift; retract = 0; cxt = None }
      | 28 ->
          let name =
            Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(1)) + 0)
              (((lexbuf.Lexing.lex_mem).(0)) + 0)
          and txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let old = lexbuf.lex_start_p in
          let c = new_cxt () in
          (store c lexbuf;
           push_loc_cont c lexbuf lex_quotation;
           `Ant
             {
               loc =
                 {
                   loc_start = old;
                   loc_end = (lexbuf.lex_curr_p);
                   loc_ghost = false
                 };
               kind = ((match name with | Some n -> n | None  -> ""));
               txt = (buff_contents c);
               shift = (String.length txt);
               retract = 1;
               cxt = None
             })
      | 29 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
          err (Illegal_character c) (!! lexbuf)
      | 30 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = !! lexbuf in `EOI { loc; txt = "" }))
      | 31 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
          (err (Illegal_character c)) @@ (!! lexbuf)
      | _ -> failwith "lexing: empty token"))
let from_lexbuf lb =
  (let next _ = Some (token lb) in Streamf.from next : Tokenf.stream )
