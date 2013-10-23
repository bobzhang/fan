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
let lexing_store = Lexing_util.lexing_store
let with_store = Lexing_util.with_store
let (--) = Location_util.( -- ) 
let rec token: Lexing.lexbuf -> (Tokenf.t* Locf.t) =
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
      | 37 -> __ocaml_lex_state3 lexbuf
      | 40 -> __ocaml_lex_state6 lexbuf
      | 13 -> __ocaml_lex_state11 lexbuf
      | 34 -> __ocaml_lex_state9 lexbuf
      | 9|12|32 -> __ocaml_lex_state4 lexbuf
      | 39 -> __ocaml_lex_state8 lexbuf
      | 35|41|42|43|60|61|63|91|93|94|124 -> __ocaml_lex_state5 lexbuf
      | 45 -> __ocaml_lex_state7 lexbuf
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
          -> __ocaml_lex_state10 lexbuf
      | 10 -> __ocaml_lex_state12 lexbuf
      | 256 -> __ocaml_lex_state2 lexbuf
      | _ -> __ocaml_lex_state1 lexbuf
    and __ocaml_lex_state1 lexbuf = 11
    and __ocaml_lex_state2 lexbuf = 10
    and __ocaml_lex_state3 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 11;
      (match __ocaml_lex_next_char lexbuf with
       | 123 -> __ocaml_lex_state27 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state4 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 7;
      (match __ocaml_lex_next_char lexbuf with
       | 9|12|32 -> __ocaml_lex_state4 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state5 lexbuf = 6
    and __ocaml_lex_state6 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 6;
      (match __ocaml_lex_next_char lexbuf with
       | 42 ->
           ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state25 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state7 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 6;
      (match __ocaml_lex_next_char lexbuf with
       | 62 -> __ocaml_lex_state5 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state8 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 11;
      (match __ocaml_lex_next_char lexbuf with
       | 92 -> __ocaml_lex_state13 lexbuf
       | 13 -> __ocaml_lex_state15 lexbuf
       | 256 ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action)
       | 10 -> __ocaml_lex_state16 lexbuf
       | _ -> __ocaml_lex_state14 lexbuf)
    and __ocaml_lex_state9 lexbuf = 2
    and __ocaml_lex_state10 lexbuf =
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
           -> __ocaml_lex_state10 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state11 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 0;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state12 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state12 lexbuf = 0
    and __ocaml_lex_state13 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 120 -> __ocaml_lex_state20 lexbuf
      | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state22 lexbuf
      | 256 ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state21 lexbuf
      | _ -> __ocaml_lex_state19 lexbuf
    and __ocaml_lex_state14 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state18 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state15 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state17 lexbuf
      | 10 -> __ocaml_lex_state16 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state16 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 39 -> __ocaml_lex_state17 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state17 lexbuf = 3
    and __ocaml_lex_state18 lexbuf = 4
    and __ocaml_lex_state19 lexbuf = 5
    and __ocaml_lex_state20 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
           -> __ocaml_lex_state24 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state21 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state23 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state22 lexbuf =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 39 -> __ocaml_lex_state18 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state23 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state14 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state24 lexbuf =
      match __ocaml_lex_next_char lexbuf with
      | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
          -> __ocaml_lex_state14 lexbuf
      | _ ->
          (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
           lexbuf.Lexing.lex_last_action)
    and __ocaml_lex_state25 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 8;
      (match __ocaml_lex_next_char lexbuf with
       | 41 -> __ocaml_lex_state26 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state26 lexbuf =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1); 8
    and __ocaml_lex_state27 lexbuf = 9 in
    __ocaml_lex_init_lexbuf lexbuf 2;
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
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          (update_loc lexbuf;
           (let loc = !! lexbuf in ((`Newline { loc; txt }), loc)))
      | 1 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let loc = !! lexbuf in ((`Lid { loc; txt }), loc)
      | 2 ->
          let c = new_cxt () in
          let old = lexbuf.lex_start_p in
          (push_loc_cont c lexbuf lex_string;
           (let loc = old -- lexbuf.lex_curr_p in
            ((`Str { loc; txt = (buff_contents c) }), loc)))
      | 3 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          (update_loc lexbuf ~retract:1;
           (let loc = !! lexbuf in ((`Chr { loc; txt }), loc)))
      | 4 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
              (lexbuf.Lexing.lex_curr_pos + (-1)) in
          let loc = !! lexbuf in ((`Chr { loc; txt }), loc)
      | 5 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
          (err (Illegal_escape (String.make 1 c))) @@ (!! lexbuf)
      | 6 ->
          let txt =
            Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
              (lexbuf.Lexing.lex_curr_pos + 0) in
          let loc = !! lexbuf in ((`Sym { loc; txt }), loc)
      | 7 -> token lexbuf
      | 8 ->
          let x =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
          let c = new_cxt () in
          (if x <> None then warn Comment_start (!! lexbuf);
           store c lexbuf;
           push_loc_cont c lexbuf lex_comment;
           token lexbuf)
      | 9 ->
          let old = lexbuf.lex_start_p in
          let c = new_cxt () in
          (store c lexbuf;
           push_loc_cont c lexbuf lex_quotation;
           (let loc = old -- lexbuf.lex_curr_p in
            ((`Quot
                {
                  Tokenf.name = Tokenf.empty_name;
                  meta = None;
                  content = (buff_contents c);
                  shift = 2;
                  retract = 1;
                  loc
                }), loc)))
      | 10 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = !! lexbuf in ((`EOI { loc; txt = "" }), loc)))
      | 11 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
          (err (Illegal_character c)) @@ (!! lexbuf)
      | _ -> failwith "lexing: empty token"))
let from_lexbuf lb = Streamf.from (fun _  -> Some (token lb))
let from_stream (loc : Locf.t) strm =
  let lb = Lexing.from_function (lexing_store strm) in
  lb.lex_abs_pos <- (loc.loc_start).pos_cnum;
  lb.lex_curr_p <- loc.loc_start;
  from_lexbuf lb