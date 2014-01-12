let fprintf = Format.fprintf
let eprintf = Format.eprintf
let lexing_store s buff max =
  let self n s =
    if n >= max
    then n
    else
      (match Streamf.peek s with
       | Some x -> (Streamf.junk s; buff.[n] <- x; n + 1)
       | _ -> n) in
  self 0 s
type lex_error =  
  | Illegal_character of char
  | Illegal_escape of string
  | Illegal_quotation of string
  | Illegal_antiquote
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_quotation
  | Unterminated_antiquot
  | Comment_start
  | Comment_not_end 
exception Lexing_error of lex_error
let print_lex_error ppf e =
  match e with
  | Illegal_antiquote  -> fprintf ppf "Illegal_antiquote"
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_quotation s ->
      fprintf ppf "Illegal quotation (%s)" (String.escaped s)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment  -> fprintf ppf "Comment not terminated"
  | Unterminated_string  -> fprintf ppf "String literal not terminated"
  | Unterminated_quotation  -> fprintf ppf "Quotation not terminated"
  | Unterminated_antiquot  -> fprintf ppf "Antiquotation not terminated"
  | Comment_start  -> fprintf ppf "this is the start of a comment"
  | Comment_not_end  -> fprintf ppf "this is not the end of a comment"
let lex_error_to_string = Formatf.to_string print_lex_error
let debug = ref false
let opt_char_len = function | Some _ -> 1 | None  -> 0
let print_opt_char fmt =
  function | Some c -> fprintf fmt "Some %c" c | None  -> fprintf fmt "None"
let turn_on_quotation_debug () = debug := true
let turn_off_quotation_debug () = debug := false
type context =  {
  mutable loc: Locf.position list;
  buffer: Buffer.t} 
let new_cxt () = { loc = []; buffer = (Buffer.create 256) }
let (++) = Buffer.add_string
let (+>) = Buffer.add_char
let from_lexbuf = Location_util.from_lexbuf
let (!!) = Location_util.from_lexbuf
let (--) = Location_util.( -- ) 
let store c lexbuf = c.buffer ++ (Lexing.lexeme lexbuf)
let with_store c lexbuf f = store c lexbuf; f c lexbuf
let buff_contents c =
  let contents = Buffer.contents c.buffer in Buffer.reset c.buffer; contents
let move_curr_p shift (lexbuf : Lexing.lexbuf) =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + shift
let push_loc_cont c lexbuf lexer =
  c.loc <- (Lexing.lexeme_start_p lexbuf) :: (c.loc); lexer c lexbuf
let pop_loc c = c.loc <- List.tl c.loc
let null_loc c = c.loc = []
let update_loc ?file  ?(absolute= false)  ?(retract= 0)  ?(line= 1) 
  (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with | None  -> pos.pos_fname | Some s -> s in
  lexbuf.lex_curr_p <-
    {
      pos with
      pos_fname = new_file;
      pos_lnum = (if absolute then line else pos.pos_lnum + line);
      pos_bol = (pos.pos_cnum - retract)
    }
let err (error : lex_error) (loc : Locf.t) =
  raise (Locf.Exc_located (loc, (Lexing_error error)))
let warn error (loc : Locf.t) =
  (Fan_warnings.emitf loc.loc_start "Warning: %s") @@
    (lex_error_to_string error)
let rec lex_comment c (lexbuf : Lexing.lexbuf) =
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
    | 13 -> __ocaml_lex_state3 ()
    | 40 -> __ocaml_lex_state6 ()
    | 10 -> __ocaml_lex_state4 ()
    | 42 -> __ocaml_lex_state5 ()
    | 256 -> __ocaml_lex_state2 ()
    | _ -> __ocaml_lex_state1 ()
  and __ocaml_lex_state1 () = 4
  and __ocaml_lex_state2 () = 3
  and __ocaml_lex_state3 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 2;
    (match __ocaml_lex_next_char () with
     | 10 -> __ocaml_lex_state4 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state4 () = 2
  and __ocaml_lex_state5 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 4;
    (match __ocaml_lex_next_char () with
     | 41 -> __ocaml_lex_state8 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state6 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 4;
    (match __ocaml_lex_next_char () with
     | 42 -> __ocaml_lex_state7 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state7 () = 0
  and __ocaml_lex_state8 () = 1 in
  (let pos = lexbuf.lex_curr_pos in
   lexbuf.lex_start_pos <- pos;
   lexbuf.lex_last_pos <- pos;
   lexbuf.lex_last_action <- (-1));
  (let __ocaml_lex_result = __ocaml_lex_state0 () in
   lexbuf.lex_start_p <- lexbuf.lex_curr_p;
   lexbuf.lex_curr_p <-
     {
       (lexbuf.lex_curr_p) with
       pos_cnum = (lexbuf.lex_abs_pos + lexbuf.lex_curr_pos)
     };
   (match __ocaml_lex_result with
    | 0 ->
        (store c lexbuf;
         push_loc_cont c lexbuf lex_comment;
         lex_comment c lexbuf)
    | 1 -> (store c lexbuf; pop_loc c)
    | 2 -> (update_loc lexbuf; with_store c lexbuf lex_comment)
    | 3 ->
        (err Unterminated_comment) @@
          (Location_util.of_positions (List.hd c.loc) lexbuf.lex_curr_p)
    | 4 -> with_store c lexbuf lex_comment
    | _ ->
        failwith
          ("Lexing_util" ^
             ("." ^ ("Lexing_util.lex_comment" ^ " lexing: empty token")))))
let rec lex_string c (lexbuf : Lexing.lexbuf) =
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
    | 13 -> __ocaml_lex_state3 ()
    | 34 -> __ocaml_lex_state6 ()
    | 10 -> __ocaml_lex_state4 ()
    | 92 -> __ocaml_lex_state5 ()
    | 256 -> __ocaml_lex_state2 ()
    | _ -> __ocaml_lex_state1 ()
  and __ocaml_lex_state1 () = 6
  and __ocaml_lex_state2 () = 5
  and __ocaml_lex_state3 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 4;
    (match __ocaml_lex_next_char () with
     | 10 -> __ocaml_lex_state4 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state4 () = 4
  and __ocaml_lex_state5 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 6;
    (match __ocaml_lex_next_char () with
     | 13 ->
         ((lexbuf.lex_mem).(1) <- lexbuf.lex_curr_pos; __ocaml_lex_state11 ())
     | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state9 ()
     | 120 -> __ocaml_lex_state8 ()
     | 256 ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
     | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state10 ()
     | 10 ->
         ((lexbuf.lex_mem).(1) <- lexbuf.lex_curr_pos; __ocaml_lex_state12 ())
     | _ -> __ocaml_lex_state7 ())
  and __ocaml_lex_state6 () = 0
  and __ocaml_lex_state7 () = 3
  and __ocaml_lex_state8 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 3;
    (match __ocaml_lex_next_char () with
     | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
         -> __ocaml_lex_state14 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state9 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 3;
    (match __ocaml_lex_next_char () with
     | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state13 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state10 () = 2
  and __ocaml_lex_state11 () =
    (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(1);
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 1;
    (match __ocaml_lex_next_char () with
     | 9|10|32 -> __ocaml_lex_state12 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state12 () =
    (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(1);
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 1;
    (match __ocaml_lex_next_char () with
     | 9|32 -> __ocaml_lex_state12 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state13 () =
    match __ocaml_lex_next_char () with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state10 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state14 () =
    match __ocaml_lex_next_char () with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state10 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action) in
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
    | 0 -> pop_loc c
    | 1 ->
        let space =
          Lexing.sub_lexeme lexbuf ((lexbuf.lex_mem).(0)) lexbuf.lex_curr_pos in
        (update_loc lexbuf ~retract:(String.length space);
         lex_string c lexbuf)
    | 2 -> with_store c lexbuf lex_string
    | 3 ->
        let x = Lexing.sub_lexeme_char lexbuf (lexbuf.lex_start_pos + 1) in
        ((warn (Illegal_escape (String.make 1 x))) @@
           (Location_util.from_lexbuf lexbuf);
         with_store c lexbuf lex_string)
    | 4 -> (update_loc lexbuf; with_store c lexbuf lex_string)
    | 5 ->
        (err Unterminated_string) @@
          (Location_util.of_positions (List.hd c.loc) lexbuf.lex_curr_p)
    | 6 -> with_store c lexbuf lex_string
    | _ ->
        failwith
          ("Lexing_util" ^
             ("." ^ ("Lexing_util.lex_string" ^ " lexing: empty token")))))
let rec lex_quotation c (lexbuf : Lexing.lexbuf) =
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
    | 10 -> __ocaml_lex_state6 ()
    | 256 -> __ocaml_lex_state3 ()
    | 125 -> __ocaml_lex_state9 ()
    | 34 -> __ocaml_lex_state4 ()
    | 123 -> __ocaml_lex_state8 ()
    | 13 -> __ocaml_lex_state5 ()
    | 40 -> __ocaml_lex_state7 ()
    | 37 -> __ocaml_lex_state10 ()
    | 39 -> __ocaml_lex_state2 ()
    | _ -> __ocaml_lex_state1 ()
  and __ocaml_lex_state1 () = 8
  and __ocaml_lex_state2 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 8;
    (match __ocaml_lex_next_char () with
     | 10|13|256 ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
     | 92 -> __ocaml_lex_state19 ()
     | _ -> __ocaml_lex_state20 ())
  and __ocaml_lex_state3 () = 6
  and __ocaml_lex_state4 () = 5
  and __ocaml_lex_state5 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 4;
    (match __ocaml_lex_next_char () with
     | 10 -> __ocaml_lex_state6 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state6 () = 4
  and __ocaml_lex_state7 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 8;
    (match __ocaml_lex_next_char () with
     | 42 -> __ocaml_lex_state18 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state8 () = 2
  and __ocaml_lex_state9 () = 1
  and __ocaml_lex_state10 () =
    lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
    lexbuf.lex_last_action <- 8;
    (match __ocaml_lex_next_char () with
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
                                                             |252|253|254|255
         -> __ocaml_lex_state13 ()
     | 123 -> __ocaml_lex_state11 ()
     | 46 -> __ocaml_lex_state15 ()
     | 37 -> __ocaml_lex_state16 ()
     | 64 -> __ocaml_lex_state12 ()
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
                                                         |218|219|220|221|222
         -> __ocaml_lex_state14 ()
     | _ ->
         (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action))
  and __ocaml_lex_state11 () = 0
  and __ocaml_lex_state12 () =
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
                                                                  |200
                                                                   |201
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
        -> __ocaml_lex_state17 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state13 () =
    match __ocaml_lex_next_char () with
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
                                                                  |119
                                                                   |120
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
    | 123 -> __ocaml_lex_state11 ()
    | 64 -> __ocaml_lex_state12 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state14 () =
    match __ocaml_lex_next_char () with
    | 46 -> __ocaml_lex_state15 ()
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
                                                                  |120
                                                                   |121
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
        -> __ocaml_lex_state14 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state15 () =
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
                                                            |252|253|254|255
        -> __ocaml_lex_state13 ()
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
                                                        |218|219|220|221|222
        -> __ocaml_lex_state14 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state16 () =
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
                                                            |252|253|254|255
        -> __ocaml_lex_state13 ()
    | 123 -> __ocaml_lex_state11 ()
    | 46 -> __ocaml_lex_state15 ()
    | 64 -> __ocaml_lex_state12 ()
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
                                                        |218|219|220|221|222
        -> __ocaml_lex_state14 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state17 () =
    match __ocaml_lex_next_char () with
    | 123 -> __ocaml_lex_state11 ()
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
                                                                  |120
                                                                   |121
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
        -> __ocaml_lex_state17 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state18 () = 3
  and __ocaml_lex_state19 () =
    match __ocaml_lex_next_char () with
    | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state20 ()
    | 120 -> __ocaml_lex_state22 ()
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state23 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state20 () =
    match __ocaml_lex_next_char () with
    | 39 -> __ocaml_lex_state21 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state21 () = 7
  and __ocaml_lex_state22 () =
    match __ocaml_lex_next_char () with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state25 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state23 () =
    match __ocaml_lex_next_char () with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state24 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state24 () =
    match __ocaml_lex_next_char () with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state20 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action)
  and __ocaml_lex_state25 () =
    match __ocaml_lex_next_char () with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state20 ()
    | _ ->
        (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos; lexbuf.lex_last_action) in
  (let pos = lexbuf.lex_curr_pos in
   lexbuf.lex_start_pos <- pos;
   lexbuf.lex_last_pos <- pos;
   lexbuf.lex_last_action <- (-1));
  (let __ocaml_lex_result = __ocaml_lex_state0 () in
   lexbuf.lex_start_p <- lexbuf.lex_curr_p;
   lexbuf.lex_curr_p <-
     {
       (lexbuf.lex_curr_p) with
       pos_cnum = (lexbuf.lex_abs_pos + lexbuf.lex_curr_pos)
     };
   (match __ocaml_lex_result with
    | 0 ->
        (store c lexbuf;
         push_loc_cont c lexbuf lex_quotation;
         lex_quotation c lexbuf)
    | 1 -> (store c lexbuf; pop_loc c)
    | 2 ->
        (store c lexbuf;
         push_loc_cont c lexbuf lex_quotation;
         lex_quotation c lexbuf)
    | 3 ->
        (store c lexbuf;
         push_loc_cont c lexbuf lex_comment;
         lex_quotation c lexbuf)
    | 4 -> (update_loc lexbuf; with_store c lexbuf lex_quotation)
    | 5 ->
        (store c lexbuf;
         push_loc_cont c lexbuf lex_string;
         Buffer.add_char c.buffer '"';
         lex_quotation c lexbuf)
    | 6 ->
        (err Unterminated_quotation) @@
          (Location_util.of_positions (List.hd c.loc) lexbuf.lex_curr_p)
    | 7 -> with_store c lexbuf lex_quotation
    | 8 -> with_store c lexbuf lex_quotation
    | _ ->
        failwith
          ("Lexing_util" ^
             ("." ^ ("Lexing_util.lex_quotation" ^ " lexing: empty token")))))
let adapt_to_stream token (loc : Locf.t) strm =
  let lb = Lexing.from_function (lexing_store strm) in
  lb.lex_abs_pos <- (loc.loc_start).pos_cnum;
  lb.lex_curr_p <- loc.loc_start;
  Streamf.from (fun _  -> Some (token lb))
let adapt_to_string token str =
  let loc = Locf.string_loc in
  let lb = Lexing.from_string str in
  lb.lex_abs_pos <- (loc.loc_start).pos_cnum;
  lb.lex_curr_p <- loc.loc_start;
  (let next _ = Some (token lb) in Streamf.from next)
let adapt_to_buf token lb = Streamf.from (fun _  -> Some (token lb))
let rec clean: Tokenf.stream -> Tokenf.stream =
  fun (__strm : _ Streamf.t)  ->
    match Streamf.peek __strm with
    | Some (`EOI _ as x) -> (Streamf.junk __strm; Streamf.ising x)
    | Some x ->
        (Streamf.junk __strm;
         (let xs = __strm in
          Streamf.icons x (Streamf.slazy (fun _  -> clean xs))))
    | _ -> Streamf.sempty
let rec strict_clean: Tokenf.stream -> Tokenf.stream =
  fun (__strm : _ Streamf.t)  ->
    match Streamf.peek __strm with
    | Some (`EOI _) -> (Streamf.junk __strm; Streamf.sempty)
    | Some x ->
        (Streamf.junk __strm;
         (let xs = __strm in
          Streamf.icons x (Streamf.slazy (fun _  -> strict_clean xs))))
    | _ -> Streamf.sempty
let debug_of_string token str =
  let from_string = adapt_to_string token in
  let stream = from_string str in
  (stream |> clean) |>
    (Streamf.iter
       (fun t  -> Format.fprintf Format.std_formatter "%a@\n" Tokenf.print t))
let debug_of_file token file =
  let from_stream = adapt_to_stream token in
  let loc = Locf.mk file in
  let chan = open_in file in
  let stream = Streamf.of_channel chan in
  ((from_stream loc stream) |> clean) |>
    (Streamf.iter @@
       (fun t  -> Format.fprintf Format.std_formatter "%a@\n" Tokenf.print t))
let list_of_string token ?(verbose= true)  str =
  let from_string = adapt_to_string token in
  let result = ref [] in
  let stream = from_string str in
  (stream |> clean) |>
    (Streamf.iter
       (fun t  ->
          result := (t :: (!result));
          if verbose
          then Format.fprintf Format.std_formatter "%a@\n" Tokenf.print t));
  List.rev (!result)
let get_tokens = list_of_string ~verbose:false
let _ =
  Printexc.register_printer @@
    (function | Lexing_error e -> Some (lex_error_to_string e) | _ -> None)
