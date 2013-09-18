let fprintf = Format.fprintf
let eprintf = Format.eprintf
let lexing_store s buff max =
  let self n s =
    if n >= max
    then n
    else
      (match Fstream.peek s with
       | Some x -> (Fstream.junk s; buff.[n] <- x; n + 1)
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
let lex_error_to_string = LibUtil.to_string_of_printer print_lex_error
let debug = ref false
let opt_char_len = function | Some _ -> 1 | None  -> 0
let print_opt_char fmt =
  function | Some c -> fprintf fmt "Some %c" c | None  -> fprintf fmt "None"
module CStack =
  struct
    include Stack
    let push v stk =
      if debug.contents
      then Format.eprintf "Push %a@." print_opt_char v
      else ();
      push v stk
    let pop stk =
      if debug.contents
      then Format.eprintf "Pop %a@." print_opt_char (top stk);
      pop stk
  end
let opt_char: char option Stack.t = Stack.create ()
let turn_on_quotation_debug () = debug := true
let turn_off_quotation_debug () = debug := false
let clear_stack () = Stack.clear opt_char
let show_stack () =
  eprintf "stack expand to check the error message@.";
  Stack.iter (Format.eprintf "%a@." print_opt_char) opt_char
type context =  {
  loc: FLoc.position;
  buffer: Buffer.t} 
let default_cxt lb =
  { loc = (Lexing.lexeme_start_p lb); buffer = (Buffer.create 256) }
let (++) = Buffer.add_string
let (+>) = Buffer.add_char
let (!!) = Location_util.from_lexbuf
let (--) = Location_util.( -- ) 
let store c lexbuf = c.buffer ++ (Lexing.lexeme lexbuf)
let with_store f c lexbuf = store c lexbuf; f c lexbuf
let buff_contents c =
  let contents = Buffer.contents c.buffer in Buffer.reset c.buffer; contents
let move_curr_p shift (lexbuf : Lexing.lexbuf) =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + shift
let with_curr_loc lexer c lexbuf =
  lexer { c with loc = (Lexing.lexeme_start_p lexbuf) } lexbuf
let mk_quotation quotation c (lexbuf : Lexing.lexbuf) ~name  ~meta  ~shift 
  ~retract  =
  let old = lexbuf.lex_start_p in
  let s = with_curr_loc quotation c lexbuf; buff_contents c in
  let content = String.sub s 0 ((String.length s) - retract) in
  let loc = old -- lexbuf.lex_curr_p in
  ((`Quot { Ftoken.name = name; meta; shift; content; loc }), loc)
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
let err (error : lex_error) (loc : FLoc.t) =
  raise (FLoc.Exc_located (loc, (Lexing_error error)))
let warn error (loc : FLoc.t) =
  (Fan_warnings.emitf loc.loc_start "Warning: %s") @@
    (lex_error_to_string error)
let rec lex_comment c (lexbuf : Lexing.lexbuf) =
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
       else (lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
    else
      (let i = lexbuf.Lexing.lex_curr_pos in
       let c = (lexbuf.Lexing.lex_buffer).[i] in
       lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
  and __ocaml_lex_state0 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 13 -> __ocaml_lex_state3 lexbuf
    | 40 -> __ocaml_lex_state6 lexbuf
    | 10 -> __ocaml_lex_state4 lexbuf
    | 42 -> __ocaml_lex_state5 lexbuf
    | 256 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 4
  and __ocaml_lex_state2 lexbuf = 3
  and __ocaml_lex_state3 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 2;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state4 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf = 2
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 4;
    (match __ocaml_lex_next_char lexbuf with
     | 41 -> __ocaml_lex_state8 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state6 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 4;
    (match __ocaml_lex_next_char lexbuf with
     | 42 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state7 lexbuf = 0
  and __ocaml_lex_state8 lexbuf = 1 in
  __ocaml_lex_init_lexbuf lexbuf 0;
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
        (store c lexbuf;
         with_curr_loc lex_comment c lexbuf;
         lex_comment c lexbuf)
    | 1 -> store c lexbuf
    | 2 -> (update_loc lexbuf; with_store lex_comment c lexbuf)
    | 3 ->
        (err Unterminated_comment) @@
          (Location_util.of_positions c.loc lexbuf.lex_curr_p)
    | 4 -> with_store lex_comment c lexbuf
    | _ -> failwith "lexing: empty token"))
let rec lex_string c (lexbuf : Lexing.lexbuf) =
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
       else (lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
    else
      (let i = lexbuf.Lexing.lex_curr_pos in
       let c = (lexbuf.Lexing.lex_buffer).[i] in
       lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
  and __ocaml_lex_state0 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 13 -> __ocaml_lex_state3 lexbuf
    | 34 -> __ocaml_lex_state6 lexbuf
    | 10 -> __ocaml_lex_state4 lexbuf
    | 92 -> __ocaml_lex_state5 lexbuf
    | 256 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 6
  and __ocaml_lex_state2 lexbuf = 5
  and __ocaml_lex_state3 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 4;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state4 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf = 4
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 13 ->
         ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state11 lexbuf)
     | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state9 lexbuf
     | 120 -> __ocaml_lex_state8 lexbuf
     | 256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state10 lexbuf
     | 10 ->
         ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state12 lexbuf)
     | _ -> __ocaml_lex_state7 lexbuf)
  and __ocaml_lex_state6 lexbuf = 0
  and __ocaml_lex_state7 lexbuf = 3
  and __ocaml_lex_state8 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 3;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
         -> __ocaml_lex_state14 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state9 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 3;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state13 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state10 lexbuf = 2
  and __ocaml_lex_state11 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 1;
    (match __ocaml_lex_next_char lexbuf with
     | 9|10|32 -> __ocaml_lex_state12 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state12 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 1;
    (match __ocaml_lex_next_char lexbuf with
     | 9|32 -> __ocaml_lex_state12 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state13 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state10 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state14 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state10 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action) in
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
    | 0 -> ()
    | 1 ->
        let space =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        (update_loc lexbuf ~retract:(String.length space);
         lex_string c lexbuf)
    | 2 -> with_store lex_string c lexbuf
    | 3 ->
        let x =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
        ((warn (Illegal_escape (String.make 1 x))) @@
           (Location_util.from_lexbuf lexbuf);
         with_store lex_string c lexbuf)
    | 4 -> (update_loc lexbuf; with_store lex_string c lexbuf)
    | 5 ->
        (err Unterminated_string) @@
          (Location_util.of_positions c.loc lexbuf.lex_curr_p)
    | 6 -> with_store lex_string c lexbuf
    | _ -> failwith "lexing: empty token"))
let rec lex_antiquot c (lexbuf : Lexing.lexbuf) =
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
       else (lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
    else
      (let i = lexbuf.Lexing.lex_curr_pos in
       let c = (lexbuf.Lexing.lex_buffer).[i] in
       lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
  and __ocaml_lex_state0 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state6 lexbuf
    | 256 -> __ocaml_lex_state3 lexbuf
    | 41 -> __ocaml_lex_state9 lexbuf
    | 34 -> __ocaml_lex_state4 lexbuf
    | 40 -> __ocaml_lex_state8 lexbuf
    | 13 -> __ocaml_lex_state5 lexbuf
    | 123 -> __ocaml_lex_state7 lexbuf
    | 39 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 7
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 7;
    (match __ocaml_lex_next_char lexbuf with
     | 92 -> __ocaml_lex_state18 lexbuf
     | 10|13|256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | _ -> __ocaml_lex_state19 lexbuf)
  and __ocaml_lex_state3 lexbuf = 5
  and __ocaml_lex_state4 lexbuf = 4
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 3;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state6 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state6 lexbuf = 3
  and __ocaml_lex_state7 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 7;
    (match __ocaml_lex_next_char lexbuf with
     | 64 -> __ocaml_lex_state11 lexbuf
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state10 lexbuf)
     | 58 -> __ocaml_lex_state12 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state8 lexbuf = 1
  and __ocaml_lex_state9 lexbuf = 0
  and __ocaml_lex_state10 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 2;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state17 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state11 lexbuf =
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
        -> __ocaml_lex_state16 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state12 lexbuf =
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
                                                            |252|253|254|255
        -> __ocaml_lex_state13 lexbuf
    | 46 -> __ocaml_lex_state15 lexbuf
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
        -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state13 lexbuf =
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
        -> __ocaml_lex_state13 lexbuf
    | 64 -> __ocaml_lex_state11 lexbuf
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state10 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state14 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 46 -> __ocaml_lex_state15 lexbuf
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
        -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state15 lexbuf =
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
                                                            |252|253|254|255
        -> __ocaml_lex_state13 lexbuf
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
        -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state16 lexbuf =
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
        -> __ocaml_lex_state16 lexbuf
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state10 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state17 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1); 2
  and __ocaml_lex_state18 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state22 lexbuf
    | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state19 lexbuf
    | 120 -> __ocaml_lex_state21 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state19 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 39 -> __ocaml_lex_state20 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state20 lexbuf = 6
  and __ocaml_lex_state21 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state24 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state22 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state23 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state23 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state19 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state24 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state19 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action) in
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
    | 0 -> store c lexbuf
    | 1 ->
        (store c lexbuf;
         with_curr_loc lex_antiquot c lexbuf;
         lex_antiquot c lexbuf)
    | 2 ->
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        (Stack.push p opt_char;
         store c lexbuf;
         with_curr_loc lex_quotation c lexbuf;
         lex_antiquot c lexbuf)
    | 3 -> (update_loc lexbuf; with_store lex_antiquot c lexbuf)
    | 4 ->
        (store c lexbuf;
         with_curr_loc lex_string c lexbuf;
         c.buffer +> '"';
         lex_antiquot c lexbuf)
    | 5 ->
        (err Unterminated_antiquot) @@
          (Location_util.of_positions c.loc lexbuf.lex_curr_p)
    | 6 -> with_store lex_antiquot c lexbuf
    | 7 -> with_store lex_antiquot c lexbuf
    | _ -> failwith "lexing: empty token"))
and lex_quotation c (lexbuf : Lexing.lexbuf) =
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
       else (lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
    else
      (let i = lexbuf.Lexing.lex_curr_pos in
       let c = (lexbuf.Lexing.lex_buffer).[i] in
       lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
  and __ocaml_lex_state0 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state6 lexbuf
    | 256 -> __ocaml_lex_state3 lexbuf
    | 123 -> __ocaml_lex_state9 lexbuf
    | 34 -> __ocaml_lex_state4 lexbuf
    | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 -> __ocaml_lex_state8 lexbuf
    | 13 -> __ocaml_lex_state5 lexbuf
    | 124 -> __ocaml_lex_state7 lexbuf
    | 39 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 6
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 10|13|256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | 92 -> __ocaml_lex_state21 lexbuf
     | _ -> __ocaml_lex_state22 lexbuf)
  and __ocaml_lex_state3 lexbuf = 4
  and __ocaml_lex_state4 lexbuf = 3
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 2;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state6 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state6 lexbuf = 2
  and __ocaml_lex_state7 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 125 -> __ocaml_lex_state20 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state8 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 124 -> __ocaml_lex_state18 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state9 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 64 -> __ocaml_lex_state11 lexbuf
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(2) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state10 lexbuf)
     | 58 -> __ocaml_lex_state12 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state10 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 0;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state17 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state11 lexbuf =
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
        -> __ocaml_lex_state16 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state12 lexbuf =
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
                                                            |252|253|254|255
        -> __ocaml_lex_state13 lexbuf
    | 46 -> __ocaml_lex_state15 lexbuf
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
        -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state13 lexbuf =
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
        -> __ocaml_lex_state13 lexbuf
    | 64 -> __ocaml_lex_state11 lexbuf
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(2) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state10 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state14 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 46 -> __ocaml_lex_state15 lexbuf
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
        -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state15 lexbuf =
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
                                                            |252|253|254|255
        -> __ocaml_lex_state13 lexbuf
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
        -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state16 lexbuf =
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
        -> __ocaml_lex_state16 lexbuf
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(2) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state10 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state17 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(2); 0
  and __ocaml_lex_state18 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 125 -> __ocaml_lex_state19 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state19 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1); 1
  and __ocaml_lex_state20 lexbuf = (lexbuf.Lexing.lex_mem).(0) <- (-1); 1
  and __ocaml_lex_state21 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state22 lexbuf
    | 120 -> __ocaml_lex_state24 lexbuf
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state25 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state22 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 39 -> __ocaml_lex_state23 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state23 lexbuf = 5
  and __ocaml_lex_state24 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state27 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state25 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state26 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state26 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state22 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state27 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state22 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action) in
  (__ocaml_lex_init_lexbuf lexbuf 3;
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
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        (store c lexbuf;
         Stack.push p opt_char;
         with_curr_loc lex_quotation c lexbuf;
         lex_quotation c lexbuf)
    | 1 ->
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        if not (Stack.is_empty opt_char)
        then
          let top = Stack.top opt_char in
          (if p <> top
           then with_store lex_quotation c lexbuf
           else (ignore (Stack.pop opt_char); store c lexbuf))
        else with_store lex_quotation c lexbuf
    | 2 -> (update_loc lexbuf; with_store lex_quotation c lexbuf)
    | 3 ->
        (store c lexbuf;
         with_curr_loc lex_string c lexbuf;
         Buffer.add_char c.buffer '"';
         lex_quotation c lexbuf)
    | 4 ->
        (show_stack ();
         (err Unterminated_quotation) @@
           (Location_util.of_positions c.loc lexbuf.lex_curr_p))
    | 5 -> with_store lex_quotation c lexbuf
    | 6 -> with_store lex_quotation c lexbuf
    | _ -> failwith "lexing: empty token"))
let rec lex_simple_quotation depth c (lexbuf : Lexing.lexbuf) =
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
       else (lexbuf.Lexing.refill_buff lexbuf; __ocaml_lex_next_char lexbuf))
    else
      (let i = lexbuf.Lexing.lex_curr_pos in
       let c = (lexbuf.Lexing.lex_buffer).[i] in
       lexbuf.Lexing.lex_curr_pos <- i + 1; Char.code c)
  and __ocaml_lex_state0 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state6 lexbuf
    | 256 -> __ocaml_lex_state3 lexbuf
    | 125 -> __ocaml_lex_state9 lexbuf
    | 34 -> __ocaml_lex_state4 lexbuf
    | 123 -> __ocaml_lex_state8 lexbuf
    | 13 -> __ocaml_lex_state5 lexbuf
    | 40 -> __ocaml_lex_state7 lexbuf
    | 39 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 7
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 7;
    (match __ocaml_lex_next_char lexbuf with
     | 92 -> __ocaml_lex_state11 lexbuf
     | 10|13|256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | _ -> __ocaml_lex_state12 lexbuf)
  and __ocaml_lex_state3 lexbuf = 5
  and __ocaml_lex_state4 lexbuf = 4
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 3;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state6 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state6 lexbuf = 3
  and __ocaml_lex_state7 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 7;
    (match __ocaml_lex_next_char lexbuf with
     | 42 -> __ocaml_lex_state10 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state8 lexbuf = 1
  and __ocaml_lex_state9 lexbuf = 0
  and __ocaml_lex_state10 lexbuf = 2
  and __ocaml_lex_state11 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state15 lexbuf
    | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state12 lexbuf
    | 120 -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state12 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 39 -> __ocaml_lex_state13 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state13 lexbuf = 6
  and __ocaml_lex_state14 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state17 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state15 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state16 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state16 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state12 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state17 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state12 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action) in
  __ocaml_lex_init_lexbuf lexbuf 0;
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
        if depth > 0
        then with_store (lex_simple_quotation (depth - 1)) c lexbuf
        else ()
    | 1 ->
        (store c lexbuf;
         with_curr_loc (lex_simple_quotation (depth + 1)) c lexbuf)
    | 2 ->
        (with_store lex_comment { c with loc = (lexbuf.lex_start_p) } lexbuf;
         lex_simple_quotation depth c lexbuf)
    | 3 ->
        (update_loc lexbuf; with_store (lex_simple_quotation depth) c lexbuf)
    | 4 ->
        (store c lexbuf;
         with_curr_loc lex_string c lexbuf;
         Buffer.add_char c.buffer '"';
         lex_simple_quotation depth c lexbuf)
    | 5 -> (err Unterminated_quotation) @@ (c.loc -- lexbuf.lex_curr_p)
    | 6 -> with_store (lex_simple_quotation depth) c lexbuf
    | 7 -> with_store (lex_simple_quotation depth) c lexbuf
    | _ -> failwith "lexing: empty token"))
let _ =
  Printexc.register_printer @@
    (function | Lexing_error e -> Some (lex_error_to_string e) | _ -> None)