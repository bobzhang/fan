open LibUtil
open Format
open Lexing
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
let lex_error_to_string = to_string_of_printer print_lex_error
let debug = ref false
let opt_char_len = function | Some _ -> 1 | None  -> 0
let print_opt_char fmt =
  function | Some c -> fprintf fmt "Some %c" c | None  -> fprintf fmt "None"
module Stack =
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
let store c lexbuf = c.buffer ++ (Lexing.lexeme lexbuf)
let with_store f c lexbuf = store c lexbuf; f c lexbuf
let buff_contents c =
  let contents = Buffer.contents c.buffer in Buffer.reset c.buffer; contents
let move_curr_p shift lexbuf =
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos + shift
let with_curr_loc lexer c lexbuf =
  lexer { c with loc = (Lexing.lexeme_start_p lexbuf) } lexbuf
let mk_quotation quotation c lexbuf ~name  ~loc  ~shift  ~retract  =
  let old = lexbuf.lex_start_p in
  let s =
    with_curr_loc quotation c lexbuf;
    lexbuf.lex_start_p <- old;
    buff_contents c in
  let content = String.sub s 0 ((String.length s) - retract) in
  `Quot { FToken.name = name; loc; shift; content }
let update_loc ?file  ?(absolute= false)  ?(retract= 0)  ?(line= 1)  lexbuf =
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
let rec lex_comment c lexbuf =
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
let rec lex_string c lexbuf =
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
let rec lex_antiquot c lexbuf =
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
    | 0 -> (store c lexbuf; lexbuf.lex_start_p <- c.loc)
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
and lex_quotation c lexbuf =
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
let token lexbuf =
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
    | 36 -> __ocaml_lex_state3 lexbuf
    | 42 -> __ocaml_lex_state6 lexbuf
    | 39 -> __ocaml_lex_state20 lexbuf
    | 44|96|125 -> __ocaml_lex_state13 lexbuf
    | 63 -> __ocaml_lex_state26 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state11 lexbuf
    | 49|50|51|52|53|54|55|56|57 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state22 lexbuf)
    | 62 -> __ocaml_lex_state9 lexbuf
    | 9|12|32 -> __ocaml_lex_state4 lexbuf
    | 59 -> __ocaml_lex_state15 lexbuf
    | 60|61 -> __ocaml_lex_state8 lexbuf
    | 13 -> __ocaml_lex_state28 lexbuf
    | 46 -> __ocaml_lex_state17 lexbuf
    | 48 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state23 lexbuf)
    | 10 -> __ocaml_lex_state29 lexbuf
    | 126 -> __ocaml_lex_state27 lexbuf
    | 33|43|45|92 -> __ocaml_lex_state5 lexbuf
    | 35 ->
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state18 lexbuf)
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
        -> __ocaml_lex_state24 lexbuf
    | 40 ->
        ((lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state19 lexbuf)
    | 41|93 -> __ocaml_lex_state7 lexbuf
    | 58 -> __ocaml_lex_state16 lexbuf
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
        -> __ocaml_lex_state25 lexbuf
    | 34 -> __ocaml_lex_state21 lexbuf
    | 124 -> __ocaml_lex_state10 lexbuf
    | 91 -> __ocaml_lex_state12 lexbuf
    | 123 -> __ocaml_lex_state14 lexbuf
    | 256 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 30
  and __ocaml_lex_state2 lexbuf = 29
  and __ocaml_lex_state3 lexbuf = 28
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
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state5 lexbuf
     | 41 -> __ocaml_lex_state144 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state7 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 41|46|58|60|61|62|93|124 -> __ocaml_lex_state102 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state109 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state8 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state139 lexbuf
     | 41|93 -> __ocaml_lex_state102 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state11 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state8 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state9 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 125 -> __ocaml_lex_state13 lexbuf
     | 58|124 -> __ocaml_lex_state141 lexbuf
     | 41 -> __ocaml_lex_state102 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state140 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state142 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 93 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state10 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state139 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state11 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state8 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 41|93 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state11 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state138 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state11 lexbuf
     | 41|93 -> __ocaml_lex_state107 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state12 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 46 -> __ocaml_lex_state135 lexbuf
     | 58|60|61|62|124 -> __ocaml_lex_state136 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state137 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state13 lexbuf = 14
  and __ocaml_lex_state14 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 60 -> __ocaml_lex_state13 lexbuf
     | 64 -> __ocaml_lex_state112 lexbuf
     | 58 -> __ocaml_lex_state111 lexbuf
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(16) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state113 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state15 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 59 -> __ocaml_lex_state13 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state16 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|47|64|94 -> __ocaml_lex_state105 lexbuf
     | 124 -> __ocaml_lex_state104 lexbuf
     | 46|60 -> __ocaml_lex_state103 lexbuf
     | 61|62 -> __ocaml_lex_state106 lexbuf
     | 41|93 -> __ocaml_lex_state7 lexbuf
     | 58 -> __ocaml_lex_state110 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state17 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 41|93 -> __ocaml_lex_state102 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state105 lexbuf
     | 58|124 -> __ocaml_lex_state104 lexbuf
     | 60|61|62 -> __ocaml_lex_state103 lexbuf
     | 46 -> __ocaml_lex_state106 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state18 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57 ->
         ((lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state85 lexbuf)
     | 123 -> __ocaml_lex_state87 lexbuf
     | 9|32 ->
         ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state86 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state19 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 111 -> __ocaml_lex_state61 lexbuf
     | 42 -> __ocaml_lex_state57 lexbuf
     | 97 -> __ocaml_lex_state58 lexbuf
     | 9|12|32 ->
         ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
          (lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state62 lexbuf)
     | 33|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state63 lexbuf)
     | 37|38|47|64|94 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state66 lexbuf)
     | 108 -> __ocaml_lex_state59 lexbuf
     | 109 -> __ocaml_lex_state60 lexbuf
     | 46|60|61|62 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state64 lexbuf)
     | 58|124 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state65 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state20 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state48 lexbuf
     | 92 -> __ocaml_lex_state45 lexbuf
     | 13 -> __ocaml_lex_state47 lexbuf
     | 256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | _ -> __ocaml_lex_state46 lexbuf)
  and __ocaml_lex_state21 lexbuf = 7
  and __ocaml_lex_state22 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|95 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state22 lexbuf)
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 46 -> __ocaml_lex_state35 lexbuf
     | 69|101 -> __ocaml_lex_state34 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state23 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|95 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state22 lexbuf)
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 79|111 -> __ocaml_lex_state38 lexbuf
     | 46 -> __ocaml_lex_state35 lexbuf
     | 66|98 -> __ocaml_lex_state37 lexbuf
     | 69|101 -> __ocaml_lex_state34 lexbuf
     | 88|120 -> __ocaml_lex_state39 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state24 lexbuf =
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
         -> __ocaml_lex_state24 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state25 lexbuf =
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
         -> __ocaml_lex_state25 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state26 lexbuf =
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
                                                             |252|253|254|255
         -> __ocaml_lex_state32 lexbuf
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state27 lexbuf =
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
                                                             |252|253|254|255
         -> __ocaml_lex_state30 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state28 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 0;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state29 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state29 lexbuf = 0
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
        -> __ocaml_lex_state30 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state31 lexbuf = 1
  and __ocaml_lex_state32 lexbuf =
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
        -> __ocaml_lex_state32 lexbuf
    | 58 -> __ocaml_lex_state33 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state33 lexbuf = 2
  and __ocaml_lex_state34 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 43|45 -> __ocaml_lex_state44 lexbuf
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state43 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state35 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state35 lexbuf
     | 69|101 -> __ocaml_lex_state34 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state36 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(6); 5
  and __ocaml_lex_state37 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state42 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state38 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state41 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state39 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state40 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state40 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 48
       |49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|95|97|98|99|100|101|102
         ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state40 lexbuf)
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state41 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 48|49|50|51|52|53|54|55|95 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state41 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state42 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 48|49|95 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state42 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state43 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state43 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state44 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state43 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state45 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 120 -> __ocaml_lex_state52 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state53 lexbuf
    | 32|34|39|92|98|110|114|116 -> __ocaml_lex_state54 lexbuf
    | _ -> __ocaml_lex_state51 lexbuf
  and __ocaml_lex_state46 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 39 -> __ocaml_lex_state50 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state47 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state48 lexbuf
    | 39 -> __ocaml_lex_state49 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state48 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 39 -> __ocaml_lex_state49 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state49 lexbuf = 8
  and __ocaml_lex_state50 lexbuf = 9
  and __ocaml_lex_state51 lexbuf = 10
  and __ocaml_lex_state52 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 10;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
         -> __ocaml_lex_state56 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state53 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 10;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state55 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state54 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 10;
    (match __ocaml_lex_next_char lexbuf with
     | 39 -> __ocaml_lex_state50 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state55 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state46 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state56 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state46 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state57 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 17;
    (match __ocaml_lex_next_char lexbuf with
     | 41 -> __ocaml_lex_state84 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state58 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 115 -> __ocaml_lex_state83 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state59 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 120 -> __ocaml_lex_state78 lexbuf
    | 115 -> __ocaml_lex_state77 lexbuf
    | 97 -> __ocaml_lex_state80 lexbuf
    | 111 -> __ocaml_lex_state79 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state60 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 111 -> __ocaml_lex_state76 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state61 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 114 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state62 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 111 -> __ocaml_lex_state61 lexbuf
    | 97 -> __ocaml_lex_state58 lexbuf
    | 9|12|32 ->
        ((lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state62 lexbuf)
    | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
        ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state71 lexbuf)
    | 108 -> __ocaml_lex_state59 lexbuf
    | 109 -> __ocaml_lex_state60 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state63 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 -> __ocaml_lex_state69 lexbuf
    | 41 -> __ocaml_lex_state68 lexbuf
    | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
        ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state63 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state64 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 -> __ocaml_lex_state69 lexbuf
    | 41 -> __ocaml_lex_state68 lexbuf
    | 33|42|43|45|63|92|126 ->
        ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state63 lexbuf)
    | 37|38|47|64|94 ->
        ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state66 lexbuf)
    | 46|58|60|61|62|124 ->
        ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state64 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state65 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 9|12|32 -> __ocaml_lex_state69 lexbuf
     | 41 -> __ocaml_lex_state68 lexbuf
     | 33|42|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state63 lexbuf)
     | 37|38|47|64|94 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state70 lexbuf)
     | 46|58|60|61|62|124 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state65 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state66 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 40|91 -> __ocaml_lex_state67 lexbuf
     | 9|12|32 -> __ocaml_lex_state69 lexbuf
     | 41 -> __ocaml_lex_state68 lexbuf
     | 33|42|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state63 lexbuf)
     | 37|38|46|47|58|60|61|62|64|94|124 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state66 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state67 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|40|46|47|58|60|61|62|64|91|94|124 -> __ocaml_lex_state67 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state68 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(8); 11
  and __ocaml_lex_state69 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 -> __ocaml_lex_state69 lexbuf
    | 41 -> __ocaml_lex_state68 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state70 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 40|91 -> __ocaml_lex_state67 lexbuf
     | 9|12|32 -> __ocaml_lex_state69 lexbuf
     | 41 -> __ocaml_lex_state68 lexbuf
     | 33|42|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state63 lexbuf)
     | 37|38|46|47|58|60|61|62|64|94|124 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state70 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state71 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 -> __ocaml_lex_state73 lexbuf
    | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
        ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state71 lexbuf)
    | 41 -> __ocaml_lex_state72 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state72 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(7);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(9);
    12
  and __ocaml_lex_state73 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 -> __ocaml_lex_state73 lexbuf
    | 41 -> __ocaml_lex_state72 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state74 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41 -> __ocaml_lex_state75 lexbuf
    | 9|12|32 -> __ocaml_lex_state74 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state75 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(5);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(10);
    13
  and __ocaml_lex_state76 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 100 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state77 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 108|114 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state78 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 111 -> __ocaml_lex_state82 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state79 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 114 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state80 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 110 -> __ocaml_lex_state81 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state81 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 100 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state82 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 114 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state83 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 114 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state74 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state84 lexbuf = 18
  and __ocaml_lex_state85 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 ->
        ((lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state85 lexbuf)
    | 34 ->
        ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state97 lexbuf)
    | 9|32 -> __ocaml_lex_state98 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 10 -> __ocaml_lex_state95 lexbuf
    | 13 -> __ocaml_lex_state94 lexbuf
    | _ -> __ocaml_lex_state96 lexbuf
  and __ocaml_lex_state86 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57 ->
        ((lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state85 lexbuf)
    | 9|32 ->
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state86 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state87 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 58 -> __ocaml_lex_state88 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state88 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 46 -> __ocaml_lex_state91 lexbuf
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
        ->
        ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
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
                                                        |218|219|220|221|222
        -> __ocaml_lex_state90 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state89 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(13) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state92 lexbuf)
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
        ->
        ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state89 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state90 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 46 -> __ocaml_lex_state91 lexbuf
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
        -> __ocaml_lex_state90 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state91 lexbuf =
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
        ->
        ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
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
                                                        |218|219|220|221|222
        -> __ocaml_lex_state90 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state92 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(12);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state93 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state93 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(13);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(12);
    26
  and __ocaml_lex_state94 lexbuf =
    (lexbuf.Lexing.lex_mem).(3) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(11);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 27;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state95 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state95 lexbuf =
    (lexbuf.Lexing.lex_mem).(3) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(11);
    27
  and __ocaml_lex_state96 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 10 -> __ocaml_lex_state95 lexbuf
    | 13 -> __ocaml_lex_state94 lexbuf
    | _ -> __ocaml_lex_state96 lexbuf
  and __ocaml_lex_state97 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 34 -> __ocaml_lex_state99 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 10 -> __ocaml_lex_state95 lexbuf
    | 13 -> __ocaml_lex_state94 lexbuf
    | _ ->
        ((lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state97 lexbuf)
  and __ocaml_lex_state98 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 34 ->
        ((lexbuf.Lexing.lex_mem).(14) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(15) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state97 lexbuf)
    | 9|32 -> __ocaml_lex_state98 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 10 -> __ocaml_lex_state95 lexbuf
    | 13 -> __ocaml_lex_state94 lexbuf
    | _ -> __ocaml_lex_state96 lexbuf
  and __ocaml_lex_state99 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state101 lexbuf
    | 13 -> __ocaml_lex_state100 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | _ -> __ocaml_lex_state99 lexbuf
  and __ocaml_lex_state100 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(11);
    (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(14);
    (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(15);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 27;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state101 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state101 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(11);
    (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(14);
    (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(15);
    27
  and __ocaml_lex_state102 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41|46|58|60|61|62|93|124 -> __ocaml_lex_state102 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state109 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state103 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41|93 -> __ocaml_lex_state102 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state105 lexbuf
    | 58|124 -> __ocaml_lex_state104 lexbuf
    | 46|60|61|62 -> __ocaml_lex_state103 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state104 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 93 -> __ocaml_lex_state102 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state105 lexbuf
    | 58|124 -> __ocaml_lex_state104 lexbuf
    | 46|60|61|62 -> __ocaml_lex_state103 lexbuf
    | 41 -> __ocaml_lex_state7 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state105 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 58|124 -> __ocaml_lex_state108 lexbuf
    | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state105 lexbuf
    | 41|93 -> __ocaml_lex_state107 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state106 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 41|93 -> __ocaml_lex_state102 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state105 lexbuf
     | 58|124 -> __ocaml_lex_state104 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state103 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state107 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 41|93 -> __ocaml_lex_state107 lexbuf
     | 37|38|46|47|58|60|61|62|64|94|124 -> __ocaml_lex_state109 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state108 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 58|124 -> __ocaml_lex_state108 lexbuf
    | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state105 lexbuf
    | 41|93 -> __ocaml_lex_state107 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state109 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41|93 -> __ocaml_lex_state107 lexbuf
    | 37|38|46|47|58|60|61|62|64|94|124 -> __ocaml_lex_state109 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state110 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 93 -> __ocaml_lex_state102 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state105 lexbuf
     | 58|124 -> __ocaml_lex_state104 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state103 lexbuf
     | 41 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state111 lexbuf =
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
        ->
        ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state123 lexbuf)
    | 46 -> __ocaml_lex_state125 lexbuf
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
        -> __ocaml_lex_state124 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | _ -> __ocaml_lex_state122 lexbuf
  and __ocaml_lex_state112 lexbuf =
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
        ->
        ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state118 lexbuf)
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | _ -> __ocaml_lex_state117 lexbuf
  and __ocaml_lex_state113 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 19;
    (match __ocaml_lex_next_char lexbuf with
     | 124 -> __ocaml_lex_state114 lexbuf
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state115 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state114 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 125 -> __ocaml_lex_state116 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state115 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(16); 19
  and __ocaml_lex_state116 lexbuf = 20
  and __ocaml_lex_state117 lexbuf = 22
  and __ocaml_lex_state118 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 22;
    (match __ocaml_lex_next_char lexbuf with
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state119 lexbuf)
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
         ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state120 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state119 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(17);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 21;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state121 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state120 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(18) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state119 lexbuf)
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
        ->
        ((lexbuf.Lexing.lex_mem).(17) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state120 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state121 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(18);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(17);
    21
  and __ocaml_lex_state122 lexbuf = 25
  and __ocaml_lex_state123 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 25;
    (match __ocaml_lex_next_char lexbuf with
     | 64 -> __ocaml_lex_state129 lexbuf
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
          (lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state126 lexbuf)
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(21) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state130 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state124 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 25;
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
         -> __ocaml_lex_state127 lexbuf
     | 46 -> __ocaml_lex_state128 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state125 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 25;
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
                                                         |218|219|220|221|222
         -> __ocaml_lex_state127 lexbuf
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
         ->
         ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
          (lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state126 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state126 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 64 -> __ocaml_lex_state129 lexbuf
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
        ->
        ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state126 lexbuf)
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(21) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state130 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state127 lexbuf =
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
        -> __ocaml_lex_state127 lexbuf
    | 46 -> __ocaml_lex_state128 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state128 lexbuf =
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
                                                        |218|219|220|221|222
        -> __ocaml_lex_state127 lexbuf
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
        ->
        ((lexbuf.Lexing.lex_mem).(20) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(19) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state126 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state129 lexbuf =
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
        ->
        ((lexbuf.Lexing.lex_mem).(22) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state132 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state130 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(19);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 23;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state131 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state131 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(21);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(19);
    23
  and __ocaml_lex_state132 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(23) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state133 lexbuf)
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
        ->
        ((lexbuf.Lexing.lex_mem).(22) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state132 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state133 lexbuf =
    (lexbuf.Lexing.lex_mem).(2) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(20);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(22);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state134 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state134 lexbuf =
    (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(23);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(20);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(22);
    24
  and __ocaml_lex_state135 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 46|58|61|62|124 -> __ocaml_lex_state135 lexbuf
    | 60 -> __ocaml_lex_state136 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state137 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state136 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 46|58|61|62|124 -> __ocaml_lex_state135 lexbuf
     | 60 -> __ocaml_lex_state136 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state137 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state137 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 40|91 -> __ocaml_lex_state67 lexbuf
     | 37|38|46|47|58|60|61|62|64|94|124 -> __ocaml_lex_state137 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state138 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state138 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state11 lexbuf
     | 41|93 -> __ocaml_lex_state107 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state139 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state139 lexbuf
     | 93 -> __ocaml_lex_state102 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state11 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state8 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 41 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state140 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state141 lexbuf
     | 41 -> __ocaml_lex_state102 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state140 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state142 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 93 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state141 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state141 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state140 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state142 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 41|93 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state142 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 41|93 -> __ocaml_lex_state107 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state142 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 58|124 -> __ocaml_lex_state143 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state143 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 14;
    (match __ocaml_lex_next_char lexbuf with
     | 41|93 -> __ocaml_lex_state107 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state142 lexbuf
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state5 lexbuf
     | 58|124 -> __ocaml_lex_state143 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state144 lexbuf = 15 in
  __ocaml_lex_init_lexbuf lexbuf 24;
  (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
   lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
   lexbuf.Lexing.lex_curr_p <-
     {
       (lexbuf.Lexing.lex_curr_p) with
       Lexing.pos_cnum =
         (lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos)
     };
   (match __ocaml_lex_result with
    | 0 -> (update_loc lexbuf; `NEWLINE)
    | 1 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        `LABEL x
    | 2 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        `OPTLABEL x
    | 3 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Lid x
    | 4 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Uid x
    | 5 ->
        let s =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        (match s with
         | Some 'l' -> `Int32 x
         | Some 'L' -> `Int64 x
         | Some 'n' -> `Nativeint x
         | _ -> `Int x)
    | 6 ->
        let f =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Flo f
    | 7 ->
        let c = default_cxt lexbuf in
        let old = lexbuf.lex_start_p in
        (with_curr_loc lex_string c lexbuf;
         lexbuf.lex_start_p <- old;
         `Str (buff_contents c))
    | 8 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        (update_loc lexbuf ~retract:1; `Chr x)
    | 9 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        `Chr x
    | 10 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
        (err (Illegal_escape (String.make 1 c))) @@
          (Location_util.from_lexbuf lexbuf)
    | 11 ->
        let op =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        `Eident op
    | 12 ->
        let op =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        `Eident op
    | 13 ->
        let op =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        `Eident op
    | 14 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Sym x
    | 15 ->
        (warn Comment_not_end (Location_util.from_lexbuf lexbuf);
         move_curr_p (-1) lexbuf;
         `Sym "*")
    | 16 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `BLANKS x
    | 17 ->
        let c = default_cxt lexbuf in
        let old = lexbuf.lex_start_p in
        (store c lexbuf;
         with_curr_loc lex_comment c lexbuf;
         lexbuf.lex_start_p <- old;
         `COMMENT (buff_contents c))
    | 18 ->
        let c = default_cxt lexbuf in
        let old = lexbuf.lex_start_p in
        (warn Comment_start (Location_util.from_lexbuf lexbuf);
         lex_comment c lexbuf;
         lexbuf.lex_start_p <- old;
         `COMMENT (buff_contents c))
    | 19 ->
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        let c = default_cxt lexbuf in
        (Stack.push p opt_char;
         (let len = 2 + (opt_char_len p) in
          mk_quotation lex_quotation c lexbuf ~name:FToken.empty_name ~loc:""
            ~shift:len ~retract:len))
    | 20 ->
        `Quot
          {
            FToken.name = FToken.empty_name;
            loc = "";
            shift = 2;
            content = ""
          }
    | 21 ->
        let loc =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2)
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        let c = default_cxt lexbuf in
        (Stack.push p opt_char;
         mk_quotation lex_quotation c lexbuf ~name:FToken.empty_name ~loc
           ~shift:(((2 + 1) + (String.length loc)) + (opt_char_len p))
           ~retract:(2 + (opt_char_len p)))
    | 22 ->
        let c =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_start_pos + 3) in
        (err (Illegal_quotation c)) @@ (Location_util.from_lexbuf lexbuf)
    | 23 ->
        let name =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2)
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        let c = default_cxt lexbuf in
        let len = String.length name in
        let name = FToken.name_of_string name in
        (Stack.push p opt_char;
         mk_quotation lex_quotation c lexbuf ~name ~loc:""
           ~shift:(((2 + 1) + len) + (opt_char_len p))
           ~retract:(2 + (opt_char_len p)))
    | 24 ->
        let name =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2)
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and loc =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 1)
            (((lexbuf.Lexing.lex_mem).(1)) + 0)
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(2)) + 0) in
        let c = default_cxt lexbuf in
        let len = String.length name in
        let name = FToken.name_of_string name in
        (Stack.push p opt_char;
         mk_quotation lex_quotation c lexbuf ~name ~loc
           ~shift:((((2 + 2) + (String.length loc)) + len) + (opt_char_len p))
           ~retract:(2 + (opt_char_len p)))
    | 25 ->
        let c =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_start_pos + 3) in
        (err (Illegal_quotation c)) @@ (Location_util.from_lexbuf lexbuf)
    | 26 ->
        let name =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 3)
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        let c = default_cxt lexbuf in
        let len = String.length name in
        let () = Stack.push p opt_char in
        let retract = (opt_char_len p) + 2 in
        let old = lexbuf.lex_start_p in
        let s =
          with_curr_loc lex_quotation c lexbuf;
          lexbuf.lex_start_p <- old;
          buff_contents c in
        let contents = String.sub s 0 ((String.length s) - retract) in
        `DirQuotation ((((3 + 1) + len) + (opt_char_len p)), name, contents)
    | 27 ->
        let num =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (((lexbuf.Lexing.lex_mem).(1)) + 0)
        and name =
          Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(3)) + 0)
            (((lexbuf.Lexing.lex_mem).(2)) + 0) in
        let inum = int_of_string num in
        (update_loc lexbuf ?file:name ~line:inum ~absolute:true;
         `LINE_DIRECTIVE (inum, name))
    | 28 ->
        let dollar c lexbuf =
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
                                                                  |250
                                                                   |251
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
                                                                  |208
                                                                   |209
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
                                                                  |111
                                                                   |112
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
                                                                  |111
                                                                   |112
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
                                                                  |111
                                                                   |112
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
                                                                  |111
                                                                   |112
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
                                                                  |111
                                                                   |112
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
                                                                  |112
                                                                   |113
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
                                                                  |112
                                                                   |113
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
                                                                  |111
                                                                   |112
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
                                                                  |112
                                                                   |113
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
                                                                  |112
                                                                   |113
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
                  Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
                    (((lexbuf.Lexing.lex_mem).(0)) + 0)
                and x =
                  Lexing.sub_lexeme lexbuf
                    (((lexbuf.Lexing.lex_mem).(0)) + 1)
                    (lexbuf.Lexing.lex_curr_pos + 0) in
                (lexbuf.lex_start_p <-
                   FLoc.move_pos ((String.length name) + 1)
                     lexbuf.lex_start_p;
                 `Ant (name, x))
            | 1 ->
                let x =
                  Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
                    (lexbuf.Lexing.lex_curr_pos + 0) in
                `Ant ("", x)
            | 2 ->
                let name =
                  Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
                    (lexbuf.Lexing.lex_curr_pos + (-1)) in
                (c.buffer +> '(';
                 lex_antiquot
                   {
                     c with
                     loc =
                       (FLoc.move_pos
                          ((((1 + 1) + 1) + (String.length name)) - 1) 
                          c.loc)
                   } lexbuf;
                 `Ant (name, (buff_contents c)))
            | 3 ->
                (c.buffer +> '(';
                 lex_antiquot
                   { c with loc = (FLoc.move_pos ((1 + 1) - 1) c.loc) }
                   lexbuf;
                 `Ant ("", (buff_contents c)))
            | 4 ->
                let c =
                  Lexing.sub_lexeme_char lexbuf
                    (lexbuf.Lexing.lex_start_pos + 0) in
                err (Illegal_character c) (Location_util.from_lexbuf lexbuf)
            | _ -> failwith "lexing: empty token")) in
        let c = default_cxt lexbuf in
        if FConfig.antiquotations.contents
        then with_curr_loc dollar c lexbuf
        else err Illegal_antiquote (Location_util.from_lexbuf lexbuf)
    | 29 ->
        let pos = lexbuf.lex_curr_p in
        (lexbuf.lex_curr_p <-
           {
             pos with
             pos_bol = (pos.pos_bol + 1);
             pos_cnum = (pos.pos_cnum + 1)
           };
         `EOI)
    | 30 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
        (err (Illegal_character c)) @@ (Location_util.from_lexbuf lexbuf)
    | _ -> failwith "lexing: empty token"))
let from_lexbuf lb =
  let next _ =
    let tok = token lb in
    let loc = Location_util.from_lexbuf lb in Some (tok, loc) in
  XStream.from next
let _ =
  Printexc.register_printer @@
    (function | Lexing_error e -> Some (lex_error_to_string e) | _ -> None)