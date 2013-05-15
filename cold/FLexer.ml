open FanUtil

open LibUtil

open Format

open Lexing

type lex_error =  
  | Illegal_character of char
  | Illegal_escape of string
  | Illegal_quotation of string
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_quotation
  | Unterminated_antiquot
  | Unterminated_string_in_comment
  | Unterminated_string_in_quotation
  | Unterminated_string_in_antiquot
  | Comment_start
  | Comment_not_end
  | Literal_overflow of string 

exception Lexing_error of lex_error

let print_lex_error ppf =
  function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Illegal_quotation s ->
      fprintf ppf "Illegal quotation (%s)" (String.escaped s)
  | Illegal_escape s ->
      fprintf ppf "Illegal backslash escape in string or character (%s)" s
  | Unterminated_comment  -> fprintf ppf "Comment not terminated"
  | Unterminated_string  -> fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment  ->
      fprintf ppf "This comment contains an unterminated string literal"
  | Unterminated_string_in_quotation  ->
      fprintf ppf "This quotation contains an unterminated string literal"
  | Unterminated_string_in_antiquot  ->
      fprintf ppf "This antiquotaion contains an unterminated string literal"
  | Unterminated_quotation  -> fprintf ppf "Quotation not terminated"
  | Unterminated_antiquot  -> fprintf ppf "Antiquotation not terminated"
  | Literal_overflow ty ->
      fprintf ppf
        "Integer literal exceeds the range of representable integers of type %s"
        ty
  | Comment_start  -> fprintf ppf "this is the start of a comment"
  | Comment_not_end  -> fprintf ppf "this is not the end of a comment"

let lex_error_to_string = to_string_of_printer print_lex_error

let _ =
  Printexc.register_printer
    (function | Lexing_error e -> Some (lex_error_to_string e) | _ -> None)

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

type context = 
  {
  loc: FanLoc.position;
  in_comment: bool;
  quotations: bool;
  antiquots: bool;
  lexbuf: lexbuf;
  buffer: Buffer.t} 

let default_context lb =
  {
    loc = FanLoc.dummy_pos;
    in_comment = false;
    quotations = true;
    antiquots = false;
    lexbuf = lb;
    buffer = (Buffer.create 256)
  }

let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)

let istore_char c i =
  Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)

let buff_contents c =
  let contents = Buffer.contents c.buffer in Buffer.reset c.buffer; contents

let loc_merge c = FanLoc.of_positions c.loc (Lexing.lexeme_end_p c.lexbuf)

let quotations c = c.quotations

let antiquots c = c.antiquots

let is_in_comment c = c.in_comment

let in_comment c = { c with in_comment = true }

let set_start_p c = (c.lexbuf).lex_start_p <- c.loc

let move_curr_p shift c =
  (c.lexbuf).lex_curr_pos <- (c.lexbuf).lex_curr_pos + shift

let move_start_p shift c =
  (c.lexbuf).lex_start_p <- FanLoc.move_pos shift (c.lexbuf).lex_start_p

let with_curr_loc lexer c =
  lexer { c with loc = (Lexing.lexeme_start_p c.lexbuf) } c.lexbuf

let parse_nested ~lexer  c =
  with_curr_loc lexer c; set_start_p c; buff_contents c

let store_parse f c = store c; f c c.lexbuf

let parse f c = f c c.lexbuf

let mk_quotation quotation c ~name  ~loc  ~shift  ~retract  =
  let s =
    parse_nested ~lexer:quotation
      { c with loc = (Lexing.lexeme_start_p c.lexbuf) } in
  let contents = String.sub s 0 ((String.length s) - retract) in
  `QUOTATION
    {
      FanToken.q_name = name;
      q_loc = loc;
      q_shift = shift;
      q_contents = contents
    }

let update_loc ?file  ?(absolute= false)  ?(retract= 0)  ?(line= 1)  c =
  let lexbuf = c.lexbuf in
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with | None  -> pos.pos_fname | Some s -> s in
  lexbuf.lex_curr_p <-
    {
      pos with
      pos_fname = new_file;
      pos_lnum = (if absolute then line else pos.pos_lnum + line);
      pos_bol = (pos.pos_cnum - retract)
    }

let err (error : lex_error) (loc : FanLoc.t) =
  raise (FanLoc.Exc_located (loc, (Lexing_error error)))

let warn error loc =
  Format.eprintf "Warning: %a: %a@." FanLoc.print loc print_lex_error error

let _ = ()

let rec comment c lexbuf =
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
    | 10 -> __ocaml_lex_state3 lexbuf
    | 40 -> __ocaml_lex_state6 lexbuf
    | 256 -> __ocaml_lex_state4 lexbuf
    | 42 -> __ocaml_lex_state5 lexbuf
    | 13 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 4
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 3;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state3 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state3 lexbuf = 3
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
    | 0 -> (store c; with_curr_loc comment c; parse comment c)
    | 1 -> store c
    | 2 -> err Unterminated_comment (loc_merge c)
    | 3 -> (update_loc c; store_parse comment c)
    | 4 -> store_parse comment c
    | _ -> failwith "lexing: empty token"))

let rec string c lexbuf =
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
  and __ocaml_lex_state1 lexbuf = 8
  and __ocaml_lex_state2 lexbuf = 7
  and __ocaml_lex_state3 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state4 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf = 6
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 8;
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
  and __ocaml_lex_state7 lexbuf = 5
  and __ocaml_lex_state8 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
         -> __ocaml_lex_state15 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state9 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
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
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state14 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state14 lexbuf = 3
  and __ocaml_lex_state15 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state16 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state16 lexbuf = 4 in
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
    | 0 -> set_start_p c
    | 1 ->
        let space =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        (update_loc c ~retract:(String.length space); store_parse string c)
    | 2 -> store_parse string c
    | 3 -> store_parse string c
    | 4 -> store_parse string c
    | 5 ->
        let x =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 1) in
        if is_in_comment c
        then store_parse string c
        else
          (warn (Illegal_escape (String.make 1 x)) (FanLoc.of_lexbuf lexbuf);
           store_parse string c)
    | 6 -> (update_loc c; store_parse string c)
    | 7 -> err Unterminated_string (loc_merge c)
    | 8 -> store_parse string c
    | _ -> failwith "lexing: empty token"))

let symbolchar_star beginning _c lexbuf =
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
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 0;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state0 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)) in
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
        let tok =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `SYMBOL (beginning ^ tok)
    | _ -> failwith "lexing: empty token"))

let rec maybe_quotation_at c lexbuf =
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
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
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
        ((lexbuf.Lexing.lex_mem).(2) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state2 lexbuf)
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 1
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 1;
    (match __ocaml_lex_next_char lexbuf with
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state3 lexbuf)
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
         ((lexbuf.Lexing.lex_mem).(2) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state4 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state3 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(2);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 0;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state3 lexbuf)
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
        ((lexbuf.Lexing.lex_mem).(2) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state4 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state5 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(3);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(2);
    0 in
  __ocaml_lex_init_lexbuf lexbuf 4;
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
        let loc =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        (move_start_p (-2) c;
         Stack.push p opt_char;
         mk_quotation quotation c ~name:FanToken.empty_name ~loc
           ~shift:(((2 + 1) + (String.length loc)) + (opt_char_len p))
           ~retract:(2 + (opt_char_len p)))
    | 1 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
        err (Illegal_quotation (String.make 1 c)) (FanLoc.of_lexbuf lexbuf)
    | _ -> failwith "lexing: empty token"))
and maybe_quotation_colon c lexbuf =
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
        -> __ocaml_lex_state3 lexbuf
    | 46 -> __ocaml_lex_state4 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
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
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state2 lexbuf)
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 2
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 2;
    (match __ocaml_lex_next_char lexbuf with
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state9 lexbuf)
     | 64 -> __ocaml_lex_state8 lexbuf
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
         ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
          (lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state5 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state3 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 2;
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
         -> __ocaml_lex_state6 lexbuf
     | 46 -> __ocaml_lex_state7 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 2;
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
         -> __ocaml_lex_state6 lexbuf
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
         ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
          (lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state5 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state5 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state9 lexbuf)
    | 64 -> __ocaml_lex_state8 lexbuf
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
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state5 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state6 lexbuf =
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
        -> __ocaml_lex_state6 lexbuf
    | 46 -> __ocaml_lex_state7 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state7 lexbuf =
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
        -> __ocaml_lex_state6 lexbuf
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
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(3) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state5 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state8 lexbuf =
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
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state11 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state9 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(3);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 0;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state10 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state10 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(5);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(3);
    0
  and __ocaml_lex_state11 lexbuf =
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
        ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state11 lexbuf)
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state12 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state12 lexbuf =
    (lexbuf.Lexing.lex_mem).(2) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(6);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 1;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state13 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state13 lexbuf =
    (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(7);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(6);
    1 in
  __ocaml_lex_init_lexbuf lexbuf 8;
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
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        let len = String.length name in
        let name = FanToken.resolve_name (FanToken.name_of_string name) in
        (move_start_p (-2) c;
         Stack.push p opt_char;
         mk_quotation quotation c ~name ~loc:""
           ~shift:(((2 + 1) + len) + (opt_char_len p))
           ~retract:(2 + (opt_char_len p)))
    | 1 ->
        let name =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (((lexbuf.Lexing.lex_mem).(0)) + 0)
        and loc =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 1)
            (((lexbuf.Lexing.lex_mem).(1)) + 0)
        and p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(2)) + 0) in
        let len = String.length name in
        let name = FanToken.resolve_name (FanToken.name_of_string name) in
        (move_start_p (-2) c;
         Stack.push p opt_char;
         mk_quotation quotation c ~name ~loc
           ~shift:((((2 + 2) + (String.length loc)) + len) + (opt_char_len p))
           ~retract:(2 + (opt_char_len p)))
    | 2 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
        err (Illegal_quotation (String.make 1 c)) (FanLoc.of_lexbuf lexbuf)
    | _ -> failwith "lexing: empty token"))
and dollar c lexbuf =
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
                                                            |252|253|254|255
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
                                                            |210
                                                             |211
                                                              |212
                                                               |213
                                                                |214
                                                                 |216
                                                                  |217
                                                                   |218
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
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 1)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        (move_start_p ((String.length name) + 1) c; `Ant (name, x))
    | 1 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Ant ("", x)
    | 2 ->
        let name =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        antiquot name 0
          { c with loc = (FanLoc.move_pos (3 + (String.length name)) c.loc) }
          c.lexbuf
    | 3 -> antiquot "" 0 { c with loc = (FanLoc.move_pos 2 c.loc) } c.lexbuf
    | 4 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
        err (Illegal_character c) (FanLoc.of_lexbuf lexbuf)
    | _ -> failwith "lexing: empty token"))
and antiquot name depth c lexbuf =
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
    | 123 -> __ocaml_lex_state3 lexbuf
    | 256 -> __ocaml_lex_state6 lexbuf
    | 13 -> __ocaml_lex_state4 lexbuf
    | 41 -> __ocaml_lex_state8 lexbuf
    | 10 -> __ocaml_lex_state5 lexbuf
    | 40 -> __ocaml_lex_state7 lexbuf
    | 34 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 6
  and __ocaml_lex_state2 lexbuf = 5
  and __ocaml_lex_state3 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 58 -> __ocaml_lex_state11 lexbuf
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state9 lexbuf)
     | 64 -> __ocaml_lex_state10 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 3;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state5 lexbuf = 3
  and __ocaml_lex_state6 lexbuf = 2
  and __ocaml_lex_state7 lexbuf = 1
  and __ocaml_lex_state8 lexbuf = 0
  and __ocaml_lex_state9 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (-1);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 4;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         __ocaml_lex_state14 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state10 lexbuf =
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
        -> __ocaml_lex_state13 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
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
        -> __ocaml_lex_state12 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state12 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state9 lexbuf)
    | 64 -> __ocaml_lex_state10 lexbuf
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
        -> __ocaml_lex_state12 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state13 lexbuf =
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
        -> __ocaml_lex_state13 lexbuf
    | 124 ->
        ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state9 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state14 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1); 4 in
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
        if depth = 0
        then let () = set_start_p c in `Ant (name, (buff_contents c))
        else store_parse (antiquot name (depth - 1)) c
    | 1 -> store_parse (antiquot name (depth + 1)) c
    | 2 -> err Unterminated_antiquot (loc_merge c)
    | 3 -> (update_loc c; store_parse (antiquot name depth) c)
    | 4 ->
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        let () = Stack.push p opt_char in
        let () = store c in
        let () = with_curr_loc quotation c in parse (antiquot name depth) c
    | 5 ->
        (store c;
         (try with_curr_loc string c
          with
          | FanLoc.Exc_located (_,Lexing_error (Unterminated_string )) ->
              err Unterminated_string_in_antiquot (loc_merge c));
         Buffer.add_char c.buffer '"';
         parse (antiquot name depth) c)
    | 6 -> store_parse (antiquot name depth) c
    | _ -> failwith "lexing: empty token"))
and quotation c lexbuf =
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
    | 10 -> __ocaml_lex_state3 lexbuf
    | 34 -> __ocaml_lex_state6 lexbuf
    | 123 -> __ocaml_lex_state9 lexbuf
    | 256 -> __ocaml_lex_state4 lexbuf
    | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 -> __ocaml_lex_state8 lexbuf
    | 39 -> __ocaml_lex_state5 lexbuf
    | 124 -> __ocaml_lex_state7 lexbuf
    | 13 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 6
  and __ocaml_lex_state2 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 5;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state3 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state3 lexbuf = 5
  and __ocaml_lex_state4 lexbuf = 4
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 10|13|256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | 92 -> __ocaml_lex_state21 lexbuf
     | _ -> __ocaml_lex_state22 lexbuf)
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
  and __ocaml_lex_state23 lexbuf = 3
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
        (store c;
         Stack.push p opt_char;
         with_curr_loc quotation c;
         parse quotation c)
    | 1 ->
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        if not (Stack.is_empty opt_char)
        then
          let top = Stack.top opt_char in
          (if p <> top
           then store_parse quotation c
           else (ignore (Stack.pop opt_char); store c))
        else store_parse quotation c
    | 2 ->
        (store c;
         (try with_curr_loc string c
          with
          | FanLoc.Exc_located (_,Lexing_error (Unterminated_string )) ->
              err Unterminated_string_in_quotation (loc_merge c));
         Buffer.add_char c.buffer '"';
         parse quotation c)
    | 3 -> store_parse quotation c
    | 4 -> (show_stack (); err Unterminated_quotation (loc_merge c))
    | 5 -> (update_loc c; store_parse quotation c)
    | 6 -> store_parse quotation c
    | _ -> failwith "lexing: empty token"))

let token c lexbuf =
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
    | 33|43|45|92 -> __ocaml_lex_state3 lexbuf
    | 60|61 -> __ocaml_lex_state6 lexbuf
    | 34 -> __ocaml_lex_state20 lexbuf
    | 46 -> __ocaml_lex_state13 lexbuf
    | 126 -> __ocaml_lex_state26 lexbuf
    | 48 -> __ocaml_lex_state22 lexbuf
    | 59 -> __ocaml_lex_state11 lexbuf
    | 91 -> __ocaml_lex_state9 lexbuf
    | 36 -> __ocaml_lex_state4 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state8 lexbuf
    | 62 -> __ocaml_lex_state15 lexbuf
    | 13 -> __ocaml_lex_state28 lexbuf
    | 42 -> __ocaml_lex_state17 lexbuf
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
        -> __ocaml_lex_state23 lexbuf
    | 9|12|32 -> __ocaml_lex_state27 lexbuf
    | 10 -> __ocaml_lex_state29 lexbuf
    | 41|93 -> __ocaml_lex_state5 lexbuf
    | 40 -> __ocaml_lex_state18 lexbuf
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
        -> __ocaml_lex_state24 lexbuf
    | 39 -> __ocaml_lex_state19 lexbuf
    | 124 -> __ocaml_lex_state7 lexbuf
    | 63 -> __ocaml_lex_state25 lexbuf
    | 123 -> __ocaml_lex_state16 lexbuf
    | 49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state21 lexbuf
    | 44|96|125 -> __ocaml_lex_state10 lexbuf
    | 35 ->
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state14 lexbuf)
    | 58 -> __ocaml_lex_state12 lexbuf
    | 256 -> __ocaml_lex_state2 lexbuf
    | _ -> __ocaml_lex_state1 lexbuf
  and __ocaml_lex_state1 lexbuf = 28
  and __ocaml_lex_state2 lexbuf = 27
  and __ocaml_lex_state3 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state3 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state4 lexbuf = 25
  and __ocaml_lex_state5 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|47|64|94 -> __ocaml_lex_state87 lexbuf
     | 41|46|58|60|61|62|93|124 -> __ocaml_lex_state80 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state6 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state6 lexbuf
     | 58|124 -> __ocaml_lex_state108 lexbuf
     | 41|93 -> __ocaml_lex_state80 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state8 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state7 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state6 lexbuf
     | 58|124 -> __ocaml_lex_state108 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state8 lexbuf
     | 41|93 -> __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state8 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 41|93 -> __ocaml_lex_state85 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state8 lexbuf
     | 58|124 -> __ocaml_lex_state107 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state9 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 46 -> __ocaml_lex_state104 lexbuf
     | 58|60|61|62|124 -> __ocaml_lex_state105 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state106 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state10 lexbuf = 24
  and __ocaml_lex_state11 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 59 -> __ocaml_lex_state10 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state12 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 124 -> __ocaml_lex_state99 lexbuf
     | 61|62 -> __ocaml_lex_state101 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state100 lexbuf
     | 58 -> __ocaml_lex_state103 lexbuf
     | 41|93 -> __ocaml_lex_state5 lexbuf
     | 46|60 -> __ocaml_lex_state98 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state13 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state99 lexbuf
     | 41|93 -> __ocaml_lex_state80 lexbuf
     | 46 -> __ocaml_lex_state101 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state100 lexbuf
     | 60|61|62 -> __ocaml_lex_state98 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state14 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 9|32 ->
         ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state89 lexbuf)
     | 48|49|50|51|52|53|54|55|56|57 ->
         ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state88 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state15 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state81 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state83 lexbuf
     | 41 -> __ocaml_lex_state80 lexbuf
     | 93 -> __ocaml_lex_state5 lexbuf
     | 58|124 -> __ocaml_lex_state82 lexbuf
     | 125 -> __ocaml_lex_state84 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state16 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 64 -> __ocaml_lex_state73 lexbuf
     | 60 -> __ocaml_lex_state75 lexbuf
     | 124 ->
         ((lexbuf.Lexing.lex_mem).(9) <- lexbuf.Lexing.lex_curr_pos;
          (lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state74 lexbuf)
     | 58 -> __ocaml_lex_state72 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state17 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state3 lexbuf
     | 41 -> __ocaml_lex_state71 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state18 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|47|64|94 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state61 lexbuf)
     | 9|12|32 ->
         ((lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state57 lexbuf)
     | 33|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state58 lexbuf)
     | 42 -> __ocaml_lex_state62 lexbuf
     | 46|60|61|62 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state59 lexbuf)
     | 58|124 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state60 lexbuf)
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state19 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state48 lexbuf
     | 92 -> __ocaml_lex_state45 lexbuf
     | 13 -> __ocaml_lex_state47 lexbuf
     | 256 ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)
     | _ -> __ocaml_lex_state46 lexbuf)
  and __ocaml_lex_state20 lexbuf = 8
  and __ocaml_lex_state21 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 46 -> __ocaml_lex_state35 lexbuf
     | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state21 lexbuf
     | 69|101 -> __ocaml_lex_state34 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state22 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 79|111 -> __ocaml_lex_state38 lexbuf
     | 46 -> __ocaml_lex_state35 lexbuf
     | 66|98 -> __ocaml_lex_state37 lexbuf
     | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state21 lexbuf
     | 69|101 -> __ocaml_lex_state34 lexbuf
     | 88|120 -> __ocaml_lex_state39 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state23 lexbuf =
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
         -> __ocaml_lex_state23 lexbuf
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
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state3 lexbuf
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
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state26 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
         __ocaml_lex_state3 lexbuf
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
  and __ocaml_lex_state27 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 1;
    (match __ocaml_lex_next_char lexbuf with
     | 9|12|32 -> __ocaml_lex_state27 lexbuf
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
  and __ocaml_lex_state31 lexbuf = 2
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
  and __ocaml_lex_state33 lexbuf = 3
  and __ocaml_lex_state34 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 43|45 -> __ocaml_lex_state44 lexbuf
    | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state43 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state35 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 7;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|95 -> __ocaml_lex_state35 lexbuf
     | 69|101 -> __ocaml_lex_state34 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state36 lexbuf = 6
  and __ocaml_lex_state37 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49 -> __ocaml_lex_state42 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state38 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55 -> __ocaml_lex_state41 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state39 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102 ->
        __ocaml_lex_state40 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state40 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 48
       |49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|95|97|98|99|100|101|102
         -> __ocaml_lex_state40 lexbuf
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state41 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 48|49|50|51|52|53|54|55|95 -> __ocaml_lex_state41 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state42 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 6;
    (match __ocaml_lex_next_char lexbuf with
     | 76|108|110 -> __ocaml_lex_state36 lexbuf
     | 48|49|95 -> __ocaml_lex_state42 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state43 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 7;
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
  and __ocaml_lex_state49 lexbuf = 9
  and __ocaml_lex_state50 lexbuf = 10
  and __ocaml_lex_state51 lexbuf = 11
  and __ocaml_lex_state52 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 11;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57|65|66|67|68|69|70|97|98|99|100|101|102
         -> __ocaml_lex_state56 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state53 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 11;
    (match __ocaml_lex_next_char lexbuf with
     | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state55 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state54 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 11;
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
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 ->
        ((lexbuf.Lexing.lex_mem).(5) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state57 lexbuf)
    | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
        ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state68 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state58 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state58 lexbuf)
    | 9|12|32 -> __ocaml_lex_state66 lexbuf
    | 41 -> __ocaml_lex_state65 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state59 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 37|38|47|64|94 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state61 lexbuf)
    | 33|42|43|45|63|92|126 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state58 lexbuf)
    | 9|12|32 -> __ocaml_lex_state66 lexbuf
    | 46|58|60|61|62|124 ->
        ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state59 lexbuf)
    | 41 -> __ocaml_lex_state65 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state60 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|47|64|94 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state67 lexbuf)
     | 33|42|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state58 lexbuf)
     | 9|12|32 -> __ocaml_lex_state66 lexbuf
     | 46|58|60|61|62|124 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state60 lexbuf)
     | 41 -> __ocaml_lex_state65 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state61 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|46|47|58|60|61|62|64|94|124 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state61 lexbuf)
     | 33|42|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state58 lexbuf)
     | 9|12|32 -> __ocaml_lex_state66 lexbuf
     | 40|91 -> __ocaml_lex_state64 lexbuf
     | 41 -> __ocaml_lex_state65 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state62 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 12;
    (match __ocaml_lex_next_char lexbuf with
     | 41 -> __ocaml_lex_state63 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state63 lexbuf = 13
  and __ocaml_lex_state64 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|40|46|47|58|60|61|62|64|91|94|124 -> __ocaml_lex_state64 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state65 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(6); 22
  and __ocaml_lex_state66 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|12|32 -> __ocaml_lex_state66 lexbuf
    | 41 -> __ocaml_lex_state65 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state67 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|46|47|58|60|61|62|64|94|124 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state67 lexbuf)
     | 33|42|43|45|63|92|126 ->
         ((lexbuf.Lexing.lex_mem).(6) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state58 lexbuf)
     | 9|12|32 -> __ocaml_lex_state66 lexbuf
     | 40|91 -> __ocaml_lex_state64 lexbuf
     | 41 -> __ocaml_lex_state65 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state68 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41 -> __ocaml_lex_state69 lexbuf
    | 33|37|38|42|43|45|46|47|58|60|61|62|63|64|92|94|124|126 ->
        ((lexbuf.Lexing.lex_mem).(7) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state68 lexbuf)
    | 9|12|32 -> __ocaml_lex_state70 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state69 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(5);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(7);
    23
  and __ocaml_lex_state70 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41 -> __ocaml_lex_state69 lexbuf
    | 9|12|32 -> __ocaml_lex_state70 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state71 lexbuf = 14
  and __ocaml_lex_state72 lexbuf = 20
  and __ocaml_lex_state73 lexbuf = 19
  and __ocaml_lex_state74 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(8);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 17;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|43|45|46|47|58|61|63|64|92|94|126 ->
         ((lexbuf.Lexing.lex_mem).(8) <- lexbuf.Lexing.lex_curr_pos;
          __ocaml_lex_state78 lexbuf)
     | 124 -> __ocaml_lex_state77 lexbuf
     | 42 -> __ocaml_lex_state76 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state75 lexbuf = 15
  and __ocaml_lex_state76 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(8);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 17;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|61|63|64|92|94|124|126 ->
         __ocaml_lex_state76 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state77 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(8);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 17;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|61|63|64|92|94|124|126 ->
         __ocaml_lex_state76 lexbuf
     | 125 -> __ocaml_lex_state79 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state78 lexbuf =
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(9);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(8);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 17;
    (match __ocaml_lex_next_char lexbuf with
     | 33|37|38|42|43|45|46|47|58|61|63|64|92|94|124|126 ->
         __ocaml_lex_state78 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state79 lexbuf = 18
  and __ocaml_lex_state80 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 37|38|47|64|94 -> __ocaml_lex_state87 lexbuf
    | 41|46|58|60|61|62|93|124 -> __ocaml_lex_state80 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state81 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state81 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state83 lexbuf
     | 41 -> __ocaml_lex_state80 lexbuf
     | 93 -> __ocaml_lex_state5 lexbuf
     | 58|124 -> __ocaml_lex_state82 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state82 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state81 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state83 lexbuf
     | 41|93 -> __ocaml_lex_state5 lexbuf
     | 58|124 -> __ocaml_lex_state82 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state83 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 41|93 -> __ocaml_lex_state85 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state83 lexbuf
     | 58|124 -> __ocaml_lex_state86 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state84 lexbuf = 16
  and __ocaml_lex_state85 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 41|93 -> __ocaml_lex_state85 lexbuf
     | 37|38|46|47|58|60|61|62|64|94|124 -> __ocaml_lex_state87 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state86 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 41|93 -> __ocaml_lex_state85 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state83 lexbuf
     | 58|124 -> __ocaml_lex_state86 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state87 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41|93 -> __ocaml_lex_state85 lexbuf
    | 37|38|46|47|58|60|61|62|64|94|124 -> __ocaml_lex_state87 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state88 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state91 lexbuf
    | 34 ->
        ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state93 lexbuf)
    | 48|49|50|51|52|53|54|55|56|57 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state88 lexbuf)
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 9|32 -> __ocaml_lex_state94 lexbuf
    | 13 -> __ocaml_lex_state90 lexbuf
    | _ -> __ocaml_lex_state92 lexbuf
  and __ocaml_lex_state89 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 9|32 ->
        ((lexbuf.Lexing.lex_mem).(4) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state89 lexbuf)
    | 48|49|50|51|52|53|54|55|56|57 ->
        ((lexbuf.Lexing.lex_mem).(10) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state88 lexbuf)
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state90 lexbuf =
    (lexbuf.Lexing.lex_mem).(3) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(10);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 21;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state91 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state91 lexbuf =
    (lexbuf.Lexing.lex_mem).(3) <- (-1);
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(10);
    21
  and __ocaml_lex_state92 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state91 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 13 -> __ocaml_lex_state90 lexbuf
    | _ -> __ocaml_lex_state92 lexbuf
  and __ocaml_lex_state93 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state91 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 34 -> __ocaml_lex_state95 lexbuf
    | 13 -> __ocaml_lex_state90 lexbuf
    | _ ->
        ((lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state93 lexbuf)
  and __ocaml_lex_state94 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 10 -> __ocaml_lex_state91 lexbuf
    | 34 ->
        ((lexbuf.Lexing.lex_mem).(11) <- lexbuf.Lexing.lex_curr_pos;
         (lexbuf.Lexing.lex_mem).(12) <- lexbuf.Lexing.lex_curr_pos;
         __ocaml_lex_state93 lexbuf)
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | 9|32 -> __ocaml_lex_state94 lexbuf
    | 13 -> __ocaml_lex_state90 lexbuf
    | _ -> __ocaml_lex_state92 lexbuf
  and __ocaml_lex_state95 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 13 -> __ocaml_lex_state96 lexbuf
    | 10 -> __ocaml_lex_state97 lexbuf
    | 256 ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
    | _ -> __ocaml_lex_state95 lexbuf
  and __ocaml_lex_state96 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(10);
    (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(11);
    (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(12);
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 21;
    (match __ocaml_lex_next_char lexbuf with
     | 10 -> __ocaml_lex_state97 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state97 lexbuf =
    (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(4);
    (lexbuf.Lexing.lex_mem).(1) <- (lexbuf.Lexing.lex_mem).(10);
    (lexbuf.Lexing.lex_mem).(3) <- (lexbuf.Lexing.lex_mem).(11);
    (lexbuf.Lexing.lex_mem).(2) <- (lexbuf.Lexing.lex_mem).(12);
    21
  and __ocaml_lex_state98 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 58|124 -> __ocaml_lex_state99 lexbuf
    | 41|93 -> __ocaml_lex_state80 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state100 lexbuf
    | 46|60|61|62 -> __ocaml_lex_state98 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state99 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 58|124 -> __ocaml_lex_state99 lexbuf
    | 93 -> __ocaml_lex_state80 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state100 lexbuf
    | 41 -> __ocaml_lex_state5 lexbuf
    | 46|60|61|62 -> __ocaml_lex_state98 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state100 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41|93 -> __ocaml_lex_state85 lexbuf
    | 58|124 -> __ocaml_lex_state102 lexbuf
    | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state100 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state101 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state99 lexbuf
     | 41|93 -> __ocaml_lex_state80 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state100 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state98 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state102 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 41|93 -> __ocaml_lex_state85 lexbuf
    | 58|124 -> __ocaml_lex_state102 lexbuf
    | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state100 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state103 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 58|124 -> __ocaml_lex_state99 lexbuf
     | 93 -> __ocaml_lex_state80 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state100 lexbuf
     | 41 -> __ocaml_lex_state5 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state98 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state104 lexbuf =
    match __ocaml_lex_next_char lexbuf with
    | 46|58|61|62|124 -> __ocaml_lex_state104 lexbuf
    | 60 -> __ocaml_lex_state105 lexbuf
    | 37|38|47|64|94 -> __ocaml_lex_state106 lexbuf
    | _ ->
        (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
         lexbuf.Lexing.lex_last_action)
  and __ocaml_lex_state105 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 46|58|61|62|124 -> __ocaml_lex_state104 lexbuf
     | 60 -> __ocaml_lex_state105 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state106 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state106 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 24;
    (match __ocaml_lex_next_char lexbuf with
     | 37|38|46|47|58|60|61|62|64|94|124 -> __ocaml_lex_state106 lexbuf
     | 40|91 -> __ocaml_lex_state64 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state107 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 41|93 -> __ocaml_lex_state85 lexbuf
     | 37|38|46|47|60|61|62|64|94 -> __ocaml_lex_state8 lexbuf
     | 58|124 -> __ocaml_lex_state107 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action))
  and __ocaml_lex_state108 lexbuf =
    lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
    lexbuf.Lexing.lex_last_action <- 26;
    (match __ocaml_lex_next_char lexbuf with
     | 33|42|43|45|63|92|126 -> __ocaml_lex_state3 lexbuf
     | 46|60|61|62 -> __ocaml_lex_state6 lexbuf
     | 58|124 -> __ocaml_lex_state108 lexbuf
     | 93 -> __ocaml_lex_state80 lexbuf
     | 37|38|47|64|94 -> __ocaml_lex_state8 lexbuf
     | 41 -> __ocaml_lex_state5 lexbuf
     | _ ->
         (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
          lexbuf.Lexing.lex_last_action)) in
  __ocaml_lex_init_lexbuf lexbuf 13;
  (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
   lexbuf.Lexing.lex_start_p <- lexbuf.Lexing.lex_curr_p;
   lexbuf.Lexing.lex_curr_p <-
     {
       (lexbuf.Lexing.lex_curr_p) with
       Lexing.pos_cnum =
         (lexbuf.Lexing.lex_abs_pos + lexbuf.Lexing.lex_curr_pos)
     };
   (match __ocaml_lex_result with
    | 0 -> (update_loc c; `NEWLINE)
    | 1 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `BLANKS x
    | 2 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        `LABEL x
    | 3 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        `OPTLABEL x
    | 4 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Lid x
    | 5 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `Uid x
    | 6 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        (try cvt_int_literal x
         with
         | Failure _ -> err (Literal_overflow x) (FanLoc.of_lexbuf lexbuf))
    | 7 ->
        let f =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        (try `Flo ((float_of_string f), f)
         with
         | Failure _ -> err (Literal_overflow f) (FanLoc.of_lexbuf lexbuf))
    | 8 ->
        (with_curr_loc string c;
         (let s = buff_contents c in `STR ((TokenEval.string s), s)))
    | 9 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        (update_loc c ~retract:1; `CHAR ((TokenEval.char x), x))
    | 10 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (lexbuf.Lexing.lex_curr_pos + (-1)) in
        `CHAR ((TokenEval.char x), x)
    | 11 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 2) in
        err (Illegal_escape (String.make 1 c)) (FanLoc.of_lexbuf lexbuf)
    | 12 -> (store c; `COMMENT (parse_nested ~lexer:comment (in_comment c)))
    | 13 ->
        (warn Comment_start (FanLoc.of_lexbuf lexbuf);
         parse comment (in_comment c);
         `COMMENT (buff_contents c))
    | 14 ->
        (warn Comment_not_end (FanLoc.of_lexbuf lexbuf);
         move_curr_p (-1) c;
         `SYMBOL "*")
    | 15 ->
        let s =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_start_pos + 2) in
        `SYMBOL s
    | 16 ->
        let s =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_start_pos + 2) in
        `SYMBOL s
    | 17 ->
        let p =
          Lexing.sub_lexeme_char_opt lexbuf
            (((lexbuf.Lexing.lex_mem).(1)) + 0)
        and beginning =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        if quotations c
        then
          (move_curr_p (- (String.length beginning)) c;
           Stack.push p opt_char;
           (let len = 2 + (opt_char_len p) in
            mk_quotation quotation c ~name:FanToken.empty_name ~loc:""
              ~shift:len ~retract:len))
        else
          parse
            (symbolchar_star
               ("{|" ^
                  ((match p with | Some x -> String.make 1 x | None  -> "") ^
                     beginning))) c
    | 18 ->
        `QUOTATION
          {
            FanToken.q_name = FanToken.empty_name;
            q_loc = "";
            q_shift = 2;
            q_contents = ""
          }
    | 19 -> with_curr_loc maybe_quotation_at c
    | 20 -> with_curr_loc maybe_quotation_colon c
    | 21 ->
        let num =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (((lexbuf.Lexing.lex_mem).(1)) + 0)
        and name =
          Lexing.sub_lexeme_opt lexbuf (((lexbuf.Lexing.lex_mem).(3)) + 0)
            (((lexbuf.Lexing.lex_mem).(2)) + 0) in
        let inum = int_of_string num in
        (update_loc c ?file:name ~line:inum ~absolute:true;
         `LINE_DIRECTIVE (inum, name))
    | 22 ->
        let op =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1)
            (((lexbuf.Lexing.lex_mem).(0)) + 0) in
        `ESCAPED_IDENT op
    | 23 ->
        let op =
          Lexing.sub_lexeme lexbuf (((lexbuf.Lexing.lex_mem).(0)) + 0)
            (((lexbuf.Lexing.lex_mem).(1)) + 0) in
        `ESCAPED_IDENT op
    | 24 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `SYMBOL x
    | 25 ->
        if antiquots c
        then with_curr_loc dollar c
        else parse (symbolchar_star "$") c
    | 26 ->
        let x =
          Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 0)
            (lexbuf.Lexing.lex_curr_pos + 0) in
        `SYMBOL x
    | 27 ->
        let pos = lexbuf.lex_curr_p in
        (lexbuf.lex_curr_p <-
           {
             pos with
             pos_bol = (pos.pos_bol + 1);
             pos_cnum = (pos.pos_cnum + 1)
           };
         `EOI)
    | 28 ->
        let c =
          Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
        err (Illegal_character c) (FanLoc.of_lexbuf lexbuf)
    | _ -> failwith "lexing: empty token"))