let rec filter =
  function
  | fmt ->
      (function
       | (lexbuf : Lexing.lexbuf) ->
           let rec __ocaml_lex_next_char =
             function
             | () ->
                 if lexbuf.lex_curr_pos >= lexbuf.lex_buffer_len
                 then
                   (if lexbuf.lex_eof_reached
                    then 256
                    else
                      (lexbuf.refill_buff lexbuf; __ocaml_lex_next_char ()))
                 else
                   (let i = lexbuf.lex_curr_pos in
                    lexbuf.lex_curr_pos <- i + 1;
                    Char.code ((lexbuf.lex_buffer).[i]))
           and __ocaml_lex_state0 =
             function
             | () ->
                 (match __ocaml_lex_next_char () with
                  | 91 -> __ocaml_lex_state3 ()
                  | 256 -> __ocaml_lex_state1 ()
                  | _ -> __ocaml_lex_state2 ())
           and __ocaml_lex_state1 = function | () -> 2
           and __ocaml_lex_state2 = function | () -> 1
           and __ocaml_lex_state3 =
             function
             | () ->
                 (lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
                  lexbuf.lex_last_action <- 1;
                  (match __ocaml_lex_next_char () with
                   | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state4 ()
                   | _ ->
                       (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
                        lexbuf.lex_last_action)))
           and __ocaml_lex_state4 =
             function
             | () ->
                 (match __ocaml_lex_next_char () with
                  | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state4 ()
                  | 44 -> __ocaml_lex_state5 ()
                  | _ ->
                      (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
                       lexbuf.lex_last_action))
           and __ocaml_lex_state5 =
             function
             | () ->
                 (match __ocaml_lex_next_char () with
                  | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state6 ()
                  | _ ->
                      (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
                       lexbuf.lex_last_action))
           and __ocaml_lex_state6 =
             function
             | () ->
                 (match __ocaml_lex_next_char () with
                  | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state6 ()
                  | 43 -> __ocaml_lex_state7 ()
                  | _ ->
                      (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
                       lexbuf.lex_last_action))
           and __ocaml_lex_state7 =
             function
             | () ->
                 (match __ocaml_lex_next_char () with
                  | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state8 ()
                  | _ ->
                      (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
                       lexbuf.lex_last_action))
           and __ocaml_lex_state8 =
             function
             | () ->
                 (match __ocaml_lex_next_char () with
                  | 93 -> __ocaml_lex_state9 ()
                  | 48|49|50|51|52|53|54|55|56|57 -> __ocaml_lex_state8 ()
                  | _ ->
                      (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
                       lexbuf.lex_last_action))
           and __ocaml_lex_state9 = function | () -> 0 in
           ((let pos = lexbuf.lex_curr_pos in
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
              | 0 -> (Format.pp_print_string fmt "[]"; filter fmt lexbuf)
              | 1 ->
                  let c = Lexing.sub_lexeme_char lexbuf lexbuf.lex_start_pos in
                  (Format.pp_print_char fmt c; filter fmt lexbuf)
              | 2 -> ()
              | _ ->
                  failwith
                    ("Diff_filter" ^
                       ("." ^ ("filter" ^ " lexing: empty token")))))))
let () =
  let in_chan = stdin in
  let lex_buf = Lexing.from_channel in_chan in
  let () = filter Format.std_formatter lex_buf in close_in in_chan
