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
      | 10 -> __ocaml_lex_state6 lexbuf
      | 34 -> __ocaml_lex_state3 lexbuf
      | 40 -> __ocaml_lex_state4 lexbuf
      | 13 -> __ocaml_lex_state5 lexbuf
      | 9|12|32 -> __ocaml_lex_state7 lexbuf
      | 256 -> __ocaml_lex_state2 lexbuf
      | _ -> __ocaml_lex_state1 lexbuf
    and __ocaml_lex_state1 (lexbuf : Lexing.lexbuf) = 5
    and __ocaml_lex_state2 (lexbuf : Lexing.lexbuf) = 4
    and __ocaml_lex_state3 (lexbuf : Lexing.lexbuf) = 3
    and __ocaml_lex_state4 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 5;
      (match __ocaml_lex_next_char lexbuf with
       | 42 ->
           ((lexbuf.Lexing.lex_mem).(1) <- lexbuf.Lexing.lex_curr_pos;
            __ocaml_lex_state8 lexbuf)
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state5 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 1;
      (match __ocaml_lex_next_char lexbuf with
       | 10 -> __ocaml_lex_state6 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state6 (lexbuf : Lexing.lexbuf) = 1
    and __ocaml_lex_state7 (lexbuf : Lexing.lexbuf) =
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 0;
      (match __ocaml_lex_next_char lexbuf with
       | 9|12|32 -> __ocaml_lex_state7 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state8 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (-1);
      lexbuf.Lexing.lex_last_pos <- lexbuf.Lexing.lex_curr_pos;
      lexbuf.Lexing.lex_last_action <- 2;
      (match __ocaml_lex_next_char lexbuf with
       | 41 -> __ocaml_lex_state9 lexbuf
       | _ ->
           (lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_last_pos;
            lexbuf.Lexing.lex_last_action))
    and __ocaml_lex_state9 (lexbuf : Lexing.lexbuf) =
      (lexbuf.Lexing.lex_mem).(0) <- (lexbuf.Lexing.lex_mem).(1); 2 in
    __ocaml_lex_init_lexbuf lexbuf 2;
    (let __ocaml_lex_result = __ocaml_lex_state0 lexbuf in
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
          let x =
            Lexing.sub_lexeme_char_opt lexbuf
              (((lexbuf.Lexing.lex_mem).(0)) + 0) in
          ((let c = Lexing_util.new_cxt () in
            if x <> None
            then
              Lexing_util.warn Comment_start (Lexing_util.from_lexbuf lexbuf);
            Lexing_util.store c lexbuf;
            Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_comment;
            ignore (Lexing_util.buff_contents c));
           token lexbuf)
      | 3 ->
          let c = Lexing_util.new_cxt () in
          let old = lexbuf.lex_start_p in
          (Lexing_util.push_loc_cont c lexbuf Lexing_util.lex_string;
           `Str
             {
               loc = (Location_util.( -- )  old lexbuf.lex_curr_p);
               txt = (Lexing_util.buff_contents c)
             })
      | 4 ->
          let pos = lexbuf.lex_curr_p in
          (lexbuf.lex_curr_p <-
             {
               pos with
               pos_bol = (pos.pos_bol + 1);
               pos_cnum = (pos.pos_cnum + 1)
             };
           (let loc = Lexing_util.from_lexbuf lexbuf in
            (`EOI { loc; txt = "" } : Tokenf.t )))
      | 5 ->
          let c =
            Lexing.sub_lexeme_char lexbuf (lexbuf.Lexing.lex_start_pos + 0) in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ -> failwith "lexing: empty token"))
let lexer = Lexing_util.adapt_to_stream token
let include_quot = Gramf.mk "include_quot"
let _ =
  Gramf.extend_single (include_quot : 'include_quot Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                 Tokenf.pattern )];
            annot =
              "let (keep,cf) = let open State in (keep, current_filters) in\nlet fan_keep__0 = !keep and fan_cf__1 = !cf in\ntry\n  let fan_res__2 = State.reset (); Gramlib.parse_include_file Syntaxf.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    (let (keep,cf) =
                       let open State in (keep, current_filters) in
                     let fan_keep__0 = !keep and fan_cf__1 = !cf in
                     try
                       let fan_res__2 =
                         State.reset ();
                         Gramlib.parse_include_file Syntaxf.strus s in
                       let _ = keep := fan_keep__0; cf := fan_cf__1 in
                       fan_res__2
                     with
                     | fan_e__3 ->
                         ((keep := fan_keep__0; cf := fan_cf__1);
                          raise fan_e__3) : 'include_quot ) : Tokenf.txt ->
                                                                Locf.t ->
                                                                  'include_quot ))
          }]
     } : Gramf.olevel )
let _ =
  Ast_quotation.of_stru ~lexer ~name:{ domains = Ns.lang; name = "include" }
    ~entry:include_quot ()
