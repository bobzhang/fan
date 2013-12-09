let rec token: Lexing.lexbuf -> Tokenf.t =
  fun (lexbuf : Lexing.lexbuf)  ->
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
      | 34 -> __ocaml_lex_state3 ()
      | 40 -> __ocaml_lex_state4 ()
      | 13 -> __ocaml_lex_state5 ()
      | 9|12|32 -> __ocaml_lex_state7 ()
      | 256 -> __ocaml_lex_state2 ()
      | _ -> __ocaml_lex_state1 ()
    and __ocaml_lex_state1 () = 5
    and __ocaml_lex_state2 () = 4
    and __ocaml_lex_state3 () = 3
    and __ocaml_lex_state4 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 5;
      (match __ocaml_lex_next_char () with
       | 42 ->
           ((lexbuf.lex_mem).(1) <- lexbuf.lex_curr_pos;
            __ocaml_lex_state8 ())
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state5 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 1;
      (match __ocaml_lex_next_char () with
       | 10 -> __ocaml_lex_state6 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state6 () = 1
    and __ocaml_lex_state7 () =
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 0;
      (match __ocaml_lex_next_char () with
       | 9|12|32 -> __ocaml_lex_state7 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state8 () =
      (lexbuf.lex_mem).(0) <- (-1);
      lexbuf.lex_last_pos <- lexbuf.lex_curr_pos;
      lexbuf.lex_last_action <- 2;
      (match __ocaml_lex_next_char () with
       | 41 -> __ocaml_lex_state9 ()
       | _ ->
           (lexbuf.lex_curr_pos <- lexbuf.lex_last_pos;
            lexbuf.lex_last_action))
    and __ocaml_lex_state9 () =
      (lexbuf.lex_mem).(0) <- (lexbuf.lex_mem).(1); 2 in
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
      | 0 -> ((); token lexbuf)
      | 1 -> (Lexing_util.update_loc lexbuf; token lexbuf)
      | 2 ->
          let x = Lexing.sub_lexeme_char_opt lexbuf ((lexbuf.lex_mem).(0)) in
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
          let c = Lexing.sub_lexeme_char lexbuf lexbuf.lex_start_pos in
          (Lexing_util.err (Illegal_character c)) @@
            (Lexing_util.from_lexbuf lexbuf)
      | _ ->
          failwith
            ("Lang_include" ^ ("." ^ ("token" ^ " lexing: empty token")))))
let lexer = Lexing_util.adapt_to_stream token
let include_quot = Gramf.mk "include_quot"
let _ =
  Gramf.extend_single
    ({
       entry = (include_quot : 'include_quot Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                       } : Tokenf.pattern )];
                 annot =
                   "let (keep,cf) = let open State in (keep, current_filters) in\nlet keep__001_ = !keep and cf__002_ = !cf in\ntry\n  let res__003_ = State.reset (); Gramlib.parse_include_file Syntaxf.strus s in\n  let _ = keep := keep__001_; cf := cf__002_ in res__003_\nwith | e__004_ -> ((keep := keep__001_; cf := cf__002_); raise e__004_)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in
                         (let (keep,cf) =
                            let open State in (keep, current_filters) in
                          let keep__001_ = !keep and cf__002_ = !cf in
                          try
                            let res__003_ =
                              State.reset ();
                              Gramlib.parse_include_file Syntaxf.strus s in
                            let _ = keep := keep__001_; cf := cf__002_ in
                            res__003_
                          with
                          | e__004_ ->
                              ((keep := keep__001_; cf := cf__002_);
                               raise e__004_) : 'include_quot ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'include_quot ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let _ =
  Ast_quotation.of_stru ~lexer ~name:{ domain = Ns.lang; name = "include" }
    ~entry:include_quot ()
