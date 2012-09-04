open Format

module Make =
              functor (Loc : Sig.Loc) ->
               (struct
                 module Loc = Loc

                 open Sig

                 type t = camlp4_token

                 type token = t

                 let to_string =
                  function
                  | KEYWORD (s) -> (sprintf "KEYWORD %S" s)
                  | SYMBOL (s) -> (sprintf "SYMBOL %S" s)
                  | LIDENT (s) -> (sprintf "LIDENT %S" s)
                  | UIDENT (s) -> (sprintf "UIDENT %S" s)
                  | INT (_, s) -> (sprintf "INT %s" s)
                  | INT32 (_, s) -> (sprintf "INT32 %sd" s)
                  | INT64 (_, s) -> (sprintf "INT64 %sd" s)
                  | NATIVEINT (_, s) -> (sprintf "NATIVEINT %sd" s)
                  | FLOAT (_, s) -> (sprintf "FLOAT %s" s)
                  | CHAR (_, s) -> (sprintf "CHAR '%s'" s)
                  | STRING (_, s) -> (sprintf "STRING \"%s\"" s)
                  | LABEL (s) -> (sprintf "LABEL %S" s)
                  | OPTLABEL (s) -> (sprintf "OPTLABEL %S" s)
                  | ANTIQUOT (n, s) -> (sprintf "ANTIQUOT %s: %S" n s)
                  | QUOTATION (x) ->
                     (sprintf
                       "QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
                       ( x.q_name ) ( x.q_loc ) ( x.q_shift ) ( x.q_contents
                       ))
                  | COMMENT (s) -> (sprintf "COMMENT %S" s)
                  | BLANKS (s) -> (sprintf "BLANKS %S" s)
                  | NEWLINE -> (sprintf "NEWLINE")
                  | EOI -> (sprintf "EOI")
                  | ESCAPED_IDENT (s) -> (sprintf "ESCAPED_IDENT %S" s)
                  | LINE_DIRECTIVE (i, None) ->
                     (sprintf "LINE_DIRECTIVE %d" i)
                  | LINE_DIRECTIVE (i, Some (s)) ->
                     (sprintf "LINE_DIRECTIVE %d %S" i s)

                 let print =
                  fun ppf -> fun x -> (pp_print_string ppf ( (to_string x) ))

                 let match_keyword =
                  fun kwd ->
                   function
                   | KEYWORD (kwd') when (kwd = kwd') -> (true)
                   | _ -> (false)

                 let extract_string =
                  function
                  | (((((((((((((((KEYWORD (s) | SYMBOL (s)) | LIDENT (s))
                                 | UIDENT (s)) | INT (_, s)) | INT32 (_, s))
                              | INT64 (_, s)) | NATIVEINT (_, s))
                            | FLOAT (_, s)) | CHAR (_, s)) | STRING (_, s))
                         | LABEL (s)) | OPTLABEL (s)) | COMMENT (s))
                      | BLANKS (s)) | ESCAPED_IDENT (s)) ->
                     s
                  | tok ->
                     (invalid_arg (
                       ("Cannot extract a string from this token: " ^ (
                         (to_string tok) )) ))

                 module Error =
                  struct
                   type t =
                      Illegal_token of string
                    | Keyword_as_label of string
                    | Illegal_token_pattern of string * string
                    | Illegal_constructor of string

                   exception E of t

                   let print =
                    fun ppf ->
                     function
                     | Illegal_token (s) ->
                        (fprintf ppf "Illegal token (%s)" s)
                     | Keyword_as_label (kwd) ->
                        (fprintf ppf
                          "`%s' is a keyword, it cannot be used as label name"
                          kwd)
                     | Illegal_token_pattern (p_con, p_prm) ->
                        (fprintf ppf "Illegal token pattern: %s %S" p_con
                          p_prm)
                     | Illegal_constructor (con) ->
                        (fprintf ppf "Illegal constructor %S" con)

                   let to_string =
                    fun x ->
                     let b = (Buffer.create 50) in
                     let () = (bprintf b "%a" print x) in (Buffer.contents b)

                  end

                 let _ = let module M = (ErrorHandler.Register)(Error) in ()

                 module Filter =
                  struct
                   type token_filter = (t, Loc.t) stream_filter

                   type t = {
                              is_kwd:(string -> bool);
                              mutable filter:token_filter}

                   let err =
                    fun error ->
                     fun loc ->
                      (raise ( (Loc.Exc_located (loc, ( (Error.E (error)) )))
                        ))

                   let keyword_conversion =
                    fun tok ->
                     fun is_kwd ->
                      (match tok with
                       | ((SYMBOL (s) | LIDENT (s)) | UIDENT (s)) when
                          (is_kwd s) ->
                          (KEYWORD (s))
                       | ESCAPED_IDENT (s) -> (LIDENT (s))
                       | _ -> tok)

                   let check_keyword_as_label =
                    fun tok ->
                     fun loc ->
                      fun is_kwd ->
                       let s =
                        (match tok with
                         | LABEL (s) -> s
                         | OPTLABEL (s) -> s
                         | _ -> "") in
                       if (( (s <> "") ) && ( (is_kwd s) )) then
                        (
                        (err ( (Error.Keyword_as_label (s)) ) loc)
                        )
                       else ()

                   let check_unknown_keywords =
                    fun tok ->
                     fun loc ->
                      (match tok with
                       | SYMBOL (s) ->
                          (err ( (Error.Illegal_token (s)) ) loc)
                       | _ -> ())

                   let error_no_respect_rules =
                    fun p_con ->
                     fun p_prm ->
                      (raise (
                        (Error.E
                          ((Error.Illegal_token_pattern (p_con, p_prm)))) ))

                   let check_keyword = fun _ -> (true)

                   let error_on_unknown_keywords = (ref false )

                   let rec ignore_layout =
                    fun (__strm :
                      _ Stream.t) ->
                     (match (Stream.peek __strm) with
                      | Some
                         ((((COMMENT (_) | BLANKS (_)) | NEWLINE)
                           | LINE_DIRECTIVE (_, _)), _) ->
                         ( (Stream.junk __strm) ); (ignore_layout __strm)
                      | Some (x) ->
                         (
                         (Stream.junk __strm)
                         );
                         let s = __strm in
                         (Stream.icons x (
                           (Stream.slazy ( fun _ -> (ignore_layout s) )) ))
                      | _ -> Stream.sempty)

                   let mk =
                    fun is_kwd -> {is_kwd = is_kwd; filter = ignore_layout}

                   let filter =
                    fun x ->
                     let f =
                      fun tok ->
                       fun loc ->
                        let tok = (keyword_conversion tok ( x.is_kwd )) in
                        (
                        (check_keyword_as_label tok loc ( x.is_kwd ))
                        );
                        (
                        if !error_on_unknown_keywords then
                         (
                         (check_unknown_keywords tok loc)
                         )
                        else ()
                        );
                        (tok, loc) in
                     let rec filter =
                      fun (__strm :
                        _ Stream.t) ->
                       (match (Stream.peek __strm) with
                        | Some (tok, loc) ->
                           (
                           (Stream.junk __strm)
                           );
                           let s = __strm in
                           (Stream.lcons ( fun _ -> (f tok loc) ) (
                             (Stream.slazy ( fun _ -> (filter s) )) ))
                        | _ -> Stream.sempty) in
                     let rec tracer =
                      fun (__strm :
                        _ Stream.t) ->
                       (match (Stream.peek __strm) with
                        | Some ((_tok, _loc) as x) ->
                           (
                           (Stream.junk __strm)
                           );
                           let xs = __strm in
                           (Stream.icons x (
                             (Stream.slazy ( fun _ -> (tracer xs) )) ))
                        | _ -> Stream.sempty) in
                     fun strm -> (tracer ( ((x.filter) ( (filter strm) )) ))

                   let define_filter =
                    fun x -> fun f -> x.filter <- (f ( x.filter ))

                   let keyword_added = fun _ -> fun _ -> fun _ -> ()

                   let keyword_removed = fun _ -> fun _ -> ()

                  end
 end :
               (Sig.Camlp4Token with module Loc = Loc))

module Eval =
                                                          struct
                                                           let valch =
                                                            fun x ->
                                                             (( (Char.code x)
                                                               ) - (
                                                               (Char.code
                                                                 '0') ))

                                                           let valch_hex =
                                                            fun x ->
                                                             let d =
                                                              (Char.code x) in
                                                             if (d >= 97) then
                                                              (
                                                              (d - 87)
                                                              )
                                                             else if 
                                                                   (d >= 65) then
                                                                   (
                                                                   (d - 55)
                                                                   )
                                                             else (d - 48)

                                                           let rec skip_indent =
                                                            fun (__strm :
                                                              _ Stream.t) ->
                                                             (match
                                                                (Stream.peek
                                                                  __strm) with
                                                              | Some
                                                                 (' '
                                                                  | '\009') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 (skip_indent
                                                                   __strm)
                                                              | _ -> ())

                                                           let skip_opt_linefeed =
                                                            fun (__strm :
                                                              _ Stream.t) ->
                                                             (match
                                                                (Stream.peek
                                                                  __strm) with
                                                              | Some ('\010') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 ()
                                                              | _ -> ())

                                                           let chr =
                                                            fun c ->
                                                             if (( (c < 0) )
                                                                  || (
                                                                  (c > 255)
                                                                  )) then
                                                              (
                                                              (failwith
                                                                "invalid char token")
                                                              )
                                                             else
                                                              (Char.chr c)

                                                           let rec backslash =
                                                            fun (__strm :
                                                              _ Stream.t) ->
                                                             (match
                                                                (Stream.peek
                                                                  __strm) with
                                                              | Some ('\010') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\010'
                                                              | Some ('\013') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\013'
                                                              | Some ('n') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\010'
                                                              | Some ('r') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\013'
                                                              | Some ('t') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\009'
                                                              | Some ('b') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\008'
                                                              | Some ('\\') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\\'
                                                              | Some ('"') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '"'
                                                              | Some ('\'') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 '\''
                                                              | Some (' ') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 ' '
                                                              | Some
                                                                 (('0'
                                                                   | 
                                                                   ('1'
                                                                    | 
                                                                    ('2'
                                                                    | 
                                                                    ('3'
                                                                    | 
                                                                    ('4'
                                                                    | 
                                                                    ('5'
                                                                    | 
                                                                    ('6'
                                                                    | 
                                                                    ('7'
                                                                    | 
                                                                    ('8'
                                                                    | '9'))))))))) as
                                                                  c1) ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 (match
                                                                    (Stream.peek
                                                                    __strm) with
                                                                  | Some
                                                                    (('0'
                                                                    | ('1'
                                                                    | ('2'
                                                                    | ('3'
                                                                    | ('4'
                                                                    | ('5'
                                                                    | ('6'
                                                                    | ('7'
                                                                    | ('8'
                                                                    | '9'))))))))) as
                                                                    c2) ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    (
                                                                    match
                                                                    (Stream.peek
                                                                    __strm) with
                                                                    | Some
                                                                    (('0'
                                                                    | ('1'
                                                                    | ('2'
                                                                    | ('3'
                                                                    | ('4'
                                                                    | ('5'
                                                                    | ('6'
                                                                    | ('7'
                                                                    | ('8'
                                                                    | '9'))))))))) as
                                                                    c3) ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    (chr (
                                                                    ((
                                                                    ((
                                                                    (100 * (
                                                                    (valch
                                                                    c1) )) )
                                                                    + (
                                                                    (10 * (
                                                                    (valch
                                                                    c2) )) ))
                                                                    ) + (
                                                                    (valch
                                                                    c3) )) ))
                                                                    | 
                                                                    _ ->
                                                                    (raise (
                                                                    (Stream.Error
                                                                    ("")) )))
                                                                  | _ ->
                                                                    (raise (
                                                                    (Stream.Error
                                                                    ("")) )))
                                                              | Some ('x') ->
                                                                 (
                                                                 (Stream.junk
                                                                   __strm)
                                                                 );
                                                                 (match
                                                                    (Stream.peek
                                                                    __strm) with
                                                                  | Some
                                                                    (((('0'
                                                                    | ('1'
                                                                    | ('2'
                                                                    | ('3'
                                                                    | ('4'
                                                                    | ('5'
                                                                    | ('6'
                                                                    | ('7'
                                                                    | ('8'
                                                                    | '9')))))))))
                                                                    | ('a'
                                                                    | ('b'
                                                                    | ('c'
                                                                    | ('d'
                                                                    | ('e'
                                                                    | 'f'))))))
                                                                    | ('A'
                                                                    | ('B'
                                                                    | ('C'
                                                                    | ('D'
                                                                    | ('E'
                                                                    | 'F')))))) as
                                                                    c1) ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    (
                                                                    match
                                                                    (Stream.peek
                                                                    __strm) with
                                                                    | Some
                                                                    (((('0'
                                                                    | ('1'
                                                                    | ('2'
                                                                    | ('3'
                                                                    | ('4'
                                                                    | ('5'
                                                                    | ('6'
                                                                    | ('7'
                                                                    | ('8'
                                                                    | '9')))))))))
                                                                    | ('a'
                                                                    | ('b'
                                                                    | ('c'
                                                                    | ('d'
                                                                    | ('e'
                                                                    | 'f'))))))
                                                                    | ('A'
                                                                    | ('B'
                                                                    | ('C'
                                                                    | ('D'
                                                                    | ('E'
                                                                    | 'F')))))) as
                                                                    c2) ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    (chr (
                                                                    ((
                                                                    (16 * (
                                                                    (valch_hex
                                                                    c1) )) )
                                                                    + (
                                                                    (valch_hex
                                                                    c2) )) ))
                                                                    | 
                                                                    _ ->
                                                                    (raise (
                                                                    (Stream.Error
                                                                    ("")) )))
                                                                  | _ ->
                                                                    (raise (
                                                                    (Stream.Error
                                                                    ("")) )))
                                                              | _ ->
                                                                 (raise
                                                                   Stream.Failure
                                                                   ))

                                                           let rec backslash_in_string =
                                                            fun strict ->
                                                             fun store ->
                                                              fun (__strm :
                                                                _ Stream.t) ->
                                                               (match
                                                                  (Stream.peek
                                                                    __strm) with
                                                                | Some
                                                                   ('\010') ->
                                                                   (
                                                                   (Stream.junk
                                                                    __strm)
                                                                   );
                                                                   (skip_indent
                                                                    __strm)
                                                                | Some
                                                                   ('\013') ->
                                                                   (
                                                                   (Stream.junk
                                                                    __strm)
                                                                   );
                                                                   let s =
                                                                    __strm in
                                                                   (
                                                                   (skip_opt_linefeed
                                                                    s)
                                                                   );
                                                                   (skip_indent
                                                                    s)
                                                                | _ ->
                                                                   (match
                                                                    (try
                                                                    (Some
                                                                    (backslash
                                                                    __strm))
                                                                    with
                                                                    Stream.Failure ->
                                                                    (None)) with
                                                                    | Some
                                                                    (x) ->
                                                                    (store x)
                                                                    | 
                                                                    _ ->
                                                                    (
                                                                    match
                                                                    (Stream.peek
                                                                    __strm) with
                                                                    | Some
                                                                    (c) when
                                                                    (not
                                                                    strict) ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    (
                                                                    (store
                                                                    '\\')
                                                                    );
                                                                    (store c)
                                                                    | 
                                                                    _ ->
                                                                    (failwith
                                                                    "invalid string token"))))

                                                           let char =
                                                            fun s ->
                                                             if ((
                                                                  (String.length
                                                                    s) ) = 1) then
                                                              (
                                                              (String.get s
                                                                0)
                                                              )
                                                             else if 
                                                                   ((
                                                                    (String.length
                                                                    s) ) = 0) then
                                                                   (
                                                                   (failwith
                                                                    "invalid char token")
                                                                   )
                                                             else
                                                              let (__strm :
                                                                _ Stream.t) =
                                                               (Stream.of_string
                                                                 s) in
                                                              (match
                                                                 (Stream.peek
                                                                   __strm) with
                                                               | Some ('\\') ->
                                                                  (
                                                                  (Stream.junk
                                                                    __strm)
                                                                  );
                                                                  (try
                                                                    (backslash
                                                                    __strm)
                                                                   with
                                                                   Stream.Failure ->
                                                                    (raise (
                                                                    (Stream.Error
                                                                    ("")) )))
                                                               | _ ->
                                                                  (failwith
                                                                    "invalid char token"))

                                                           let string =
                                                            fun ?strict ->
                                                             fun s ->
                                                              let buf =
                                                               (Buffer.create
                                                                 23) in
                                                              let store =
                                                               (Buffer.add_char
                                                                 buf) in
                                                              let rec parse =
                                                               fun (__strm :
                                                                 _ Stream.t) ->
                                                                (match
                                                                   (Stream.peek
                                                                    __strm) with
                                                                 | Some
                                                                    ('\\') ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    let _ =
                                                                    (try
                                                                    (backslash_in_string
                                                                    (
                                                                    (strict
                                                                    <> None )
                                                                    ) store
                                                                    __strm)
                                                                    with
                                                                    Stream.Failure ->
                                                                    (raise (
                                                                    (Stream.Error
                                                                    ("")) ))) in
                                                                    (parse
                                                                    __strm)
                                                                 | Some (c) ->
                                                                    (
                                                                    (Stream.junk
                                                                    __strm)
                                                                    );
                                                                    let s =
                                                                    __strm in
                                                                    (
                                                                    (store c)
                                                                    );
                                                                    (parse s)
                                                                 | _ ->
                                                                    (Buffer.contents
                                                                    buf)) in
                                                              (parse (
                                                                (Stream.of_string
                                                                  s) ))

                                                          end
