open Format

open FanSig

type t = camlp4_token

let to_string =
                                                  function
                                                  | KEYWORD (s) ->
                                                     (sprintf "KEYWORD %S" s)
                                                  | SYMBOL (s) ->
                                                     (sprintf "SYMBOL %S" s)
                                                  | LIDENT (s) ->
                                                     (sprintf "LIDENT %S" s)
                                                  | UIDENT (s) ->
                                                     (sprintf "UIDENT %S" s)
                                                  | INT (_, s) ->
                                                     (sprintf "INT %s" s)
                                                  | INT32 (_, s) ->
                                                     (sprintf "INT32 %sd" s)
                                                  | INT64 (_, s) ->
                                                     (sprintf "INT64 %sd" s)
                                                  | NATIVEINT (_, s) ->
                                                     (sprintf "NATIVEINT %sd"
                                                       s)
                                                  | FLOAT (_, s) ->
                                                     (sprintf "FLOAT %s" s)
                                                  | CHAR (_, s) ->
                                                     (sprintf "CHAR '%s'" s)
                                                  | STRING (_, s) ->
                                                     (sprintf "STRING \"%s\""
                                                       s)
                                                  | LABEL (s) ->
                                                     (sprintf "LABEL %S" s)
                                                  | OPTLABEL (s) ->
                                                     (sprintf "OPTLABEL %S"
                                                       s)
                                                  | ANTIQUOT (n, s) ->
                                                     (sprintf
                                                       "ANTIQUOT %S: %S" n s)
                                                  | QUOTATION (x) ->
                                                     (sprintf
                                                       "QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
                                                       ( x.q_name ) ( 
                                                       x.q_loc ) ( x.q_shift
                                                       ) ( x.q_contents ))
                                                  | COMMENT (s) ->
                                                     (sprintf "COMMENT %S" s)
                                                  | BLANKS (s) ->
                                                     (sprintf "BLANKS %S" s)
                                                  | NEWLINE ->
                                                     (sprintf "NEWLINE")
                                                  | EOI -> (sprintf "EOI")
                                                  | ESCAPED_IDENT (s) ->
                                                     (sprintf
                                                       "ESCAPED_IDENT %S" s)
                                                  | LINE_DIRECTIVE (i, None) ->
                                                     (sprintf
                                                       "LINE_DIRECTIVE %d" i)
                                                  | LINE_DIRECTIVE
                                                     (i, Some (s)) ->
                                                     (sprintf
                                                       "LINE_DIRECTIVE %d %S"
                                                       i s)

let print =
                                                              fun ppf ->
                                                               fun x ->
                                                                (pp_print_string
                                                                  ppf (
                                                                  (to_string
                                                                    x) ))


let match_keyword =
 fun kwd ->
  function | KEYWORD (kwd') when (kwd = kwd') -> (true) | _ -> (false)


let extract_string =
 function
 | (((((((((((((((KEYWORD (s) | SYMBOL (s)) | LIDENT (s)) | UIDENT (s))
               | INT (_, s)) | INT32 (_, s)) | INT64 (_, s))
            | NATIVEINT (_, s)) | FLOAT (_, s)) | CHAR (_, s))
         | STRING (_, s)) | LABEL (s)) | OPTLABEL (s)) | COMMENT (s))
     | BLANKS (s)) | ESCAPED_IDENT (s)) ->
    s
 | tok ->
    (invalid_arg (
      ("Cannot extract a string from this token: " ^ ( (to_string tok) )) ))


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
    | Illegal_token (s) -> (fprintf ppf "Illegal token (%s)" s)
    | Keyword_as_label (kwd) ->
       (fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd)
    | Illegal_token_pattern (p_con, p_prm) ->
       (fprintf ppf "Illegal token pattern: %s %S" p_con p_prm)
    | Illegal_constructor (con) -> (fprintf ppf "Illegal constructor %S" con)

  let to_string =
   fun x ->
    let b = (Buffer.create 50) in
    let () = (bprintf b "%a" print x) in (Buffer.contents b)

 end

let _ = let module M = (FanUtil.ErrorHandler.Register)(Error) in ()


module Filter =
 struct
  type token_filter = (t, FanLoc.t) stream_filter

  type t = {is_kwd:(string -> bool); mutable filter:token_filter}

  let err =
   fun error ->
    fun loc -> (raise ( (FanLoc.Exc_located (loc, ( (Error.E (error)) ))) ))

  let keyword_conversion =
   fun tok ->
    fun is_kwd ->
     (match tok with
      | ((SYMBOL (s) | LIDENT (s)) | UIDENT (s)) when (is_kwd s) ->
         (KEYWORD (s))
      | ESCAPED_IDENT (s) -> (LIDENT (s))
      | _ -> tok)

  let check_keyword_as_label =
   fun tok ->
    fun loc ->
     fun is_kwd ->
      let s = (match tok with | LABEL (s) -> s | OPTLABEL (s) -> s | _ -> "") in
      if (( (s <> "") ) && ( (is_kwd s) )) then
       (
       (err ( (Error.Keyword_as_label (s)) ) loc)
       )
      else ()

  let check_unknown_keywords =
   fun tok ->
    fun loc ->
     (match tok with
      | SYMBOL (s) -> (err ( (Error.Illegal_token (s)) ) loc)
      | _ -> ())

  let error_no_respect_rules =
   fun p_con ->
    fun p_prm ->
     (raise ( (Error.E ((Error.Illegal_token_pattern (p_con, p_prm)))) ))

  let check_keyword = fun _ -> (true)

  let error_on_unknown_keywords = (ref false )

  let rec ignore_layout =
   fun (__strm :
     _ Stream.t) ->
    (match (Stream.peek __strm) with
     | Some
        ((((COMMENT (_) | BLANKS (_)) | NEWLINE) | LINE_DIRECTIVE (_, _)), _) ->
        ( (Stream.junk __strm) ); (ignore_layout __strm)
     | Some (x) ->
        (
        (Stream.junk __strm)
        );
        let s = __strm in
        (Stream.icons x ( (Stream.slazy ( fun _ -> (ignore_layout s) )) ))
     | _ -> Stream.sempty)

  let mk = fun is_kwd -> {is_kwd = is_kwd; filter = ignore_layout}

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
       if error_on_unknown_keywords.contents then
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
          (Stream.icons x ( (Stream.slazy ( fun _ -> (tracer xs) )) ))
       | _ -> Stream.sempty) in
    fun strm -> (tracer ( ((x.filter) ( (filter strm) )) ))

  let define_filter = fun x -> fun f -> x.filter <- (f ( x.filter ))

  let keyword_added = fun _ -> fun _ -> fun _ -> ()

  let keyword_removed = fun _ -> fun _ -> ()

 end
