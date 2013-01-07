open Format
type quotation = 
  {
  q_name: string;
  q_loc: string;
  q_shift: int;
  q_contents: string} 
type t =
  [ `KEYWORD of string | `SYMBOL of string | `LID of string | `UID of string
  | `ESCAPED_IDENT of string | `INT of (int* string)
  | `INT32 of (int32* string) | `INT64 of (int64* string)
  | `NATIVEINT of (nativeint* string) | `FLO of (float* string)
  | `CHAR of (char* string) | `STR of (string* string) | `LABEL of string
  | `OPTLABEL of string | `QUOTATION of quotation | `ANT of (string* string)
  | `COMMENT of string | `BLANKS of string | `NEWLINE
  | `LINE_DIRECTIVE of (int* string option) | `EOI] 
type 'a token = [> t] as 'a 
type error =  
  | Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string* string)
  | Illegal_constructor of string 
type stream = (t* FanLoc.t) XStream.t 
type 'a estream = ('a token* FanLoc.t) XStream.t 
type 'a parse = stream -> 'a 
type filter = stream -> stream 
exception TokenError of error
let print_basic_error ppf =
  function
  | Illegal_token s -> fprintf ppf "Illegal token (%s)" s
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Illegal_token_pattern (p_con,p_prm) ->
      fprintf ppf "Illegal token pattern: %s %S" p_con p_prm
  | Illegal_constructor con -> fprintf ppf "Illegal constructor %S" con
let to_string_of_printer printer v =
  let buf = Buffer.create 30 in
  let () = Format.bprintf buf "@[%a@]" printer v in Buffer.contents buf
let string_of_error_msg = to_string_of_printer print_basic_error
let _ =
  Printexc.register_printer
    (function | TokenError e -> Some (string_of_error_msg e) | _ -> None)
let token_to_string: t -> string =
  function
  | `KEYWORD s -> sprintf "`KEYWORD %S" s
  | `SYMBOL s -> sprintf "`SYMBOL %S" s
  | `LID s -> sprintf "`LID %S" s
  | `UID s -> sprintf "`UID %S" s
  | `INT (_,s) -> sprintf "`INT %s" s
  | `INT32 (_,s) -> sprintf "`INT32 %sd" s
  | `INT64 (_,s) -> sprintf "`INT64 %sd" s
  | `NATIVEINT (_,s) -> sprintf "`NATIVEINT %sd" s
  | `FLO (_,s) -> sprintf "`FLO %s" s
  | `CHAR (_,s) -> sprintf "`CHAR '%s'" s
  | `STR (_,s) -> sprintf "`STR \"%s\"" s
  | `LABEL s -> sprintf "`LABEL %S" s
  | `OPTLABEL s -> sprintf "`OPTLABEL %S" s
  | `ANT (n,s) -> sprintf "`ANT %S: %S" n s
  | `QUOTATION x ->
      sprintf "`QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
        x.q_name x.q_loc x.q_shift x.q_contents
  | `COMMENT s -> sprintf "`COMMENT %S" s
  | `BLANKS s -> sprintf "`BLANKS %S" s
  | `NEWLINE -> sprintf "`NEWLINE"
  | `EOI -> sprintf "`EOI"
  | `ESCAPED_IDENT s -> sprintf "`ESCAPED_IDENT %S" s
  | `LINE_DIRECTIVE (i,None ) -> sprintf "`LINE_DIRECTIVE %d" i
  | `LINE_DIRECTIVE (i,Some s) -> sprintf "`LINE_DIRECTIVE %d %S" i s
let to_string =
  function
  | #t as x -> token_to_string x
  | _ -> invalid_arg "token_to_string not implemented for this token"
let err error loc = raise (FanLoc.Exc_located (loc, (TokenError error)))
let error_no_respect_rules p_con p_prm =
  raise (TokenError (Illegal_token_pattern (p_con, p_prm)))
let check_keyword _ = true
let error_on_unknown_keywords = ref false
let rec ignore_layout (__strm : _ XStream.t) =
  match XStream.peek __strm with
  | Some ((`COMMENT _|`BLANKS _|`NEWLINE|`LINE_DIRECTIVE _),_) ->
      (XStream.junk __strm; ignore_layout __strm)
  | Some x ->
      (XStream.junk __strm;
       (let s = __strm in
        XStream.icons x (XStream.slazy (fun _  -> ignore_layout s))))
  | _ -> XStream.sempty
let print ppf x = pp_print_string ppf (to_string x)
let match_keyword kwd =
  function | `KEYWORD kwd' when kwd = kwd' -> true | _ -> false
let extract_string: [> t] -> string =
  function
  | `KEYWORD s|`SYMBOL s|`LID s|`UID s|`INT (_,s)|`INT32 (_,s)|`INT64 (_,s)
    |`NATIVEINT (_,s)|`FLO (_,s)|`CHAR (_,s)|`STR (_,s)|`LABEL s|`OPTLABEL s
    |`COMMENT s|`BLANKS s|`ESCAPED_IDENT s -> s
  | tok ->
      invalid_arg
        ("Cannot extract a string from this token: " ^ (to_string tok))
let keyword_conversion tok is_kwd =
  match tok with
  | `SYMBOL s|`LID s|`UID s when is_kwd s -> `KEYWORD s
  | `ESCAPED_IDENT s -> `LID s
  | _ -> tok
let check_keyword_as_label tok loc is_kwd =
  let s = match tok with | `LABEL s -> s | `OPTLABEL s -> s | _ -> "" in
  if (s <> "") && (is_kwd s) then err (Keyword_as_label s) loc else ()
let check_unknown_keywords tok loc =
  match tok with | `SYMBOL s -> err (Illegal_token s) loc | _ -> ()