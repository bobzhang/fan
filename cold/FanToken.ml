open Format
open LibUtil
open FanSig
type error =  
  | Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string* string)
  | Illegal_constructor of string 
exception TokenError of error
let print_basic_error ppf =
  function
  | Illegal_token s -> fprintf ppf "Illegal token (%s)" s
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
  | Illegal_token_pattern (p_con,p_prm) ->
      fprintf ppf "Illegal token pattern: %s %S" p_con p_prm
  | Illegal_constructor con -> fprintf ppf "Illegal constructor %S" con
let string_of_error_msg = to_string_of_printer print_basic_error
let _ =
  Printexc.register_printer
    (function | TokenError e -> Some (string_of_error_msg e) | _ -> None)
let to_string: [> FanSig.token] -> string =
  function
  | `KEYWORD s -> sprintf "`KEYWORD %S" s
  | `SYMBOL s -> sprintf "`SYMBOL %S" s
  | `LID s -> sprintf "`LID %S" s
  | `UID s -> sprintf "`UID %S" s
  | `INT (_,s) -> sprintf "`INT %s" s
  | `INT32 (_,s) -> sprintf "`INT32 %sd" s
  | `INT64 (_,s) -> sprintf "`INT64 %sd" s
  | `NATIVEINT (_,s) -> sprintf "`NATIVEINT %sd" s
  | `FLOAT (_,s) -> sprintf "`FLOAT %s" s
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
let token_to_string =
  function
  | #token as x -> to_string x
  | _ -> invalid_arg "token_to_string not implemented for this token"
let err error loc = raise (FanLoc.Exc_located (loc, (TokenError error)))
let error_no_respect_rules p_con p_prm =
  raise (TokenError (Illegal_token_pattern (p_con, p_prm)))
let check_keyword _ = true
let error_on_unknown_keywords = ref false
let rec ignore_layout (__strm : _ Stream.t) =
  match Stream.peek __strm with
  | Some ((`COMMENT _|`BLANKS _|`NEWLINE|`LINE_DIRECTIVE _),_) ->
      (Stream.junk __strm; ignore_layout __strm)
  | Some x ->
      (Stream.junk __strm;
       (let s = __strm in
        Stream.icons x (Stream.slazy (fun _  -> ignore_layout s))))
  | _ -> Stream.sempty
let print ppf x = pp_print_string ppf (token_to_string x)
let match_keyword kwd =
  function | `KEYWORD kwd' when kwd = kwd' -> true | _ -> false
let extract_string: [> FanSig.token] -> string =
  function
  | `KEYWORD s|`SYMBOL s|`LID s|`UID s|`INT (_,s)|`INT32 (_,s)|`INT64 (_,s)|
      `NATIVEINT (_,s)|`FLOAT (_,s)|`CHAR (_,s)|`STR (_,s)|`LABEL s|`OPTLABEL
                                                                    s|
      `COMMENT s|`BLANKS s|`ESCAPED_IDENT s -> s
  | tok ->
      invalid_arg
        ("Cannot extract a string from this token: " ^ (token_to_string tok))
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
module Filter = struct
  let mk ~is_kwd  = { is_kwd; filter = ignore_layout }
  let filter x =
    let f (tok,loc) =
      let tok = keyword_conversion tok x.is_kwd in
      check_keyword_as_label tok loc x.is_kwd; (tok, loc) in
    fun strm  -> x.filter (Stream.map f strm)
  let define_filter x f = x.filter <- f x.filter let keyword_added _ _ _ = ()
  let keyword_removed _ _ = ()
  end