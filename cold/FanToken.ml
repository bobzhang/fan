let _ = ()
open StdLib
type quotation = 
  {
  q_name: string;
  q_loc: string;
  q_shift: int;
  q_contents: string} 
type t =
  [ `KEYWORD of string | `SYMBOL of string | `Lid of string | `Uid of string
  | `ESCAPED_IDENT of string | `INT of (int* string)
  | `INT32 of (int32* string) | `INT64 of (int64* string)
  | `NATIVEINT of (nativeint* string) | `Flo of (float* string)
  | `CHAR of (char* string) | `STR of (string* string) | `LABEL of string
  | `OPTLABEL of string | `QUOTATION of quotation | `Ant of (string* string)
  | `COMMENT of string | `BLANKS of string | `NEWLINE
  | `LINE_DIRECTIVE of (int* string option) | `EOI] 
type error =  
  | Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string* string)
  | Illegal_constructor of string 
let pp_print_quotation: 'fmt -> quotation -> 'result =
  fun fmt  { q_name = a0; q_loc = a1; q_shift = a2; q_contents = a3 }  ->
    Format.fprintf fmt
      "@[<hv 1>{q_name:%a;@,q_loc:%a;@,q_shift:%a;@,q_contents:%a}@]"
      pp_print_string a0 pp_print_string a1 pp_print_int a2 pp_print_string
      a3
let pp_print_t: 'fmt -> t -> 'result =
  fun fmt  ->
    function
    | `KEYWORD a0 ->
        Format.fprintf fmt "@[<1>(`KEYWORD@ %a)@]" pp_print_string a0
    | `SYMBOL a0 ->
        Format.fprintf fmt "@[<1>(`SYMBOL@ %a)@]" pp_print_string a0
    | `Lid a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" pp_print_string a0
    | `Uid a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" pp_print_string a0
    | `ESCAPED_IDENT a0 ->
        Format.fprintf fmt "@[<1>(`ESCAPED_IDENT@ %a)@]" pp_print_string a0
    | `INT (a0,a1) ->
        Format.fprintf fmt "@[<1>(`INT@ %a@ %a)@]" pp_print_int a0
          pp_print_string a1
    | `INT32 (a0,a1) ->
        Format.fprintf fmt "@[<1>(`INT32@ %a@ %a)@]" pp_print_int32 a0
          pp_print_string a1
    | `INT64 (a0,a1) ->
        Format.fprintf fmt "@[<1>(`INT64@ %a@ %a)@]" pp_print_int64 a0
          pp_print_string a1
    | `NATIVEINT (a0,a1) ->
        Format.fprintf fmt "@[<1>(`NATIVEINT@ %a@ %a)@]" pp_print_nativeint
          a0 pp_print_string a1
    | `Flo (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" pp_print_float a0
          pp_print_string a1
    | `CHAR (a0,a1) ->
        Format.fprintf fmt "@[<1>(`CHAR@ %a@ %a)@]" pp_print_char a0
          pp_print_string a1
    | `STR (a0,a1) ->
        Format.fprintf fmt "@[<1>(`STR@ %a@ %a)@]" pp_print_string a0
          pp_print_string a1
    | `LABEL a0 ->
        Format.fprintf fmt "@[<1>(`LABEL@ %a)@]" pp_print_string a0
    | `OPTLABEL a0 ->
        Format.fprintf fmt "@[<1>(`OPTLABEL@ %a)@]" pp_print_string a0
    | `QUOTATION a0 ->
        Format.fprintf fmt "@[<1>(`QUOTATION@ %a)@]" pp_print_quotation a0
    | `Ant (a0,a1) ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" pp_print_string a0
          pp_print_string a1
    | `COMMENT a0 ->
        Format.fprintf fmt "@[<1>(`COMMENT@ %a)@]" pp_print_string a0
    | `BLANKS a0 ->
        Format.fprintf fmt "@[<1>(`BLANKS@ %a)@]" pp_print_string a0
    | `NEWLINE -> Format.fprintf fmt "`NEWLINE"
    | `LINE_DIRECTIVE (a0,a1) ->
        Format.fprintf fmt "@[<1>(`LINE_DIRECTIVE@ %a@ %a)@]" pp_print_int a0
          (pp_print_option pp_print_string) a1
    | `EOI -> Format.fprintf fmt "`EOI"
let pp_print_error: 'fmt -> error -> 'result =
  fun fmt  ->
    function
    | Illegal_token a0 ->
        Format.fprintf fmt "@[<1>(Illegal_token@ %a)@]" pp_print_string a0
    | Keyword_as_label a0 ->
        Format.fprintf fmt "@[<1>(Keyword_as_label@ %a)@]" pp_print_string a0
    | Illegal_token_pattern a0 ->
        Format.fprintf fmt "@[<1>(Illegal_token_pattern@ %a)@]"
          (fun fmt  (a0,a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_string a0
               pp_print_string a1) a0
    | Illegal_constructor a0 ->
        Format.fprintf fmt "@[<1>(Illegal_constructor@ %a)@]" pp_print_string
          a0
type 'a token = [> t] as 'a 
type stream = (t* FanLoc.t) XStream.t 
type 'a estream = ('a token* FanLoc.t) XStream.t 
type 'a parse = stream -> 'a 
type filter = stream -> stream 
exception TokenError of error
open LibUtil
let string_of_error_msg = to_string_of_printer pp_print_error
let _ =
  Printexc.register_printer
    (function | TokenError e -> Some (string_of_error_msg e) | _ -> None)
let token_to_string = to_string_of_printer pp_print_t
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
  | `KEYWORD s|`SYMBOL s|`Lid s|`Uid s|`INT (_,s)|`INT32 (_,s)|`INT64 (_,s)
    |`NATIVEINT (_,s)|`Flo (_,s)|`CHAR (_,s)|`STR (_,s)|`LABEL s|`OPTLABEL s
    |`COMMENT s|`BLANKS s|`ESCAPED_IDENT s -> s
  | tok ->
      invalid_arg
        ("Cannot extract a string from this token: " ^ (to_string tok))
let keyword_conversion tok is_kwd =
  match tok with
  | `SYMBOL s|`Lid s|`Uid s when is_kwd s -> `KEYWORD s
  | `ESCAPED_IDENT s -> `Lid s
  | _ -> tok
let check_keyword_as_label tok loc is_kwd =
  let s = match tok with | `LABEL s -> s | `OPTLABEL s -> s | _ -> "" in
  if (s <> "") && (is_kwd s) then err (Keyword_as_label s) loc else ()
let check_unknown_keywords tok loc =
  match tok with | `SYMBOL s -> err (Illegal_token s) loc | _ -> ()