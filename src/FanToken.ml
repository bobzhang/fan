open Format;
open LibUtil;
open FanSig;


type error = 
    [ Illegal_token of string
    | Keyword_as_label of string
    | Illegal_token_pattern of (string * string)
    | Illegal_constructor of string];


exception TokenError of  error;

let print_basic_error ppf = fun
    [Illegal_token s ->
      fprintf ppf "Illegal token (%s)" s
    |Keyword_as_label kwd ->
        fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
    |Illegal_token_pattern (p_con, p_prm) ->
        fprintf ppf "Illegal token pattern: %s %S" p_con p_prm
    |Illegal_constructor con ->
        fprintf ppf "Illegal constructor %S" con
    ];

let string_of_error_msg = to_string_of_printer print_basic_error;

Printexc.register_printer (fun
  [TokenError e -> Some (string_of_error_msg e)
  | _ -> None]);
  
let to_string  =fun
  [ `KEYWORD s    -> sprintf "`KEYWORD %S" s
  | `SYMBOL s     -> sprintf "`SYMBOL %S" s
  | `LIDENT s     -> sprintf "`LIDENT %S" s
  | `UIDENT s     -> sprintf "`UIDENT %S" s
  | `INT (_,s)      -> sprintf "`INT %s" s
  | `INT32 (_, s)    -> sprintf "`INT32 %sd" s
  | `INT64 (_, s)    -> sprintf "`INT64 %sd" s
  | `NATIVEINT (_, s)-> sprintf "`NATIVEINT %sd" s
  | `FLOAT (_, s)    -> sprintf "`FLOAT %s" s
  | `CHAR (_,s)     -> sprintf "`CHAR '%s'" s
  | `STRING (_, s)   -> sprintf "`STRING \"%s\"" s
        (* here it's not %S since the string is already escaped *)
  | `LABEL s      -> sprintf "`LABEL %S" s
  | `OPTLABEL s   -> sprintf "`OPTLABEL %S" s
  | `ANTIQUOT (n, s) -> sprintf "`ANTIQUOT %S: %S" n s (* use S for n FIX*)
  | `QUOTATION x  -> sprintf "`QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
        x.q_name x.q_loc x.q_shift x.q_contents
  | `COMMENT s    -> sprintf "`COMMENT %S" s
  | `BLANKS s     -> sprintf "`BLANKS %S" s
  | `NEWLINE      -> sprintf "`NEWLINE"
  | `EOI          -> sprintf "`EOI"
  | `ESCAPED_IDENT s -> sprintf "`ESCAPED_IDENT %S" s
  | `LINE_DIRECTIVE (i, None) -> sprintf "`LINE_DIRECTIVE %d" i
  | `LINE_DIRECTIVE (i, (Some s)) -> sprintf "`LINE_DIRECTIVE %d %S" i s ];

let token_to_string = fun
  [ #token as x -> to_string x
  | _ -> invalid_arg "token_to_string not implemented for this token"]; (* FIXME*)
  
let err error loc =
  raise (FanLoc.Exc_located loc (TokenError error));
    

let error_no_respect_rules p_con p_prm =
  raise (TokenError (Illegal_token_pattern p_con p_prm));

let check_keyword _ = true;
  (* FIXME let lb = Lexing.from_string s in
     let next () = token default_context lb in
     try
     match next () with
     [ SYMBOL _ | UIDENT _ | LIDENT _ -> (next () = EOI)
     | _ -> false ]
     with [ Stream.Error _ -> false ];                        *)

let error_on_unknown_keywords = ref false;

let rec ignore_layout  = parser
  [ [< (`COMMENT _ | `BLANKS _ | `NEWLINE | `LINE_DIRECTIVE _ , _); 's >] ->
    ignore_layout s
  | [< x; 's >] -> [< x; '(ignore_layout s) >]
  | [< >] -> [< >] ];

  
let print ppf x = pp_print_string ppf (token_to_string x);
    
let match_keyword kwd =  fun
  [ `KEYWORD kwd' when kwd = kwd' -> true
  | _ -> false ];

(*
  {[
  x=STRING -> extract_string x
  ]}
 *)  
let extract_string = fun
  [ `KEYWORD s | `SYMBOL s | `LIDENT s | `UIDENT s | `INT (_, s) | `INT32 (_, s) |
  `INT64 (_, s) | `NATIVEINT (_ ,s) | `FLOAT (_, s) | `CHAR (_, s) | `STRING (_, s) |
  `LABEL s | `OPTLABEL s | `COMMENT s | `BLANKS s | `ESCAPED_IDENT s -> s
  | tok ->
      invalid_arg ("Cannot extract a string from this token: "^
                   to_string tok) ];

let keyword_conversion tok is_kwd = match tok with
  [ `SYMBOL s | `LIDENT s | `UIDENT s when is_kwd s -> `KEYWORD s
  | `ESCAPED_IDENT s -> `LIDENT s (* ESCAPED_IDENT *)
  | _ -> tok ];

let check_keyword_as_label tok loc is_kwd =
  let s =  match tok with
  [ `LABEL s         -> s
  | `OPTLABEL s      -> s
  | _               -> "" ] in
  if s <> "" && is_kwd s then err (Keyword_as_label s) loc else ();
    
let check_unknown_keywords tok loc = match tok with
  [ `SYMBOL s -> err (Illegal_token s) loc
  | _        -> () ];
  
  

module Filter = struct
  let mk ~is_kwd ={
    is_kwd ;
    filter = ignore_layout };
    
  let filter x =
    let f (tok, loc) = 
      let tok = keyword_conversion tok x.is_kwd in begin 
        (* check_keyword_as_label tok loc x.is_kwd ; *)
        (* if !error_on_unknown_keywords  then *)
        (*   check_unknown_keywords tok loc *)
        (* else (); *)
        (* debug token "@[<hov 2>Lexer before filter:@ %a@ at@ %a@]@." *)
        (*   print tok FanLoc.dump loc in *)
        (tok, loc)
      end in
      (* let rec filter = parser *)
      (*   [ [< (tok, loc); 's >] -> [< f tok loc; '(filter s) >] *)
      (*   | [< >] -> [< >] ] in *)
      (* let rec tracer = (\* FIXME add a debug block construct *\) parser *)
      (*   [ [< ((_tok, _loc) as x); 'xs >] -> *)
      (*       debug token "@[<hov 2>Lexer after filter:@ %a@ at@ %a@]@." *)
      (*                   print _tok FanLoc.dump _loc in *)
      (*       [< x; 'tracer xs >] *)
      (*   | [< >] -> [< >] ] *)
  (* in fun strm -> tracer (x.filter (filter strm)); *)
    fun strm -> x.filter (Stream.map f strm);

  let define_filter x f = x.filter <- f x.filter;
    
  let keyword_added _ _ _ = ();
  let keyword_removed _ _ = ();
end;



