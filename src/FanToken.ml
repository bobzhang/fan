open Format;


(** The generic quotation type.
    To see how fields are used here is an example:
    {:q_name@q_loc|q_contents|}
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)
type quotation ={
    q_name : string;
    q_loc : string;
    q_shift : int;
    q_contents : string
  };


      
(*
  For some tokens the data constructor holds two representations with the
  evaluated one and the source one. For example
  the INT data constructor holds an integer and a string, this string can
  contains more information that's needed for a good pretty-printing
  ("42", "4_2", "0000042", "0b0101010"...).

  The meaning of the tokens are:
  -      [KEYWORD s] is the keyword [s].
  -      [LIDENT s] is the ident [s] starting with a lowercase letter.
  -      [UIDENT s] is the ident [s] starting with an uppercase letter.
  -      [INT i s] (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s])
         the integer constant [i] whose string source is [s].
  -      [FLOAT f s] is the float constant [f] whose string source is [s].
  -      [STRING s s'] is the string constant [s] whose string source is [s'].
  -      [CHAR c s] is the character constant [c] whose string source is [s].
  -      [QUOTATION q] is a quotation [q], see {!AstQuotation.t} for more information.
  -      [ANTIQUOT n s] is an antiquotation [n] holding the string [s].
  -      [EOI] is the end of input.

  Warning: the second string associated with the constructor [STRING] is
  the string found in the source without any interpretation. In particular,
  the backslashes are not interpreted. For example, if the input is ["\n"]
  the string is *not* a string with one element containing the character
  "return", but a string of two elements: the backslash and the character
  ["n"]. To interpret a string use the first string of the [STRING]
  constructor (or if you need to compute it use the module
  {!Camlp4.Struct.Token.Eval}. Same thing for the constructor [CHAR]. *)
  
type t =
  [=  `KEYWORD of string
  | `SYMBOL of string
  | `LID of string
  | `UID of string
  | `ESCAPED_IDENT of string (* (+)*)
  | `INT of (int * string )
  | `INT32 of (int32 * string )
  | `INT64 of (int64 * string )
  | `NATIVEINT of (nativeint * string )
  | `FLO of (float * string )
  | `CHAR of (char * string )
  | `STR of (string * string )
  | `LABEL of string
  | `OPTLABEL of string
  | `QUOTATION of quotation
  | `ANT of (string * string )
  | `COMMENT of string
  | `BLANKS of string
  | `NEWLINE
  | `LINE_DIRECTIVE of (int * option string )
  | `EOI];

type token 'a = [> t] as 'a;

type error = 
    [ Illegal_token of string
    | Keyword_as_label of string
    | Illegal_token_pattern of (string * string)
    | Illegal_constructor of string];

type stream = XStream.t (t * FanLoc.t);

type estream 'a = XStream.t (token 'a * FanLoc.t);

type parse 'a = stream -> 'a;

type filter = stream -> stream;

  
exception TokenError of  error;

let print_basic_error ppf = fun
    [Illegal_token s ->
      fprintf ppf "Illegal token (%s)" s
    |Keyword_as_label kwd ->
        fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
    |Illegal_token_pattern (p_con, p_prm) ->
        fprintf ppf "Illegal token pattern: %s %S" p_con p_prm
    |Illegal_constructor con ->
        fprintf ppf "Illegal constructor %S" con  ];

(* FIXME duplicate copy of LibUtil *)  
let to_string_of_printer printer v =
  let buf = Buffer.create 30 in
  let () = Format.bprintf buf "@[%a@]" printer v in Buffer.contents buf;

let string_of_error_msg = to_string_of_printer print_basic_error;

Printexc.register_printer (fun
  [TokenError e -> Some (string_of_error_msg e)
  | _ -> None]);
  
let token_to_string  : t -> string =fun
  [ `KEYWORD s    -> sprintf "`KEYWORD %S" s
  | `SYMBOL s     -> sprintf "`SYMBOL %S" s
  | `LID s     -> sprintf "`LID %S" s
  | `UID s     -> sprintf "`UID %S" s
  | `INT (_,s)      -> sprintf "`INT %s" s
  | `INT32 (_, s)    -> sprintf "`INT32 %sd" s
  | `INT64 (_, s)    -> sprintf "`INT64 %sd" s
  | `NATIVEINT (_, s)-> sprintf "`NATIVEINT %sd" s
  | `FLO (_, s)    -> sprintf "`FLO %s" s
  | `CHAR (_,s)     -> sprintf "`CHAR '%s'" s
  | `STR (_, s)   -> sprintf "`STR \"%s\"" s
        (* here it's not %S since the string is already escaped *)
  | `LABEL s      -> sprintf "`LABEL %S" s
  | `OPTLABEL s   -> sprintf "`OPTLABEL %S" s
  | `ANT (n, s) -> sprintf "`ANT %S: %S" n s (* use S for n FIX*)
  | `QUOTATION x  -> sprintf "`QUOTATION { q_name=%S; q_loc=%S; q_shift=%d; q_contents=%S }"
        x.q_name x.q_loc x.q_shift x.q_contents
  | `COMMENT s    -> sprintf "`COMMENT %S" s
  | `BLANKS s     -> sprintf "`BLANKS %S" s
  | `NEWLINE      -> sprintf "`NEWLINE"
  | `EOI          -> sprintf "`EOI"
  | `ESCAPED_IDENT s -> sprintf "`ESCAPED_IDENT %S" s
  | `LINE_DIRECTIVE (i, None) -> sprintf "`LINE_DIRECTIVE %d" i
  | `LINE_DIRECTIVE (i, (Some s)) -> sprintf "`LINE_DIRECTIVE %d %S" i s];

let to_string = fun
  [ #t as x -> token_to_string x
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
     with [ XStream.Error _ -> false ];                        *)

let error_on_unknown_keywords = ref false;

let rec ignore_layout  = parser
  [ [< (`COMMENT _ | `BLANKS _ | `NEWLINE | `LINE_DIRECTIVE _ , _); 's >] ->
    ignore_layout s
  | [< x; 's >] -> [< x; '(ignore_layout s) >]
  | [< >] -> [< >] ];

  
let print ppf x = pp_print_string ppf (to_string x);
    
let match_keyword kwd =  fun
  [ `KEYWORD kwd' when kwd = kwd' -> true
  | _ -> false ];

(*
  {[
  x=STRING -> extract_string x
  ]}
 *)  
let extract_string : [> t] -> string = fun
  [ `KEYWORD s | `SYMBOL s | `LID s | `UID s | `INT (_, s) | `INT32 (_, s) |
  `INT64 (_, s) | `NATIVEINT (_ ,s) | `FLO (_, s) | `CHAR (_, s) | `STR (_, s) |
  `LABEL s | `OPTLABEL s | `COMMENT s | `BLANKS s | `ESCAPED_IDENT s-> s
  | tok ->
      invalid_arg ("Cannot extract a string from this token: "^ to_string tok) ];


(* [SYMBOL] should all be filtered into keywords *)  
let keyword_conversion tok is_kwd = match tok with
  [ `SYMBOL s | `LID s | `UID s when is_kwd s -> `KEYWORD s
  | `ESCAPED_IDENT s -> `LID s (* ESCAPED_IDENT *)
  | _ -> tok ];

let check_keyword_as_label tok loc is_kwd =
  let s =  match tok with
  [ `LABEL s         -> s
  | `OPTLABEL s      -> s
  | _               -> "" ] in
  if s <> "" && is_kwd s then err (Keyword_as_label s) loc else ();
    
let check_unknown_keywords tok loc =
  match tok with
  [ `SYMBOL s -> err (Illegal_token s) loc
  | _        -> () ];
  
  




  
