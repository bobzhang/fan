(* open LibUtil *)
type quotation ={
    q_name : string;
    q_loc : string;
    q_shift : int;
    q_contents : string
  }
type t =
  [  `KEYWORD of string
  | `SYMBOL of string
  | `Lid of string
  | `Uid of string
  | `ESCAPED_IDENT of string (* (+)*)
  | `INT of (int * string )
  | `INT32 of (int32 * string )
  | `INT64 of (int64 * string )
  | `NATIVEINT of (nativeint * string )
  | `Flo of (float * string )
  | `CHAR of (char * string )
  | `STR of (string * string )
  | `LABEL of string
  | `OPTLABEL of string
  | `QUOTATION of quotation
  | `Ant of (string * string )
  | `COMMENT of string
  | `BLANKS of string
  | `NEWLINE
  | `LINE_DIRECTIVE of (int *  string option )
  | `EOI]
      
type error =
    Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string * string)
  | Illegal_constructor of string

exception TokenError of error


type stream = (t * FanLoc.t) XStream.t 

type 'a token  = [> t] as 'a
      
type 'a estream  = ('a token * FanLoc.t) XStream.t
      
type 'a parse = stream -> 'a

type filter = stream -> stream

val pp_print_error: Format.formatter -> error -> unit

val string_of_error_msg: error -> string

val token_to_string: t  -> string

val to_string: [> t]  -> string

val err : error -> FanLoc.t -> 'a

val error_no_respect_rules : string -> string -> 'a

val check_keyword : 'a -> bool

val error_on_unknown_keywords : bool ref

val ignore_layout:  (([>  t ] as 'a) * 'e) XStream.t -> ('a * 'e) XStream.t

val print: Format.formatter -> [> t ] -> unit

val match_keyword: 'a -> [> `KEYWORD of 'a ] -> bool

val extract_string: [> t ] -> string

val keyword_conversion:  ([> t ] as 'a) ->  (string -> bool) -> 'a

val check_keyword_as_label:
  [> `LABEL of string | `OPTLABEL of string ] ->
  FanLoc.t -> (string -> bool) -> unit

val check_unknown_keywords: [> `SYMBOL of string ] -> FanLoc.t -> unit


