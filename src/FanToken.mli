(* open LibUtil *)
type quotation ={
    q_name : string;
    q_loc : string;
    q_shift : int;
    q_contents : string
  }
type token =
  [  `KEYWORD of string
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
  | `LINE_DIRECTIVE of (int *  string option )
  | `EOI]
      
type error =
    Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string * string)
  | Illegal_constructor of string

exception TokenError of error

val print_basic_error : Format.formatter -> error -> unit

val string_of_error_msg : error -> string

val to_string : (* FanSig. *)token  -> string

val token_to_string : [> (* FanSig. *)token ] -> string

val err : error -> FanLoc.t -> 'a

val error_no_respect_rules : string -> string -> 'a

val check_keyword : 'a -> bool

val error_on_unknown_keywords : bool ref

val ignore_layout :
  (([>  (* FanSig. *)token ] as 'a) * 'e) XStream.t -> ('a * 'e) XStream.t

val print : Format.formatter -> [> (* FanSig. *)token ] -> unit

val match_keyword : 'a -> [> `KEYWORD of 'a ] -> bool

val extract_string : [> (* FanSig. *)token ] -> string

val keyword_conversion :
  ([>(* FanSig. *)token] as 'a) ->
  (string -> bool) -> 'a

val check_keyword_as_label :
  [> `LABEL of string | `OPTLABEL of string ] ->
  FanLoc.t -> (string -> bool) -> unit

val check_unknown_keywords : [> `SYMBOL of string ] -> FanLoc.t -> unit

(* module Filter : *)
(*   sig *)
(*     val mk : is_kwd:(string -> bool) -> FanSig.filter *)
(*     val filter : *)
(*       FanSig.filter -> *)
(*       (FanSig.token * FanLoc.t) XStream.t -> *)
(*       (FanSig.token * FanLoc.t) XStream.t *)
(*     val define_filter : *)
(*       FanSig.filter -> (FanSig.token_filter -> FanSig.token_filter) -> unit *)
(*     val keyword_added : 'a -> 'b -> 'c -> unit *)
(*     val keyword_removed : 'a -> 'b -> unit *)
(*   end *)
