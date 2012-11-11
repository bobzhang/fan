open LibUtil
  
type error =
    Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string * string)
  | Illegal_constructor of string

exception TokenError of error

val print_basic_error : Format.formatter -> error -> unit

val string_of_error_msg : error -> string

val to_string : FanSig.token  -> string

val token_to_string : [> FanSig.token ] -> string

val err : error -> FanLoc.t -> 'a

val error_no_respect_rules : string -> string -> 'a

val check_keyword : 'a -> bool

val error_on_unknown_keywords : bool ref

val ignore_layout :
  (([>  FanSig.token ] as 'a) * 'e) Stream.t -> ('a * 'e) Stream.t

val print : Format.formatter -> [> FanSig.token ] -> unit

val match_keyword : 'a -> [> `KEYWORD of 'a ] -> bool

val extract_string : [> FanSig.token ] -> string

val keyword_conversion :
  ([>FanSig.token] as 'a) ->
  (string -> bool) -> 'a

val check_keyword_as_label :
  [> `LABEL of string | `OPTLABEL of string ] ->
  FanLoc.t -> (string -> bool) -> unit

val check_unknown_keywords : [> `SYMBOL of string ] -> FanLoc.t -> unit

module Filter :
  sig
    val mk : is_kwd:(string -> bool) -> FanSig.filter
    val filter :
      FanSig.filter ->
      (FanSig.token * FanLoc.t) Stream.t ->
      (FanSig.token * FanLoc.t) Stream.t
    val define_filter :
      FanSig.filter -> (FanSig.token_filter -> FanSig.token_filter) -> unit
    val keyword_added : 'a -> 'b -> 'c -> unit
    val keyword_removed : 'a -> 'b -> unit
  end
