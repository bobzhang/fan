val normal_handler : exn -> string option
val valid_float_lexeme : string -> string
val float_repres : float -> string
val cvt_int_literal : string -> int
val cvt_int32_literal : string -> int32
val cvt_int64_literal : string -> int64
val cvt_nativeint_literal : string -> nativeint

val mk_anti : ?c:string -> string -> string -> string
    
val append_eLem : 'a list -> 'a -> 'a list
val is_antiquot : string -> bool
val view_antiquot : string -> (string * string) option
val handle_antiquot_in_string :
  s:string ->
  default:'a ->
  parse:('b -> string -> 'c) -> loc:'b -> decorate:(string -> 'c -> 'a) -> 'a
val neg_string : string -> string
val list_remove : 'a -> ('a * 'b) list -> ('a * 'b) list
val symbolchar : string -> int -> bool
val stopped_at : FanLoc.t -> FanLoc.t option
val with_open_out_file : string option -> (out_channel -> 'a) -> unit
val dump_ast : string -> 'a -> out_channel -> unit
val dump_pt : string -> string -> 'a -> out_channel -> unit
val char_of_char_token : FanLoc.t -> string -> char
val string_of_string_token : FanLoc.t -> string -> string
val remove_underscores : string -> string


