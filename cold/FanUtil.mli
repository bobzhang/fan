val normal_handler : exn -> string option
val valid_float_lexeme : string -> string
val float_repres : float -> string

val cvt_int_literal: string ->
  [> `INT of (int*string) | `INT32 of (int32*string) | `INT64 of (int64*string) | `NATIVEINT of (nativeint*string) ]

val mk_anti : ?c:string -> string -> string -> string
    
(* val append_eLem : 'a list -> 'a -> 'a list *)
val is_antiquot : string -> bool
val view_antiquot : string -> (string * string) option
val add_context: string -> string -> string    
val handle_antiquot_in_string :
  s:string ->
  default:'a ->
  parse:('b -> string -> 'c) -> loc:'b -> decorate:(string -> 'c -> 'a) -> 'a


val symbolchar : string -> int -> bool

val with_open_out_file : string option -> (out_channel -> 'a) -> unit
val dump_ast : string -> 'a -> out_channel -> unit
val dump_pt : string -> string -> 'a -> out_channel -> unit
val char_of_char_token : FanLoc.t -> string -> char
val string_of_string_token : FanLoc.t -> string -> string
val remove_underscores : string -> string


type anti_cxt
val mk_anti_cxt: ?c:string -> string -> string -> anti_cxt
val add_anti_context: anti_cxt -> string -> anti_cxt
val pp_print_anti_cxt : Format.formatter -> anti_cxt -> unit    
  
