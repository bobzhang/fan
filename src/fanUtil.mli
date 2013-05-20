val normal_handler : exn -> string option
val valid_float_lexeme : string -> string
val float_repres : float -> string

val cvt_int_literal: string ->
  [> `INT of (int*string) | `INT32 of (int32*string) | `INT64 of (int64*string) | `NATIVEINT of (nativeint*string) ]


val symbolchar : string -> int -> bool

val with_open_out_file : string option -> (out_channel -> 'a) -> unit
val dump_ast : string -> 'a -> out_channel -> unit
val dump_pt : string -> string -> 'a -> out_channel -> unit
val char_of_char_token : FanLoc.t -> string -> char
val string_of_string_token : FanLoc.t -> string -> string
val remove_underscores : string -> string



type anti_cxt = {
    cxt:string;
    sep:  string option;
    mutable decorations:  string; (* keep it simple first*)
    content:string;
  }
      
val mk_anti: ?c:string ->
  ?sep:string -> 'a -> string -> string -> [> `Ant of 'a * anti_cxt ]
(* val mk_anti: ?c:string -> ?sep:string -> string -> string -> anti_cxt     *)

val add_context: anti_cxt -> string -> anti_cxt        
val pp_print_anti_cxt : Format.formatter -> anti_cxt -> unit    
  
