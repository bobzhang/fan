

include module type of Format with type formatter = Format.formatter

type 'a t  = formatter -> 'a -> unit       

type space_formatter =  (unit, formatter, unit )format

val pp_print_list : 'a t -> 'a list t 

val pp_print_int32 : int32 t 

val pp_print_int64 : int64 t 

val pp_print_nativeint : nativeint t 

val pp_print_float : float t 

val pp_print_string : string t 

val pp_print_bool : bool t 

val pp_print_char : char t 

val pp_print_unit : unit t

val pp_print_option : 'b t  -> 'b option t


val pp_list : ?sep:space_formatter ->
  ?first:space_formatter ->
    ?last:space_formatter -> 'a t -> 'a list t 
          
val pp_option : ?first:space_formatter ->
  ?last:space_formatter -> 'a  t  -> 'a option t 

val to_string:  'a t  -> 'a -> string
