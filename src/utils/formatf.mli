

include module type of Format with type formatter = Format.formatter
val pp_print_list : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
val pp_print_int32 : formatter -> int32 -> unit
val pp_print_int64 : formatter -> int64 -> unit
val pp_print_nativeint : formatter -> nativeint -> unit
val pp_print_float : formatter -> float -> unit
val pp_print_string : formatter -> string -> unit
val pp_print_bool : formatter -> bool -> unit
val pp_print_char : formatter -> char -> unit
val pp_print_unit : formatter -> unit -> unit
val pp_print_option : (formatter -> 'b -> unit) -> formatter -> 'b option -> unit

type space_formatter =  (unit, formatter, unit )format

val pp_list : ?sep:space_formatter ->
  ?first:space_formatter ->
    ?last:space_formatter ->
      (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
          
val pp_option : ?first:space_formatter ->
  ?last:space_formatter ->
    (formatter -> 'a -> unit) -> formatter -> 'a option -> unit
