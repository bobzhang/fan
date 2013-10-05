

(** Extension to char module
 *)  
include module type of Char

val is_whitespace : char -> bool

val is_newline : char -> bool

val is_digit : char -> bool

val is_uppercase : char -> bool

val is_lowercase : char -> bool

