

include module type of String
val init : int -> (int -> char) -> string
val is_empty : string -> bool
val not_empty : string -> bool
val starts_with : string -> string -> bool
val ends_with : string -> string -> bool
val of_char : char -> string
val drop_while : (char -> bool) -> string -> string
val neg : string -> string
val map : (char -> char) -> string -> string
val lowercase : string -> string
val find_from : string -> int -> string -> int
val find : string -> string -> int
val split : string -> string -> string * string
val rfind_from : string -> int -> string -> int
val rfind : string -> string -> int
val nsplit : string -> string -> string list

