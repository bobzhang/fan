type t = (int * int) list
val max_code : int
val min_code : int
val empty : 'a list
val singleton : 'a -> ('a * 'a) list
val is_empty : 'a list -> bool
val interval : 'a -> 'a -> ('a * 'a) list
val eof : (int * int) list
val any : (int * int) list
val print : Format.formatter -> (int * int) list -> unit
val dump : (int * int) list -> unit
val union : (int * int) list -> (int * int) list -> (int * int) list
val complement : (int * int) list -> (int * int) list
val intersection : (int * int) list -> (int * int) list -> (int * int) list
val difference : (int * int) list -> (int * int) list -> (int * int) list
val base_char : (int * int) list
val ideographic : (int * int) list
val combining_char : (int * int) list
val digit : (int * int) list
val extender : (int * int) list
val blank : (int * int) list
val letter : (int * int) list
val tr8876_ident_char : (int * int) list
