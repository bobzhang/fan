open Gstructure

(* val name_of_descr : [> `Antiquot ] * string -> string *)

val name_of_symbol :  entry -> [> symbol ] -> string
val name_of_symbol_failed :  entry -> symbol -> string

val name_of_tree_failed :  entry -> tree -> string

val magic : 'a -> 'b -> 'c

val tree_failed :  ?verbose:bool -> entry ->  'a -> symbol -> tree -> string

val symb_failed :  entry ->  'a -> symbol -> symbol -> string

val symb_failed_txt :  entry -> symbol -> symbol -> string

val tree_in_entry :  symbol -> tree -> desc -> tree
