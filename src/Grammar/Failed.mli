open Structure
val name_of_descr : [> `Antiquot ] * string -> string
val name_of_symbol :
  internal_entry -> [> symbol ] -> string
val name_of_symbol_failed :
  internal_entry -> symbol -> string
val name_of_tree_failed :
  internal_entry -> tree -> string
val magic : 'a -> 'b -> 'c
val tree_failed :
  internal_entry ->
  'a -> symbol -> tree -> string
val symb_failed :
  internal_entry ->
  'a -> symbol -> symbol -> string
val symb_failed_txt :
  internal_entry -> symbol -> symbol -> string

val tree_in_entry :
  symbol -> tree -> desc -> tree
