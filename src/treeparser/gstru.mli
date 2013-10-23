open Gstructure

val mk_action : 'a -> Gaction.t

val string_of_token : [> Tokenf.t ] -> string

val flatten_tree : tree -> symbol list list

type brothers = private
  | Bro of symbol * brothers list
  | End

val get_brothers : tree -> brothers list
val get_children : brothers list -> symbol list
val get_first : tree -> symbol list
val get_first_from :
  level list -> symbol Hashset.t -> unit
