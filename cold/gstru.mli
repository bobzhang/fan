open Gstructure
(* val gram_of_entry : entry -> gram *)
val mk_action : 'a -> Gaction.t
val string_of_token : [> FToken.t ] -> string
val flatten_tree : tree -> symbol list list

type brothers = private
  | Bro of symbol * brothers list
  | End

val get_brothers : tree -> brothers list
val get_children : brothers list -> symbol list
val get_first : tree -> symbol list
val get_first_from :
  level list -> (symbol, unit) Hashtbl.t -> unit
