open Structure

val is_before :
    [> Structure.terminal ] -> [> Structure.terminal ] -> bool
  
val derive_eps : symbol -> bool
val tree_derive_eps : tree -> bool
val empty_lev : string option -> assoc option -> level

val change_lev:  level ->  string
  -> string option -> assoc option -> level
val change_to_self: 'a -> ([> `Snterm of 'a | `Sself ] as 'b) -> 'b
val levels_of_entry: internal_entry -> level list option
val find_level:
    ?position:position -> internal_entry -> level list
      -> level list * (string option -> assoc option -> level) * level list
val check_gram: internal_entry -> symbol -> unit

val tree_check_gram: internal_entry -> tree -> unit

val get_initial: ([> `Sself ] as 'a) list -> bool * 'a list

val insert_tokens: gram -> symbol list -> unit

val insert_production_in_tree:
    internal_entry -> production -> tree -> tree

val insert_production_in_level:
    internal_entry -> bool ->
      production -> level -> level
          
val insert_olevels_in_levels :
    internal_entry ->
      position option ->
        olevel list
        -> level list
val extend :
  internal_entry -> position  option *
      (string option * assoc option *
         (symbol list * Action.t) list) list -> unit
