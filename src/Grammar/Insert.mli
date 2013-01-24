open Structure

val higher :
    [> Structure.terminal ] -> [> Structure.terminal ] -> bool
  
val derive_eps: symbol -> bool

val tree_derive_eps: tree -> bool

val empty_lev: label  -> assoc  -> level

val levels_of_entry: entry -> level list option

val find_level:
    ?position:position -> entry -> level list
      -> level list * (level * string) option * level list

val check_gram: entry -> symbol -> unit

val using_symbols: gram -> symbol list -> unit
    
val get_initial: ([> `Sself ] as 'a) list -> bool * 'a list

val add_production: production -> tree -> tree

val add_production_in_level: bool -> production -> level -> level


val merge_level: level -> olevel -> level
val level_of_olevel: olevel -> level    
val insert_olevels_in_levels:
    entry -> position option -> olevel list -> level list
            
val extend :
  entry -> position  option * olevel list -> unit

val extend_single :
  entry -> position  option * olevel  -> unit
