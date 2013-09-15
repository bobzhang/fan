open Gstructure

open Ftoken
    
val with_loc: 'b parse -> ('b*FLoc.t) parse


val level_number: entry -> string -> int

val parser_of_tree:
    entry -> int * assoc -> (Gaction.t * FLoc.t) Stack.t ->  tree ->
      (Gaction.t * FLoc.t) parse

val parser_of_terminals:
    terminal list -> (Gaction.t * FLoc.t) list  parse

val parser_of_symbol:
    entry ->  symbol -> (Gaction.t * FLoc.t) parse
    

val start_parser_of_levels: entry -> level list -> int -> Gaction.t parse 
val start_parser_of_entry:  entry ->  int -> Gaction.t parse 

val continue_parser_of_levels: entry -> int -> level list -> int -> Gaction.t cont_parse 
val continue_parser_of_entry:  entry -> int -> Gaction.t cont_parse
