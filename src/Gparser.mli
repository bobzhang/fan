open Gstructure

open FanToken
    
val with_loc: 'b parse -> ('b*FanLoc.t) parse


val level_number: entry -> string -> int

val parser_of_tree:
    entry -> int * assoc -> (Gaction.t * FanLoc.t) Stack.t ->  tree ->
      (Gaction.t * FanLoc.t) parse

val parser_of_terminals:
    terminal list -> (Gaction.t * FanLoc.t) list  parse

val parser_of_symbol:
    entry ->  symbol -> int  -> (Gaction.t * FanLoc.t) parse
    

val start_parser_of_levels: entry -> level list -> int -> Gaction.t parse 
val start_parser_of_entry:  entry ->  int -> Gaction.t parse 

val continue_parser_of_levels: entry -> int -> level list -> int -> Gaction.t cont_parse 
val continue_parser_of_entry:  entry -> int -> Gaction.t cont_parse
