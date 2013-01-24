open Structure

open FanToken
    
val add_loc: FanLoc.t -> 'b parse -> ('b*FanLoc.t) parse


val level_number: entry -> string -> int

    
    
val entry_of_symb: entry -> symbol -> entry

val parser_of_tree: entry ->
  int * assoc ->
  tree -> Action.t parse

val parser_of_terminals: terminal list -> Action.t cont_parse  -> Action.t parse
val parser_of_symbol: entry ->  symbol -> int  -> Action.t parse
    

val start_parser_of_levels: entry -> level list -> int -> Action.t parse 
val start_parser_of_entry:  entry ->  int -> Action.t parse 

val continue_parser_of_levels: entry -> int -> level list -> int -> Action.t cont_parse 
val continue_parser_of_entry:  entry -> int -> Action.t cont_parse
