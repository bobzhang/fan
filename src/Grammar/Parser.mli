open Structure


val get_cur_loc: FanLoc.t parse
val get_prev_loc: FanLoc.t parse 
    
val add_loc: FanLoc.t -> 'b parse -> ('b*FanLoc.t) parse


val level_number: internal_entry -> string -> int

    
val top_symb: internal_entry -> symbol -> symbol
val top_tree: internal_entry -> tree -> tree
    
val entry_of_symb: internal_entry -> symbol -> internal_entry

val continue:
    internal_entry  ->
      symbol ->
        tree ->
          Action.t parse -> Action.t cont_parse 
val skip_if_empty: FanLoc.t -> Action.t parse

val do_recover:
  (internal_entry -> int -> int -> tree -> Action.t parse) ->
  internal_entry -> int -> int  -> symbol ->  tree -> Action.t cont_parse

val recover:
  (internal_entry -> int -> int -> tree -> Action.t parse) ->
  internal_entry -> int -> int -> symbol -> tree -> Action.t cont_parse

val parser_of_tree: internal_entry -> int -> int -> tree -> Action.t parse

val parser_cont:
     internal_entry -> int ->  int -> symbol -> tree -> 
       Action.t parse -> Action.t cont_parse 

val parser_of_terminals: terminal list -> Action.t cont_parse  -> Action.t parse
val parser_of_symbol: internal_entry ->  symbol -> int  -> Action.t parse
    
val parse_top_symb: internal_entry -> symbol -> Action.t parse
val start_parser_of_levels: internal_entry -> level list -> int -> Action.t parse 
val start_parser_of_entry:  internal_entry ->  int -> Action.t parse 

val continue_parser_of_levels: internal_entry -> int -> level list -> int -> Action.t cont_parse 
val continue_parser_of_entry:  internal_entry -> int -> Action.t cont_parse
