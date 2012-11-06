open Structure


val get_cur_loc: FanLoc.t parse
val get_prev_loc: FanLoc.t parse 
    
val add_loc: FanLoc.t -> 'b parse -> ('b*FanLoc.t) parse

val try_parser: 'b parse  -> 'b parse 
val level_number : internal_entry -> string -> int

val strict_parsing : bool ref
val strict_parsing_warning : bool ref
    
val top_symb : internal_entry -> symbol -> symbol
val top_tree : internal_entry -> tree -> tree
    
val entry_of_symb : internal_entry -> symbol -> internal_entry

val continue : internal_entry -> FanLoc.t -> Action.t ->  symbol ->  tree ->  efun -> efun 
val skip_if_empty : FanLoc.t -> efun

val do_recover :
  (internal_entry -> 'a -> 'b -> tree -> efun) ->
  internal_entry -> 'a -> 'b -> FanLoc.t -> Action.t -> symbol ->  tree -> efun

val recover :
  (internal_entry -> 'a -> 'b -> tree -> efun) ->
  internal_entry -> 'a -> 'b -> FanLoc.t -> Action.t -> symbol -> tree -> efun

val parser_of_tree : internal_entry -> int -> int -> tree -> efun
val parser_cont :efun -> internal_entry -> int ->  int -> symbol -> tree -> FanLoc.t -> Action.t -> efun 
val parser_of_token_list : (FanLoc.t ->  Action.t -> efun  ) ->  symbol list -> efun
val parser_of_symbol : internal_entry -> int -> symbol -> efun 
val parse_top_symb : internal_entry -> symbol -> efun
val start_parser_of_levels : internal_entry -> level list -> int -> efun 
val start_parser_of_entry :  internal_entry ->  int -> efun 
val continue_parser_of_levels : internal_entry -> int -> level list -> int -> FanLoc.t -> 'a -> efun 
val continue_parser_of_entry :  internal_entry -> int -> FanLoc.t -> Action.t -> efun
