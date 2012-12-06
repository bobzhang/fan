
open Structure
  
val get_prev_loc_only: bool ref

val empty_entry: string -> 'a -> 'b

val keep_prev_loc:
    (FanToken.token * FanLoc.t) XStream.t -> token_stream

val drop_prev_loc: token_stream -> (FanToken.token * FanLoc.t) XStream.t

val get_cur_loc:  token_stream -> FanLoc.t
       
val get_prev_loc:
    token_stream -> FanLoc.t
        
val is_level_labelled:  string -> level -> bool
        
val warning_verbose:  bool ref
    
val get_terminals:
  node ->
  (terminal list * terminal * tree) option
      
val eq_Stoken_ids:
    descr -> descr -> bool
        
val logically_eq_symbols:
  internal_entry -> symbol -> symbol -> bool
      
val eq_symbol:  symbol ->symbol -> bool
