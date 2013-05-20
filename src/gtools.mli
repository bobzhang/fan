
open Gstructure
  
open FanToken

val empty_entry: string -> 'a -> 'b

val get_cur_loc:  stream -> FanLoc.t
       
val get_prev_loc:  stream -> FanLoc.t
        
val is_level_labelled:  string -> level -> bool
        
    
val get_terminals: node ->  (terminal list * terminal * tree) option
      
val eq_Stoken_ids: descr -> descr -> bool
        
val logically_eq_symbols: entry -> symbol -> symbol -> bool


(** used in [Ginsert] *)      
val eq_symbol:  symbol ->symbol -> bool
