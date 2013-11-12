

(** Internal: Utilities for Fan's grammar *)  
open Gdefs
  


val empty_entry: string -> 'a -> 'b
        
(* val is_level_labelled:  string -> level -> bool *)
        
    
val get_terminals: node ->  (Tokenf.pattern list * Tokenf.pattern * tree) option
      
        
val logically_eq_symbols: entry -> symbol -> symbol -> bool


(** used in [Ginsert] *)      
val eq_symbol :  symbol ->symbol -> bool


val entry_first : entry -> string list    



val flatten_tree : tree -> symbol list list

type brothers = private
  | Bro of symbol * brothers list
  | End

val get_brothers : tree -> brothers list

val get_children : brothers list -> symbol list

val get_first : tree -> symbol list

val get_first_from :
  level list -> symbol Hashset.t -> unit
    
