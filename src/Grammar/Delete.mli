
open Structure
  
val delete_rule_in_tree :
  internal_entry ->
  symbol list ->
  tree -> (symbol list option * tree) option
      
val decr_keyw_use : gram -> symbol -> unit

val decr_keyw_use_in_tree : gram -> tree -> unit
    
val delete_rule_in_suffix :
  internal_entry ->
  symbol list -> level list -> level list
      
val delete_rule_in_prefix :
  internal_entry ->
  symbol list -> level list -> level list
      
val delete_rule_in_level_list :
  internal_entry ->
  symbol list -> level list -> level list
      
val delete_rule : internal_entry -> symbol list -> unit
