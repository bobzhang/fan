
open Gstructure
  
val delete_rule_in_tree :
  entry ->
  symbol list ->
  tree -> (symbol list option * tree) option
      
val decr_keyw_use : gram -> symbol -> unit

val decr_keyw_use_in_tree : gram -> tree -> unit
    
val delete_rule_in_suffix :
  entry ->
  symbol list -> level list -> level list
      
val delete_rule_in_prefix :
  entry ->
  symbol list -> level list -> level list
      
val delete_rule_in_level_list :
  entry ->
  symbol list -> level list -> level list
      
val delete_rule : entry -> symbol list -> unit
