type node = { 
  id : int; 
  mutable eps : node list; 
  mutable trans : (Cset.t * node) list;
}

type regexp = private node -> node

type state = node list
      
val chars: Cset.t -> regexp
val seq: regexp -> regexp -> regexp
val alt: regexp -> regexp -> regexp
val rep: regexp -> regexp
val plus: regexp -> regexp
val eps: regexp

val compile: regexp array -> (int * int array * bool array) array
val partitions: unit -> (int * (int * int * int) list) list

val new_node : unit -> node
val compile_re : (node -> 'a) -> 'a * node
val add_node : node list -> node -> node list
val add_nodes : node list -> node list -> node list
val transition : node list -> (int * int) list array * node list array
val find_alloc : ('a, int) Hashtbl.t -> int ref -> 'a -> int
val get_part : Cset.t array -> int

    

val named_regexps : (string, regexp) Hashtbl.t
type decision_tree =
    Lte of int * decision_tree * decision_tree
  | Table of int * int array
  | Return of int
val decision : (int * int * int) list -> decision_tree
val limit : int
val decision_table : (int * int * int) list -> decision_tree
val simplify : int -> int -> decision_tree -> decision_tree
    
