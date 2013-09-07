(* raised when there are too many bindings (>= 254 memory cells) *)
exception Memory_overflow

type tag_action =
  | SetTag of int * int
  | EraseTag of int

type memory_action =
  | Copy of int * int
  | Set of int

type automata_move =
  | Backtrack
  | Goto of int

type automata_trans =
  | No_remember
  | Remember of int * tag_action list
        
type automata =
  | Perform of int * tag_action list
  | Shift of automata_trans * (automata_move * memory_action list) array

type 'a automata_entry = {
    auto_mem_size : int ;
    auto_initial_state : (int * memory_action list);
    auto_actions : (int * Automata_def.t_env * 'a) list
  }

      


      
open Translate_lex
val make_single_dfa :
    'a entry ->
      'a automata_entry * automata array
val make_dfa :
    'a entry list ->
      'a automata_entry list * automata array 
