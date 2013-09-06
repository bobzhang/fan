(* raised when there are too many bindings (>= 254 memory cells) *)
exception Memory_overflow



open Translate_lex
val make_single_dfa :
    'a entry ->
      'a Automata_def.automata_entry * Automata_def.automata array
val make_dfa :
    'a entry list ->
      'a Automata_def.automata_entry list * Automata_def.automata array 
