(* raised when there are too many bindings (>= 254 memory cells) *)
exception Memory_overflow




val make_single_dfa :
    'a Automata_def.entry ->
      'a Automata_def.automata_entry * Automata_def.automata array
val make_dfa :
    'a Automata_def.entry list ->
      'a Automata_def.automata_entry list * Automata_def.automata array 
