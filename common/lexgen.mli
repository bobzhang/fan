(* raised when there are too many bindings (>= 254 memory cells) *)
exception Memory_overflow

open Automata_def

(* The entry point *)
val make_single_dfa:
    entry ->
      Automata_def.automata_entry * Automata_def.automata array
val make_dfa :
    entry list -> Automata_def.automata_entry list * Automata_def.automata array
