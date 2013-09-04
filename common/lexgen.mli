(* raised when there are too many bindings (>= 254 memory cells) *)
exception Memory_overflow



(* The entry point *)
val make_single_dfa:
    LexSyntax.entry ->
      Automata_def.automata_entry * Automata_def.automata array
val make_dfa :
    LexSyntax.entry list -> Automata_def.automata_entry list * Automata_def.automata array
