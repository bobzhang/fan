


open Automata_def

val encode_lexdef :
  entry list ->
  Fcset.t array * (lexer_entry * bool) list

val encode_single_lexdef :
  entry -> Fcset.t array * (lexer_entry * bool)
      
