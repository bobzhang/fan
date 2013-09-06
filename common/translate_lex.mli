



(* open Automata_def *)

type concrete_regexp =
  | Epsilon
  | Characters of Fcset.t
  | Eof
  | Sequence of concrete_regexp * concrete_regexp
  | Alternative of concrete_regexp * concrete_regexp
  | Repetition of concrete_regexp
  | Bind of concrete_regexp * (FLoc.t * string)

  
type 'a entry = {
    shortest : bool ;
    clauses : (concrete_regexp * 'a) list
  }
  
val encode_lexdef :
    'a entry list ->
      Fcset.t array * ('a Automata_def.lexer_entry * bool) list


val encode_single_lexdef :
    'a entry -> Fcset.t array * ('a Automata_def.lexer_entry * bool)


        
