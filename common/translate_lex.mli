



open Automata_def
open Tag_regexp
type concrete_regexp
      =
  | Epsilon
  | Eof      
  | Characters of Fcset.t
  | Sequence of concrete_regexp * concrete_regexp
  | Alternative of concrete_regexp * concrete_regexp
  | Repetition of concrete_regexp
  | Bind of concrete_regexp * (FLoc.t * string)

type 'a lexer_entry = { 
    lex_regexp: regexp;
    lex_mem_tags: int ;
    lex_actions: (int *  t_env * 'a) list
  }        
(**
   {[
   regexp_for_string "abcd";;

   Sequence (Characters [ 97,97 ],
     Sequence (Characters [ 98,98 ],
     Sequence (Characters [ 99,99 ], Characters [ 100,100 ])))

   ]}
 *)
val regexp_for_string : string -> concrete_regexp

val as_cset : concrete_regexp -> Fcset.t

(** remove Bind in [concrete_regexp] *)    
val remove_as : concrete_regexp -> concrete_regexp
    
type 'a entry = {
    shortest : bool ;
    clauses : (concrete_regexp * 'a) list
  }
  
val encode_lexdef :
    'a entry list ->
      Fcset.t array * ('a lexer_entry * bool) list


val encode_single_lexdef :
    'a entry -> Fcset.t array * ('a lexer_entry * bool)


        
