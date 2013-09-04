(* Representation of automata *)


type automata =
  | Perform of int * tag_action list
  | Shift of automata_trans * (automata_move * memory_action list) array
and automata_trans =
  | No_remember
  | Remember of int * tag_action list
and automata_move =
  | Backtrack
  | Goto of int
and memory_action =
  | Copy of int * int
  | Set of int

and tag_action =
  | SetTag of int * int
  | EraseTag of int

type ident = FAst.lident

(* Representation of entry points *)
type tag_base =
  | Start | End | Mem of int
type tag_addr = Sum of (tag_base * int)
type ident_info =
  | Ident_string of bool * tag_addr * tag_addr
  | Ident_char of bool * tag_addr

type t_env = (ident * ident_info) list

type  automata_entry =
  { auto_mem_size : int ;
    auto_initial_state: (int * memory_action list);
    auto_actions: (int * t_env * FAst.exp) list }


type concrete_regexp =
  | Epsilon
  | Characters of Fcset.t
  | Eof
  | Sequence of concrete_regexp * concrete_regexp
  | Alternative of concrete_regexp * concrete_regexp
  | Repetition of concrete_regexp
  | Bind of concrete_regexp * FAst.lident 

type  entry =
  {shortest : bool ;
   clauses : (concrete_regexp * FAst.exp) list}

type tag_info = {id : string ; start : bool ; action : int}

type regexp =
  | Empty
  | Chars of int * bool
  | Action of int
  | Tag of tag_info
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp

type  lexer_entry = { 
    lex_regexp: regexp;
    lex_mem_tags: int ;
    lex_actions: (int *  t_env * FAst.exp) list
  }


      
      
