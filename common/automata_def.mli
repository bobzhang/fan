(* Representation of automata *)
type tag_info = {id : string ; start : bool ; action : int}

type regexp =
  | Empty
  | Chars of int * bool
  | Action of int
  | Tag of tag_info
  | Seq of regexp * regexp
  | Alt of regexp * regexp
  | Star of regexp



type ident = FLoc.t * string

(* Representation of entry points *)
type tag_base =
  | Start | End | Mem of int

type tag_addr = (tag_base * int)

type ident_info =
  | Ident_string of bool * (tag_base * int) * (tag_base * int)
  | Ident_char of bool * (tag_base * int)

type t_env = (ident * ident_info) list




type 'a lexer_entry = { 
    lex_regexp: regexp;
    lex_mem_tags: int ;
    lex_actions: (int *  t_env * 'a) list
  }


      
      
