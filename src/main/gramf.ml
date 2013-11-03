

open Format
include Gdefs
  
include Gentry


let mk_action = Gaction.mk
type action = Gaction.t 


(*
  source info : lexer.mll from 4.02dev+trunk
  "mod", INFIXOP3("mod");
  "land", INFIXOP3("land");
  "lor", INFIXOP3("lor");
  "lxor", INFIXOP3("lxor");
  "lsl", INFIXOP4("lsl");
  "lsr", INFIXOP4("lsr");
  "asr", INFIXOP4("asr")

  | "#"  
  | "&"  
  | "&&" 
  | "`"  
  | "'"  
  | "("  
  | ")"  
  | "*"  
  | ","  
  | "->" 
  | "."  
  | ".." 
  | ":"  
  | "::" 
  | ":=" 
  | ":>" 
  | ";"  
  | ";;" 
  | "<"  
  | "<-" 
  | "="  
  | "["  
  | "[|" 
  | "[<" 
  | "[>" 
  | "]"  
  | "{"  
  | "{<" 
  | "|"  
  | "||" 
  | "|]" 
  | ">"  
  | ">]" 
  | "}"  
  | ">}" 
  | "[@" 
  | "[%" 
  | "[%%"
  | "[@@"
  | "!"  
  | "!=" { INFIXOP0  }
  | "+"  
  | "+." 
  | "-"  
  | "-." 
  | "!" symbolchar + { PREFIXOP }
  | ['~' '?'] symbolchar + { PREFIXOP }
  | ['=' '<' '>' '|' '&' '$'] symbolchar * { INFIXOP0 }
  | ['@' '^'] symbolchar * { INFIXOP1 }
  | ['+' '-'] symbolchar * { INFIXOP2 }
  | "**" symbolchar * { INFIXOP4 }
  | '%'     
  | ['*' '/' '%'] symbolchar * { INFIXOP3 }
 *)    
let default_keywords =
  ["&&";
   "#";
   "!";
   "-." ;
   "-";
   "+";   
   "_";
   ">]" ;
   "??" ; (* FIXME removed to conform OCaml*)
   "||";
   "<";
   "~";
   ",";
   "|]";
   "->";
   "..";
   ")";
   "=";
   ":";
   "|";
   "[<";
   "==";
   ".";
   "{<";
   ">}";
   ":>";
   "*";
   "<-";
   "&";
   ";;";
   "{";
   "'";
   ";";
   "[";
   "}";
   "[|";
   "[^";
   "`";
   "::";
   "]";
   "[>";
   ":=";
   "(";
   "?";
   ">";
   "[";
   "functor";
   "private";
   "sig";
   "include";
   "exception";
   "inherit";
   "and"; 
   "when";
   "mod";
   "then";
   "initializer";
   "in" ;
   "downto";
   "lsr";
   "as";
   "function";
   "begin";
   "class";
   "land";
   "lxor";
   "do";
   "end";
   "assert";
   "external";
   "virtual";
   "to";
   "try";
   "lsl";
   "struct";
   "else";
   "val" ;
   "constraint";
   "type";
   "new";
   "of";
   "done";
   "for";
   "fun";
   "method" ;
   "mutable";
   "lazy";
   "with";
   "asr";
   "if";
   "while" ;
   "rec";
   "object";
   "or";
   "match" ;
   "open";
   "module";
   "let";
   "lor";
 ]

let gkeywords = ref (Setf.String.of_list default_keywords)
  


          
let gram =  {
  annot="Fan";
  gfilter =
  { kwds =   Setf.String.of_list default_keywords ;
    filter = None;  }
}

let filter = Tokenf.filter gram.gfilter
  
let create_lexer ?(filter=None) ~annot ~keywords   () = {
  annot;
  gfilter = {
  kwds = Setf.String.of_list keywords;
  filter;  
  }
 }


(* FIXME duplicate some code from Entry *)
let mk f = mk_dynamic gram f


let get_filter () = gram.gfilter


let token_stream_of_string s =
    lex_string Locf.string_loc s

  
let debug_origin_token_stream (entry:'a t ) tokens : 'a =
  parse_origin_tokens entry tokens
  
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry tokens 

(* with a special exception handler *)  
let parse_string_safe ?(loc=Locf.string_loc) entry  s =
  try
    parse_string entry ~loc s
  with Locf.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      Locf.error_report (loc,s);
      Locf.raise loc e ;
  end 
;;
    

%import{
Gfold:
  sfold0
  sfold1
  sfold0sep
  sfold1sep
  ;
};;    



let find_level ?position (entry:Gdefs.entry) =
  let (_,f,_) = Ginsert.find_level ?position entry entry.desc  in f



(*************************************************************************)
(** utilities for parsing *)      
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) ( "./" :: !Configf.include_dirs )) ^ file
      with | Not_found -> file  in
    let ch = open_in file in
    let st = Streamf.of_channel ch in
      parse entry (Locf.mk file) st
    

let parse_string_of_entry ?(loc=Locf.mk "<string>") entry  s =
  try parse_string entry  ~loc s  with
    Locf.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      Locf.error_report (loc,s);
      Locf.raise loc e ;
  end

let wrap_stream_parser ?(loc=Locf.mk "<stream>") p s =
  try p ~loc s
  with Locf.Exc_located(loc,e) -> begin
    eprintf "error: %s" (Locf.to_string loc) ;
    Locf.raise loc e;
  end 











(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramf.cmo" *)
(* end: *)
