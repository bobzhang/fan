

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
   "functor";   "private";   "sig";
   "include";   "exception";   "inherit";
   "and";    "when";   "then";
   "initializer";   "in" ;   "downto";
   "as"; "function";   "begin";
   "class";   "do";   "end";
   "assert";   "external";   "virtual";
   "to";   "try";   "struct";
   "else";   "val" ;   "constraint";
   "type";   "new";   "of";
   "done";   "for";   "fun";
   "method" ;   "mutable";   "lazy";
   "with";   "if";   "while" ;
   "rec";   "object";   "or";
   "match" ;   "open";   "module";   "let";
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



  
let debug_origin_token_stream (entry:'a t ) tokens : 'a =
  parse_origin_tokens entry tokens
  
let debug_filtered_token_stream entry tokens =
  filter_and_parse_tokens entry tokens 

(* (\* with a special exception handler *\)   *)
(* let parse_string_safe ?(loc=Locf.string_loc) entry  s = *)
(*   try *)
(*     parse_string entry ~loc s *)
(*   with Locf.Exc_located(loc, e) -> begin *)
(*       eprintf "%s" (Printexc.to_string e); *)
(*       Locf.error_report (loc,s); *)
(*       Locf.raise loc e ; *)
(*   end  *)
(* ;; *)
    
let wrap_stream_parser ?(loc=Locf.mk "<stream>") p s =
  try p ~loc s
  with Locf.Exc_located(loc,e) -> begin
    eprintf "error: %s" (Locf.to_string loc) ;
    Locf.raise loc e;
  end 











(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gramf.cmo" *)
(* end: *)
