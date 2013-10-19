%import{
Gram_gen:
  gm
  grammar_module_name
  text_of_functorial_extend
  mk_name
  mk_entry
  mk_level
  retype_rule_list_without_patterns
  mk_rule
  check_not_tok
  mk_slist
  mk_symbol
  token_of_simple_pat
  ;

Ast_gen:
  sem_of_list
  loc_of
  seq_sem
  tuple_com
  ;
}

open FAst
open Util

let g =
  Fgram.create_lexer ~annot:"Grammar's lexer"
    ~keywords:["`";"("; ")" ; ","; "as"; "|"; "_"; ":";
               "."; ";"; "{"; "}"; "let";"[";"]";
               "SEP";"LEVEL"; "S";
               "EOI"; "Lid";"Uid";
               "Ant";"Quot";
               "DirQuotation";
               "Str";
               "Label";
               "Optlabel";
               "Chr";
               "Int";
               "Int32";
               "Int64";
               "Int64";
               "Nativeint";
               "Flo"
             ]
    ();;


%create{(g:Fgram.t)
   extend_header
   (qualuid : vid Fgram.t)
   (qualid:vid Fgram.t)
   (t_qualid:vid Fgram.t )
   (entry_name : ([`name of Ftoken.name option | `non] * Gram_def.name) Fgram.t )
    entry position assoc name string rules
    symbol rule meta_rule rule_list psymbol level level_list
   (entry: Gram_def.entry Fgram.t)
   extend_body
   unsafe_extend_body

   (simple : Gram_pat.t Fgram.t)
}


type words =
  | A of string list
  | Any
      
type data = {
    tag : string;
    words : words;
  }
      
%extend{(g:Fgram.t)
  simple :
  [ "`"; "EOI" %{{tag = "EOI"; words = A [] }}
  | "`"; "Lid"; `Str v %{{tag = "Lid"; words = A [v]}}
  | "`"; "Uid"; `Str v %{{tag = "Uid"; words = A [v]}}      
  | "`"; "Lid" ; `Lid x %{{tag = "Lid"; words = Any }}
  | "`"; "Uid" ; `Lid x %{{tag = "Uid"; words = Any } }
  | "`"; "Quot"; `Lid x %{{tag = "Quot"; words = Any} }
  | "`"; "Label"; `Lid x %{{tag = "Label"; words = Any } }      
  | "`"; "DirQuotation"; `Lid x %{{tag = "DirQuotation"; words = Any}}
  | "`"; "Optlabel"; `Lid x %{{tag = "Optlabel"; words = Any}}      
  | "`"; "Str"; `Lid x %{{tag = "Str"; words = Any }}
  | "`"; "Chr"; `Lid x %{{tag = "Chr"; words = Any}}
  | "`"; "Int"; `Lid x %{{tag = "Int"; words = Any}}
  | "`"; "Int32"; `Lid x %{{tag = "Int32"; words = Any}}
  | "`"; "Int64"; `Lid x %{{tag = "Int64"; words = Any}}
  | "`"; "Nativeint"; `Lid x %{{tag = "Nativeint"; words = Any}}
  | "`"; "Flo"; `Lid x %{{tag = "Flo"; words = Any}}
  | "`"; "Lid" ; "_"    %{{tag = "Lid"; words = Any}}
  | "`"; "Uid"; "_" %{{tag = "Uid"; words = Any }}
  | "`"; "Ant"; "("; L1 internal_pat SEP "," {v}; ")" %{
    {tag = "Ant"; words = v  }
    }
  | "`"; "Uid"; "("; L1 internal_pat SEP "," {v}; ")" %{
    {tag = "Uid"; words = v}
  ]
  let internal_pat :
  {
   "as"
     [S{p1} ; "as";`Lid s  %{ p1}
     "|"
     [S{p1}; "|"; S{p2}    %{ p1 @ p2 } ]
     "simple"
     [ `Str s    %pat'{ $str:s}
     | `Lid x      %pat'{ $lid:x}
     ]
 }
}
