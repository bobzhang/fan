open FAst
open Automata_def
  
val regexp_for_string : string -> regular_expression
val named_regexps : (string, regular_expression) LibUtil.Hashtbl.t
val remove_as : regular_expression ->regular_expression
val as_cset : regular_expression -> Fcset.t
val regexp : regular_expression Fgram.t
val char_class : Fcset.t Fgram.t
val char_class1 : Fcset.t Fgram.t
val lex : exp Fgram.t
val declare_regexp : stru Fgram.t

