open FAst
open Translate_lex


val named_regexps : (string, concrete_regexp) LibUtil.Hashtbl.t

val regexp : concrete_regexp Fgram.t

val char_class : Fcset.t Fgram.t

val char_class1 : Fcset.t Fgram.t

val lex : exp Fgram.t

val declare_regexp : stru Fgram.t

