open FAst
open Translate_lex


val regexp_for_string : string -> concrete_regexp

val named_regexps : (string, concrete_regexp) LibUtil.Hashtbl.t

val remove_as : concrete_regexp ->concrete_regexp

val as_cset : concrete_regexp -> Fcset.t

val regexp : concrete_regexp Fgram.t

val char_class : Fcset.t Fgram.t

val char_class1 : Fcset.t Fgram.t

val lex : exp Fgram.t

val declare_regexp : stru Fgram.t

