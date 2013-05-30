open FAst
  
val regexp_for_string : string -> LexSyntax.regular_expression
val named_regexps : (string, LexSyntax.regular_expression) LibUtil.Hashtbl.t
val remove_as : LexSyntax.regular_expression -> LexSyntax.regular_expression
val as_cset : LexSyntax.regular_expression -> Fcset.t
val regexp : LexSyntax.regular_expression Fgram.t
val char_class : Fcset.t Fgram.t
val char_class1 : Fcset.t Fgram.t
val lex : exp Fgram.t
val declare_regexp : stru Fgram.t

