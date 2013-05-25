open FAst
  
val regexp_for_string : string -> LexSyntax.regular_expression
val named_regexps : (string, LexSyntax.regular_expression) LibUtil.Hashtbl.t
val remove_as : LexSyntax.regular_expression -> LexSyntax.regular_expression
val as_cset : LexSyntax.regular_expression -> Cset.t
val regexp : LexSyntax.regular_expression Gram.t
val char_class : Cset.t Gram.t
val char_class1 : Cset.t Gram.t
val lex : exp Gram.t
val declare_regexp : stru Gram.t

