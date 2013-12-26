type desc = {
    quot_opt : Tokenf.quot option;
    tokens_opt : Tokenf.txt list option;
    loc : Locf.t
  }    

val named_cases :
    (string, desc -> (Translate_lex.concrete_regexp * Astf.exp) list)
    Hashtblf.t


val named_regexps :
    (string, Translate_lex.concrete_regexp) Hashtblf.t
