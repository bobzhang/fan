

    

val parse_string_eoi :
  ?lexer:Tokenf.stream Tokenf.lex
  ->
    ?loc:Locf.t -> 'a Gramf.t -> string -> 'a

val parse :
    ?lexer: Tokenf.stream Tokenf.lex 
    ->
      'a Gramf.t -> 'a Tokenf.lex



val parse_include_file : 'a Gramf.t -> string -> 'a


    



