
val lex_string : Locf.t -> string -> Tokenf.stream
    
val parse_string :
  ?lexer:Tokenf.stream Tokenf.lex
  ->
    ?loc:Locf.t -> 'a Gramf.t -> string -> 'a

val parse_string_eoi :
  ?lexer:Tokenf.stream Tokenf.lex
  ->
    ?loc:Locf.t -> 'a Gramf.t -> string -> 'a

val parse :
    ?lexer: Tokenf.stream Tokenf.lex 
    ->
      'a Gramf.t -> 'a Tokenf.lex

val token_stream_of_string : string -> Tokenf.stream

val parse_include_file : 'a Gramf.t -> string -> 'a

val parse_string_of_entry : ?loc:Locf.t -> 'a Gramf.t -> string -> 'a
    
(* val eoi_entry : 'a Gramf.t -> 'a Gramf.t *)


