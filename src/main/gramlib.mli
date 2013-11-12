
val lex_string : Locf.t -> string -> Tokenf.stream
val parse_string :
  ?lexer:(Locf.t -> char Streamf.t -> Tokenf.stream) ->
  ?loc:Locf.t -> 'a Gramf.t -> string -> 'a

val parse : 'a Gramf.t -> Locf.t -> char Streamf.t -> 'a

val token_stream_of_string : string -> Tokenf.stream

val parse_include_file : 'a Gramf.t -> string -> 'a

val parse_string_of_entry : ?loc:Locf.t -> 'a Gramf.t -> string -> 'a
val eoi_entry : 'a Gramf.t -> 'a Gramf.t


