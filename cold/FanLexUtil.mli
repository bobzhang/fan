val lexing_store : char XStream.t -> string -> int -> int
val from_context :
  FanLexer.context -> ([> FanToken.token ] * FanLoc.t) XStream.t
val from_lexbuf :
  ?quotations:bool ->
  Lexing.lexbuf -> ([> FanToken.token ] * FanLoc.t) XStream.t
val setup_loc : Lexing.lexbuf -> FanLoc.t -> unit
val from_string :
  ?quotations:bool ->
  FanLoc.t -> string -> ([> FanToken.token ] * FanLoc.t) XStream.t
val from_stream :
  ?quotations:bool ->
  FanLoc.t ->
  char XStream.t -> ([> FanToken.token ] * FanLoc.t) XStream.t
val mk :
  unit ->
  FanLoc.t ->
  char XStream.t -> ([> FanToken.token ] * FanLoc.t) XStream.t
val clean :
  (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t
val strict_clean :
  (([> `EOI ] as 'a) * 'b) XStream.t -> ('a * 'b) XStream.t
val debug_from_string : ?quotations:bool -> string -> unit
val debug_from_file : ?quotations:bool -> string -> unit
