
(** Utilities for char and string escaping *)
val char : string -> char
 (** Convert a char token, where the escape sequences (backslashes)
          remain to be interpreted; raise [Failure] if an
          incorrect backslash sequence is found; [Token.Eval.char (Char.escaped c)]
          returns [c] *)

val string : ?strict:unit -> string -> string
  (** [Taken.Eval.string strict s]
      Convert a string token, where the escape sequences (backslashes)
      remain to be interpreted; raise [Failure] if [strict] and an
      incorrect backslash sequence is found;
      [Token.Eval.string strict (String.escaped s)] returns [s] *)


val char_of_char_token : FLoc.t -> string -> char
(** see [char], raise an location aware exception *)

val string_of_string_token : FLoc.t -> string -> string
(** see [string], raise an location aware exception*)    
