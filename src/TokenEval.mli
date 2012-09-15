value char : string -> char;
 (** Convert a char token, where the escape sequences (backslashes)
          remain to be interpreted; raise [Failure] if an
          incorrect backslash sequence is found; [Token.Eval.char (Char.escaped c)]
          returns [c] *)

value string : ?strict:unit -> string -> string;
  (** [Taken.Eval.string strict s]
      Convert a string token, where the escape sequences (backslashes)
      remain to be interpreted; raise [Failure] if [strict] and an
      incorrect backslash sequence is found;
      [Token.Eval.string strict (String.escaped s)] returns [s] *)
