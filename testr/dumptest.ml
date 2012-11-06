

module P = MakePreCast.Make (struct end) ;
open P;

Gram.dump Format.std_formatter Syntax.ipatt;
