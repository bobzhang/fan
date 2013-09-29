
open Ast;

type my_patt =
   [= `Id of (loc * ident)
   |`PaApp of (loc * my_patt * my_patt )];

let u (f:my_patt) = (f:>patt);



















