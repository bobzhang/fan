

<:fan< lang "compile"; >> ;
<<
let open Camlp4.PreCast.Syntax in 
EXTEND Gram  GLOBAL: expr;
  expr: LEVEL "top"
  [ [ "repeat"; e1 = sequence; "until"; e2 = SELF ->
    <:expr< do { .$e1$.; while not .$e2$. do { .$e1$. } } >> ] ] ;
 sequence:
  [ [ el = LIST1 expr_semi -> <:expr< do { .$list:el$. } >> ] ] ;
 expr_semi:
  [ [ e = expr; ";" -> e ] ] ;
 END;
>>;


let i = ref 1 in
  repeat print_int i.val;
    print_endline "";
    incr i;
    until i.val = 10;  
