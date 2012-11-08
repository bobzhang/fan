
let arith :  Gram.t int (* Ast.expr *) = Gram.mk "arith";

{| Gram  arith:
    [[ `INT(x,_) ->  x ]
    |[ "sum";  FOLD0 ( (+) ) (0)  SELF SEP ";" {e};
       "end" -> e]]
|};

(* print_int (Gram.parse_string arith FanLoc.string_loc "sum 3 ; 2; 4 end"); *)
(* type u = option int; *)
