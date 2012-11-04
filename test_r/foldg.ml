
let arith :  Gram.t int (* Ast.expr *) = Gram.mk "arith";
  
EXTEND Gram
    arith:
    [[ `INT(x,_) ->  x ]
    |[ "sum";  FOLD0 ( (+) ) (0)  SELF SEP ";" {e};
       "end" -> e]]
  END;

print_int (Gram.parse_string arith FanLoc.string_loc "sum 3 ; 2; 4 end");
