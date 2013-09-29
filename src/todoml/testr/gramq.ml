(* {:extend| (U.U.g:MGram.t) LOCAL: a b; *)
(*   expr: *)
(*   [SELF{(a,(d,f),(b,c),d)};"+";SELF] *)
(* |}; *)



(* let test x =  *)
(*   {:extend| (U.U.g:MGram.t) LOCAL: a b; *)
(*    expr: Before $x *)
(*      [SELF{(a,(d,f),(b,c),d)};"+";SELF] *)
(*   |}; *)

(* {:extend| Gram *)
(*   arith: *)
(*   [ `INT(x,_) ->  x  *)
(*   | "sum";  FOLD0  $y $x  SELF SEP ";" {e}; *)
(*        "end" -> e] *)
(* |}; *)


{:expr|$(int:"32")|};

    
