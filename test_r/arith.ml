



let arith: Gram.t int = Gram.mk "arith" ;
let eoi_arith: Gram.t int = Gram.mk "eoi_arith";
  
EXTEND Gram GLOBAL:arith eoi_arith;
  arith:
  [ "plus"
  [ SELF{x}; "+"; SELF{y} -> x + y
  | SELF{x}; "-"; SELF{y} -> x - y]    
  |"simple"
  [ `INT(x,_) -> x ]]
  eoi_arith:
  [ [arith{x};";"->x]]  
  END  ;

let _ = begin 
  print_int (Gram.parse eoi_arith FanLoc.ghost (Stream.of_channel stdin));
  prerr_endline "finished."
end;


















