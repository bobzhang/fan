

open Camlp4.PreCast;

value arith = Gram.Entry.mk "arith" ;

EXTEND Gram GLOBAL:arith;
  arith:
  [ "plus"
  [ x=SELF; "+"; y=SELF -> x + y
  | x=SELF; "-"; y=SELF -> x - y]    
  |"simple"
  [ `INT(x,_) -> x ]];
  END  ;

value eoi_arith= Gram.Entry.mk "simple_arith";

(* idea,
   LOCAL bla bla bla
   EXTEND simple_arith:[[x=arith;";" -> x] ]; END
 *)  

EXTEND Gram GLOBAL:eoi_arith;
   eoi_arith:[[x=arith;";" -> x]];
END;
  
begin 
  print_int (Gram.parse eoi_arith Loc.ghost (Stream.of_channel stdin));
  prerr_endline "finished."
end;






































