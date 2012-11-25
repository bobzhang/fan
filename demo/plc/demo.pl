
(* After "Monty Python and the Holy Grail" *)

witch(X) :- female(X), burns(X).
  
burns(X) :- wooden(X).
  
wooden(X) :- floats(X).
  
floats(X) :- sameweight(duck,X).
  
female(girl).
  
female(guinevere).
  
sameweight(duck,girl). (* by experiment *)
