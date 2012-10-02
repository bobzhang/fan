
(**
   ocaml 3.12.1 can not type this program
   
 *)

type o = [A of  [= `Int] ]
;


type twice 'a = 'a
;

class map = object ((self : 's))
  method twice : ! 'a0 'b0. ('s -> 'a0 -> 'b0)
    -> twice 'a0 ->  twice 'b0=
    fun mf_a x -> mf_a (self : 's)  (x:  twice 'a0);
end
;


















