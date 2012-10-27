class type a = object
  method v : int
end
class type b = object ('b)
  method new_x:int-> 'b
end
      
class point = object(self:'self)
end

class a = object end


      
class ['a] circle (c : 'a) = object
  constraint 'a = #point
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = center#move
end
  
let f x = object
    method x = print_int x
end

class ['a,'b] f (v:'a) (u:'b) = object
  method x = v
  method y = u
end


class ['a,'b] f ~v:(v:'a) ~u:u = object
  method x = v
  method y :'b= u
end;;


(* module type S = sig *)
(*   class ['a, 'b] f : 'a -> 'b -> object *)
(*     method x : 'a method y : 'b *)
(*   end *)
(* end *)

(* module type S = sig *)
(*   class ['a, 'b] f : f:'a -> ?g:'b -> object *)
(*     method x : 'a method y : 'b *)
(*   end *)
(*   class a : ?f:int -> g:'a -> object *)
(*   end *)
(* end;; *)


(* class ['a, 'b] f : v:'a -> u:'b -> object method x : 'a method y : 'b end;; *)
class a ?(f=3) ~g:g0 =object
end;;

class a :  ?f:int -> object
  method x:int
end = fun ?(f=3)  -> object
  method x = f
end;;


(* class a ?(f=3) : object *)
(*   method x:int *)
(* end = object *)
(*   method x = f *)
(* end;; *)


(* class a f : object *)
(*   method x:int *)
(* end = object *)
(*   method x = f *)
(* end;; *)

(* class ['a] f = object *)
(* end *)

      


















