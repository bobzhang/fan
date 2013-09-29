class a ?(f=3) ~g:g0 =object
end;;

class a :  ?f:int -> object
  method x:int
end = fun ?(f=3)  -> object
  method x = f
end;;

class a ?(f=3) : object
  method x:int
end = object
  method x = f
end;;


class a f : object
  method x:int
end = object
  method p x = f x
end;;

class ['a] f = object
end

      



class point = object(self:'self)
end
and a = object(self) end
class ['a] circle (c : 'a) = object
  constraint 'a = #point
  val mutable center = c
  method center = center
  method set_center c = center <- c
  method move = center#move
end
and circle (c : 'a) = object
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



                                                           










