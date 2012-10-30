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



class map =
  object (o : 'self_type)
    method string : string  -> string = o#unknown
    method list :
        'a_out 'a .
        ('self_type -> 'a -> 'a_out) -> 'a list  -> 'a_out list =
          fun _f_a ->
            function
              | [] -> []
              | _x::_x_i1 ->
                  let _x = _f_a o _x in
                  let _x_i1 = (o#list) _f_a _x_i1 in _x::_x_i1
  end
                                                           










