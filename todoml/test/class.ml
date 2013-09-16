class type a = object
    method x:int
end

let v : #a = object
    method x = 3
end


let v : a = object
    method x = 3
end

let c = 3;;
type b    = x:int  -> a

(* class type virtual c =  object *)
(*     method x : int *)
(* end *)


class d ~x = object
    method y = (x+1) 
end

let v = new d    
class virtual c (z:int) = object
  method x = z
end


class type h = object
    inherit object method x:int end 
end
    
