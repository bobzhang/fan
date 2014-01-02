let v = object (x)
  method v = __SELF_OBJ__#v 
end

let v = object (x:ty)
  method v = __SELF_OBJ__#v 
end

__SELF_OBJ__#v    

let z = object (x:ty)
  method v = 
    fun x ->
      __SELF_OBJ__#v 
end

let v = object 
  method v : '__THIS_OBJ_TYPE__ = __THIS_OBJ__#v 
end
