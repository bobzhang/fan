let v = object (x)
  method v = __SELF_OBJ__#v 
end

let v = object (x:ty)
  method v = __SELF_OBJ__#v 
end
