let v = object 
  method v : '__THIS_OBJ_TYPE__ =
    begin
      ignore(__THIS_OBJ__#v);
      __THIS_OBJ__
    end
end


(* at least -- an error or warning message ..
   the semantics of [__THIS_OBJ__] should not be shadowed ..
   and this would emit an error ideally....
   if  we make __THIS_OBJ__ a keyword?
 *)
let u = object 
  method v : string =
    begin
      ignore(__THIS_OBJ__#v);
      let __THIS_OBJ__ = "xx" in
      __THIS_OBJ__
    end 

let u = object (u)
  method v : ' __THIS_OBJ_TYPE__ =
    3
end
class fold =  object(x)
  method x : '__THIS_OBJ_TYPE__ =
    "ghos"
end
