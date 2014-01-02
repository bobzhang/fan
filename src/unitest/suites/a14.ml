let v = object 
  method v : '__THIS_OBJ_TYPE__ =
    begin
      ignore(__THIS_OBJ__#v);
      __THIS_OBJ__
    end
end
