


type  t     = Obj.t   
let mk :'a -> t   = Obj.repr
let get: t -> 'a  = Obj.obj 
let getf: t-> 'a -> 'b  = Obj.obj 
let getf2: t -> 'a -> 'b -> 'c = Obj.obj 

