


type  t     = Obj.t   
let mk :'a -> t   = Obj.repr

let get : t -> 'a  = Obj.obj 


let apply : t -> 'a -> 'b = Obj.obj 

let apply2 : t -> 'a -> 'b -> 'c = Obj.obj
    


