open Format
module type S = sig
  val  v : int 
end

let plugins : (string, (module S)) Hashtbl.t =  Hashtbl.create 40



















