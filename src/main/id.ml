


  

let x ?(off=0) (i:int)    =
  if off > 25 then invalid_arg "unsupported offset in x "
  else
    let base = Char.(code 'a' + off |> chr) in
    "_"^ Stringf.of_char base ^ string_of_int i
    
let xid ?(off=0) (i:int)   = `Lid(x ~off i)
let allx ?(off=0) i =  "all" ^x ~off i 
let allxid ?(off=0) i = `Lid (allx ~off i)




  

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/id.cmo" *)
(* end: *)
