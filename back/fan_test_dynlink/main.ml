open Format
open Dynlink
open Plugins  
let foo_loader = "foo_dynlink.cmo"

let (&) f x = f x    
let () = begin
  try 
    loadfile foo_loader;
    let (module M) = Hashtbl.find plugins "foo_dynlink" in
    print_int M.v
  with
    Dynlink.Error(msg) -> prerr_endline & error_message msg
end 
















