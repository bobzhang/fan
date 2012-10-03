open Format

(*  
type encoder
type encoder_handler = {
    create : unit -> encoder;
    encode : encoder -> float array array -> string
  }
type handler = { mutable encoder_handler : encoder_handler option }
let handler = { encoder_handler = None }
*)
open Plugins
let () = begin
  printf "plugins loaded...";
  Hashtbl.add plugins "foo_dynlink"
    (module struct let v = 3 end)
end















