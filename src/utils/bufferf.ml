
include Buffer

let (+>) buf chr = begin Buffer.add_char buf chr; buf end

let (+>>) buf str = begin Buffer.add_string buf str; buf end  


(* local variables: *)
(* compile-command: "pmake bufferf.cmo" *)
(* end: *)
