open Format
open Dynlink

let () = try
 print_endline "init";
 Dynlink.init ();
 print_endline "allowing unsafe modules";
 Dynlink.allow_unsafe_modules true;
 Dynlink.loadfile "/home1/h/hongboz/ocaml/lib/ocaml/stdlib.cma";
 Dynlink.loadfile
    "/home1/h/hongboz/ocaml/lib/ocaml/compiler-libs/ocamlcommon.cma";
 print_endline "loading toplevellib.cma";
 Dynlink.loadfile
    "/home1/h/hongboz/ocaml/lib/ocaml/compiler-libs/ocamlbytecomp.cma";

 Dynlink.loadfile
    "/home1/h/hongboz/ocaml/lib/ocaml/compiler-libs/ocamltoplevel.cma";
 Dynlink.loadfile
    "/home1/h/hongboz/ocaml/lib/ocaml/compiler-libs/topstart.cmo";

 (* Dynlink.loadfile "/usr/local/lib/ocaml/topstart.cmo"; *)
 (* now the ocaml toplevel should be running *)
with Dynlink.Error error -> print_endline (Dynlink.error_message error);;

















