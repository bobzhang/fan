
let () = 
Printast.implementation Format.std_formatter 
  @@ Parse.implementation (Lexing.from_channel stdin)



(* local variables: *)
(* compile-command: "ocamlopt.opt -I +compiler-libs ocamlcommon.cmxa dparsetree.ml -o dparsetree.native" *)
(* end: *)
