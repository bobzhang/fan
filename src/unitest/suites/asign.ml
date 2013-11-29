
(* %exp{u.x<-v} *)
 (* let f u v =  *)
 (*  x <- v  *)
(* Pexp_setinstvar "x" Pexp_ident "v" *)

let f u v = 
   f.x <- v 

(* Pexp_setfield *)
(* expression (asign.ml[8,111+3]..[8,111+4]) *)
(*   Pexp_ident "f" (asign.ml[8,111+3]..[8,111+4]) *)
(* "x" (asign.ml[8,111+5]..[8,111+6]) *)
(* expression (asign.ml[8,111+10]..[8,111+11]) *)
(*   Pexp_ident "v" (asign.ml[8,111+10]..[8,111+11]) *)
       
(* local variables: *)
(* compile-command: "ocamlc.opt -dparsetree -c asign.ml" *)
(* end: *)
