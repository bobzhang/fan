(* the dependency is there.. to fix the build system *)
(* open Astf *)

%fans{keep off; derive (LocType);};;

%ocaml{ %include{ "../common/astf.mli"};; };;



















(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/astfn.cmo" *)
(* end: *)
