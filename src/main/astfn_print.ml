open StdFan
open Astfn

%fans{keep off;
      derive (Print OPrint PrintWrapper);
    };;
%ocaml{ %include{ "astfn.ml" };; };;


(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/astfn_print.cmo" *)
(* end: *)
