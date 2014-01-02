
open StdFan

open Astfn;;

%fans{keep off;
      derive (Fold);
};;


%ocaml{ %include{ "astfn.ml" };; };;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/astfn_fold.cmo" *)
(* end: *)
