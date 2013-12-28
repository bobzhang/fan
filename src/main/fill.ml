open Astfn

(** this is caused by #ant problem, which requires [fill_ant] to be
    specifiied *)
let ant _loc x = x;;

%fans{keep off;
      derive
        (Fill);};;

%ocaml{ %include{ "astfn.ml" };; };;



(* local variables: *)
(* compile-command: "cd .. && pmake  main_annot/fill.cmo" *)
(* end: *)
