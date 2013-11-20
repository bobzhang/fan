(** generate [loc_of] function per type
    for example:
    [loc_of (e:exp)]
    [loc_of (p:pat)] will both return the location information *)
%fans{keep off; derive (GenLoc);};;
%ocaml{%include{"../common/astf.mli"};;};;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/ast_loc.cmo" *)
(* end: *)
