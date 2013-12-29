
open Astf  
let ant x  = x ;;
 %fans{
keep off;
derive(Strip);};;
%ocaml{%include{ "../common/astf.mli"};;  };;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/strip.cmo" *)
(* end: *)
