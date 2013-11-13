


%create{b}  ;;

let make_op i = 
%extend{
b :
  [Inf@xloc ($i, op) %{(xloc,i,op)}]}


(* local variables: *)
(* compile-command: "ocamlc -I ../../common   -I ../../treeparser -I ../../cold -pp ../../fan -c a6.ml" *)
(* end: *)
