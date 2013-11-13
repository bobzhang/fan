


%create{b}  ;;

let make_op i = 
%extend{
b :
  [ $key:i@xloc  %{(xloc,i)}]}


(* local variables: *)
(* compile-command: "ocamlc -I ../../common   -I ../../treeparser -I ../../cold -pp ../../fan -c a7.ml" *)
(* end: *)
