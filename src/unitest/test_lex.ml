
(* -*- mode:caml -*- *)
let paths = [
  "utils";
  "common";
  "treeparser";
  "main_annot";
  "+compiler-libs"
]

let () =
  List.iter Topdirs.dir_directory paths
;;

#load "ocamlcommon.cma";;
#load "dynlink.cma";;
#load "libutils.cma";;
#load "libcommon.cma";;
#load "libtreeparser.cma";;
#load "liblex.cma"

(* local variables: *)
(* compile-command: "ocaml -init test_lex.ml" *)
(* end: *)
