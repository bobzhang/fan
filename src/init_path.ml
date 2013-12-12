(* -*- mode:caml -*- *)
let paths = [
  "utils";
  "common";
  "treeparser";
  "cold";
  "+compiler-libs"
]

let () =
  List.iter Topdirs.dir_directory paths
;;
